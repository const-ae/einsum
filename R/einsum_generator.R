
#' @rdname einsum
#' @export
einsum_generator <- function(equation_string, compile_function = TRUE){
  parsed <- parse_equation(equation_string)
  strings <- parsed$strings
  string_vec <- parsed$string_vec
  result_string_vec <- parsed$result_string_vec
  all_vars <- parsed$all_vars

  array_vars_list <- lapply(string_vec, function(st){
    vapply(st, function(s) which(all_vars == s) - 1L, FUN.VALUE = 0L)
  })
  result_vars_vec <-  vapply(result_string_vec, function(s) which(all_vars == s) - 1L, FUN.VALUE = 0L)
  not_result_vars_vec <- setdiff(seq_along(all_vars) - 1L, result_vars_vec)

  variable_names <- paste0("array", seq_along(array_vars_list))

  # --- Function declaration ---
  method_decl <- glue::glue('NumericVector einsum_impl_func(',
                            paste0("NumericVector ", variable_names, collapse = ", "),
                            "){{")

  # --- Size vector and dimension validation (shared by both paths) ---
  size_vec <- glue::glue('NumericVector size({length(all_vars)});\n')
  variable_dim_names <- paste0(variable_names, "_dim")
  for(idx in seq_along(variable_names)){
    size_vec <- paste0(size_vec, "\nIntegerVector ", variable_dim_names[idx], " = ", variable_names[idx], '.hasAttribute("dim") ? ',
                       variable_names[idx], '.attr("dim") : IntegerVector::create(', variable_names[idx], '.length());')
  }
  written <- rep(FALSE, length(all_vars))
  for(idx1 in seq_along(all_vars)){
    for(idx2 in seq_along(array_vars_list)){
      for(idx3 in which(all_vars[idx1] == names(array_vars_list[[idx2]]))){
        if(! written[idx1]){
          size_vec <- paste0(size_vec, "\n", paste0("size[",idx1-1, "] = ", variable_dim_names[idx2], '[', idx3 - 1, "];" ))
          written[idx1] <- TRUE
        }else{
          size_vec <- paste0(size_vec, "\nif(", paste0("size[",idx1-1, "] != ", variable_dim_names[idx2], '[', idx3 - 1, "])"),
                             ' stop("Dimension ',  idx3,' of object ', variable_names[idx2],' does not match!");')
        }
      }
    }
  }

  # --- Computation code ---
  # Try pairwise code generation for 3+ tensors without repeated indices.
  # For ≤2 tensors, repeated indices, or if pairwise path encounters
  # unsupported patterns (e.g. batch dimensions), fall back to the
  # original single-loop-nest code generation.
  use_pairwise <- can_use_pairwise(string_vec) && length(strings) >= 3
  computation_code <- NULL

  if(use_pairwise) {
    dummy_lengths <- rep(10, length(all_vars))
    names(dummy_lengths) <- all_vars
    path <- plan_contraction_path(string_vec, result_string_vec, dummy_lengths)
    computation_code <- generate_pairwise_cpp(
      path, string_vec, variable_names, result_string_vec, all_vars
    )
  }

  if(!is.null(computation_code)) {
    code <- paste0(method_decl, "\n", size_vec, "\n\n", computation_code, "\n}")
  } else {
    # --- Original single-loop-nest code generation ---
    if(length(result_vars_vec) == 0){
      for_loop_string <- glue::glue('NumericVector result(1);')
    }else{
      for_loop_string <- glue::glue('NumericVector result({paste0("size[", result_vars_vec, "]", collapse = " * ")});')
    }
    for(res_var in result_vars_vec){
      for_loop_string <- glue::glue('|{for_loop_string}
                                     |for(int {all_vars[res_var +1]} = 0; {all_vars[res_var+1]} < size[{res_var}]; ++{all_vars[res_var+1]}){{')

    }
    for_loop_string <- glue::glue('|{for_loop_string}
                                   |double sum = 0.0;')
    for(sum_var in not_result_vars_vec){
      for_loop_string <- glue::glue('|{for_loop_string}
                                     |for(int {all_vars[sum_var +1]} = 0; {all_vars[sum_var+1]} < size[{sum_var}]; ++{all_vars[sum_var+1]}){{')
    }
    prod_string <- paste(sapply(seq_along(variable_names), function(idx){
      summands <-  paste0(c("1", glue::glue('size[{head(array_vars_list[[idx]],n=length(array_vars_list[[idx]])-1)}]')),
                            " * (", all_vars[array_vars_list[[idx]] + 1])
      glue::glue('{variable_names[idx]}[', paste0(paste0(summands, collapse = " + "), paste0(rep(")", times = length(array_vars_list[[idx]])), collapse = "")), "]")
    }), collapse = " * ")
    for_loop_string <- glue::glue('|{for_loop_string}
                                   |sum += {prod_string};')

    for_loop_string <- glue::glue('|{for_loop_string}
                                   |{paste0(rep("}", times = length(not_result_vars_vec)), collapse = "\n")}')
    if(length(result_vars_vec) == 0){
      assignment_lhs <- 'result[0]'
    }else{
      summands <-  paste0(c("1", glue::glue('size[{head(result_vars_vec,n=length(result_vars_vec)-1)}]')),
                          " * (", all_vars[result_vars_vec + 1])
      assignment_lhs <- glue::glue('result[', paste0(paste0(summands, collapse = " + "), paste0(rep(")", times = length(result_vars_vec)), collapse = "")), "]")
    }
    for_loop_string <- glue::glue('|{for_loop_string}
                                   |{assignment_lhs} = sum;')
    for_loop_string <- glue::glue('|{for_loop_string}
                                   |{paste0(rep("}", times = length(result_vars_vec)), collapse = "\n")}
                                   |result.attr("dim") = IntegerVector::create({ if(length(result_vars_vec) == 0) 1 else paste0("size[", result_vars_vec ,"]", collapse = ",")});
                                   |return result;')
    code <- paste0(method_decl, "\n", size_vec, "\n", gsub(pattern = "\\s*\\|+", "\n",  for_loop_string), "\n\n}")
  }

  if(! compile_function){
    code
  }else{
    Rcpp::cppFunction(code)
  }
}


# Generate C++ code for a sequence of pairwise contraction steps.
generate_pairwise_cpp <- function(path, string_vec, variable_names,
                                  result_string_vec, all_vars) {
  current_names <- variable_names
  current_string_vec <- string_vec
  temp_count <- 0L
  code <- ""

  for(step_idx in seq_along(path)) {
    step <- path[[step_idx]]
    i <- step$i
    j <- step$j

    name1 <- current_names[i]
    name2 <- current_names[j]
    chars1 <- current_string_vec[[i]]
    chars2 <- current_string_vec[[j]]

    is_last <- (step_idx == length(path))
    if(is_last) {
      out_name <- "result"
      out_chars <- result_string_vec
    } else {
      temp_count <- temp_count + 1L
      out_name <- paste0("temp", temp_count)
      out_chars <- step$result_chars
    }

    code <- paste0(code, generate_single_contraction_cpp(
      name1, chars1, name2, chars2, out_name, out_chars, all_vars
    ))

    current_names <- c(current_names[-c(i, j)], out_name)
    current_string_vec <- c(current_string_vec[-c(i, j)], list(out_chars))
  }

  # dim attribute and return
  if(length(result_string_vec) == 0) {
    dim_expr <- "1"
  } else {
    var_indices <- vapply(result_string_vec, function(ch) which(all_vars == ch) - 1L, 0L)
    dim_expr <- paste0("size[", var_indices, "]", collapse = ", ")
  }
  code <- paste0(code, 'result.attr("dim") = IntegerVector::create(', dim_expr, ');\n')
  code <- paste0(code, "return result;\n")
  code
}


# Generate C++ code for a single two-tensor contraction step.
generate_single_contraction_cpp <- function(name1, chars1, name2, chars2,
                                            result_name, result_chars, all_vars) {
  all_local <- unique(c(chars1, chars2))
  contracted <- setdiff(all_local, result_chars)

  # Column-major flat index expression: 1 * (c1 + size[s1] * (c2 + size[s2] * (c3)))
  make_index_expr <- function(chars) {
    if(length(chars) == 0) return("0")
    if(length(chars) == 1) return(chars)
    var_indices <- vapply(chars, function(ch) which(all_vars == ch) - 1L, 0L)
    summands <- paste0(
      c("1", paste0("size[", head(var_indices, -1), "]")),
      " * (", chars
    )
    paste0(paste0(summands, collapse = " + "),
           paste0(rep(")", length(chars)), collapse = ""))
  }

  # Result array declaration
  if(length(result_chars) == 0) {
    code <- paste0("NumericVector ", result_name, "(1);\n")
  } else {
    var_indices <- vapply(result_chars, function(ch) which(all_vars == ch) - 1L, 0L)
    size_expr <- paste0("size[", var_indices, "]", collapse = " * ")
    code <- paste0("NumericVector ", result_name, "(", size_expr, ");\n")
  }

  # Outer loops (result indices)
  for(rc in result_chars) {
    vi <- which(all_vars == rc) - 1L
    code <- paste0(code, "for(int ", rc, " = 0; ", rc, " < size[", vi, "]; ++", rc, "){\n")
  }

  code <- paste0(code, "double sum = 0.0;\n")

  # Inner loops (contracted indices)
  for(cc in contracted) {
    vi <- which(all_vars == cc) - 1L
    code <- paste0(code, "for(int ", cc, " = 0; ", cc, " < size[", vi, "]; ++", cc, "){\n")
  }

  # Product
  idx1 <- make_index_expr(chars1)
  idx2 <- make_index_expr(chars2)
  code <- paste0(code, "sum += ", name1, "[", idx1, "] * ", name2, "[", idx2, "];\n")

  # Close inner loops
  code <- paste0(code, paste0(rep("}\n", length(contracted)), collapse = ""))

  # Assignment
  if(length(result_chars) == 0) {
    code <- paste0(code, result_name, "[0] = sum;\n")
  } else {
    code <- paste0(code, result_name, "[", make_index_expr(result_chars), "] = sum;\n")
  }

  # Close outer loops
  code <- paste0(code, paste0(rep("}\n", length(result_chars)), collapse = ""))

  code
}

