
#' @rdname einsum
#' @export
einsum_generator <- function(equation_string, compile_function = TRUE){
  equation_string <- gsub("\\s", "", equation_string)

  if(length(grep("->", equation_string)) == 0){
    stop("The 'equation_string' must contain `->`: ", equation_string)
  }

  tmp <- strsplit(equation_string, "->")[[1]]
  result_string <- if(length(tmp) == 1) ""
  else if(length(tmp) == 2) tmp[2]
  else stop("the equation string contains more than one '->': ", equation_string)
  lhs_strings <- tmp[1]
  strings <- unlist(strsplit(lhs_strings, ","))
  if(any(grepl("[^a-zA-Z]", strings)) || grepl("[^a-zA-Z]", result_string)) stop("'equation_string' contains a non alphabetical (a-z and A-Z) character.")

  result_string_vec <- strsplit(result_string, "")[[1]]
  string_vec <- strsplit(strings, "")


  all_vars <- sort(unique(unlist(string_vec)))
  if(! all(result_string_vec %in% all_vars)){
    missing_result_indices <- setdiff(result_string_vec, all_vars)
    stop("The result contains indices (", paste0(missing_result_indices, collapse = ", "), ") which are not on the left-hand side: ", equation_string)
  }
  array_vars_list <- lapply(string_vec, function(st){
    vapply(st, function(s) which(all_vars == s) - 1L, FUN.VALUE = 0L)
  })
  result_vars_vec <-  vapply(result_string_vec, function(s) which(all_vars == s) - 1L, FUN.VALUE = 0L)
  not_result_vars_vec <- setdiff(seq_along(all_vars) - 1L, result_vars_vec)



  # NumericVector res(I * K);
  # res.attr("dim") = IntegerVector::create(I, K);
  # for(int i = 0; i < I; i++){
  #   for(int k = 0; k < K; k++){
  #     double sum = 0.0;
  #     for(int j = 0; j < J; j++){
  #       sum += mat1[j * I + i] * mat2[k * J + j];
  #     }
  #     res[k * I + i] = sum;
  #   }
  # }
  # return res;

  variable_names <- paste0("array", seq_along(array_vars_list))
  # index_names <- unique(unlist(strsplit(strings, "")))

  method_decl <- glue::glue('NumericVector einsum_impl_func(',
                            paste0("NumericVector ", variable_names, collapse = ", "),
                            "){{")

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
  prod_string <- "sum += "
  for(sum_var in not_result_vars_vec){
    for_loop_string <- glue::glue('|{for_loop_string}
                                   |for(int {all_vars[sum_var +1]} = 0; {all_vars[sum_var+1]} < size[{sum_var}]; ++{all_vars[sum_var+1]}){{')
  }
  prod_string <- paste(sapply(seq_along(variable_names), function(idx){
    # print("THis should look like this: sum += array1[i*1 + j*size[1] + b*size[2]*size[1]] * array2[i*1 + j*size[1] + b*size[2]*size[1]];!!!!!!!")
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
    # assignment_lhs <- glue::glue('result[', paste0(all_vars[result_vars_vec + 1], "*",  c("1", glue::glue('size[{head(result_vars_vec,n=length(result_vars_vec)-1)}]')), collapse = " + "), "]")
  }
  for_loop_string <- glue::glue('|{for_loop_string}
                                 |{assignment_lhs} = sum;')
  for_loop_string <- glue::glue('|{for_loop_string}
                                 |{paste0(rep("}", times = length(result_vars_vec)), collapse = "\n")}
                                 |result.attr("dim") = IntegerVector::create({ if(length(result_vars_vec) == 0) 1 else paste0("size[", result_vars_vec ,"]", collapse = ",")});
                                 |return result;')
  code <- paste0(method_decl, "\n", size_vec, "\n", gsub(pattern = "\\s*\\|+", "\n",  for_loop_string), "\n\n}")
  if(! compile_function){
    code
  }else{
    Rcpp::cppFunction(code)
  }
}

