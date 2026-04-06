
# Shared equation parsing and contraction path planning for einsum() and
# einsum_generator().

parse_equation <- function(equation_string) {
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
  if(any(grepl("[^a-zA-Z]", strings)) || grepl("[^a-zA-Z]", result_string))
    stop("'equation_string' contains a non alphabetical (a-z and A-Z) character.")
  result_string_vec <- strsplit(result_string, "")[[1]]
  string_vec <- strsplit(strings, "")
  all_vars <- sort(unique(unlist(string_vec)))
  if(! all(result_string_vec %in% all_vars)){
    missing_result_indices <- setdiff(result_string_vec, all_vars)
    stop("The result contains indices (", paste0(missing_result_indices, collapse = ", "),
         ") which are not on the left-hand side: ", equation_string)
  }
  list(
    strings = strings,
    string_vec = string_vec,
    result_string = result_string,
    result_string_vec = result_string_vec,
    all_vars = all_vars
  )
}


# Greedy contraction path: at each step, pick the tensor pair whose
# contraction involves the fewest total elements.
# Returns a list of steps, each containing: i, j (positions in the current
# tensor list), chars_i, chars_j, and result_chars.
plan_contraction_path <- function(string_vec, result_string_vec, lengths_vec) {
  current_string_vec <- string_vec
  steps <- list()

  while(length(current_string_vec) > 1) {
    n <- length(current_string_vec)
    best_cost <- Inf
    best_i <- 1L
    best_j <- 2L
    for(i in 1:(n - 1)) {
      for(j in (i + 1):n) {
        ij_chars <- unique(c(current_string_vec[[i]], current_string_vec[[j]]))
        cost <- prod(lengths_vec[ij_chars])
        if(cost < best_cost) {
          best_cost <- cost
          best_i <- i
          best_j <- j
        }
      }
    }

    i <- best_i
    j <- best_j

    other_chars <- unique(unlist(current_string_vec[-c(i, j)]))
    needed <- union(result_string_vec, other_chars)
    ij_chars <- unique(c(current_string_vec[[i]], current_string_vec[[j]]))
    intermediate_chars <- intersect(ij_chars, needed)

    steps <- c(steps, list(list(
      i = i, j = j,
      chars_i = current_string_vec[[i]],
      chars_j = current_string_vec[[j]],
      result_chars = intermediate_chars
    )))

    current_string_vec <- c(current_string_vec[-c(i, j)], list(intermediate_chars))
  }
  steps
}
