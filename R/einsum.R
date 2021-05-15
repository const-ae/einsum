
#' Einstein Summation
#'

#'
#' @param equation_string a string in Einstein notation where arrays
#'   are separated by ',' and the result is separated by '->'. For
#'   example \code{"ij,jk->ik"} corresponds to a standard matrix multiplication.
#'   Whitespace inside the \code{equation_string} is ignored. Unlike the
#'   equivalent functions in Python, \code{einsum()} only supports the explicit
#'   mode. This means that the \code{equation_string} must contain '->'.
#' @param compile_function boolean that decides if \code{einsum_generator()}
#'   returns the result of \code{Rcpp::cppFunction()} or the program as a
#'   string. Default: \code{TRUE}.
#' @param ... the arrays that are combined. All arguments are converted
#'   to arrays with \code{as.array}.
#'
#' @description
#' \loadmathjax
#' Einstein summation is a convenient and concise notation for operations
#' on n-dimensional arrays.
#'
#'
#' @details
#' The following table show, how the Einstein notation abbreviates complex
#' summation for arrays/matrices:
#'
#' \tabular{lrr}{
#'   \code{equation_string} \tab Formula \tab \cr
#'   ------------------------\tab--------------------------------------\tab----------------------------------\cr
#'   \code{"ij,jk->ik"} \tab \mjseqn{ Y_{ik} = \sum_{j}{A_{ij} B_{jk}}  } \tab Matrix multiplication \cr
#'   \code{"ij->ji"}` \tab \mjseqn{ Y = A^{T}  } \tab Transpose \cr
#'   \code{"ii->i"} \tab \mjeqn{y = \textrm{diag}(A)}{y = diag(A)} \tab Diagonal \cr
#'   \code{"ii->ii"} \tab \mjeqn{Y = \textrm{diag}(A) I}{Y = diag(A) I} \tab Diagonal times Identity  \cr
#'   \code{"ii->"} \tab \mjeqn{y = \textrm{trace}(A) = \sum_i{A_{ii}} }{y = trace(A) = Sum_i(A_{ii})} \tab Trace \cr
#'   \code{"ijk,mjj->i"} \tab \mjeqn{ y_i = \sum_{j}\sum_{k}\sum_{m}A_{ijk}B_{mjj}  }{y_i = Sum_j Sum_k Sum_m A_{ijk} B_{mjj}} \tab Complex 3D operation  \cr
#' }
#'
#'
#' The function and the conventions are inspired by the \code{einsum()} function
#' in NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.einsum.html}{documentation}).
#' Unlike NumPy, 'einsum' only supports the explicit mode. The explicit mode is more flexible and
#' can avoid confusion. The common summary of the Einstein  summation to
#' "sum over duplicated indices" however is not a good mental model. A better rule of thumb is
#' "sum over all indices not in the result".
#'
#' \emph{Note:} \code{einsum()} internally uses C++ code to provide results quickly, the repeated
#' parsing of the \code{equation_string} comes with some overhead. Thus,
#' if you need to do the same calculation over and over again it can be worth to use
#' \code{einsum_generator()} and call the returned the function. \code{einsum_generator()}
#' generates efficient C++ code that can be one or two orders of magnitude faster than
#' \code{einsum()}.
#'
#' @return
#'  The \code{einsum()} function returns an array with one dimension for each index in the result
#'  of the \code{equation_string}. For example \code{"ij,jk->ik"} produces a 2-dimensional array,
#'  \code{"abc,cd,de->abe"} produces a 3-dimensional array.
#'
#'  The \code{einsum_generator()} function returns a function that takes one array for each
#'  comma-separated input in the \code{equation_string} and returns the same result as \code{einsum()}.
#'  Or if \code{compile_function = FALSE}, \code{einsum_generator()} function returns a string with the
#'  C++ code for such a function.
#'
#' @examples
#' mat1 <- matrix(rnorm(n = 4 * 8), nrow = 4, ncol = 8)
#' mat2 <- matrix(rnorm(n = 8 * 3), nrow = 8, ncol = 3)
#'
#' # Matrix Multiply
#' mat1 %*% mat2
#' einsum("ij,jk -> ik", mat1, mat2)
#'
#' # einsum_generator() works just like einsum() but returns a performant function
#' mat_mult <- einsum_generator("ij,jk -> ik")
#' mat_mult(mat1, mat2)
#'
#' # Diag
#' mat_sq <- matrix(rnorm(n = 4 * 4), nrow = 4, ncol = 4)
#' diag(mat_sq)
#' einsum("ii->i", mat_sq)
#' einsum("ii->ii", mat_sq)
#'
#' # Trace
#' sum(diag(mat_sq))
#' einsum("ii->", mat_sq)
#'
#'
#' # Scalar product
#' mat3 <- matrix(rnorm(n = 4 * 8), nrow = 4, ncol = 8)
#' mat3 * mat1
#' einsum("ij,ij->ij", mat3, mat1)
#'
#' # Transpose
#' t(mat1)
#' einsum("ij->ji", mat1)
#'
#'
#' # Batched L2 norm
#' arr1 <- array(c(mat1, mat3), dim = c(dim(mat1), 2))
#' c(sum(mat1^2), sum(mat3^2))
#' einsum("ijb,ijb->b", arr1, arr1)
#'
#' @export
einsum <- function(equation_string, ...){
  arrays <- list(...)
  arrays <- lapply(arrays, as.array)
  equation_string <- gsub("\\s", "", equation_string)

  tmp <- strsplit(equation_string, "->")[[1]]
  result_string <- if(length(tmp) == 1) ""
  else if(length(tmp) == 2) tmp[2]
  else stop("the equation string contains more than one '->': ", equation_string)
  lhs_strings <- tmp[1]
  strings <- unlist(strsplit(lhs_strings, ","))
  if(any(grepl("[^a-zA-Z]", strings)) || grepl("[^a-zA-Z]", result_string)) stop("'equation_string' contains a non alphabetical (a-z and A-Z) character.")

  stopifnot("The number of strings on the left-hand side does not match the number of arrays" =
              length(strings) == length(arrays))
  stopifnot("Number of dimensions of array does not match the number of indices" =
              all(nchar(strings) == vapply(arrays, function(a)length(dim(a)), 0.0)))

  result_string_vec <- strsplit(result_string, "")[[1]]
  string_vec <- strsplit(strings, "")

  # Get the lengths of the indices as a named vector
  lengths_vec <- get_lengths_vec(strings, arrays)
  lengths_vec <- lengths_vec[order(names(lengths_vec))]

  all_vars <- sort(unique(unlist(string_vec)))
  array_vars_list <- lapply(string_vec, function(st){
    vapply(st, function(s) which(all_vars == s) - 1L, FUN.VALUE = 0L)
  })
  result_vars_vec <-  vapply(result_string_vec, function(s) which(all_vars == s) - 1L, FUN.VALUE = 0L)
  not_result_vars_vec <- setdiff(seq_along(all_vars) - 1L, result_vars_vec)

  einsum_impl_fast(lengths_vec, array_vars_list, not_result_vars_vec, result_vars_vec, arrays)

}



get_lengths_vec <- function(strings, objects){
  keys <- unlist(strsplit(strings, ""))
  values <- unlist(lapply(objects, dim))
  lengths <- numeric(length(unique(keys)))
  names(lengths) <- unique(keys)
  for(key in unique(keys)){
    pos_values <- values[which(keys == key)]
    if(any(pos_values != pos_values[1])){
      stop("The length for index '", key, "' differ: ", paste0(unique(pos_values), collapse = ", "), "\n",
           "Dimensions with the same index must have the same length.")
    }
    lengths[key] <- pos_values[1]
  }
  lengths
}

