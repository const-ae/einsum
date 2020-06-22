
#' Einstein Summation
#'

#'
#' @param equation_string a string in Einstein notation where arrays
#'   are separated by ',' and the result is separated by '->'. For
#'   example \code{"ij,jk->ik"} corresponds to a standard matrix multiplication.
#'   Whitespace inside the \code{equation_string} is ignored. Unlike the
#'   equivalent functions in Python, \code{einsum()} only supports the explicit
#'   mode. This means that the \code{equation_string} must contain '->'.
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
#'   \code{"ii->i"} \tab \mjseqn{y = \text{diag}(A)} \tab Diagonal \cr
#'   \code{"ii->ii"} \tab \mjseqn{Y = \text{diag}(A) I} \tab Diagonal times Identity  \cr
#'   \code{"ii->"} \tab \mjseqn{y = \text{trace}(A) = \sum_i{A_{i}} } \tab Trace \cr
#'   \code{"ijk,mjj->i"} \tab \mjseqn{ y_i = \sum_{j}\sum_{k}\sum_{m}A_{ijk}B_{mjj}  } \tab Complex 3D operation  \cr
#' }
#'
#'
#' The function and the conventions are inspired by the \code{einsum()} function
#' in NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.einsum.html}{documentation}).
#' Unlike NumPy, 'einsum' only supports the explicit mode. The explict mode is more flexible and
#' can avoid confusion. The common summary of the Einstein  summation to
#' "sum over duplicated indices" however is not a good mental model. A better rule of thumb is
#' "sum over all indices not in the result".
#'
#' \emph{Note:} This function is implemented in pure R with a few for loops. Do not expect
#' impressive performance. However, if there is continued demand for a more performant
#' version, I would be open to implement it with Rcpp.
#'
#' @examples
#' mat1 <- matrix(rnorm(n = 4 * 8), nrow = 4, ncol = 8)
#' mat2 <- matrix(rnorm(n = 8 * 3), nrow = 8, ncol = 3)
#'
#' # Matrix Multiply
#' mat1 %*% mat2
#' einsum("ij,jk -> ik", mat1, mat2)
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

  objects <- list(...)
  objects <- lapply(objects, as.array)
  equation_string <- gsub("\\s", "", equation_string)

  tmp <- strsplit(equation_string, "->")[[1]]
  result_string <- if(length(tmp) == 1) ""
  else if(length(tmp) == 2) tmp[2]
  else stop("the equation string contains more than one '->': ", equation_string)
  lhs_strings <- tmp[1]
  strings <- unlist(strsplit(lhs_strings, ","))
  if(any(grepl("[^a-zA-Z]", strings)) || grepl("[^a-zA-Z]", result_string)) stop("'equation_string' contains a non alphabetical (a-z and A-Z) character.")
  einsum_impl(strings, result_string, objects)

}




einsum_impl <- function(strings, result_string, arrays){
  stopifnot("The number of strings on the left-hand side does not match the number of arrays" =
              length(strings) == length(arrays))
  stopifnot("Number of dimensions of array does not match the number of indices" =
              all(nchar(strings) == vapply(arrays, function(a)length(dim(a)), 0.0)))
  result_string_vec <- strsplit(result_string, "")[[1]]
  sum_string_vec <- setdiff(unique(unlist(strsplit(strings, ""))), result_string_vec)

  # Get the lengths of the indices as a named vector
  lengths_vec <- get_lengths_vec(strings, arrays)

  # Make converters for output
  out_pos2idx <- pos2idx_gen(result_string, lengths_vec)
  out_idx2pos <- idx2pos_gen(result_string, lengths_vec)

  # Mke converters for summing variables
  sum_idx2pos <- idx2pos_gen(sum_string_vec, lengths_vec)
  arr_pos2idx_list <- lapply(strings, function(str)pos2idx_gen(str, lengths_vec))


  # Initialize memory for result
  if(length(result_string_vec) == 0){
    # array cannot handle empty dim aka zero dimensional array aka scalar
    res <- array(0)
  }else{
    res <- array(0, dim = unname(lengths_vec[result_string_vec]))
  }

  # have a simple index running through all elements of res
  for(out_idx in seq_len(prod(lengths_vec[result_string_vec]))-1){
    out_pos <- out_idx2pos(out_idx)
    s <- 0
    for(sum_idx in seq_len(prod(lengths_vec[sum_string_vec]))-1){
      sum_pos <- c(out_pos, sum_idx2pos(sum_idx))
      s <- s + Reduce(`*`, vapply(seq_along(arrays), function(i)arrays[[i]][arr_pos2idx_list[[i]](sum_pos) + 1], FUN.VALUE = 0.0))
    }
    res[out_pos2idx(out_pos) + 1] <- s
  }
  res
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

pos2idx_gen <- function(str, lengths){
  str_vec <- unlist(strsplit(str, ""))
  stopifnot("Result string contains index that does not appear on the left-hand side" =
              all(str_vec %in% names(lengths)))
  length_lookup <- c(1, cumprod(lengths[str_vec]))[seq_len(length(str_vec))]
  function(pos){
    stopifnot(all(pos[str_vec] < lengths[str_vec]))
    sum(pos[str_vec] * length_lookup)
  }
}

idx2pos_gen <- function(str, lengths){
  str_vec <- unlist(strsplit(str, ""))
  stopifnot("Result string contains index that does not appear on the left-hand side" =
              all(str_vec %in% names(lengths)))
  length_lookup <- c(1, cumprod(lengths[str_vec]))[seq_len(length(str_vec))]
  names(length_lookup) <- str_vec
  function(idx){
    res <- vapply(str_vec, function(s) floor(idx / length_lookup[s]) %% lengths[s], FUN.VALUE = 0.0)
    names(res) <- str_vec
    res
  }
}
