
#' Einstein Summation
#'
#' Einstein summation is a concise mathematical notation that
#' implicitly sums over repeated indices of n-dimensional arrays. Many ordinary
#' matrix operations (e.g. transpose, matrix multiplication, scalar product, 'diag()', trace etc.)
#' can be written using Einstein notation. The notation is particularly convenient for
#' expressing operations on arrays with more than two dimensions because the
#' respective operators ('tensor products') might not have a standardized name.
#'
#' @docType  package
#' @name einsum_package
NULL





## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL


## usethis namespace: start
#' @useDynLib einsum, .registration = TRUE
## usethis namespace: end
NULL
