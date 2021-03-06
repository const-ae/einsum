% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/einsum.R, R/einsum_generator.R
\name{einsum}
\alias{einsum}
\alias{einsum_generator}
\title{Einstein Summation}
\usage{
einsum(equation_string, ...)

einsum_generator(equation_string, compile_function = TRUE)
}
\arguments{
\item{equation_string}{a string in Einstein notation where arrays
are separated by ',' and the result is separated by '->'. For
example \code{"ij,jk->ik"} corresponds to a standard matrix multiplication.
Whitespace inside the \code{equation_string} is ignored. Unlike the
equivalent functions in Python, \code{einsum()} only supports the explicit
mode. This means that the \code{equation_string} must contain '->'.}

\item{...}{the arrays that are combined. All arguments are converted
to arrays with \code{as.array}.}

\item{compile_function}{boolean that decides if \code{einsum_generator()}
returns the result of \code{Rcpp::cppFunction()} or the program as a
string. Default: \code{TRUE}.}
}
\value{
The \code{einsum()} function returns an array with one dimension for each index in the result
 of the \code{equation_string}. For example \code{"ij,jk->ik"} produces a 2-dimensional array,
 \code{"abc,cd,de->abe"} produces a 3-dimensional array.

 The \code{einsum_generator()} function returns a function that takes one array for each
 comma-separated input in the \code{equation_string} and returns the same result as \code{einsum()}.
 Or if \code{compile_function = FALSE}, \code{einsum_generator()} function returns a string with the
 C++ code for such a function.
}
\description{
\loadmathjax
Einstein summation is a convenient and concise notation for operations
on n-dimensional arrays.
}
\details{
The following table show, how the Einstein notation abbreviates complex
summation for arrays/matrices:

\tabular{lrr}{
  \code{equation_string} \tab Formula \tab \cr
  ------------------------\tab--------------------------------------\tab----------------------------------\cr
  \code{"ij,jk->ik"} \tab \mjseqn{ Y_{ik} = \sum_{j}{A_{ij} B_{jk}}  } \tab Matrix multiplication \cr
  \code{"ij->ji"}` \tab \mjseqn{ Y = A^{T}  } \tab Transpose \cr
  \code{"ii->i"} \tab \mjeqn{y = \textrm{diag}(A)}{y = diag(A)} \tab Diagonal \cr
  \code{"ii->ii"} \tab \mjeqn{Y = \textrm{diag}(A) I}{Y = diag(A) I} \tab Diagonal times Identity  \cr
  \code{"ii->"} \tab \mjeqn{y = \textrm{trace}(A) = \sum_i{A_{ii}} }{y = trace(A) = Sum_i(A_{ii})} \tab Trace \cr
  \code{"ijk,mjj->i"} \tab \mjeqn{ y_i = \sum_{j}\sum_{k}\sum_{m}A_{ijk}B_{mjj}  }{y_i = Sum_j Sum_k Sum_m A_{ijk} B_{mjj}} \tab Complex 3D operation  \cr
}


The function and the conventions are inspired by the \code{einsum()} function
in NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.einsum.html}{documentation}).
Unlike NumPy, 'einsum' only supports the explicit mode. The explicit mode is more flexible and
can avoid confusion. The common summary of the Einstein  summation to
"sum over duplicated indices" however is not a good mental model. A better rule of thumb is
"sum over all indices not in the result".

\emph{Note:} \code{einsum()} internally uses C++ code to provide results quickly, the repeated
parsing of the \code{equation_string} comes with some overhead. Thus,
if you need to do the same calculation over and over again it can be worth to use
\code{einsum_generator()} and call the returned the function. \code{einsum_generator()}
generates efficient C++ code that can be one or two orders of magnitude faster than
\code{einsum()}.
}
\examples{
mat1 <- matrix(rnorm(n = 4 * 8), nrow = 4, ncol = 8)
mat2 <- matrix(rnorm(n = 8 * 3), nrow = 8, ncol = 3)

# Matrix Multiply
mat1 \%*\% mat2
einsum("ij,jk -> ik", mat1, mat2)

# einsum_generator() works just like einsum() but returns a performant function
mat_mult <- einsum_generator("ij,jk -> ik")
mat_mult(mat1, mat2)

# Diag
mat_sq <- matrix(rnorm(n = 4 * 4), nrow = 4, ncol = 4)
diag(mat_sq)
einsum("ii->i", mat_sq)
einsum("ii->ii", mat_sq)

# Trace
sum(diag(mat_sq))
einsum("ii->", mat_sq)


# Scalar product
mat3 <- matrix(rnorm(n = 4 * 8), nrow = 4, ncol = 8)
mat3 * mat1
einsum("ij,ij->ij", mat3, mat1)

# Transpose
t(mat1)
einsum("ij->ji", mat1)


# Batched L2 norm
arr1 <- array(c(mat1, mat3), dim = c(dim(mat1), 2))
c(sum(mat1^2), sum(mat3^2))
einsum("ijb,ijb->b", arr1, arr1)

}
