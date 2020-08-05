
<!-- README.md is generated from README.Rmd. Please edit that file -->

# einsum

<!-- badges: start -->

[![R build
status](https://github.com/const-ae/einsum/workflows/R-CMD-check/badge.svg)](https://github.com/const-ae/einsum/actions)
[![Codecov test
coverage](https://codecov.io/gh/const-ae/einsum/branch/master/graph/badge.svg)](https://codecov.io/gh/const-ae/einsum?branch=master)
<!-- badges: end -->

[Einstein summation](https://en.wikipedia.org/wiki/Einstein_notation) is
a concise mathematical notation that implicitly sums over repeated
indices of n-dimensional arrays. Many ordinary matrix operations
(e.g. transpose, matrix multiplication, scalar product, ‘diag()’, trace
etc.) can be written using Einstein notation. The notation is
particularly convenient for expressing operations on arrays with more
than two dimensions because the respective operators (‘tensor products’)
might not have a standardized name.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("const-ae/einsum")
```

## Example

Load the package:

``` r
library(einsum)
```

Let’s make two matrices:

``` r
mat1 <- matrix(rnorm(n = 8 * 4), nrow = 8, ncol = 4)
mat2 <- matrix(rnorm(n = 4 * 4), nrow = 4, ncol = 4)
```

We can use `einsum()` to calculate the matrix product:

``` r
einsum("ij, jk -> ik", mat1, mat2)
#>            [,1]       [,2]       [,3]        [,4]
#> [1,] -0.5087677  0.6680792  0.2909357  0.49456493
#> [2,] -1.1888008  0.9411126  0.6737345  0.39054429
#> [3,] -1.4715071  1.0242759  0.2400887 -0.31436543
#> [4,]  0.3899863 -1.1212621 -0.7660189 -2.28836686
#> [5,] -0.9058902  0.5529122  0.4775118  0.18014286
#> [6,] -1.4494020  1.6341965  1.4738795  2.73834635
#> [7,] -0.5380896  0.8600228  0.4430880 -0.06022769
#> [8,]  3.0707573 -2.5552313 -1.5538108 -1.28101253
```

This is exactly the same as the standard matrix multiplication

``` r
mat1 %*% mat2
#>            [,1]       [,2]       [,3]        [,4]
#> [1,] -0.5087677  0.6680792  0.2909357  0.49456493
#> [2,] -1.1888008  0.9411126  0.6737345  0.39054429
#> [3,] -1.4715071  1.0242759  0.2400887 -0.31436543
#> [4,]  0.3899863 -1.1212621 -0.7660189 -2.28836686
#> [5,] -0.9058902  0.5529122  0.4775118  0.18014286
#> [6,] -1.4494020  1.6341965  1.4738795  2.73834635
#> [7,] -0.5380896  0.8600228  0.4430880 -0.06022769
#> [8,]  3.0707573 -2.5552313 -1.5538108 -1.28101253
```

This is a fairly simple example and there seems little benefit of using
it over the more familiar matrix product expression. Furthermore,
‘einsum’ is a lot slower.

However, ‘einsum’ truly shines when working with more than 2-dimensional
arrays, where it can be difficult to figure out the correct kind of
tensor product:

``` r
# Make some n-dimensional arrays
arr1 <- array(rnorm(3 * 9 * 2), dim = c(3, 9, 2))
arr2 <- array(rnorm(2 * 5), dim = c(2, 5))
arr3 <- array(rnorm(9 * 3), dim = c(9, 3))

einsum("abc, cd, ba -> d", arr1, arr2, arr3)
#> [1] -0.7015596 -4.0114655 -1.6420695 -3.4131292  0.7233701
```

The equivalent expression using tensor products (which are not
intuitive) would look like this (and believe me it took me more than one
try to figure out this formula):

``` r
tensor::tensor(tensor::tensor(arr1, arr2, alongA = 3, alongB = 1), arr3, alongA = c(2,1), alongB = c(1, 2))
#> [1] -0.7015596 -4.0114655 -1.6420695 -3.4131292  0.7233701
```

If you need to do the same computation over and over again, you can use
`einsum_generator()` which generates and compiles efficient C++ to do
exactly that calculation. It can take a few seconds, to create the
function with `einsum_generator()`, however the returned function can be
one or two orders of magnitude faster than `einsum()`

``` r
# einsum_generator returns a function
array_prod <- einsum_generator("abc, cd, ba -> d")
array_prod(arr1, arr2, arr3)
#> [1] -0.7015596 -4.0114655 -1.6420695 -3.4131292  0.7233701
```

``` r
bench::mark(
  tensor = tensor::tensor(tensor::tensor(arr1, arr2, alongA = 3, alongB = 1), 
                          arr3, alongA = c(2,1), alongB = c(1, 2)),
  einsum = einsum("abc, cd, ba -> d", arr1, arr2, arr3),
  einsum_generator = array_prod(arr1, arr2, arr3)
)
#> # A tibble: 3 x 6
#>   expression            min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>       <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 tensor            55.04µs   64.8µs    15168.    2.93KB     97.7
#> 2 einsum           247.69µs 263.22µs     3629.    2.49KB     25.4
#> 3 einsum_generator   3.08µs   3.47µs   255553.    2.49KB     25.6
```

# Credit

This package is inspired by the equivalent function in
[NumPy](https://numpy.org/doc/stable/reference/generated/numpy.einsum.html).
