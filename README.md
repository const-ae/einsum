
<!-- README.md is generated from README.Rmd. Please edit that file -->

# einsum

<!-- badges: start -->

<!-- badges: end -->

Einstein summation is a concise mathematical notation that implicitly
sums over repeated indices of n-dimensional arrays. Many ordinary matrix
operations (e.g. transpose, matrix multiplication, scalar product,
‘diag()’, trace etc.) can be written using Einstein notation. The
notation is particularly convenient for expressing operations on arrays
with more than two dimensions because the respective operators (‘tensor
products’) might not have a standardized name.

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
#>            [,1]       [,2]       [,3]       [,4]
#> [1,]  2.3370943  0.1641971 -1.3346851 -1.4001060
#> [2,] -0.1842167  0.3763168 -0.5497296  2.0302845
#> [3,] -1.4652338  0.4687049  1.4359160  0.3128279
#> [4,] -1.7476644  0.5721526  0.9893850  2.7779409
#> [5,] -2.0693620  0.5033233  0.2558220  3.9510589
#> [6,] -0.3559761 -1.2150244 -0.5995681  1.8524066
#> [7,]  0.2408359  0.7972073 -0.2463330  0.2446464
#> [8,] -3.1118622 -0.3911014  2.4418254  1.0096469
```

This is exactly the same as the standard matrix multiplication

``` r
mat1 %*% mat2
#>            [,1]       [,2]       [,3]       [,4]
#> [1,]  2.3370943  0.1641971 -1.3346851 -1.4001060
#> [2,] -0.1842167  0.3763168 -0.5497296  2.0302845
#> [3,] -1.4652338  0.4687049  1.4359160  0.3128279
#> [4,] -1.7476644  0.5721526  0.9893850  2.7779409
#> [5,] -2.0693620  0.5033233  0.2558220  3.9510589
#> [6,] -0.3559761 -1.2150244 -0.5995681  1.8524066
#> [7,]  0.2408359  0.7972073 -0.2463330  0.2446464
#> [8,] -3.1118622 -0.3911014  2.4418254  1.0096469
```

This was a fairly simple example and there seems little benefit of using
it over the more familiar matrix product expression. ‘einsum’ even is a
lot slower.

However, ‘einsum’ truly shines when working with more than 2-dimensional
arrays, where it can be difficult to figure out the correct kind of
tensor product:

``` r
# Make some n-dimensional arrays
arr1 <- array(rnorm(3 * 9 * 2), dim = c(3, 9, 2))
arr2 <- array(rnorm(2 * 5), dim = c(2, 5))
arr3 <- array(rnorm(9 * 3), dim = c(9, 3))

einsum("abc, cd, ba -> d", arr1, arr2, arr3)
#> [1]  3.978173 -4.807091 -3.273022 -0.775103 -5.353696
```

The equivalent expression using tensor products (which are not
intuitive) would look like this (and believe me it took me more than one
try to figure out this formula):

``` r
tensor::tensor(tensor::tensor(arr1, arr2, alongA = 3, alongB = 1), arr3, alongA = c(2,1), alongB = c(1, 2))
#> [1]  3.978173 -4.807091 -3.273022 -0.775103 -5.353696
```
