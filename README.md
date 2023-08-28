
<!-- README.md is generated from README.Rmd. Please edit that file -->

# einsum

<!-- badges: start -->

[![R build
status](https://github.com/const-ae/einsum/workflows/R-CMD-check/badge.svg)](https://github.com/const-ae/einsum/actions)
[![Codecov test
coverage](https://codecov.io/gh/const-ae/einsum/branch/master/graph/badge.svg)](https://app.codecov.io/gh/const-ae/einsum?branch=master)
<!-- badges: end -->

[Einstein summation](https://en.wikipedia.org/wiki/Einstein_notation) is
a concise mathematical notation that implicitly sums over repeated
indices of n-dimensional arrays. Many ordinary matrix operations (e.g.,
transpose, matrix multiplication, scalar product, ‘diag()’, trace, etc.)
can be written using Einstein notation. The notation is particularly
convenient for expressing operations on arrays with more than two
dimensions because the respective operators (‘tensor products’) might
not have a standardized name.

## Installation

You can install the package from
[CRAN](https://CRAN.R-project.org/package=einsum) with:

``` r
install.packages("einsum")
```

or if you want to use the development version from
[GitHub](https://github.com/einsum):

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

We can use `einsum()` to calculate the matrix product

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

which produces the same as the standard matrix multiplication

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

The matrix multiplication example is straightforward, and there is
little benefit of using the Einstein notation over the more familiar
matrix product expression. Furthermore, ‘einsum’ is a lot slower.

However, ‘einsum’ truly shines when working with more than 2-dimensional
arrays, where it can be difficult to figure out the correct kind of
tensor product:

``` r
# Make three n-dimensional arrays
arr1 <- array(rnorm(3 * 9 * 2), dim = c(3, 9, 2))
arr2 <- array(rnorm(2 * 5), dim = c(2, 5))
arr3 <- array(rnorm(9 * 3), dim = c(9, 3))
# Sum across axes a, b, and c
einsum("abc, cd, ba -> d", arr1, arr2, arr3)
#> [1] -0.7015596 -4.0114655 -1.6420695 -3.4131292  0.7233701
```

The equivalent expression using tensor products (which are not
intuitive) would look like this:

``` r
tensor::tensor(tensor::tensor(arr1, arr2, alongA = 3, alongB = 1), arr3, alongA = c(2,1), alongB = c(1, 2))
#> [1] -0.7015596 -4.0114655 -1.6420695 -3.4131292  0.7233701
```

If you need to do the same computation repeatedly, you can use
`einsum_generator()`, which generates and compiles an efficient C++
function for that calculation (to see the function code, set
`compile_function=FALSE`). It can take a few seconds to compile the
function, but the returned function can be one or two orders of
magnitude faster than `einsum()`.

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
#> # A tibble: 3 × 6
#>   expression            min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>       <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 tensor             48.6µs  52.02µs    18215.    2.93KB     25.4
#> 2 einsum           380.66µs 403.17µs     2373.    2.49KB     23.5
#> 3 einsum_generator   3.85µs   4.32µs   196916.    2.49KB     19.7
```

Lastly, you can also generate C++ code if you need an efficient
implementation of some function, which you could (with proper credit)
for example paste into your R package:

``` r
# The C++ code underlying the tensor product
cat(einsum_generator("abc, cd, ba -> d", compile_function = FALSE))
#> NumericVector einsum_impl_func(NumericVector array1, NumericVector array2, NumericVector array3){
#> NumericVector size(4);
#> IntegerVector array1_dim = array1.hasAttribute("dim") ? array1.attr("dim") : IntegerVector::create(array1.length());
#> IntegerVector array2_dim = array2.hasAttribute("dim") ? array2.attr("dim") : IntegerVector::create(array2.length());
#> IntegerVector array3_dim = array3.hasAttribute("dim") ? array3.attr("dim") : IntegerVector::create(array3.length());
#> size[0] = array1_dim[0];
#> if(size[0] != array3_dim[1]) stop("Dimension 2 of object array3 does not match!");
#> size[1] = array1_dim[1];
#> if(size[1] != array3_dim[0]) stop("Dimension 1 of object array3 does not match!");
#> size[2] = array1_dim[2];
#> if(size[2] != array2_dim[0]) stop("Dimension 1 of object array2 does not match!");
#> size[3] = array2_dim[1];
#> 
#> NumericVector result(size[3]);
#> for(int d = 0; d < size[3]; ++d){
#> double sum = 0.0;
#> for(int a = 0; a < size[0]; ++a){
#> for(int b = 0; b < size[1]; ++b){
#> for(int c = 0; c < size[2]; ++c){
#> sum += array1[1 * (a + size[0] * (b + size[1] * (c)))] * array2[1 * (c + size[2] * (d))] * array3[1 * (b + size[1] * (a))];
#> }
#> }
#> }
#> result[1 * (d)] = sum;
#> }
#> result.attr("dim") = IntegerVector::create(size[3]);
#> return result;
#> 
#> }
```

# Credit

This package is inspired by the
[einsum](https://numpy.org/doc/stable/reference/generated/numpy.einsum.html)
function in NumPy.
