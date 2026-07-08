# einsum 0.2.0

* Optimize `einsum()` via BLAS matrix multiply and greedy pairwise contraction
  for 3+ tensor equations.
* Optimize `einsum_generator()` to emit pairwise C++ code for 3+ tensor
  equations instead of a single loop nest.
* Refactor shared equation parsing into `einsum_parse.R`.
* Update GitHub Actions workflows to current runners and actions.

# einsum 0.1.0

Initial release of the `einsum` R package.

Features:

* `einsum()`: execute complex array multiplications using a concise and unambiguous based on the Einstein notation (`"abc, b, c -> ac"`).
* `einsum_generator()`: generate an efficient C++ function that can be used for repeatedly executing the same 
  multiplication. It can also produce the source code for a C++ function which you can integrate into your own package.

Thanks to  @kokitsuyuzaki for helping me push this over the finish line and the numpy project for their equivalent function
that inspired this package.
