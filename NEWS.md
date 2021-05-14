# einsum 0.1.0

Initial release of the `einsum` R package.

Features:

* `einsum()`: execute complex array multiplications using a concise and unambiguous based on the Einstein notation (`"abc, b, c -> ac"`).
* `einsum_generator()`: generate an efficient C++ function that can be used for repeatedly executing the same 
  multiplication. It can also produce the source code for a C++ function which you can integrate into your own package.

Thanks to  @kokitsuyuzaki for helping me push this over the finish line and the numpy project for their equivalent function
that inspired this package.
