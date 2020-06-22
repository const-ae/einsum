
test_that("basic matrix operations work", {

  mat1 <- matrix(rnorm(n = 4 * 8), nrow = 4, ncol = 8)
  mat2 <- matrix(rnorm(n = 8 * 3), nrow = 8, ncol = 3)

  # Matrix Multiply
  expect_equal(einsum("ij,jk->ik", mat1, mat2), mat1 %*% mat2)
  expect_equal(einsum("ij,jk->ik", mat1, mat2), tcrossprod(mat1, t(mat2)))

  # Diag
  mat_sq <- matrix(rnorm(n = 4 * 4), nrow = 4, ncol = 4)
  expect_equal(c(einsum("ii->i", mat_sq)), diag(mat_sq))

  # Trace
  expect_equal(c(einsum("ii->", mat_sq)), sum(diag(mat_sq)))

  # Row sum
  expect_equal(c(einsum("ij->i", mat1)), rowSums(mat1))

  # Col sum
  expect_equal(c(einsum("ij->j", mat1)), colSums(mat1))

  # Scalar product
  mat3 <- matrix(rnorm(n = 4 * 8), nrow = 4, ncol = 8)
  expect_equal(einsum("ij,ij->ij", mat3, mat1), mat3 * mat1)

  # Transpose
  expect_equal(einsum("ij->ji", mat1), t(mat1))

  # Matrix times vector
  vec <- rnorm(8)
  expect_equal(c(einsum("ij,j->i", mat3, vec)), c(mat3 %*% vec))

  # Batched L2 norm
  arr1 <- array(c(mat1, mat3), dim = c(dim(mat1), 2))  # eq. to abind()
  expect_equal(c(einsum("ijb,ijb->b", arr1, arr1)), c(sum(mat1^2), sum(mat3^2)))

})


test_that("einsum can handle whitespace in equation_string", {
  mat1 <- matrix(rnorm(n = 4 * 8), nrow = 4, ncol = 8)
  mat2 <- matrix(rnorm(n = 8 * 3), nrow = 8, ncol = 3)

  expect_equal(einsum("ij,j k -> ik", mat1, mat2), mat1 %*% mat2)
})



test_that("einsum gives appropriate error messages", {
  mat1 <- matrix(rnorm(n = 4 * 8), nrow = 4, ncol = 8)
  mat2 <- matrix(rnorm(n = 8 * 3), nrow = 8, ncol = 3)

  # j is 8 and 3
  expect_error(einsum("ij,jj -> ik", mat1, mat2))

  # more arrays than elements in the lhs
  expect_error(einsum("ij,jj -> ik", mat1, mat2, mat1))

  # length(dim(array)) does not match number of indices
  expect_error(einsum("ij,jk -> ik", mat1, 1:5))

  # Invalid character in equation_string
  expect_error(einsum("ij,jk -> i3k", mat1, mat2))
  expect_error(einsum("ij,j$k -> ik", mat1, mat2))

})


