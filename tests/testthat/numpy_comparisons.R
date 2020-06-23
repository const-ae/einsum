# This is useful locally, but I comment out this code because I don't want to depend on reticulate


# np <- reticulate::import("numpy")
#
# test_that("einsum and numpy$einsum agree", {
#
#   mat1 <- matrix(rnorm(n = 4 * 8), nrow = 4, ncol = 8)
#   mat2 <- matrix(rnorm(n = 8 * 3), nrow = 8, ncol = 3)
#
#   # Matrix Multiply
#   expect_equal(einsum("ij,jk->ik", mat1, mat2), np$einsum("ij,jk->ik", mat1, mat2))
#
#   # Diag
#   mat_sq <- matrix(rnorm(n = 4 * 4), nrow = 4, ncol = 4)
#   expect_equal(einsum("ii->i", mat_sq), np$einsum("ii->i", mat_sq))
#
#   # Trace
#   expect_equal(c(einsum("ii->", mat_sq)), np$einsum("ii->", mat_sq))
#
#   # Row sum
#   expect_equal(einsum("ij->i", mat1), np$einsum("ij->i", mat1))
#
#   # Col sum
#   expect_equal(einsum("ij->j", mat1), np$einsum("ij->j", mat1))
#
#   # Scalar product
#   mat3 <- matrix(rnorm(n = 4 * 8), nrow = 4, ncol = 8)
#   expect_equal(einsum("ij,ij->ij", mat3, mat1), np$einsum("ij,ij->ij", mat3, mat1))
#
#   # Transpose
#   expect_equal(einsum("ij->ji", mat1), np$einsum("ij->ji", mat1))
#
#   # Matrix times vector
#   vec <- rnorm(8)
#   expect_equal(einsum("ij,j->i", mat3, vec), np$einsum("ij,j->i", mat3, vec))
#
#   # Batched L2 norm
#   arr1 <- array(c(mat1, mat3), dim = c(dim(mat1), 2))  # eq. to abind()
#   expect_equal(einsum("ijb,ijb->b", arr1, arr1), np$einsum("ijb,ijb->b", arr1, arr1))
#
# })
