
test_that("basic matrix operations work", {

  mat1 <- matrix(rnorm(n = 4 * 8), nrow = 4, ncol = 8)
  mat2 <- matrix(rnorm(n = 8 * 3), nrow = 8, ncol = 3)

  # Matrix Multiply
  expect_equal(einsum("ij,jk->ik", mat1, mat2), mat1 %*% mat2)
  expect_equal(einsum("ij,jk->ik", mat1, mat2), tcrossprod(mat1, t(mat2)))
  expect_equal(einsum("ij,jk->ik", mat1, mat2), einsum_generator("ij,jk->ik")(mat1, mat2))

  # Diag
  mat_sq <- matrix(rnorm(n = 4 * 4), nrow = 4, ncol = 4)
  expect_equal(c(einsum("ii->i", mat_sq)), diag(mat_sq))
  expect_equal(einsum("ii->i", mat_sq), einsum_generator("ii->i")(mat_sq))

  expect_equal(einsum("ii->ii", mat_sq), diag(diag(mat_sq)))
  expect_equal(einsum("ii->ii", mat_sq), einsum_generator("ii->ii")(mat_sq))

  expect_equal(einsum("ii->iii", mat_sq), einsum_generator("ii->iii")(mat_sq))

  # Trace
  expect_equal(c(einsum("ii->", mat_sq)), sum(diag(mat_sq)))
  expect_equal(einsum("ii->", mat_sq), einsum_generator("ii->")(mat_sq))

  # Row sum
  expect_equal(c(einsum("ij->i", mat1)), rowSums(mat1))
  expect_equal(einsum("ij->i", mat1), einsum_generator("ij->i")(mat1))

  # Col sum
  expect_equal(c(einsum("ij->j", mat1)), colSums(mat1))
  expect_equal(einsum("ij->j", mat1), einsum_generator("ij->j")(mat1))

  # Scalar product
  mat3 <- matrix(rnorm(n = 4 * 8), nrow = 4, ncol = 8)
  expect_equal(einsum("ij,ij->ij", mat3, mat1), mat3 * mat1)
  expect_equal(einsum("ij,ij->ij", mat3, mat1), einsum_generator("ij,ij->ij")(mat3, mat1))

  # Transpose
  expect_equal(einsum("ij->ji", mat1), t(mat1))
  expect_equal(einsum("ij->ji", mat1), einsum_generator("ij->ji")(mat1))

  # Matrix times vector
  vec <- rnorm(8)
  expect_equal(c(einsum("ij,j->i", mat3, vec)), c(mat3 %*% vec))
  expect_equal(einsum("ij,j->i", mat3, vec), einsum_generator("ij,j->i")(mat3, vec))

  # Batched L2 norm
  arr1 <- array(c(mat1, mat3), dim = c(dim(mat1), 2))  # eq. to abind()
  expect_equal(c(einsum("ijb,ijb->b", arr1, arr1)), c(sum(mat1^2), sum(mat3^2)))
  expect_equal(einsum("ijb,ijb->b", arr1, arr1), einsum_generator("ijb,ijb->b")(arr1, arr1))

  # More complex example
  expect_equal(c(einsum("ij,kj,kl,l->i", mat1, mat3, mat_sq, c(1:4))), c((mat1 %*% t(mat3)) %*% mat_sq %*% c(1:4)))
  expect_equal(einsum("ij,kj,kl,l->i", mat1, mat3, mat_sq, c(1:4)), einsum_generator("ij,kj,kl,l->i")(mat1, mat3, mat_sq, c(1:4)))

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

  # Missing ->
  expect_error(einsum("ij,jk", mat1, mat2))

  # Multiple ->
  expect_error(einsum("ij,jk->k->a", mat1, mat2))

  # Wrong index in result
  expect_error(einsum("ij,jk -> a", mat1, mat2))

})

test_that("einsum handles many indices via optimized pairwise contraction", {
  n <- 4

  # Non-alphabetical index names: xp,pq->xq
  a1 <- array(rnorm(n^2), dim = c(n, n))
  a2 <- array(rnorm(n^2), dim = c(n, n))
  expect_equal(einsum("xp,pq->xq", a1, a2), a1 %*% a2)

  # 5 unique indices with gaps: mwkr,wkrn->mn
  a3 <- array(rnorm(n^4), dim = rep(n, 4))
  a4 <- array(rnorm(n^4), dim = rep(n, 4))
  res <- einsum("mwkr,wkrn->mn", a3, a4)
  ref <- einsum_generator("mwkr,wkrn->mn")(a3, a4)
  expect_equal(res, ref)

  # 7 unique indices with mixed case: pAqBrC,AqBrCz->pz
  a5 <- array(rnorm(n^6), dim = rep(n, 6))
  a6 <- array(rnorm(n^6), dim = rep(n, 6))
  res <- einsum("pAqBrC,AqBrCz->pz", a5, a6)
  ref <- einsum_generator("pAqBrC,AqBrCz->pz")(a5, a6)
  expect_equal(res, ref)

  # result in reversed order: mwkr,wkrn->nm
  res <- einsum("mwkr,wkrn->nm", a3, a4)
  ref <- einsum_generator("mwkr,wkrn->nm")(a3, a4)
  expect_equal(res, ref)

  # scalar result (all indices contracted): xp,xp->
  res <- einsum("xp,xp->", a1, a2)
  expect_equal(c(res), sum(a1 * a2))

  # three tensors: gx,xr,rw->gw
  a7 <- array(rnorm(n^2), dim = c(n, n))
  res <- einsum("gx,xr,rw->gw", a1, a2, a7)
  ref <- einsum_generator("gx,xr,rw->gw")(a1, a2, a7)
  expect_equal(res, ref)

  # four tensors: nk,kq,qh,hZ->nZ
  a8 <- array(rnorm(n^2), dim = c(n, n))
  res <- einsum("nk,kq,qh,hZ->nZ", a1, a2, a7, a8)
  ref <- einsum_generator("nk,kq,qh,hZ->nZ")(a1, a2, a7, a8)
  expect_equal(res, ref)
})


test_that("einsum_generator gives appropriate error messages", {

  # Missing ->
  expect_error(einsum_generator("ij,jk"))

  # Multiple ->
  expect_error(einsum_generator("ij,jk->k->a"))

  # Wrong index in result
  expect_error(einsum_generator("ij,jk -> a"))

})



# quad_mm <- einsum_generator("ij,kj,kl,l->i")
#
# i <- 3
# j <- 500
# k <- 70
# l <- 200
#
# mat1 <- array(rnorm(n = i * j), dim = c(i,j))
# mat2 <- array(rnorm(n = k * j), dim = c(k,j))
# mat3 <- array(rnorm(n = k * l), dim = c(k,l))
# mat4 <- array(rnorm(n = l), dim = c(l))
#
# bench::mark(base_r = c((mat1 %*% t(mat2)) %*% mat3 %*% mat4),
#             einsum = c(einsum("ij,kj,kl,l->i", mat1, mat2, mat3, mat4)),
#             einsum_compiled = c(quad_mm(mat1, mat2, mat3, mat4)))

