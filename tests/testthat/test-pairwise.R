

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


test_that("einsum_generator uses pairwise contraction for 3+ tensors", {
  n <- 4
  a1 <- array(rnorm(n^2), dim = c(n, n))
  a2 <- array(rnorm(n^2), dim = c(n, n))
  a3 <- array(rnorm(n^2), dim = c(n, n))
  a4 <- array(rnorm(n^2), dim = c(n, n))

  # Three tensors: generated code should use intermediate temps
  gen3 <- einsum_generator("gx,xr,rw->gw")
  expect_equal(gen3(a1, a2, a3), einsum("gx,xr,rw->gw", a1, a2, a3))

  code3 <- einsum_generator("gx,xr,rw->gw", compile_function = FALSE)
  expect_true(grepl("temp1", code3))

  # Four tensors
  gen4 <- einsum_generator("nk,kq,qh,hZ->nZ")
  expect_equal(gen4(a1, a2, a3, a4), einsum("nk,kq,qh,hZ->nZ", a1, a2, a3, a4))

  # Scalar result from three tensors
  gen_scalar <- einsum_generator("pq,qr,rp->")
  expect_equal(gen_scalar(a1, a2, a3), einsum("pq,qr,rp->", a1, a2, a3))

  # Non-trivial index order in result
  gen_rev <- einsum_generator("xr,rw,wm->mx")
  expect_equal(gen_rev(a1, a2, a3), einsum("xr,rw,wm->mx", a1, a2, a3))

  # Mixed-case indices
  gen_mixed <- einsum_generator("aB,Bc,cD->aD")
  expect_equal(gen_mixed(a1, a2, a3), einsum("aB,Bc,cD->aD", a1, a2, a3))

  # Repeated indices within a tensor still works (falls back to single loop)
  gen_diag <- einsum_generator("ii,ij->j")
  mat_sq <- array(rnorm(n^2), dim = c(n, n))
  expect_equal(c(gen_diag(mat_sq, a1)), c(diag(mat_sq) %*% a1))
})
