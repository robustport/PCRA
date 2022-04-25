context("Test functions")

test_that("mathGmv is as expected", {
  
  x <- mathGmv(midcap.ts[,1:10])
  
  expect_that(x$wts[1],is_equivalent_to(0.264138002824520323752))
  expect_that(x$wts[2],is_equivalent_to(-0.058874497255879558455))
  expect_that(sum(x$wts),equals(1))
  expect_that(x$mu,equals(0.016525251092767000161))
  expect_that(x$vol,equals(0.05184660161563151709))
})
