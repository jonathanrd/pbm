context("Check req function")

test_that("req returns sensible values", {
  expect_equal(req(6e-7,1,6e-7), 0.5)
  expect_warning(req(-6e-7,1,6e-7))
  expect_warning(req(6e-7,-1,6e-7))
})
