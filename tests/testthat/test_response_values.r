context("Generate Binding Response Functions")

test_that("binding1to1 returns expected values", {
  expect_equal(binding1to1(0, 1000, 6e-7, 10000, 0.001, 0.5), 0)
  expect_equal(binding1to1(100, 1000, 6e-7, 10000, 0.001, 0.5), 0.2157491555)
  expect_equal(binding1to1(1600, 1000, 6e-7, 10000, 0.001, 0.5), 0.2349905077)
  expect_error(binding1to1(-100, 1000, 6e-7, 10000, 0.001, 0.5))
  expect_error(binding1to1(100, 0, 6e-7, 10000, 0.001, 0.5))
  expect_error(binding1to1(1600, 1000, 6e-7, 10000, 0.001))
})

test_that("binding1to1 drift parameter", {
  expect_warning(
    val <- binding1to1(1600, 1000, 6e-7, 10000, 0.001, 0.5, 0.0001))
  expect_equal(val, 0.3949905077)
})

test_that("binding2to1 returns expected values", {
  expect_equal(
    binding2to1(0, 1000, 6e-7, 10000, 0.001, 0.5, 1000, 0.02, 0.4), 0)
  expect_equal(
    binding2to1(100, 1000, 6e-7, 10000, 0.001, 0.5, 1000, 0.02, 0.4),
    0.2259147403)
  expect_equal(
    binding2to1(1600, 1000, 6e-7, 10000, 0.001, 0.5, 1000, 0.02, 0.4),
    0.2349905793)
  expect_error(
    binding2to1(-100, 1000, 6e-7, 10000, 0.001, 0.5, 1000, 0.02, 0.4))
  expect_error(
    binding2to1(100, 0, 6e-7, 10000, 0.001, 0.5, 1000, 0.02, 0.4))
  expect_error(
    binding2to1(1600, 1000, 6e-7, 10000, 0.001, 0.5, 1000, 0.02))
})

test_that("binding2to1 drift parameter", {
  expect_warning(
    val <- binding2to1(1600, 1000, 6e-7, 10000, 0.001, 0.5, 1000, 0.02, 0.4,
    drift = 0.00004))
  expect_equal(val, 0.2989905793)
})
