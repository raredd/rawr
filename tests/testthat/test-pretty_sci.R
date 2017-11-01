context('misc tests')

test_that('oom returns correct orders of magnitude for different types', {
  x <- c(1, 10, 1000, 10000, 1e100)
  expect_identical(oom(-x), abs(oom(1 / x)))
  expect_true(rawr::all_equal2(5, oom(1e5), oom(-1e5)))
  expect_true(rawr::all_equal2(-5, oom(1e-5), oom(-1e-5)))
})
