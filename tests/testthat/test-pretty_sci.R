context('misc tests')

test_that('oom returns correct orders of magnitude for different types', {
  x <- c(1, 10, 1000, 10000, 1e100)
  expect_identical(oom(-x), oom(1 / x))
  expect_true(ident(5, oom(1e5), oom(1e-5), oom(-1e-5), oom(-1e5)))
})
