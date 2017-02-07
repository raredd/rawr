context('confidence intervals')

test_that('two-stage input returns one row', {
  o1 <- bincon(c(3, 4), c(14, 18), method = 'two-stage')
  o2 <- bincon(c(3, 4), c(14, 18), method = 'two')
  o3 <- bincon(c(3, 4), c(14, 18), method = 't')
  
  expect_identical(nrow(o1), 1L)
  expect_identical(nrow(o2), 1L)
  expect_identical(nrow(o3), 1L)
})

test_that('single-stage input returns multiple rows', {
  o1 <- bincon(c(3, 4), c(14, 18))
  o2 <- bincon(c(3, 4), 18)
  
  expect_identical(nrow(o1), 2L)
  expect_identical(nrow(o2), 2L)
  
  expect_warning(bincon(c(3, 4), c(14, 18), method = 'all'), 'exact')
})
