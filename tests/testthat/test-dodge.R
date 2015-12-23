context('dodge points')

test_that('dodge call methods are equivalent', {
  
  d1 <- dodge(mpg ~ gear + vs, mtcars)
  d2 <- with(mtcars, dodge(list(gear, vs), mpg))
  d3 <- dodge(mtcars[, c('gear', 'vs')], mtcars$mpg)
  
  expect_identical(d1, d2)
  expect_identical(d2, d3)
})
