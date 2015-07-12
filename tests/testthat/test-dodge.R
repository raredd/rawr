context('dodge points')

test_that('dodge call methods are equivalent', {
  
  d1 <- dodge(mpg ~ gear + vs, mtcars)
  d2 <- with(mtcars, dodge(mpg, list(gear, vs)))
  d3 <- dodge(mtcars$mpg, mtcars[, c('gear', 'vs')])
  
  expect_identical(d1, d2)
  expect_identical(d2, d3)
})