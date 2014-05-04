context('dodge points')

test_that('dodge call methods are equivalent', {
  
  d1 <- dodge(mpg ~ cyl, mtcars)
  d2 <- with(mtcars, dodge(cyl ~ mpg))
  d3 <- dodge(mtcars[c('mpg', 'cyl')])
  
  expect_identical(d1, d2)
  expect_identical(d2, d3)
  
  
  
  d1 <- dodge(mpg ~ cyl, mtcars, spread = TRUE)
  d2 <- with(mtcars, dodge(mpg ~ cyl, spread = TRUE))
  d3 <- dodge(mtcars[c('mpg', 'cyl')], spread = TRUE)
  
  expect_identical(d1, d2)
  expect_identical(d2, d3)
  
  
  
  d1 <- dodge(mpg ~ cyl, mtcars, spread = TRUE, z = 0)
  d2 <- with(mtcars, dodge(mpg ~ cyl, spread = TRUE, z = 0))
  d3 <- dodge(mtcars[c('mpg', 'cyl')], spread = TRUE, z = 0)
  
  expect_identical(d1, d2)
  expect_identical(d2, d3)
  
  
  
  d1 <- dodge(mpg ~ cyl, mtcars, z = 100)
  d2 <- with(mtcars, dodge(mpg ~ cyl, z = 100))
  d3 <- dodge(mtcars[c('mpg', 'cyl')], z = 100)
  
  expect_identical(d1, d2)
  expect_identical(d2, d3)
})