context('clist')

test_that('clist expects uniquely-named lists', {
  expect_error(clist(list(1), list()), regexp = 'unique names')
  expect_error(clist(list(x = 1, list(x = 2)), list(y = 2)),
               regexp = 'unique names')
})

test_that('clist combines list elements by class method', {
  
  bp1 <- boxplot(mpg ~ vs, data = mtcars[mtcars$vs == 0, ], plot = FALSE)
  bp2 <- boxplot(mpg ~ vs, data = mtcars[mtcars$vs == 1, ], plot = FALSE)
  bp  <- boxplot(mpg ~ vs, data = mtcars, plot = FALSE)
  
  expect_identical(clist(bp1, bp2), bp)
  
})

test_that('rbind* and cbind* methods give identical results', {
  
  bp1 <- boxplot(mpg ~ vs, data = mtcars[mtcars$vs == 0, ], plot = FALSE)
  bp2 <- boxplot(mpg ~ vs, data = mtcars[mtcars$vs == 1, ], plot = FALSE)
  
  expect_identical(
    clist(bp1, bp2, how = 'rbind'),
    clist(bp1, bp2, how = 'rbindx')
  )
  
  expect_identical(
    clist(bp1, bp2, how = 'cbind'),
    clist(bp1, bp2, how = 'cbindx')
  )
  
})
