context('kw.test')


test_that('ca.test is identical to prop.trend.test', {
  x <- rbinom(100, 1, 0.5)
  y <- sample(1:3, 100, TRUE)
  
  tbl <- table(x, y)
  
  ca <- ca.test(tbl)
  pr <- prop.trend.test(tbl[1L, ], colSums(tbl))
  
  expect_identical(
    ca$statistic, pr$statistic
  )
  
  expect_identical(
    ca$p.value, pr$p.value
  )
})
