context('numeric to character string')

v <- Vectorize(num2char)

test_that('informal and formal are identical in certain circumstances', {
  
  expect_identical(v(1:99), v(1:99, informal = TRUE))
  expect_identical(v(c(1000, 100, 19600, 12)),  
                   v(c(1000, 100, 19600, 12), informal = TRUE))
  expect_identical(v(c(101,10001,7020,591)),
                   gsub(' and ', ' ', v(c(101,10001,7020,591), 
                                               informal = TRUE)))
})

test_that('negatives and positives give same result', {
  
  v1 <- v(1:10001, cap = FALSE)
  v2 <- gsub('negative ', '', v(-1:-10001, cap = FALSE))
  expect_identical(v1, v2)
})

test_that('no extra ands for informal case or 
          NAs in numbers with strings of 0s are printed', {
  
  nums <- c(1, 21, 10001, 190001)
  expect_false(any(mapply(grepl, 'And|NA', v(nums))))
})
