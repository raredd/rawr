context('cumfuns')


test_that('cumulative functions are identical to base with no NA', {
  
  x <- 1:10
  
  expect_true(
    identical2(
      cumsum(x),
      cum_na(x, cumsum, TRUE),
      cum_na(x, cumsum, FALSE),
      cumsum_na(x, TRUE),
      cumsum_na(x, FALSE)
    )
  )
  
  expect_true(
    identical2(
      cumprod(x),
      cum_na(x, cumprod, TRUE),
      cum_na(x, cumprod, FALSE),
      cumprod_na(x, TRUE),
      cumprod_na(x, FALSE)
    )
  )
  
  expect_true(
    identical2(
      cummax(x),
      cum_na(x, cummax, TRUE),
      cum_na(x, cummax, FALSE),
      cummax_na(x, TRUE),
      cummax_na(x, FALSE)
    )
  )
  
  expect_true(
    identical2(
      cummin(x),
      cum_na(x, cummin, TRUE),
      cum_na(x, cummin, FALSE),
      cummin_na(x, TRUE),
      cummin_na(x, FALSE)
    )
  )
  
})
