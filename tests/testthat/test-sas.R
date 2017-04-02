context('parse sas formats')

test_that('formats are parsed correctly', {
  
  ans <- setNames(c('missing', 'unknown', 'yes', 'no'), c(-99, -1, 1, 2))
  
  expect_identical(
    ans, parse_formats('-99=missing, -1=unknown, 1=yes, 2=no')
  )
  
  expect_identical(
    ans, parse_formats('-99=missing, -1=unknown, 1=yes, 2=no')
  )
  
  expect_identical(
    ans, parse_formats('-99=missing;-1=unknown,1=yes;2=no')
  )
  
  ## periods can be used but require whitespace
  expect_identical(
    ans, parse_formats('-99=missing. -1=unknown,1=yes. 2=no')
  )
  
  
  ## invert=TRUE
  expect_identical(
    setNames(names(ans), ans),
    parse_formats('-99=missing, -1=unknown, 1=yes, 2=no', invert = TRUE)
  )
})


test_that('formats are applied correctly', {
  
  ans <- factor(c('no', 'no', NA, 'missing', 'yes'),
                c('missing', 'unknown', 'yes', 'no'))
  
  fmt <- '-99=missing, -1=unknown, 1=yes, 2=no'
  
  expect_identical(
    ans, apply_formats(c(2, 2, NA, -99, 1), fmt)
  )
  
  expect_identical(
    ans, apply_formats(c(2, 2, NA, -99, 1), parse_formats(fmt))
  )
  
  expect_identical(
    droplevels(ans),
    apply_formats(c(2, 2, NA, -99, 1), fmt, droplevels = TRUE)
  )
  
  
  ## reverse should also work
  expect_identical(
    as.numeric(
      as.character(
        apply_formats(ans, fmt, invert = TRUE)
      )
    ), c(2, 2, NA, -99, 1)
  )
  
  
  ## "3" converted to NA but input NA count does not match output NA count
  ## which should throw a warning but results should be identical
  expect_warning(
    ans1 <- apply_formats(c(2, 2, 3, -99, 1), fmt)
  )
  expect_identical(
    ans, ans1
  )
  
})
