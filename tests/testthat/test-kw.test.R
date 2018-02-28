context('kw.test')


test_that('assuming categorical data, all types give same results', {
  kw <- function(...) {
    res <- kw.test(...)
    res$data.name <- NULL
    res
  }
  
  ## ordered cat, 2/3 unique values
  oy3 <- factor(mtcars$gear, ordered = TRUE)
  ox2 <- factor(mtcars$am,   ordered = TRUE)
  ox3 <- factor(mtcars$carb, ordered = TRUE)
  
  ## unordered cat, 2/3 unique values
  uy3 <- factor(mtcars$gear, ordered = FALSE)
  ux2 <- factor(mtcars$am,   ordered = FALSE)
  ux3 <- factor(mtcars$carb, ordered = FALSE)
  
  
  expect_identical(
    kw(ux2, oy3),
    kw(ux2, as.character(oy3))
  )
  
  expect_identical(
    kw(ux3, oy3),
    kw(ux3, as.character(oy3))
  )
  
  expect_identical(
    kw(ux3, oy3),
    kw(ux3, as.character(oy3))
  )
  
  expect_identical(
    kw(as.character(ux3), oy3),
    kw(as.character(ux3), as.character(oy3))
  )
})
