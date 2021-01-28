context('kw.test')

## see test-stat_pval for more tests

test_that('assuming categorical data, all types give same results', {
  n <- 100
  y <- rnorm(n)
  
  f <- function(i, size = n) {
    sample(seq.int(i), size, TRUE)
  }
  
  cat_test <- function(x, y, ox = FALSE, oy = FALSE) {
    x <- factor(x, ordered = ox)
    y <- factor(y, ordered = oy)
    suppressWarnings(attr(rawr:::guess_test(x, y), 'FUN'))
  }
  
  con_test <- function(x, y, ox = FALSE, oy = FALSE) {
    if (ox)
      x <- factor(x, ordered = TRUE)
    if (oy)
      y <- factor(y, ordered = TRUE)
    suppressWarnings(attr(rawr:::guess_test(x, y), 'FUN'))
  }
  
  ## categorical
  
  ## unordered - 2x2
  expect_identical(
    cat_test(f(2), f(2)), 'fisher.test'
  )
  ## unordered - 2xn
  expect_identical(
    cat_test(f(2), f(3)), 'fisher.test'
  )
  ## unordered - nx2
  expect_identical(
    cat_test(f(4), f(2)), 'fisher.test'
  )
  ## unordered - nxn
  expect_identical(
    cat_test(f(3), f(4)), 'fisher.test'
  )
  ## ordered but only two levels
  expect_identical(
    cat_test(f(2), f(4), TRUE, FALSE), 'fisher.test'
  )
  
  ## singly-ordered 2xn
  expect_identical(
    cat_test(f(2), f(4), FALSE, TRUE), 'ca.test'
  )
  ## singly-ordered nx2
  expect_identical(
    cat_test(f(3), f(2), TRUE, FALSE), 'ca.test'
  )
  ## doubly-ordered but only two levels
  expect_identical(
    cat_test(f(2), f(3), TRUE, TRUE), 'ca.test'
  )
  ## singly-ordered 3xn
  expect_identical(
    cat_test(f(3), f(4), FALSE, TRUE), 'kw.test'
  )
  ## singly-ordered nx3
  expect_identical(
    cat_test(f(3), f(4), TRUE, FALSE), 'kw.test'
  )
  
  ## doubly-ordered
  expect_identical(
    cat_test(f(2), f(2), TRUE, TRUE), 'fisher.test'
  )
  expect_identical(
    cat_test(f(3), f(5), TRUE, TRUE), 'jt.test'
  )
  
  
  ## mix
  
  # 2x-unordered
  expect_identical(
    con_test(y, f(2), FALSE, FALSE), 'wilcox.test'
  )
  # 2x-ordered but only two levels
  expect_identical(
    con_test(y, f(2), FALSE, TRUE), 'wilcox.test'
  )
  # nx-unordered
  expect_identical(
    con_test(y, f(4), FALSE, FALSE), 'kruskal.test'
  )
  # nx-ordered
  expect_identical(
    con_test(y, f(4), FALSE, TRUE), 'cuzick.test'
  )
  
})
