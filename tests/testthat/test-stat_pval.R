context('stat_pval')

## singly-ordered (y - column)
##   x - categorical
##   x - continuous
## doubly-ordered (y - column)
##   x - categorical

guess <- function(x, y, pattern) {
  suppressWarnings(
    res <- guess_test(x, y)
  )
  
  grepl(pattern, attr(res, 'FUN'), ignore.case = TRUE, perl = TRUE)
}

test_that('correct test is used based on input - contingency table', {
  
  ## ordered cat, 2/3 unique values
  oy2 <- factor(mtcars$vs,   ordered = TRUE)
  oy3 <- factor(mtcars$gear, ordered = TRUE)
  
  ox2 <- factor(mtcars$am,   ordered = TRUE)
  ox3 <- factor(mtcars$carb, ordered = TRUE)
  
  ## unordered cat, 2/3 unique values
  uy2 <- factor(mtcars$vs,   ordered = FALSE)
  uy3 <- factor(mtcars$gear, ordered = FALSE)
  
  ux2 <- factor(mtcars$am,   ordered = FALSE)
  ux3 <- factor(mtcars$carb, ordered = FALSE)
  
  
  ## both ordered
  expect_true(guess(ox2, oy2, 'fish'))
  expect_true(guess(ox2, oy3, 'krus.*trend'))
  expect_true(guess(ox3, oy2, 'krus.*trend'))
  expect_true(guess(ox3, oy3, 'jonck'))
  
  ## one ordered
  expect_true(guess(ux2, oy2, 'fish'))
  expect_true(guess(ux2, oy3, 'krus.*trend'))
  expect_true(guess(ux3, oy2, 'fish'))
  expect_true(guess(ux3, oy3, 'krus.*trend'))
  
  expect_true(guess(ox2, uy2, 'fish'))
  expect_true(guess(ox2, uy3, 'fish'))
  expect_true(guess(ox3, uy2, 'krus.*trend'))
  expect_true(guess(ox3, uy3, 'krus.*trend'))
  
  ## both unordered
  expect_true(guess(ux2, uy2, 'fish'))
  expect_true(guess(ux2, uy3, 'fish'))
  expect_true(guess(ux3, uy2, 'fish'))
  expect_true(guess(ux3, uy3, 'fish'))
})


test_that('correct test is used based on input - continuous variable', {
  
  ## ordered cat, 2/3 unique values
  oy2 <- factor(mtcars$vs,   ordered = TRUE)
  oy3 <- factor(mtcars$gear, ordered = TRUE)
  
  ## unordered cat, 2/3 unique values
  uy2 <- factor(mtcars$vs,   ordered = FALSE)
  uy3 <- factor(mtcars$gear, ordered = FALSE)
  
  x <- mtcars$mpg
  
  
  ## ordered
  expect_true(guess(x, oy2, 'wilcox'))
  expect_true(guess(x, oy3, 'cuzick'))
  
  ## unordered
  expect_true(guess(x, uy2, 'wilcox'))
  expect_true(guess(x, uy3, 'krus.*(?!trend)'))
  
  
  ## character strings
  expect_true(guess(x, as.character(oy2), 'wilcox'))
  expect_true(guess(x, as.character(oy3), 'krus.*(?!trend)'))
})

test_that('getPval* testing', {
  
  ## ordered cat, 2/3 unique values
  oy2 <- factor(mtcars$vs,   ordered = TRUE)
  oy3 <- factor(mtcars$gear, ordered = TRUE)
  
  ox2 <- factor(mtcars$am,   ordered = TRUE)
  ox3 <- factor(mtcars$carb, ordered = TRUE)
  
  ## unordered cat, 2/3 unique values
  uy2 <- factor(mtcars$vs,   ordered = FALSE)
  uy3 <- factor(mtcars$gear, ordered = FALSE)
  
  ux2 <- factor(mtcars$am,   ordered = FALSE)
  ux3 <- factor(mtcars$carb, ordered = FALSE)
  
  x <- mtcars$mpg
  
  
  expect_identical(
    getPvalKruskal(x, uy3),
    Gmisc:::getPvalKruskal(x, uy3)
  )
  
  expect_identical(
    getPvalKruskal(x, uy3),
    getPvalKruskal(x, as.character(uy3))
  )
})
