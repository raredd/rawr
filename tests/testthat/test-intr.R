context('interval formats')

test_that('intr methods are equivalent', {
  
  i1 <- intr(1:10)
  i2 <- intr(1:10, conf = 0)
  i3 <- intr(1:10, conf = 1)
  i4 <- intr(1:10, conf = -1)
  i5 <- intr(1:10, conf = 95)
  i6 <- intr(1:10, conf = NULL)
  
  expect_identical(i1, i2)
  expect_identical(i2, i3)
  expect_identical(i3, i4)
  expect_identical(i4, i5)
  expect_identical(i5, i6)
})
