context('simon two-stage')

test_that('simon and simon2 give equivalent results', {
  
  ## design matrix is stored differently in simon2
  ## but should have identical results:
  expect_equivalent(rawr:::simon(.2, .4)[[1]], 
                    simon2(.2, .4)[[1]][[1]])
  
  ## more complex test:
  ## currently, the p0 is expanded first and then pa:
  # > expand.grid(p0 = c(.1, .2), pa = c(.3, .4))
  #      p0  pa
  #   1 0.1 0.3
  #   2 0.2 0.3
  #   3 0.1 0.4
  #   4 0.2 0.4
  ## so we choose the fourth combination
  expect_equivalent(rawr:::simon(.2, .4)[[1]], 
                    simon2(c(.1, .2), c(.3, .4))[[1]][[4]])
})