context('surv')

test_that('surv_table catches case with no events and maxtime=TRUE', {
  
  library('survival')
  
  ## no events: s$time[s$n.event > 0] # integer(0) -- subscript bounds error
  dd <- data.frame(time = c(10, 20), ind = 0)
  s <- survfit(Surv(time, ind) ~ 1, dd)
  
  expect_identical(surv_table(s), surv_table(s, maxtime = FALSE))
  
  
  ## should not duplicate times -- getting ... 20 20
  dd <- data.frame(time = c(10, 20), ind = 0:1)
  s <- survfit(Surv(time, ind) ~ 1, dd)
  
  expect_identical(surv_table(s), surv_table(s, times = rep(pretty(s$time), 2)))
  
})
