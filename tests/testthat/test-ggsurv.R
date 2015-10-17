context('ggsurv compatible')

library('survival')
fit <- survfit(Surv(time, status) ~ 1, data = cancer)

test_that('ggsurv: backwards compatiblity check', {
  
  ## deprecated arguments:
  ## surv.col, surv.lty, cens.col, cens.shape, band.col, atrisk.col
  
  expect_warning(ggsurv(fit, atrisk = FALSE, surv.col = 'red'))
  expect_warning(ggsurv(fit, atrisk = FALSE, surv.lty = 2))
  expect_warning(ggsurv(fit, atrisk = FALSE, cens.col = 'red'))
  expect_warning(ggsurv(fit, atrisk = FALSE, cens.shape = 1))
  expect_warning(ggsurv(fit, atrisk = FALSE, band.col = 'red', 
                        confband = TRUE, confin = FALSE))
  expect_warning(ggsurv(fit, atrisk = TRUE, legend = FALSE, 
                        atrisk.col = 'red'))
  
  ## check multiple parse correctly
  expect_warning(ggsurv(fit, atrisk = FALSE, 
                        surv.col = 'red', surv.lty = 2))
  
  ## check combinations
  expect_warning(ggsurv(fit, atrisk = FALSE, 
                        surv.col = 'red', surv.lty = 1, unused_arg = TRUE))
})

test_that('ggsurv: unused arguments throws warning', {
  
  expect_warning(ggsurv(fit, atrisk = FALSE, unused_arg = TRUE))
  
  ## check multiple parse correctly
  expect_warning(ggsurv(fit, atrisk = FALSE, 
                        unused_arg = TRUE, another_arg = NULL))
  
  ## check combinations
  expect_warning(ggsurv(fit, atrisk = FALSE, 
                        surv.col = 'red', surv.lty = 2,
                        unused_arg = TRUE, another_arg = NULL))
})

fit <- survfit(Surv(time, status) ~ sex, data = cancer)

test_that('ggsurv: multiple args to deprecated are evaluated correctly', {
  
  expect_warning(ggsurv(fit, atrisk = FALSE, surv.col = c('green','blue')))
  expect_warning(ggsurv(fit, atrisk = FALSE, surv.lty = 1:2))
  expect_warning(ggsurv(fit, atrisk = FALSE, band.col = 1:2,
                        confband = TRUE, confin = FALSE))
  expect_warning(ggsurv(fit, atrisk = TRUE, surv.col = 1:2,
                        legend = FALSE))
})
