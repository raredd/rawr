context('Visual tests')

test_that('Visual tests for kmplot and kmplot_by', {
  quote({
    ## visual tests
    library('rawr')
    library('survival')
    colon2 <- within(colon[duplicated(colon$id), ], {
      pfs_time <- time
      pfs_ind  <- status
      sex <- c('Female','Male')[sex + 1L]
      sex1 <- 'Male'
    })
    
    ## --- color mapping --- ##
    
    ## colors should map
    kmplot_by('sex', 'pfs', colon2, by = 'sex', single = FALSE)
    
    ## colors should _not_ map
    kmplot_by('sex', 'pfs', colon2, by = 'sex', single = FALSE, col.surv = 1)
    
    ## colors should map
    kmplot_by('sex', 'pfs', colon2, by = 'sex', single = FALSE,
              col.surv = c(Male = 'green', Female = 'blue'))
    
    ## --- labels are not mapped -- low priority --- ##
    
    kmplot_by('sex', 'pfs', colon2, by = 'sex', single = FALSE, strata_lab = 1:2,
              col.surv = c('1' = 'green', '2' = 'blue'))
    
    kmplot_by('sex', 'pfs', colon2, by = 'sex', single = FALSE, strata_lab = c('1','2'))
    
    ## labels are mapped
    kmplot_by('sex', 'pfs', colon2, by = 'rx', single = FALSE, strata_lab = c('1','2'),
              col.surv = c('1' = 'magenta', '2' = 'blue'))
    
    ## --- drop a level for one plot --- ##
    
    colon2$rx[colon2$rx == 'Obs' & colon2$sex == 'Male'] <- NA
    s <- survfit(Surv(pfs_time, pfs_ind) ~ rx, colon2)
    
    kmplot(s, col.surv = 1:3)
    
    kmplot(s, col.surv = c('rx=Obs' = 'red')) ## expect warning
    
    ## second plot should drop black line not green
    kmplot_by(s, by = 'sex', col.surv = 1:3)
    
    kmplot_by(s, by = 'sex', strata_lab = FALSE,
              col.surv = c('Obs' = 1, 'Lev' = 2, 'Lev+5FU' = 'purple'))
    ## second plot should drop black line not purple
    kmplot_by(s, by = 'sex', col.surv = c(1, 'purple', 2))
  })
  
})
