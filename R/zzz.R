## sci printing in docs
options(scipen = -1L)

## binding notes
if (getRversion() >= '2.15.1') {
  utils::globalVariables(
    c('x','survfit','survdiff','biocLite','dt_assess_start','dd_reg',
      'status','dt_offstudy','dt_end','dd_prog','dd_assess_start',
      'makeCluster','detectCores','stopCluster','clusterExport','parLapply')
  )
}
