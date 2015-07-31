### package housekeeping
# ctcae_v3, ctcae_v4
###


if (getRversion() >= '2.15.1') {
  utils::globalVariables(c('corr','nQQ','y','ID','ci','surv','n.censor',
                           'upper','lower','quant','group','hazard','n.risk',
                           'n','s','digits'))
}

#' Common Terminology Criteria for Adverse Events, v3
#'
#' A dataset containing the CTCAE (version 3) toxicity codes, descriptions, and
#' categories.
#'
#' @format A data frame with 1059 rows and 4 variables:
#' 
#' \tabular{lll}{
#' \tab \code{tox_code} \tab alphanumeric toxicity code (\code{AB123}) \cr
#' \tab \code{tox_desc} \tab description \cr
#' \tab \code{tox_cat} \tab category \cr
#' \tab \code{ctc_version} \tab version number \cr
#' }
'ctcae_v3'

#' Common Terminology Criteria for Adverse Events, v4
#'
#' A dataset containing the CTCAE (version 4) toxicity codes, descriptions, and
#' categories.
#'  
#' @source
#' \href{http://www.dfhcc.harvard.edu/fileadmin/DFHCC_Admin/Clinical_Trials/QACT/Policies_and_Procedures/CTCToxVersion4.pdf}{CTCAE v4}
#'
#' @format A data frame with 790 rows and 5 variables:
#' 
#' \tabular{lll}{
#' \tab \code{tox_code} \tab alphanumeric toxicity code (\code{AB123}) \cr
#' \tab \code{tox_desc} \tab description \cr
#' \tab \code{tox_cat} \tab category \cr
#' \tab \code{ctc_version} \tab version number \cr
#' \tab \code{MedDRA_code_v12} \tab Medical Dictionary for Regulatory 
#' Activities codes, v12 \cr
#' }
'ctcae_v4'
