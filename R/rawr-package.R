#' rawr-package
#' 
#' @name rawr-package
#' @docType package
NULL

#' Common Terminology Criteria for Adverse Events, v3
#'
#' A dataset containing the CTCAE (version 3) toxicity codes, descriptions, and
#' categories.
#'
#' @docType data
#' @keywords datasets
#' @name ctcae_v3
#' @usage data(ctcae_v3)
#' @format A data frame with 1059 rows and 4 variables:
#' 
#' \tabular{lll}{
#' \tab \code{tox_code} \tab alphanumeric toxicity code (\code{AB123}) \cr
#' \tab \code{tox_desc} \tab description \cr
#' \tab \code{tox_cat} \tab category \cr
#' \tab \code{ctc_version} \tab version number \cr
#' }
NULL

#' Common Terminology Criteria for Adverse Events, v4
#'
#' A dataset containing the CTCAE (version 4) toxicity codes, descriptions, and
#' categories.
#' 
#' 
#' @references 
#' \href{http://www.dfhcc.harvard.edu/fileadmin/DFHCC_Admin/Clinical_Trials/QACT/Policies_and_Procedures/CTCToxVersion4.pdf}{CTCAE v4}
#' @docType data
#' @keywords datasets
#' @name ctcae_v4
#' @usage data(ctcae_v4)
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
NULL
