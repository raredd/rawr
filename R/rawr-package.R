#' rawr
#' 
#' Personal package with miscellaneous functions, stuff in progress, and
#' tools I use regularly.
#' 
#' @name rawr-package
#' @aliases rawr
#' @docType package
#' @import ggplot2 grid gridExtra knitr plotrix survival testthat
NULL

#' Common Terminology Criteria for Adverse Events, v3
#'
#' A dataset containing the CTCAE (version 3) toxicity codes, descriptions, and
#' categories.
#' 
#' @source
#' \url{http://ctep.cancer.gov/protocolDevelopment/electronic_applications/docs/ctcaev3.pdf}
#'
#' @format
#' A data frame with 1059 rows and 4 variables:
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
#' \url{http://evs.nci.nih.gov/ftp1/CTCAE/CTCAE_4.03_2010-06-14_QuickReference_8.5x11.pdf}
#'
#' @format
#' A data frame with 790 rows and 5 variables:
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
