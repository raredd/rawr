### data
# ctcae_v3, ctcae_v4
###


#' Common Terminology Criteria for Adverse Events, v3
#'
#' A dataset containing the CTCAE (version 3) toxicity codes, descriptions,
#' and categories.
#' 
#' @source
#' \url{http://ctep.cancer.gov/protocolDevelopment/electronic_applications/docs/ctcaev3.pdf}
#' 
#' \url{https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm}
#'
#' @format
#' A data frame with 1059 rows and 4 variables:
#' \tabular{lll}{
#' \tab \code{tox_code} \tab alphanumeric toxicity code (\code{AB123}) \cr
#' \tab \code{tox_desc} \tab description \cr
#' \tab \code{tox_cat} \tab category \cr
#' \tab \code{ctc_version} \tab version number \cr
#' }
#' 
#' @seealso
#' \code{\link{ctcae_v4}}; \code{\link{match_ctc}}; \code{\link{tox_worst}}

'ctcae_v3'

#' Common Terminology Criteria for Adverse Events, v4
#'
#' A dataset containing the CTCAE (version 4) toxicity codes, descriptions,
#' and categories.
#'  
#' @source
#' \url{https://ctep.cancer.gov/protocoldevelopment/electronic_applications/docs/ctcae_4_with_lay_terms.pdf}
#' 
#' \url{https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm}
#'
#' @format
#' A data frame with 790 rows and 5 variables:
#' \tabular{lll}{
#' \tab \code{tox_code} \tab alphanumeric toxicity code (\code{AB123}) \cr
#' \tab \code{tox_desc} \tab description \cr
#' \tab \code{tox_cat} \tab category \cr
#' \tab \code{ctc_version} \tab version number \cr
#' \tab \code{MedDRA_code_v12} \tab Medical Dictionary for Regulatory 
#' Activities codes, v12 \cr
#' }
#' 
#' @seealso
#' \code{\link{ctcae_v3}}; \code{\link{match_ctc}}; \code{\link{tox_worst}}

'ctcae_v4'
