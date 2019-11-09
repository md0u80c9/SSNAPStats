#' SSNAP fake data (for testing / development)
#'
#' Data that has been entered into SSNAP under the fictitious
#' 'Fake Hospital'. Please NB this is not meant to be a
#' representative dataset: it is mainly used to test functions and
#' has examples to test any problems.
#'
#' @docType data
#'
#' @usage data(ssnap_fake_data)
#'
#' @format An object of class \code{tibble}; the data is created
#' from a SSNAP CSV export of the fake hospital data and run through
#' \code{ssnapinterface::read_audit_csv()} to generate the dataset.
#'
#' @keywords datasets
#'
#' @references SSNAP Website
#' (\href{http://www.strokeaudit.org}{Stroke Sentinel National Audit 
#' Programme})
#'
#' @source The \href{http://www.strokeaudit.org}{Stroke Sentinel 
#' National Audit Programme} have created these records. It is
#' possible to contribute to this dataset by contacting the team
#' directly.
#'
#' @examples
#' data(ssnap_fake_data)
"ssnap_fake_data"
