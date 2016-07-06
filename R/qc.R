#' Count QC flags set.
#'
#' @param qc
#' @param id The QC flag ids.
#' @return Number of QC flags set.
#' @export
qcflags <- function(qc, id) {
  mask <- 2^(id-1)
  return(sapply(qc, function(x) {
    return(sum(bitwAnd(x, mask) > 0))
  }))
}
