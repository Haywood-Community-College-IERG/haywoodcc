#' Convert term sequence to term code
#'
#' Convert term sequence to term code.
#'
#' @param termseq A term sequence string - YYYYNN where NN is 01, 02, or 03
#' @export
term_code <- function(termseq) {
    return(paste0(substr(termseq,1,4),
        dplyr::if_else(substr(termseq,5,6)=="01","SP",
            dplyr::if_else(substr(termseq,5,6)=="02","SU","FA"))
        )
    )
}
