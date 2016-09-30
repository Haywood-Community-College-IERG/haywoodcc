library(tidyverse)

#' Convert term sequence to term code
#'
#' This function allows you to merge all the files in a specified folder. 
#' It handles removing the extra header lines, if necessary. It also 
#' adds the file name as an extra variable, if this is necessary.
#'
#' @param path Defaults to the current working directory using getwd()
#' @param pattern The pattern used to filter the files prior to the merge
#' @param type The type of merge. Possible options are csv, fixed, other
#' @param FNvar The name of the variable which will contain the filename. Will only add filename if this parameter has a variable name.
#' @keywords file
#' @export
term_code <- function(termseq) { 
    return(paste0(substr(termseq,1,4),
        if_else(substr(termseq,5,6)=="01","SP",
            if_else(substr(termseq,5,6)=="02","SU","FA"))
        )
    )
}