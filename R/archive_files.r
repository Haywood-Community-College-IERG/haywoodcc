library(dplyr)
library(magrittr)
library(tools)

informer.path <- "//INFORMER/donder"
informer.path <- "L:/TISS/RIE/Research/Surveys/Internal/student satisfaction survey/"

dest.path <- "L:/TISS/RIE/Data/DataMart/Archive"

ts <- format(Sys.time(), "%Y%m%d_%H%M%S")

files <- data.frame( sfn = list.files(informer.path, recursive=TRUE), stringsAsFactors = FALSE)
files %<>% mutate( dfn=file.path(dest.path,dirname(sfn),
                                 paste0(file.basename(basename(sfn)),"_",ts,".",file_ext(sfn))),
                   sfn=file.path(informer.path,sfn) )
