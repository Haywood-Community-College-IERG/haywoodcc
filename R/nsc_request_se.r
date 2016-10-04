#' Create a National Student Clearinghouse Subsequent Enrollment (SE) request file
#'
#' Create a National Student Clearinghouse Subsequent Enrollment (SE) request file
#' in the required format. This requires a config file which contains required
#' elements. DOB must be either a date or a character string with format "%Y%m%d".
#'
#' @param df Dataframe to convert into format for submission to NSC
#' @param config A configuration dataframe for NSC files
#' @param path Path where NSC file will be saved (default=getwd())
#' @param fn Name of the NSC file (default=same name as the dataframe with "_SE.tsv" appended)
#' @param search If the dataframe does not include a 'Search Begin Date' field, this will be used (default=TODAY)
#' @export
#' @importFrom magrittr %<>%
#'
nsc_request_se <- function(df,
                           config,
                           path=getwd(),
                           fn=paste0(deparse(substitute(df)),"_SE.tsv"),
                           search=format(Sys.Date(),"%Y%m%d")) {

    if (missing(df)) {
        stop("Dataframe not provided")
    }

    # Verify dataframe includes the required fields:
    #     FirstName, MiddleInitial, LastName, Suffix, DOB
    if (any(!(c("FirstName", "MiddleInitial", "LastName", "Suffix", "DOB") %in% colnames(df)))) {
        stop("Dataframe does not include all of the required fields: FirstName, MiddleInitial, LastName, Suffix, DOB")
    }

    if ("SSN" %in% colnames(df)) {
        warning("SSN provided - Not valid in SE request and will be ignored")
    }

    if (missing(path)) {
        path <- getwd()
        warning(paste("Path set to",path))
    }

    # If search is just YYYY, set to YYYY0101, if just YYYYMM, set to YYYYMM01.
    if (missing(search)) {
        search <- format(Sys.Date(),"%Y%m%d")
        warning(paste("search set to",search))
    } else if (nchar(search)==4) {
        search %<>% paste0("0101")
        warning(paste("search changed to",search))
    } else if (nchar(search)==6) {
        search %<>% paste0("01")
        warning(paste("search changed to",search))
    }

    if (missing(fn)) {
        # Get the name of 'df' and use as filename
        fn <- paste0(deparse(substitute(df)),"_SE.tsv")
        warning(paste("fn set to",fn))
    }

    nscFile <- file.path(path,fn)

    # Check if the following optional fields are provided:
    #     SearchBeginDate, ReturnRequestField
    if (!("SearchBeginDate" %in% colnames(df))) {
        warning(paste("SearchBeginDate not provided - defaulting to",search))
    }

    if (!("ReturnRequestField" %in% colnames(df))) {
        warning("ReturnRequestField not provided - you may have difficulty matching return records")
    }

    # Ensure DOB is a date formatted as YYYYMMDD

    # Ensure all fields are padded to the correct lengths and create 'nsc' dataframe
    r <- data.frame( a = 1:length(df$FirstName) )
    r$RecordType <- "D1"
    r$SSN <- trimws("         ") # SSN cannot be provided in SE request

    r$FirstName <- trimws(substring(format(df$FirstName,width=20),1,20))
    r$MI <- dplyr::coalesce(substring(df$MiddleInitial,1,1),"")
    r$LastName <- trimws(substring(format(df$LastName,width=20),1,20))
    r$Suffix <- trimws(substring(format(df$Suffix,width=5),1,5))

    if (class(df$DOB)=="Date")
        r$DOB <- format(df$DOB,"%Y%m%d")
    else
        r$DOB <- df$DOB

    r$SearchBeginDate <- ifelse( !("SearchBeginDate" %in% colnames(df)),
                                   search, dplyr::coalesce(df$SearchBeginDate,search)
                                   )
    r$Blank <- trimws(" ")

    r$SchoolCode <- config$schoolCode
    r$BranchCode <- config$branchCode

    if ("ReturnRequestField" %in% colnames(df))
        r$ReturnRequestField <- trimws(format(df$ReturnRequestField,width=50))
    else
        r$ReturnRequestField <- trimws(format(" ",width=50))

    r$a<-NULL

    h <- data.frame( "H1",
                     config$schoolCode,
                     config$branchCode,
                     trimws(substring(format(config$schoolName,width=40),1,40)),
                     format(Sys.Date(), "%Y%m%d"),
                     "SE",
                     "I",
                     stringsAsFactors = FALSE )

    t <- data.frame("T1",as.character(nrow(r)+2,0), stringsAsFactors = FALSE)

    # Write out the file
    readr::write_tsv(h,path=nscFile,append=FALSE,col_names=FALSE)
    readr::write_tsv(r,path=nscFile,append=TRUE,col_names=FALSE)
    readr::write_tsv(t,path=nscFile,append=TRUE,col_names=FALSE)

    return(r)
}
