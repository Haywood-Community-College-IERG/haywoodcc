#' Create a National Student Clearinghouse request file
#'
#' Create a National Student Clearinghouse request file in the required format.
#' This requires a config file which contains required elements.
#' DOB must be either a date or a character string with format "%Y%m%d".
#'
#' @param df Dataframe to convert into format for submission to NSC
#' @param config A configuration dataframe for NSC files
#' @param inquiryType One of CO (Longitudinal Cohort), DA (Declined Admission), PA (Prior Attendance), or SE (Subsequent Enrollment)
#' @param path Path where NSC file will be saved (default=getwd())
#' @param fn Name of the NSC file (default=same name as the dataframe with "_SE.tsv" appended)
#' @param search If the dataframe does not include a 'Search Begin Date' field, this will be used (default=TODAY)
#' @param enrolledStudents Are these students currently enrolled? Set to FALSE for PA query to allow SSN.
#' @export
#' @importFrom magrittr %<>%
#'
nsc_request <- function(df,
                        config,
                        inquiryType="SE",
                        path=getwd(),
                        fn=paste0(deparse(substitute(df)),"_SE.tsv"),
                        search=format(Sys.Date(),"%Y%m%d"),
                        enrolledStudents=ifelse(inquiryType=="SE",FALSE,TRUE)
                        ) {

    if (missing(df)) {
        stop("Dataframe not provided")
    }

    # Verify dataframe includes the required fields:
    #     FirstName, MiddleInitial, LastName, Suffix, DOB
    if (any(!(c("FirstName", "LastName", "DOB") %in% colnames(df)))) {
        stop("Dataframe does not include all of the required fields: FirstName, LastName, DOB")
    }

    if ( "SSN" %in% colnames(df) &
         (inquiryType!="PA" | (inquiryType=="PA" & enrolledStudents==TRUE)) ) {
        warning(paste0("SSN provided but ignored - iquiry(",inquiryType,"), enrolled(",enrolledStudents,")"))
    }

    if (missing(path)) {
        path <- getwd()
        warning(paste("Path set to",path))
    }

    # If search is just YYYY, set to YYYY0101, if just YYYYMM, set to YYYYMM01.
    if (nchar(search)==4) {
        search %<>% paste0("0101")
        warning(paste("search changed to",search))
    } else if (nchar(search)==6) {
        search %<>% paste0("01")
        warning(paste("search changed to",search))
    }

    if (missing(fn)) {
        # Get the name of 'df' and use as filename
        fn <- paste0(deparse(substitute(df)),"_",inquiryType,".tsv")
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
    # Allow this to be overridden for PA query of non-enrolled students
    if (inquiryType=="PA" & enrolledStudents==FALSE & "SSN" %in% colnames(df))
        r$SSN <- df$SSN
    else
        r$SSN <- trimws("         ") # SSN cannot be provided in SE request

    clean <- function(x) iconv(x,,to="ASCII//TRANSLIT")

    r$FirstName <- trimws(substring(format(clean(df$FirstName),width=20),1,20))

    if ("MiddleInitial" %in% colnames(df))
        r$MI <- dplyr::coalesce(substring(clean(df$MiddleInitial),1,1),"")
    else
        r$MI <- trimws(" ")

    r$LastName <- trimws(substring(format(clean(df$LastName),width=20),1,20))

    if ("Suffix" %in% colnames(df))
        r$Suffix <- trimws(substring(format(clean(df$Suffix),width=5),1,5))
    else
        r$Suffix <- trimws("     ")

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
                     inquiryType,
                     "I",
                     stringsAsFactors = FALSE )

    t <- data.frame("T1",as.character(nrow(r)+2,0), stringsAsFactors = FALSE)

    # Write out the file
    readr::write_tsv(h,path=nscFile,append=FALSE,col_names=FALSE)
    readr::write_tsv(r,path=nscFile,append=TRUE,col_names=FALSE)
    readr::write_tsv(t,path=nscFile,append=TRUE,col_names=FALSE)

    return(r)
}
