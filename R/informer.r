pkg.env <- new.env(parent = emptyenv())

#' Return a data from data warehouse Colleague tables.
#'
#' Return a data from of the IPEDS cohort data. Data will come either from the file ipeds_cohorts.csv of
#' from the CCDW_HIST SQL Server database.
#'
#' @param file The name of the Colleague file to return
#' @param schema Which schema should be used. Needed for non-Colleague tables.
#' @param version Specify which version to include. Default is for the latest data. Any other value will return the dated file.
#' @param cfg A YAML configuration file with sql section that includes driver, server, db, and schema_history
#' @param sep The separator to use in the field names. Default is a '.' as in the original Colleague file.
#' @export
#' @importFrom stringr str_c
#'
getColleagueData <- function( file, schema="history", version="latest", cfg=NA_character, sep='.' ) {

    if (is.na(cfg)) {
        cfg <- yaml::yaml.load_file("config.yml")
    }

    conn_str <- str_c( str_c("Driver",   str_c("{", cfg$sql$driver, "}"), sep="="),
                       str_c("Server",   cfg$sql$server,                  sep="="),
                       str_c("Database", cfg$sql$db,                      sep="="),
                       "Trusted_Connection=Yes",
                       "Description=Informer.r:getColleagueData()",
                       sep=";"
    )

    ccdwconn <- odbc::dbConnect( odbc::odbc(), .connection_string=conn_str )

    schema_history <- cfg$sql$schema_history

    df <- dplyr::tbl(ccdwconn, dbplyr::in_schema(schema, file) )

    if (version == "latest" & schema=="history") {
        df %<>% filter( CurrentFlag == "Y" )
    }

    if (sep != '.') names(df) <- gsub("\\.", sep, names(df))

    getColleagueData <- df
}

high_school_graduation_dates <- function() {
    institutions_attend <- getColleagueData( "INSTITUTIONS_ATTEND" ) %>%

        # Keep HS graduation records
        filter( INSTA.INST.TYPE == "HS",
                INSTA.GRAD.TYPE == "Y" ) %>%
        select( ID=INSTA.PERSON.ID,
                Institution_Name=X.INSTA.INSTITUTION,
                End_Dates=INSTA.END.DATES
        ) %>%
        collect() %>%

        # For some reason, there are some records with multiple dates, keep the earliest one
        mutate( End_Date = strsplit(End_Dates,", ") ) %>%
        unnest( End_Date ) %>%
        select( -End_Dates ) %>%
        filter( !is.na(End_Date) ) %>%
        mutate( End_Date = ymd(End_Date) ) %>%
        group_by( ID ) %>%
        summarize( HS_Grad_Date = min(End_Date) ) %>%
        ungroup() %>%

        distinct()
}
