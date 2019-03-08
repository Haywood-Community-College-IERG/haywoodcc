pkg.env <- new.env(parent = emptyenv())

pkg.env$ir_root <- "L:/IERG"
pkg.env$cfg <- yaml::yaml.load_file(file.path(pkg.env$ir_root, "Data/config.yml"))

#' Return a data from data warehouse Colleague tables.
#'
#' Return a data from of the IPEDS cohort data. Data will come either from the file ipeds_cohorts.csv of
#' from the IERG SQL Server database.
#'
#' @param file The name of the Colleague file to return
#' @param schema Which schema should be used. Needed for non-Colleague tables.
#' @param version Specify which version to include. Default is for the latest data. Any other value will return the dated file.
#' @param sep The separator to use in the field names. Default is a '.' as in the original Colleague file.
#' @export
#' @importFrom stringr str_c
#'
getColleagueData <- function( file, schema="history", version="latest", sep='.' ) {

    cfg <- yaml::yaml.load_file("L:/IERG/Data/config.yml")

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
