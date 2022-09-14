#' Load the YAML configuration file.
#'
#' Load the specified YAML configuration file. If no file is provided,
#' load "./config.yml".
#'
#' @importFrom ccdwr getColleagueData
#' @importFrom dplyr filter select collect mutate group_by summarize ungroup distinct
#' @importFrom tidyr unnest
#' @importFrom lubridate ymd
#' @importFrom rlang .data
#'
# #' @export
#'
high_school_graduation_dates <- function() {
    institutions_attend <- ccdwr::getColleagueData( "INSTITUTIONS_ATTEND" ) %>%

        # Keep HS graduation records
        dplyr::filter( .data$INSTA.INST.TYPE == "HS",
                       .data$INSTA.GRAD.TYPE == "Y" ) %>%
        dplyr::select( ID=.data$INSTA.PERSON.ID,
                       Institution_Name=.data$X.INSTA.INSTITUTION,
                       End_Dates=.data$INSTA.END.DATES
        ) %>%
        dplyr::collect() %>%

        # For some reason, there are some records with multiple dates, keep the earliest one
        dplyr::mutate( End_Date = strsplit(.data$End_Dates,", ") ) %>%
        tidyr::unnest( .data$End_Date ) %>%
        dplyr::select( -.data$End_Dates ) %>%
        dplyr::filter( !is.na(.data$End_Date) ) %>%
        dplyr::mutate( End_Date = ymd(.data$End_Date) ) %>%
        dplyr::group_by( .data$ID ) %>%
        dplyr::summarize( HS_Grad_Date = min(.data$End_Date) ) %>%
        dplyr::ungroup() %>%

        dplyr::distinct()
}
