#' Return a list of elements from x that are not in y.
#'
#' @param x Elements that are not to be found in the List y.
#' @param y List to check against.
#' @export
#'
"%nin%" <- function(x, y) !(x %in% y)

#' Coalesce a value to a non-null and non-blank.
#'
#' @param var Value to return if not null or blank.
#' @param default Value to return if var is null or blank.
#' @export
#'
coalesce_blanks <- function( var, default = '' ) {
    return( dplyr::if_else( is.na(var) | var == '', default, var ) )
}

#' Calculate the age.
#'
#' @param from The birthdate or start date.
#' @param to The current date or end date.
#' @export
#'
age <- function(from, to) {
    from_lt = as.POSIXlt(from)
    to_lt = as.POSIXlt(to)

    age = to_lt$year - from_lt$year

    ifelse(to_lt$mon < from_lt$mon |
               (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
           age - 1, age)
}

#' Return an element if it is like any one of the provided patterns.
#'
#' @param x Elements to return if it matches any one pattern.
#' @param pattern Patterns to match.
#' @export
#'
`%like any%` <- function(x, pattern) {

    pattern <- sapply(pattern, function(z){
        if (!substr(z, 1, 1) == "%") {
            z <- paste("^", z, sep="")
        } else {
            z <- substr(z, 2, nchar(z) )
        }
        if (!substr(z, nchar(z), nchar(z)) == "%") {
            z <- paste(z, "$", sep="")
        } else {
            z <- substr(z, 1, nchar(z)-1 )
        }
        return(z)
    })

    grepl(pattern=paste(pattern, collapse = "|"), x=x)

    # since 0.99.17: better returning the values, than a logical vector:
    # grep(pattern=paste(pattern, collapse = "|"), x=x, value=TRUE)

    # rolled back 26.4.2016: did not really prove successful

}

#' The opposite of \%like any\%.
#'
#' @param x Elements to return if it matches any one pattern.
#' @param pattern Patterns to match.
#' @export
#'
`%nlike%` <- function(x, pattern) !`%like any%`(x,pattern)
