#' Convert National Student Clearinghouse return file to database format
#'
#' Convert the National Student Clearinghouse return file to a database format
#' that is easier to work with. Each student will have one record per college
#' attended. Additional records are added for each degree earned. If a student
#' did not attend a college that reports to the NSC, then Record Found Y/N will
#' be 'N'.
#'
#' @param fn Full path to the details file provided by NSC
#' @param type The type of merge. Possible options are csv, fixed, other
#' @keywords file
#' @export
#' @importFrom tidyr fill
#' @importFrom magrittr %<>%
#'
nsc_return_se_convert <- function(fn) {

    # Load the specified file into a dataframe
    students <- tibble::as_tibble(
        readr::read_csv(fn, col_types=readr::cols(.default = readr::col_character()))
        )

    # get students with no activity into no_act, remove from students
    no_act <- students %>% dplyr::filter( `Record Found Y/N` == 'N' ) %>%
        # Remove graduation columns - they will be added back later
        dplyr::select( `Your Unique Identifier`:`Search Date` )

    # ...For the rest, convert the dates into dates
    students %<>% dplyr::filter( `Record Found Y/N` == 'Y' ) %>%
        dplyr::mutate( `Enrollment Begin` = as.Date( `Enrollment Begin`, "%Y%m%d" ),
                       `Enrollment End` = as.Date( `Enrollment End`, "%Y%m%d" ),
                       `Enrollment Days` = `Enrollment End` - `Enrollment Begin`
        )

    # Fix the graduation records
    graduated <- students %>%
        # Now keep only the graduated records
        dplyr::filter( `Graduated?` == 'Y' ) %>%
        dplyr::select( `Last Name`, `First Name`, `Middle Initial`, `Name Suffix`, `College Sequence`,
                       `Graduated?`, `Graduation Date`, `Degree Title`,
                       `Degree Major 1`, `Degree CIP 1`,
                       `Degree Major 2`, `Degree CIP 2`,
                       `Degree Major 3`, `Degree CIP 3`,
                       `Degree Major 4`, `Degree CIP 4`
                     ) %>%
        dplyr::mutate( `Degree Title` = ifelse(is.na(`Degree Title`), "UNKNOWN", `Degree Title`),
                       `Degree Major 1` = ifelse(is.na(`Degree Major 1`), "UNKNOWN", `Degree Major 1`),
                       `Degree CIP 1` = ifelse(is.na(`Degree CIP 1`), "UNKNOWN", `Degree CIP 1`)
                     )

    # Remove graduation records as they were handled above
    students %<>% dplyr::filter( `Graduated?` == 'N' ) %>%
        # Remove graduation columns - they will be added back later
        dplyr::select( -`Graduated?`, -`Graduation Date`, -dplyr::starts_with("Degree") ) %>%
        # Add a row index
        dplyr::mutate( RowNumber = dplyr::row_number() ) %>%
        #
        # We need to fill down the CollegeSequence value since it is missing for
        #    subsequent records
        dplyr::group_by( `Last Name`, `First Name`, `Middle Initial`, `Name Suffix`,
                         `Requester Return Field`,
                         `Enrollment Begin` ) %>%
        tidyr::fill( `College Sequence` ) %>%
        # Now regroup so we can add the number of Semesters at Institution,
        #     keeping one row with earliest Begin date and the latest End date
        dplyr::group_by( `Last Name`, `First Name`, `Middle Initial`, `Name Suffix`,
                         `Requester Return Field`,
                         `College Sequence`
                         ) %>%
        dplyr::mutate( `Semesters at Institution` = n(),
                       SemesterIndex = dplyr::row_number(),
                       `Enrollment Begin` = min(`Enrollment Begin`),
                       `Enrollment End` = max(`Enrollment End`),
                       `Total Enrollment Days` = sum(`Enrollment Days`),
                       `Last Enrollment Major 1` = dplyr::last(`Enrollment Major 1`),
                       `Last Enrollment CIP 1` = dplyr::last(`Enrollment CIP 1`),
                       `Last Enrollment Major 2` = dplyr::last(`Enrollment Major 2`),
                       `Last Enrollment CIP 2` = dplyr::last(`Enrollment CIP 2`)
                       ) %>%
        dplyr::filter(SemesterIndex == 1) %>%
        #dplyr::filter( RowNumber==max(RowNumber) ) %>%
        # Drop the RowNumber variable as it is no longer needed
        dplyr::select( -RowNumber, -SemesterIndex )


    students %<>%
        # Bring students with no activity back into the data frame
        dplyr::bind_rows(no_act) %>%
        # Bring in graduation data
        dplyr::left_join(graduated,
                         by = c("Last Name", "First Name", "Middle Initial", "Name Suffix", "College Sequence")
                        ) %>%
        dplyr::mutate( `Graduated?` = ifelse(is.na(`Graduated?`), "N", `Graduated?`)) %>%
        dplyr::arrange( `Last Name`, `First Name`, `Middle Initial`, `Name Suffix`,
                 `Requester Return Field`,
                 `College Sequence`
                 ) %>%
        dplyr::select( `Last Name`, `First Name`, `Middle Initial`, `Name Suffix`,
                       `Requester Return Field`, `Record Found Y/N`, `Search Date`,
                       `College Sequence`, `College Code/Branch`, `College Name`, `College State`,
                       `2-year / 4-year`, `Public / Private`,
                       `Enrollment Begin`, `Enrollment End`, `Enrollment Status`, `Class Level`,
                       `Enrollment Major 1`, `Enrollment CIP 1`,
                       `Enrollment Major 2`, `Enrollment CIP 2`,
                       `Last Enrollment Major 1`, `Last Enrollment CIP 1`,
                       `Last Enrollment Major 2`, `Last Enrollment CIP 2`,
                       `Semesters at Institution`, `Total Enrollment Days`,
                       `Graduated?`, `Graduation Date`, `Degree Title`,
                       `Degree Major 1`, `Degree CIP 1`,
                       `Degree Major 2`, `Degree CIP 2`,
                       `Degree Major 3`, `Degree CIP 3`,
                       `Degree Major 4`, `Degree CIP 4`
                     )

    return(students)
}
