pkg.env <- new.env(parent = emptyenv())

#' Return enrollment for specified term as of the IPEDS reporting date of October 15
#'
#' All data comes from CCDW_HIST SQL Server database
#'
#' @param report_years The ending year of the academic year of the data
#' @param report_semesters Either a single semester abbreviation or a list of semester abbreviations. If unspecified, all semesters are returned.
#' @export
#' @importFrom ccdwr getColleagueData
#' @importFrom magrittr %>%
#' @importFrom dplyr select collect mutate filter inner_join left_join
#'     group_by summarise distinct anti_join ungroup coalesce
#' @importFrom stringr str_c
#'
term_enrollment <- function( report_years = NA_integer_, report_semesters = NA_character_ ) {

    terms <- ccdwr::getColleagueData( "Term_CU", schema = "dw_dim" ) %>%
        dplyr::select( .data$Term_ID,
                       .data$Term_Index,
                       Term_Name = .data$Semester,
                       Semester = .data$Term_Abbreviation,
                       .data$Term_Start_Date,
                       .data$Term_Census_Date,
                       .data$Term_End_Date,
                       Term_Reporting_Year = .data$Reporting_Year_FSS,
                       Academic_Year = .data$Reporting_Academic_Year_FSS ) %>%
        dplyr::collect() %>%
        dplyr::mutate( Term_Index = as.integer(.data$Term_Index),
                       Term_Start_Date = as.Date(.data$Term_Start_Date),
                       Term_End_Date = as.Date(.data$Term_End_Date),
                       Term_Census_Date = as.Date(.data$Term_Census_Date),
                       Term_Reporting_Year = as.integer(.data$Term_Reporting_Year) ) %>%
        dplyr::mutate( Term_Reporting_Year = .data$Term_Reporting_Year - 1 )

    if (!is.na(report_years)) {
        if (length(report_years) == 1) {
            reporting_terms <- terms %>%
                dplyr::filter( .data$Term_Reporting_Year == report_years )
        } else {
            reporting_terms <- terms %>%
                dplyr::filter( .data$Term_Reporting_Year %in% report_years )
        }
    } else {
        reporting_terms <- terms
    }

    if (!is.na(report_semesters)) {
        if (length(report_semesters) == 1) {
            reporting_terms %<>%
                dplyr::filter( .data$Semester == report_semesters )
        } else {
            reporting_terms %<>%
                dplyr::filter( .data$Semester %in% report_semesters )
        }
    }


    # Need to get section location for distance learning courses
    # Right now, just take most recent. Probably need to do this the same way as SAC below.
    course_sections <- ccdwr::getColleagueData( "COURSE_SECTIONS" ) %>%
        dplyr::select( Course_Section_ID = .data$COURSE.SECTIONS.ID,
                       Term_ID = .data$SEC.TERM,
                       Section_Location = .data$SEC.LOCATION,
                       Delivery_Method = .data$X.SEC.DELIVERY.METHOD,
                       Delivery_Mode = .data$X.SEC.DELIVERY.MODE #,
#                       Delivery_NCIH_Flag = .data$X.SEC.DELIVERY.NCIH.FLAG,
#                       Delivery_Modifier = .data$X.SEC.DELIVERY.MODIFIER
             ) %>%
        dplyr::collect()

    student_acad_cred <- ccdwr::getColleagueData( "STUDENT_ACAD_CRED", version="history" ) %>%
        dplyr::filter( .data$STC.ACAD.LEVEL == "CU",
                       .data$STC.CRED > 0 ) %>%
        dplyr::select( ID = .data$STC.PERSON.ID,
                       Term_ID = .data$STC.TERM,
                       Course_ID = .data$STUDENT.ACAD.CRED.ID,
                       Credit = .data$STC.CRED,
                       Course_Level = .data$STC.COURSE.LEVEL,
                       Grade_Code = .data$STC.VERIFIED.GRADE,
                       Course_Section = .data$STC.SECTION.NO,
                       .data$EffectiveDatetime,
                       Course_Section_ID = .data$STC.COURSE.SECTION,
                       Course_Status = .data$STC.STATUS ) %>%
        dplyr::collect() %>%
        dplyr::mutate( Credit = as.number(.data$Credit) ) %>%
        dplyr::inner_join( terms %>%
                               dplyr::select(.data$Term_ID,
                                             .data$Term_Reporting_Year,
                                             .data$Semester,
                                             .data$Term_Census_Date),
                           by = "Term_ID" ) %>%
        dplyr::mutate( Keep_FA = ((.data$Semester == "FA") &
                                      (.data$EffectiveDatetime <= as.Date(str_c(.data$Term_Reporting_Year,"-10-15")) )),
                       Keep_NF = (.data$Semester != "FA") ) %>%
        dplyr::filter( .data$Keep_FA | .data$Keep_NF ) %>%
        dplyr::select( -c(.data$Keep_FA, .data$Keep_NF) ) %>%
        dplyr::left_join( course_sections, by = c("Term_ID","Course_Section_ID") )

    #
    # Get most recent effective date for each person for each term for each course
    #
    sac_max_effdt <- student_acad_cred %>%
        dplyr::group_by( .data$ID, .data$Term_ID, .data$Course_ID ) %>%
        dplyr::summarise( EffectiveDatetime = max(.data$EffectiveDatetime) )

    #
    # Now get the course data for the latest courses.
    # Use Status of A,N for FA since we want only enrolled courses at the cutoff date
    #     (This will be taken care of later as we need the W credits to determine load)
    # Use Status A,N,W for SP,SU since these were all the courses enrolled in at census
    #
    sac_most_recent_all <- student_acad_cred %>%
        dplyr::inner_join( sac_max_effdt,
                           by = c("ID", "Term_ID", "Course_ID", "EffectiveDatetime") ) %>%
        dplyr::filter( .data$Course_Status %in% c('A', 'N', 'W') ) %>%
        dplyr::select( -.data$EffectiveDatetime ) %>%
        dplyr::distinct() %>%
        dplyr::select( -.data$Course_ID )

    #
    # Get list of students who are taking at least 1 non-developmental/audited course
    #
    sac_most_recent_non_dev_ids <- sac_most_recent_all %>%
        dplyr::filter( dplyr::coalesce(.data$Course_Level,"ZZZ") != "DEV" ) %>%
        dplyr::filter( dplyr::coalesce(.data$Grade_Code,'X') != '9' ) %>%
        dplyr::select( .data$ID, .data$Term_ID ) %>%
        dplyr::distinct()

    #
    # Get list of students who are taking at least 1 distance course
    #
    sac_most_recent_1_distance_ids <- sac_most_recent_all %>%
        dplyr::inner_join( sac_most_recent_non_dev_ids, by = c("ID", "Term_ID") ) %>%
        dplyr::filter( .data$Delivery_Method == "IN" ) %>%

        dplyr::filter( dplyr::coalesce(.data$Grade_Code,'X') != '9' ) %>%
        dplyr::select( .data$ID, .data$Term_ID ) %>%
        dplyr::distinct()

    #
    # Get list of students who are taking at least 1 regular course
    #
    sac_most_recent_f2f_ids <- sac_most_recent_all %>%
        dplyr::filter( .data$Delivery_Method != "IN" ) %>%
        dplyr::filter( dplyr::coalesce(.data$Grade_Code,'X') != '9' ) %>%
        dplyr::select( .data$ID, .data$Term_ID ) %>%
        dplyr::distinct()

    sac_most_recent_all_distance_ids <- sac_most_recent_1_distance_ids %>%
        dplyr::anti_join( sac_most_recent_f2f_ids, by = c("ID", "Term_ID") ) %>%
        dplyr::mutate( Distance_Courses = "All" )

    sac_most_recent_distance_ids <- sac_most_recent_1_distance_ids %>%
        dplyr::left_join( sac_most_recent_all_distance_ids, by = c("ID", "Term_ID") ) %>%
        dplyr::mutate( Distance_Courses = dplyr::coalesce(.data$Distance_Courses,"At least 1") )

    # Determine which students have completely withdrawn at the end or by Oct 15
    sac_most_recent_all_withdraws <- sac_most_recent_all %>%
        dplyr::filter( .data$Course_Status %in% c('W') ) %>%
        dplyr::anti_join( sac_most_recent_all %>%
                              dplyr::filter( .data$Course_Status %in% c('A', 'N') ),
                          by = c("ID", "Term_ID" ) ) %>%
        dplyr::select( .data$ID, .data$Term_ID ) %>%
        dplyr::distinct() %>%
        dplyr::mutate( Enrollment_Status = "Withdrawn" )

    #
    # Now create a summary table to calculate load by term
    #
    sac_load_by_term <- sac_most_recent_all %>%
        dplyr::inner_join( sac_most_recent_non_dev_ids, by = c("ID", "Term_ID") ) %>%
        dplyr::inner_join( terms %>% dplyr::select( .data$Term_ID,
                                                    .data$Term_Reporting_Year,
                                                    .data$Semester ),
                           by = c("Term_ID", "Term_Reporting_Year", "Semester") ) %>%
        dplyr::group_by( .data$ID,
                         .data$Term_ID,
                         .data$Term_Reporting_Year,
                         .data$Semester ) %>%
        dplyr::summarise( Credits = sum(.data$Credit) ) %>%
        dplyr::mutate( Status = dplyr::if_else(.data$Credits >= 12, "FT", "PT") ) %>%
        dplyr::ungroup() %>%
        dplyr::left_join( sac_most_recent_distance_ids, by = c("ID", "Term_ID") ) %>%
        dplyr::left_join( sac_most_recent_all_withdraws, by = c("ID", "Term_ID") ) %>%
        dplyr::mutate( Distance_Courses = dplyr::coalesce(.data$Distance_Courses,"None"),
                       Enrollment_Status = dplyr::coalesce(.data$Enrollment_Status,"Enrolled") )

    #
    # Take term load table and reduce to the reporting terms
    #
    if (!is.na(report_years)) {
        sac_report_load_by_term <- sac_load_by_term %>%
            dplyr::inner_join( reporting_terms %>% dplyr::select( .data$Term_ID,
                                                                  .data$Term_Reporting_Year ),
                               by = c("Term_ID", "Term_Reporting_Year") )

        return( sac_report_load_by_term )
    } else {
        return( sac_load_by_term )
    }
}

#' A special function to call term_enrollment for just a fall term
#'
#' All data comes from CCDW_HIST SQL Server database
#'
#' @param report_years The year of the fall term for the data
#' @export
#'
fall_enrollment <- function( report_years = NA ) {
    return( term_enrollment( report_years, "FA" ) )
}

#' Return a data frame of students who are curriculum credential seekers (seeking an Associate's, Diploma, or Certificate)
#'
#' All data comes from CCDW_HIST SQL Server database
#'
#' @param report_years The year or a list of years of the fall term for the data. If unspecified, all years are returned.
#' @param report_semesters Either a single semester abbreviation or a list of semester abbreviations. If unspecified, all semesters are returned.
#' @param exclude_hs Should function exclude high school students from being included as credential seekers. Default is to include high school students.
#' @export
#' @importFrom ccdwr getColleagueData
#' @importFrom magrittr %<>% %>%
#' @importFrom dplyr select collect mutate filter inner_join anti_join
#'     full_join left_join distinct case_when coalesce
#'
credential_seekers <- function( report_years = NA_integer_, report_semesters = NA_character_, exclude_hs = FALSE ) {
    # Make build happy
    hs_student <- NULL

    terms <- ccdwr::getColleagueData( "Term_CU", schema = "dw_dim" ) %>%
        dplyr::select( .data$Term_ID,
                       .data$Term_Index,
                       Term_Name = .data$Semester,
                       Semester = .data$Term_Abbreviation,
                       .data$Term_Start_Date,
                       .data$Term_Census_Date,
                       .data$Term_End_Date,
                       Term_Reporting_Year = .data$Reporting_Year_FSS,
                       Academic_Year = .data$Reporting_Academic_Year_FSS ) %>%
        dplyr::collect() %>%
        dplyr::mutate( Term_Index = as.integer(.data$Term_Index),
                       Term_Start_Date = as.Date(.data$Term_Start_Date),
                       Term_End_Date = as.Date(.data$Term_End_Date),
                       Term_Census_Date = as.Date(.data$Term_Census_Date),
                       Term_Reporting_Year = as.integer(.data$Term_Reporting_Year) ) %>%
        dplyr::mutate( Term_Reporting_Year = .data$Term_Reporting_Year - 1 )

    reporting_terms <- terms

    if (!is.na(report_years)) {
        if (length(report_years) == 1) {
            reporting_terms %<>%
                dplyr::filter( .data$Term_Reporting_Year == report_years )
        } else {
            reporting_terms %<>%
                dplyr::filter( .data$Term_Reporting_Year %in% report_years )
        }
    }

    if (!is.na(report_semesters)) {
        if (length(report_semesters) == 1) {
            reporting_terms %<>%
                dplyr::filter( .data$Semester == report_semesters )
        } else {
            reporting_terms %<>%
                dplyr::filter( .data$Semester %in% report_semesters )
        }
    }

    # Get only CU programs from ACAD_PROGRAMS
    acad_programs <- ccdwr::getColleagueData( "ACAD_PROGRAMS" ) %>%
        dplyr::filter( .data$ACPG.ACAD.LEVEL == "CU" ) %>%
        dplyr::select( Program = .data$ACAD.PROGRAMS.ID ) %>%
        dplyr::collect()

    students_stu_types <- ccdwr::getColleagueData( "STUDENTS__STU_TYPES", version="history" ) %>%
        dplyr::select( ID = .data$STUDENTS.ID,
                       Student_Type = .data$STU.TYPES,
                       Student_Type_Date = .data$STU.TYPE.DATES,
                       Student_Type_End_Date = .data$STU.TYPE.END.DATES,
                       .data$CurrentFlag,
                       .data$EffectiveDatetime ) %>%
        dplyr::collect() %>%
        dplyr::mutate( Student_Type_Date = as.Date(.data$Student_Type_Date),
                       Student_Type_End_Date = as.Date(.data$Student_Type_End_Date),
                       EffectiveDatetime = as.Date(.data$EffectiveDatetime) )

    students <- students_stu_types %>%
        dplyr::filter( .data$CurrentFlag == "Y" ) %>%
        dplyr::select( .data$ID, .data$EffectiveDatetime ) %>%
        dplyr::distinct() %>%
        dplyr::inner_join( students_stu_types %>%
                               dplyr::filter( .data$Student_Type %in% c("HUSK","DUAL","CCPP","ECOL") ),
                           by=c("ID","EffectiveDatetime") ) %>%
        dplyr::select( -.data$EffectiveDatetime )

    #
    # Get program dates (this is a multi-valued field that needs to be joined with full table).
    #
    student_programs__dates <- ccdwr::getColleagueData( "STUDENT_PROGRAMS__STPR_DATES", version="history" ) %>%
        dplyr::select( ID = .data$STPR.STUDENT,
                       Program = .data$STPR.ACAD.PROGRAM,
                       Program_Start_Date = .data$STPR.START.DATE,
                       Program_End_Date = .data$STPR.END.DATE,
                       .data$EffectiveDatetime ) %>%
        dplyr::collect() %>%
        dplyr::mutate( Program_Start_Date = as.Date(.data$Program_Start_Date),
                       Program_End_Date = as.Date(.data$Program_End_Date),
                       EffectiveDatetime = as.Date(.data$EffectiveDatetime) ) %>%
        dplyr::inner_join( acad_programs, by="Program" ) %>%
        dplyr::mutate( Program_End_Date = dplyr::coalesce(.data$Program_End_Date,as.Date("9999-12-31")) )

    if (exclude_hs) {
        student_programs__dates %<>% dplyr::anti_join( haywoodcc::high_school_programs, by = "Program" )
    }

    #
    # Credential-seekers are those in A, D, or C programs
    #
    credential_seeking <- ccdwr::getColleagueData( "STUDENT_PROGRAMS__STPR_DATES" ) %>%
        dplyr::select( ID = .data$STPR.STUDENT,
                       Program = .data$STPR.ACAD.PROGRAM,
                       .data$EffectiveDatetime ) %>%
        dplyr::distinct() %>%
        dplyr::collect() %>%
        dplyr::mutate( EffectiveDatetime = as.Date(.data$EffectiveDatetime) ) %>%
        dplyr::inner_join( acad_programs, by="Program" ) %>%
        dplyr::inner_join( student_programs__dates, by = c("ID", "Program", "EffectiveDatetime") ) %>%
        dplyr::select( -.data$EffectiveDatetime ) %>%

        # Identify credential seekers.
        dplyr::mutate( Credential_Seeker = case_when(
                           substring(.data$Program,1,1) %in% c('A','D','C') ~ 1,
                           TRUE ~ 0
                       ),
                       j = 1 ) %>%

        # Cross join with terms to get all the terms they were enrolled in this credential program.
        dplyr::full_join( terms %>%
                              dplyr::select(.data$Term_ID,
                                            .data$Term_Start_Date,
                                            .data$Term_Census_Date,
                                            .data$Term_End_Date) %>%
                              dplyr::mutate(j=1),
                          by = "j" ) %>%
        dplyr::filter( .data$Program_Start_Date <= .data$Term_Census_Date,
                       .data$Program_End_Date >= .data$Term_Census_Date,
                       .data$Credential_Seeker > 0 ) %>%

        dplyr::left_join( students, by="ID" ) %>%
        dplyr::mutate( hs_student = (.data$Student_Type_Date <= .data$Term_Census_Date) &
                           (.data$Student_Type_End_Date >= .data$Term_Census_Date) ) %>%
        dplyr::mutate( hs_student = dplyr::coalesce(hs_student,FALSE) ) %>%
        dplyr::mutate( keep = case_when(
                           exclude_hs ~ dplyr::case_when( hs_student ~ FALSE, TRUE ~ TRUE ),
                           TRUE ~ TRUE
                           )) %>%
        dplyr::filter( .data$keep ) %>%

        dplyr::select( .data$ID, .data$Term_ID, .data$Credential_Seeker ) %>%
        dplyr::distinct() %>%
        dplyr::inner_join( reporting_terms %>% dplyr::select(.data$Term_ID), by = "Term_ID" )

    return( credential_seeking )
}

#' A special function to call credential_seekers for just a fall term
#'
#' All data comes from CCDW_HIST SQL Server database
#'
#' @param report_years The year of the fall term for the data
#' @param exclude_hs Should function exclude high school students from being included as credential seekers. Default is to include high school students.
#' @export
#'
fall_credential_seekers <- function( report_years, exclude_hs = FALSE ) {
    return( haywoodcc::credential_seekers( report_years, "FA", exclude_hs ) )
}


#' Return a data from of the IPEDS cohort data.
#'
#' Return a data from of the IPEDS cohort data. Data will come either from the file ipeds_cohorts.csv or
#' from the CCDW_HIST SQL Server database.
#'
#' @param report_years The year of the fall term for the data
#' @param cohorts Which cohorts to include in data frame. FT = Full-time First-time, PT = Part-time First-time,
#'                TF = Full-time Transfer, TP = Part-time Transfer,
#'                RF = Full-time Returning, RP = Part-time Returning
#' @param use Which dataset should be used for cohorts from Colleague
#'            ipeds_cohorts Use the local.ipeds_cohorts table
#'            STUDENT_TERMS Use the history.STUDENT_TERMS_Current view
#' @param useonly Use the database cohort found in the table specified in the
#'     `use` parameter only. Default is FALSE which means combine data from
#'     the database table with the file ipeds_cohorts.csv.
#' @param ipeds_path The path where ipeds_cohort.csv file is located.
#' @export
#' @importFrom ccdwr getColleagueData
#' @importFrom purrr has_element
#' @importFrom dplyr filter select collect bind_rows distinct mutate
#' @importFrom stringr str_c
#' @importFrom readr read_csv cols col_character
#'
ipeds_cohort <- function( report_years,
                          cohorts=c("FT","PT","TF","TP","RF","RP"),
                          use = "ipeds_cohorts",
                          ipeds_path,
                          useonly = TRUE ) {

    report_cohorts <- ""
    if (purrr::has_element(cohorts,"FT")) report_cohorts <- c(report_cohorts, stringr::str_c(report_years,"FT"))
    if (purrr::has_element(cohorts,"PT")) report_cohorts <- c(report_cohorts, stringr::str_c(report_years,"PT"))
    if (purrr::has_element(cohorts,"TF")) report_cohorts <- c(report_cohorts, stringr::str_c(report_years,"TF"))
    if (purrr::has_element(cohorts,"TP")) report_cohorts <- c(report_cohorts, stringr::str_c(report_years,"TP"))
    if (purrr::has_element(cohorts,"RF")) report_cohorts <- c(report_cohorts, stringr::str_c(report_years,"RF"))
    if (purrr::has_element(cohorts,"RP")) report_cohorts <- c(report_cohorts, stringr::str_c(report_years,"RP"))
    report_cohorts <- report_cohorts[-1]

    if (missing(ipeds_path)) {
        warning("Parameter ipeds_path not provided. Using local directory.")
        ipeds_path <- '.'
    }

    if (file.exists(file.path(ipeds_path,"ipeds_cohorts.csv"))) {
        ipeds_cohort_FILE_COHORTS <- readr::read_csv( file.path(ipeds_path,"ipeds_cohorts.csv"),
                                                      col_types = readr::cols(.default=readr::col_character()) ) %>%
            dplyr::filter( .data$Cohort %in% c(report_cohorts) ) %>%
            dplyr::select( .data$ID, .data$Term_ID, .data$Cohort )
    } else {
        useonly = TRUE
    }

    if (toupper(use) == "STUDENT_TERMS") {
        ipeds_cohort_COLLEAGUE_COHORTS <- ccdwr::getColleagueData( "STUDENT_TERMS" ) %>%
            dplyr::select( ID = .data$STTR.STUDENT, Cohort = .data$STTR.FED.COHORT.GROUP ) %>%
            dplyr::filter( .data$Cohort %in% c(report_cohorts) ) %>%
            dplyr::distinct() %>%
            dplyr::collect() %>%
            dplyr::mutate( Term_ID = stringr::str_c(substring(.data$Cohort,1,4),"FA") )
    } else {
        if (toupper(use) != "IPEDS_COHORTS") {
            warning(str_c("Invallid use value (",use,"), defaulting to 'ipeds_cohorts'"))
        }
        ipeds_cohort_COLLEAGUE_COHORTS <- ccdwr::getColleagueData( "ipeds_cohorts", schema="local", version="latest" ) %>%
            dplyr::select( .data$ID, .data$Term_ID, .data$Cohort ) %>%
            dplyr::filter( !is.na(.data$Cohort) ) %>%
            dplyr::collect()
    }

    if (useonly) {
        ic <- ipeds_cohort_COLLEAGUE_COHORTS
    } else {
        ic <- ipeds_cohort_FILE_COHORTS %>%
            dplyr::bind_rows( ipeds_cohort_COLLEAGUE_COHORTS )
    }

    return( ic )
}
