pkg.env <- new.env(parent = emptyenv())

pkg.env$ir_root <- "L:/IERG"
pkg.env$cfg <- yaml::yaml.load_file(file.path(pkg.env$ir_root, "Data/config.yml"))

pkg.env$ipeds_scripts_path <- pkg.env$cfg$R$scripts_path
pkg.env$ipeds_path <- file.path(pkg.env$ir_root, "Data", "IPEDS")

#' Return enrollment for specified term as of the IPEDS reporting date of October 15
#'
#' All data comes from IERG SQL Server database
#'
#' @param report_year The starting year of the academic year of the data
#' @param report_semesters Either a single semester abbreviation or a list of semester abbreviations. If unspecified, all semesters are returned.
#' @export
#' @import magrittr
#' @import dplyr
#' @import stringr
#'
term_enrollment <- function( report_year, report_semesters = NA_character_ ) {

    terms <- getColleagueData( "Term_CU", schema = "dw_dim" ) %>%
        select( Term_ID,
                Term_Index,
                Term_Name = Semester,
                Semester = Term_Abbreviation,
                Term_Start_Date,
                Term_Census_Date,
                Term_End_Date,
                Term_Reporting_Year = Reporting_Year,
                Academic_Year ) %>%
        collect() %>%
        mutate( Term_Reporting_Year = as.integer(Term_Reporting_Year) )

    reporting_terms <- terms %>%
        filter( Term_Reporting_Year == report_year )

    if (!is.na(report_semesters)) {
        if (length(report_semesters) == 1) {
            reporting_terms %<>%
                filter( Semester == report_semesters )
        } else {
            reporting_terms %<>%
                filter( Semester %in% report_semesters )
        }
    }


    student_acad_cred <- getColleagueData( "STUDENT_ACAD_CRED", version="previous" ) %>%
        filter( STC.ACAD.LEVEL == "CU",
                STC.CRED > 0 ) %>%
        select( ID = STC.PERSON.ID,
                Term_ID = STC.TERM,
                Course_ID = STUDENT.ACAD.CRED.ID,
                Course_Status = STC.STATUS,
                Credit = STC.CRED,
                Course_Level = STC.COURSE.LEVEL,
                Grade_Code = STC.VERIFIED.GRADE,
                Course_Section = STC.SECTION.NO,
                EffectiveDatetime ) %>%
        collect() %>%
        inner_join( terms %>%
                        select(Term_ID, Term_Reporting_Year, Semester, Term_Census_Date),
                    by = "Term_ID" ) %>%
        mutate( Keep_FA = ((Semester == "FA") & (EffectiveDatetime < as.Date(str_c(Term_Reporting_Year,'-10-15')) )),
                Keep_NF = (Semester != "FA") ) %>%
        filter( Keep_FA | Keep_NF ) %>%
        select( -c(Keep_FA, Keep_NF) )

    #
    # Get most recent effective date for each person for each term for each course
    #
    sac_max_effdt <- student_acad_cred %>%
        group_by( ID, Term_ID, Course_ID ) %>%
        summarise( EffectiveDatetime = max(EffectiveDatetime) )

    #
    # Now get the course data for the latest courses.
    # Use Status of A,N for FA since we want only enrolled courses at the cutoff date
    # Use Status A,N,W for SP,SU since these were all the courses enrolled in at census
    #
    sac_most_recent_all <- student_acad_cred %>%
        inner_join( sac_max_effdt,
                    by = c("ID", "Term_ID", "Course_ID", "EffectiveDatetime") ) %>%
        mutate( Keep_FA = ((Semester == "FA") & (Course_Status %in% c("A", "N"))),
                Keep_NF = (Semester != "FA") & (Course_Status %in% c("A", "N", "W")) ) %>%
        filter( Keep_FA | Keep_NF ) %>%
        # filter( ((Semester == "FA") & (Course_Status %in% c("A", "N"))) |
        #             ((Semester != "FA") & (Course_Status %in% c("A", "N", "W"))) ) %>%
        select( -c(Keep_FA, Keep_NF,EffectiveDatetime) ) %>%
        distinct() %>%
        select( -Course_ID )

    #
    # Get list of students who are taking at least 1 non-developmental/audited course
    #
    sac_most_recent_non_dev_ids <- sac_most_recent_all %>%
        filter( coalesce(Course_Level,'ZZZ') != 'DEV' ) %>%
        filter( coalesce(Grade_Code,'X') != "9" ) %>%
        select( ID, Term_ID ) %>%
        distinct()

    #
    # Get list of students who are taking at least 1 distance course
    #
    sac_most_recent_1_distance_ids <- sac_most_recent_all %>%
        filter( str_detect(coalesce(Course_Section,'ZZZ'), "W") ) %>%
        filter( coalesce(Grade_Code,'X') != "9" ) %>%
        select( ID, Term_ID ) %>%
        distinct()

    #
    # Get list of students who are taking at least 1 regular course
    #
    sac_most_recent_f2f_ids <- sac_most_recent_all %>%
        filter( !(str_detect(coalesce(Course_Section,'ZZZ'), "W")) ) %>%
        filter( coalesce(Grade_Code,'X') != "9" ) %>%
        select( ID, Term_ID ) %>%
        distinct()

    sac_most_recent_all_distance_ids <- sac_most_recent_1_distance_ids %>%
        anti_join( sac_most_recent_f2f_ids, by = c("ID", "Term_ID") ) %>%
        mutate( Distance_Courses = "All" )

    sac_most_recent_distance_ids <- sac_most_recent_1_distance_ids %>%
        left_join( sac_most_recent_all_distance_ids, by = c("ID", "Term_ID") ) %>%
        mutate( Distance_Courses = coalesce(Distance_Courses,"At least 1") )

    #
    # Now create a summary table to calculate load by term
    #
    sac_load_by_term <- sac_most_recent_all %>%
        inner_join( sac_most_recent_non_dev_ids, by = c("ID", "Term_ID") ) %>%
        inner_join( terms %>% select( Term_ID, Term_Reporting_Year, Semester ),
                    by = c("Term_ID", "Term_Reporting_Year", "Semester") ) %>%
        group_by( ID, Term_ID, Term_Reporting_Year, Semester ) %>%
        summarise( Credits = sum(Credit) ) %>%
        mutate( Status = if_else(Credits >= 12, "FT", "PT") ) %>%
        ungroup() %>%
        left_join( sac_most_recent_distance_ids, by = c("ID", "Term_ID") ) %>%
        mutate( Distance_Courses = coalesce(Distance_Courses,"None") )

    #
    # Take term load table and reduce to the reporting terms
    #
    sac_report_load_by_term <- sac_load_by_term %>%
        inner_join( reporting_terms %>% select( Term_ID, Term_Reporting_Year ),
                    by = c("Term_ID", "Term_Reporting_Year") )

    return( sac_report_load_by_term )
}

#' A special function to call term_enrollment for just a fall term
#'
#' All data comes from IERG SQL Server database
#'
#' @param report_year The year of the fall term for the data
#' @export
#'
fall_enrollment <- function( report_year ) {
    return( term_enrollment( report_year, "FA" ) )
}

#' Return a data frame of students who are curriculum credential seekers (seeking an Associate's, Diploma, or Certificate)
#'
#' All data comes from IERG SQL Server database
#'
#' @param report_year The year of the fall term for the data
#' @param report_semesters Either a single semester abbreviation or a list of semester abbreviations. If unspecified, all semesters are returned.
#' @param exlude_hs Should function exclude high school students from being included as credential seekers. Default is to include high school students.
#' @export
#' @importFrom magrittr %<>%
#'
credential_seekers <- function( report_year, report_semesters = NA_character_, exclude_hs = FALSE ) {

    terms <- getColleagueData( "Term_CU", schema = "dw_dim" ) %>%
        select( Term_ID,
                Term_Index,
                Term_Name = Semester,
                Semester = Term_Abbreviation,
                Term_Start_Date,
                Term_Census_Date,
                Term_End_Date,
                Term_Reporting_Year = Reporting_Year,
                Academic_Year ) %>%
        collect() %>%
        mutate( Term_Reporting_Year = as.integer(Term_Reporting_Year) )

    if (!is.na(report_semesters)) {
        if (length(report_semesters) == 1) {
            reporting_terms %<>%
                filter( Semester == report_semesters )
        } else {
            reporting_terms %<>%
                filter( Semester %in% report_semesters )
        }
    }

    #
    # Get program dates (this is a multi-valued field that needs to be joined with full table).
    #
    student_programs__dates <- getColleagueData( "STUDENT_PROGRAMS__STPR_DATES", version="history" ) %>%
        select( ID = STPR.STUDENT, Program = STPR.ACAD.PROGRAM, Program_Start_Date = STPR.START.DATE, Program_End_Date = STPR.END.DATE, EffectiveDatetime ) %>%
        filter( !(Program %in% c('BSP', 'AHS', 'CONED', '=GED', '=HISET', 'C11', 'C50')) ) %>%
        collect() %>%
        mutate( Program_End_Date = coalesce(Program_End_Date,as.Date('9999-12-31')) )

    if (exclude_hs) {
        student_programs__dates %<>% anti_join( high_school_programs, by = "Program" )
    }

    #
    # Credential-seekers are those in A, D, or C programs
    #
    credential_seeking <- getColleagueData( "STUDENT_PROGRAMS" ) %>%
        select( ID = STPR.STUDENT, Program = STPR.ACAD.PROGRAM, EffectiveDatetime ) %>%
        filter( !(Program %in% c('BSP', 'AHS', 'CONED', '=GED', '=HISET', 'C11', 'C50')) ) %>%
        collect() %>%
        inner_join( student_programs__dates, by = c("ID", "Program", "EffectiveDatetime") ) %>%
        select( -EffectiveDatetime ) %>%

        # Identify credential seekers.
        mutate( Credential_Seeker = case_when(
                    substring(Program,1,1) %in% c("A","D","C") ~ 1,
                    TRUE ~ 0
                ),
                j = 1 ) %>%

        # Cross join with terms to get all the terms they were enrolled in this credential program.
        full_join( terms %>%
                       select(Term_ID, Term_Start_Date, Term_Census_Date, Term_End_Date) %>%
                       mutate(j=1),
                   by = "j" ) %>%
        filter( Program_Start_Date <= Term_Census_Date,
                Program_End_Date >= Term_Census_Date,
                Credential_Seeker > 0 ) %>%
        select( ID, Term_ID, Credential_Seeker ) %>%
        distinct() %>%
        inner_join( reporting_terms %>% select(Term_ID), by = "Term_ID" )

    return( credential_seeking )
}

#' A special function to call credential_seekers for just a fall term
#'
#' All data comes from IERG SQL Server database
#'
#' @param report_year The year of the fall term for the data
#' @param exlude_hs Should function exclude high school students from being included as credential seekers. Default is to include high school students.
#' @export
#'
fall_credential_seekers <- function( report_year, exclude_hs = FALSE ) {
    return( credential_seekers( report_year, "FA", exclude_hs ) )
}


#' Return a data from of the IPEDS cohort data.
#'
#' Return a data from of the IPEDS cohort data. Data will come either from the file ipeds_cohorts.csv of
#' from the IERG SQL Server database.
#'
#' @param report_year The year of the fall term for the data
#' @param cohorts Which cohorts to include in data frame. FT = Full-time First-time, PT = Part-time First-time,
#'                TF = Full-time Transfer, TP = Part-time Transfer,
#'                RF = Full-time Returning, RP = Part-time Returning
#' @export
#'
ipeds_cohort <- function( report_year, cohorts=c("FT","PT","TF","TP","RF","RP") ) {

    report_cohorts <- ""
    if (purrr::has_element(cohorts,"FT")) report_cohorts <- c(report_cohorts, str_c(report_year,"FT"))
    if (purrr::has_element(cohorts,"PT")) report_cohorts <- c(report_cohorts, str_c(report_year,"PT"))
    if (purrr::has_element(cohorts,"TF")) report_cohorts <- c(report_cohorts, str_c(report_year,"TF"))
    if (purrr::has_element(cohorts,"TP")) report_cohorts <- c(report_cohorts, str_c(report_year,"TP"))
    if (purrr::has_element(cohorts,"RF")) report_cohorts <- c(report_cohorts, str_c(report_year,"RF"))
    if (purrr::has_element(cohorts,"RP")) report_cohorts <- c(report_cohorts, str_c(report_year,"RP"))
    report_cohorts <- report_cohorts[-1]

    ipeds_cohort_FILE_COHORTS <- read_csv( file.path(ipeds_path,"ipeds_cohorts.csv"), col_types = cols(.default=col_character()) ) %>%
        filter( Cohort %in% c(report_cohorts) ) %>%
        select( ID, Term_ID, Cohort )

    ipeds_cohort_COLLEAGUE_COHORTS <- getColleagueData( "STUDENT_TERMS" ) %>%
        select( ID = STTR.STUDENT, Cohort = STTR.FED.COHORT.GROUP ) %>%
        filter( Cohort %in% c(report_cohorts) ) %>%
        distinct() %>%
        collect() %>%
        mutate( Term_ID = str_c(substring(Cohort,1,4),"FA") )

    ic <- ipeds_cohort_FILE_COHORTS %>%
        bind_rows( ipeds_cohort_COLLEAGUE_COHORTS )

    return( ic )
}
