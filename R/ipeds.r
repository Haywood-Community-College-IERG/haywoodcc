pkg.env <- new.env(parent = emptyenv())

#' Return enrollment for specified term as of the IPEDS reporting date of October 15
#'
#' All data comes from CCDW_HIST SQL Server database
#'
#' @param report_years The ending year of the academic year of the data
#' @param report_semesters Either a single semester abbreviation or a list of semester abbreviations. If unspecified, all semesters are returned.
#' @export
#' @import magrittr
#' @import dplyr
#' @import stringr
#'
term_enrollment <- function( report_years = NA_integer_, report_semesters = NA_character_ ) {

    terms <- getColleagueData( "Term_CU", schema = "dw_dim" ) %>%
        select( Term_ID,
                Term_Index,
                Term_Name = Semester,
                Semester = Term_Abbreviation,
                Term_Start_Date,
                Term_Census_Date,
                Term_End_Date,
                Term_Reporting_Year = Reporting_Year_FSS,
                Academic_Year = Reporting_Academic_Year_FSS ) %>%
        collect() %>%
        mutate( Term_Reporting_Year = as.integer(Term_Reporting_Year) - 1 )

    if (!is.na(report_years)) {
        if (length(report_years) == 1) {
            reporting_terms <- terms %>%
                filter( Term_Reporting_Year == report_years )
        } else {
            reporting_terms <- terms %>%
                filter( Term_Reporting_Year %in% report_years )
        }
    } else {
        reporting_terms <- terms
    }

    if (!is.na(report_semesters)) {
        if (length(report_semesters) == 1) {
            reporting_terms %<>%
                filter( Semester == report_semesters )
        } else {
            reporting_terms %<>%
                filter( Semester %in% report_semesters )
        }
    }


    # Need to get section location for distance learning courses
    # Right now, just take most recent. Probably need to do this the same way as SAC below.
    course_sections <- getColleagueData( "COURSE_SECTIONS" ) %>%
        select( Course_Section_ID = COURSE.SECTIONS.ID,
                Term_ID = SEC.TERM,
                Section_Location = SEC.LOCATION,
                Delivery_Method = X.SEC.DELIVERY.METHOD,
                Delivery_Mode = X.SEC.DELIVERY.MODE #,
#                Delivery_NCIH_Flag = X.SEC.DELIVERY.NCIH.FLAG,
#                Delivery_Modifier = X.SEC.DELIVERY.MODIFIER
             ) %>%
        collect()

    student_acad_cred <- getColleagueData( "STUDENT_ACAD_CRED", version="history" ) %>%
        filter( STC.ACAD.LEVEL == "CU",
                STC.CRED > 0 ) %>%
        select( ID = STC.PERSON.ID,
                Term_ID = STC.TERM,
                Course_ID = STUDENT.ACAD.CRED.ID,
                Credit = STC.CRED,
                Course_Level = STC.COURSE.LEVEL,
                Grade_Code = STC.VERIFIED.GRADE,
                Course_Section = STC.SECTION.NO,
                EffectiveDatetime,
                Course_Section_ID = STC.COURSE.SECTION,
                Course_Status = STC.STATUS ) %>%
        collect() %>%
        inner_join( terms %>%
                        select(Term_ID, Term_Reporting_Year, Semester, Term_Census_Date),
                    by = "Term_ID" ) %>%
        mutate( Keep_FA = ((Semester == "FA") & (EffectiveDatetime <= as.Date(str_c(Term_Reporting_Year,"-10-15")) )),
                Keep_NF = (Semester != "FA") ) %>%
        filter( Keep_FA | Keep_NF ) %>%
        select( -c(Keep_FA, Keep_NF) ) %>%
        left_join( course_sections, by = c("Term_ID","Course_Section_ID") )

    #
    # Get most recent effective date for each person for each term for each course
    #
    sac_max_effdt <- student_acad_cred %>%
        group_by( ID, Term_ID, Course_ID ) %>%
        summarise( EffectiveDatetime = max(EffectiveDatetime) )

    #
    # Now get the course data for the latest courses.
    # Use Status of A,N for FA since we want only enrolled courses at the cutoff date
    #     (This will be taken care of later as we need the W credits to determine load)
    # Use Status A,N,W for SP,SU since these were all the courses enrolled in at census
    #
    sac_most_recent_all <- student_acad_cred %>%
        inner_join( sac_max_effdt,
                    by = c("ID", "Term_ID", "Course_ID", "EffectiveDatetime") ) %>%
        filter( Course_Status %in% c('A', 'N', 'W') ) %>%
        select( -EffectiveDatetime ) %>%
        distinct() %>%
        select( -Course_ID )

    #
    # Get list of students who are taking at least 1 non-developmental/audited course
    #
    sac_most_recent_non_dev_ids <- sac_most_recent_all %>%
        filter( coalesce(Course_Level,"ZZZ") != "DEV" ) %>%
        filter( coalesce(Grade_Code,'X') != '9' ) %>%
        select( ID, Term_ID ) %>%
        distinct()

    #
    # Get list of students who are taking at least 1 distance course
    #
    sac_most_recent_1_distance_ids <- sac_most_recent_all %>%
        inner_join( sac_most_recent_non_dev_ids, by = c("ID", "Term_ID") ) %>%
        filter( Delivery_Method == "IN" ) %>%

        filter( coalesce(Grade_Code,'X') != '9' ) %>%
        select( ID, Term_ID ) %>%
        distinct()

    #
    # Get list of students who are taking at least 1 regular course
    #
    sac_most_recent_f2f_ids <- sac_most_recent_all %>%
        filter( Delivery_Method != "IN" ) %>%
        filter( coalesce(Grade_Code,'X') != '9' ) %>%
        select( ID, Term_ID ) %>%
        distinct()

    sac_most_recent_all_distance_ids <- sac_most_recent_1_distance_ids %>%
        anti_join( sac_most_recent_f2f_ids, by = c("ID", "Term_ID") ) %>%
        mutate( Distance_Courses = "All" )

    sac_most_recent_distance_ids <- sac_most_recent_1_distance_ids %>%
        left_join( sac_most_recent_all_distance_ids, by = c("ID", "Term_ID") ) %>%
        mutate( Distance_Courses = coalesce(Distance_Courses,"At least 1") )

    # Determine which students have completely withdrawn at the end or by Oct 15
    sac_most_recent_all_withdraws <- sac_most_recent_all %>%
        filter( Course_Status %in% c('W') ) %>%
        anti_join( sac_most_recent_all %>% filter( Course_Status %in% c('A', 'N') ),
                by = c("ID", "Term_ID" ) ) %>%
        select( ID, Term_ID ) %>%
        distinct() %>%
        mutate( Enrollment_Status = "Withdrawn" )

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
        left_join( sac_most_recent_all_withdraws, by = c("ID", "Term_ID") ) %>%
        mutate( Distance_Courses = coalesce(Distance_Courses,"None"),
                Enrollment_Status = coalesce(Enrollment_Status,"Enrolled") )

    #
    # Take term load table and reduce to the reporting terms
    #
    if (!is.na(report_years)) {
        sac_report_load_by_term <- sac_load_by_term %>%
            inner_join( reporting_terms %>% select( Term_ID, Term_Reporting_Year ),
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
#' @param exlude_hs Should function exclude high school students from being included as credential seekers. Default is to include high school students.
#' @export
#' @importFrom magrittr %<>%
#'
credential_seekers <- function( report_years = NA_integer_, report_semesters = NA_character_, exclude_hs = FALSE ) {

    terms <- getColleagueData( "Term_CU", schema = "dw_dim" ) %>%
        select( Term_ID,
                Term_Index,
                Term_Name = Semester,
                Semester = Term_Abbreviation,
                Term_Start_Date,
                Term_Census_Date,
                Term_End_Date,
                Term_Reporting_Year = Reporting_Year_FSS,
                Academic_Year = Reporting_Academic_Year_FSS ) %>%
        collect() %>%
        mutate( Term_Reporting_Year = as.integer(Term_Reporting_Year) - 1 )

    reporting_terms <- terms

    if (!is.na(report_years)) {
        if (length(report_years) == 1) {
            reporting_terms %<>%
                filter( Term_Reporting_Year == report_years )
        } else {
            reporting_terms %<>%
                filter( Term_Reporting_Year %in% report_years )
        }
    }

    if (!is.na(report_semesters)) {
        if (length(report_semesters) == 1) {
            reporting_terms %<>%
                filter( Semester == report_semesters )
        } else {
            reporting_terms %<>%
                filter( Semester %in% report_semesters )
        }
    }

    # Get only CU programs from ACAD_PROGRAMS
    acad_programs <- getColleagueData( "ACAD_PROGRAMS" ) %>%
        filter( ACPG.ACAD.LEVEL == "CU" ) %>%
        select( Program = ACAD.PROGRAMS.ID ) %>%
        collect()

    #
    # Get program dates (this is a multi-valued field that needs to be joined with full table).
    #
    student_programs__dates <- getColleagueData( "STUDENT_PROGRAMS__STPR_DATES", version="history" ) %>%
        select( ID = STPR.STUDENT,
                Program = STPR.ACAD.PROGRAM,
                Program_Start_Date = STPR.START.DATE,
                Program_End_Date = STPR.END.DATE,
                EffectiveDatetime ) %>%
        collect() %>%
        inner_join( acad_programs, by="Program" ) %>%
        mutate( Program_End_Date = coalesce(Program_End_Date,as.Date("9999-12-31")) )

    if (exclude_hs) {
        student_programs__dates %<>% anti_join( high_school_programs, by = "Program" )
    }

    #
    # Credential-seekers are those in A, D, or C programs
    #
    credential_seeking <- getColleagueData( "STUDENT_PROGRAMS" ) %>%
        select( ID = STPR.STUDENT,
                Program = STPR.ACAD.PROGRAM,
                Academic_Level = 
                EffectiveDatetime ) %>%
        collect() %>%
        inner_join( acad_programs, by="Program" ) %>%
        inner_join( student_programs__dates, by = c("ID", "Program", "EffectiveDatetime") ) %>%
        select( -EffectiveDatetime ) %>%

        # Identify credential seekers.
        mutate( Credential_Seeker = case_when(
                    substring(Program,1,1) %in% c('A','D','C') ~ 1,
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
#' All data comes from CCDW_HIST SQL Server database
#'
#' @param report_years The year of the fall term for the data
#' @param exlude_hs Should function exclude high school students from being included as credential seekers. Default is to include high school students.
#' @export
#'
fall_credential_seekers <- function( report_years, exclude_hs = FALSE ) {
    return( credential_seekers( report_years, "FA", exclude_hs ) )
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
#' @param useonly Us the specified 'use' table only. Default is FALSE which means combine generated history.ipeds_cohorts with STUDENT_TERMS
#' @export
#'
ipeds_cohort <- function( report_years, cohorts=c("FT","PT","TF","TP","RF","RP"), use = "ipeds_cohorts", useonly = TRUE ) {

    report_cohorts <- ""
    if (purrr::has_element(cohorts,"FT")) report_cohorts <- c(report_cohorts, str_c(report_years,"FT"))
    if (purrr::has_element(cohorts,"PT")) report_cohorts <- c(report_cohorts, str_c(report_years,"PT"))
    if (purrr::has_element(cohorts,"TF")) report_cohorts <- c(report_cohorts, str_c(report_years,"TF"))
    if (purrr::has_element(cohorts,"TP")) report_cohorts <- c(report_cohorts, str_c(report_years,"TP"))
    if (purrr::has_element(cohorts,"RF")) report_cohorts <- c(report_cohorts, str_c(report_years,"RF"))
    if (purrr::has_element(cohorts,"RP")) report_cohorts <- c(report_cohorts, str_c(report_years,"RP"))
    report_cohorts <- report_cohorts[-1]

    ipeds_cohort_FILE_COHORTS <- read_csv( file.path(ipeds_path,"ipeds_cohorts.csv"), col_types = cols(.default=col_character()) ) %>%
        filter( Cohort %in% c(report_cohorts) ) %>%
        select( ID, Term_ID, Cohort )

    if (toupper(use) == "STUDENT_TERMS") {
        ipeds_cohort_COLLEAGUE_COHORTS <- getColleagueData( "STUDENT_TERMS" ) %>%
            select( ID = STTR.STUDENT, Cohort = STTR.FED.COHORT.GROUP ) %>%
            filter( Cohort %in% c(report_cohorts) ) %>%
            distinct() %>%
            collect() %>%
            mutate( Term_ID = str_c(substring(Cohort,1,4),"FA") )
    } else {
        if (toupper(use) != "IPEDS_COHORTS") {
            warning(str_c("Invallid use value (",use,"), defaulting to 'ipeds_cohorts'"))
        }
        ipeds_cohort_COLLEAGUE_COHORTS <- getColleagueData( "ipeds_cohorts", schema="local", version="latest" ) %>%
            select( ID, Term_ID, Cohort ) %>%
            filter( !is.na(Cohort) ) %>%
            collect()
    }

    if (useonly) {
        ic <- ipeds_cohort_COLLEAGUE_COHORTS
    } else {
        ic <- ipeds_cohort_FILE_COHORTS %>%
            bind_rows( ipeds_cohort_COLLEAGUE_COHORTS )
    }

    return( ic )
}
