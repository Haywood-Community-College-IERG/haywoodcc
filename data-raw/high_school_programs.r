
ccp_programs <- data.frame( Program = c( "C15200CP", "C150200CP",
                                         "C2506CP",  "C250200TP",
                                         "C25100CP", "C25120BE", "C25120CB", "C25120CM", "C25310CM", "C25310CP", "C25340CP", "C25550CB", "C25590CB", "C25590CP",
                                         "C25800CP",
                                         "C35130CP", "C35140CP", "C35140CB",
                                         "C40200CP", "C40200CD",
                                         "C50210CP", "C50210AD", "C50210IN", "C50420CP",
                                         "C55180CP", "C55180II", "C55180IV", "C55220CP", "C55400CP", "C55860CP", "C55960CP",
                                         "C60130CP", "C60130IN", "C60130WE", "C60130SP", "C60160CP", "C60160ES", "C60160IN", "C60160IM",
                                         "P1012A", "P1012B", "P1012C", "P1032C", "P1042A", "P1042B", "P1042C", "P1052C",
                                         "T-302", "T-303", "T90920", "T90930", "T90970", "T90980", "T90980CO" ),
                            stringsAsFactors = FALSE )

early_college_programs <- data.frame( Program = c( "A10100EC", "A10300EC", "A10400EC", "A10500EC", "T90970EC", "T90930" ), stringsAsFactors = FALSE )

high_school_programs <- dplyr::bind_rows(ccp_programs, early_college_programs)

haywood_county_high_schools <- data.frame( School = c( "Tuscola", "Pisgah", "Central Haywood", "Haywood Early College" ),
                                           Institution_ID = c( "0034905", "0034337", "0034402", "1113888"),
                                           stringsAsFactors = FALSE )

readr::write_csv(ccp_programs, "data-raw/ccp_programs.csv")
usethis::use_data(ccp_programs, overwrite = TRUE, compress = 'xz')

readr::write_csv(early_college_programs, "data-raw/early_college_programs.csv")
usethis::use_data(early_college_programs, overwrite = TRUE, compress = 'xz')

readr::write_csv(high_school_programs, "data-raw/high_school_programs.csv")
usethis::use_data(high_school_programs, overwrite = TRUE, compress = 'xz')

readr::write_csv(haywood_county_high_schools, "data-raw/haywood_county_high_schools.csv")
usethis::use_data(haywood_county_high_schools, overwrite = TRUE, compress = 'xz')
