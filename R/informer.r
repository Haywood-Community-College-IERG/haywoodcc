pkg.env <- rlang::env(parent = rlang::empty_env())

#pkg.env$cfg = NA_character_

#' Load the YAML configuration file.
#'
#' Load the specified YAML configuration file. If no file is provided,
#' load "./config.yml".
#'
#' @param cfg_full_path Provide the full path to the YAML configuration file. Defaults to NA.
#' @param cfg_fn The file name for the YAML configuration file. Defaults to config.yml.
#' @param cfg_path The file path to the YAML configuration file. Defaults to ".".
#' @param reload Force a reload of the config.
#' @importFrom rlang env_get env_poke
#' @importFrom glue glue
#' @importFrom yaml yaml.load_file
#' @importFrom fs path
#' @export
#'
getCfg <- function( cfg_full_path=NA_character_, cfg_fn=NA_character_, cfg_path=NA_character_, reload=FALSE ) {

    dflt_cfg_fn = "config.yml"
    dflt_cfg_path = "."

    #cfg <- pkg.env$cfg
    cfg <- rlang::env_get(pkg.env, "cfg", default=NA)

    if (is.na(cfg) || is.null(cfg) || reload) {
        # Use a cached version unless reload is specified.
        #
        # If a parameter was passed for cfg_full_path, cfg_fn, or cfg_path, then use that, Otherwise...
        #
        # Look for haywoodcc.cfg.full_path specified as an option and, if found, use it.
        # If no option found, check for the environment variable HAYWOODCC_CFG_FULL_PATH.
        # If that is not found, look for the file name and path parts in the same order.
        # If nothing is specified in the options or environment, use the default

        opt_cfg_full_path <- getOption("haywoodcc.cfg.full_path", default=NA_character_)
        opt_cfg_fn <- getOption("haywoodcc.cfg.fn", default=NA_character_)
        opt_cfg_path <- getOption("haywoodcc.cfg.path", default=NA_character_)

        env_cfg_full_path <- Sys.getenv("HAYWOODCC_CFG_FULL_PATH")
        env_cfg_fn <- Sys.getenv("HAYWOODCC_CFG_FN")
        env_cfg_path <- Sys.getenv("HAYWOODCC_CFG_PATH")

        if (is.na(cfg_full_path) && is.na(cfg_fn) && is.na(cfg_path)) {

            #print(glue::glue("WHATIS opt_cfg_full_path: {opt_cfg_full_path}"))
            #print(glue::glue("WHATIS env_cfg_full_path: {env_cfg_full_path}"))

            if (!is.na(opt_cfg_full_path) && (opt_cfg_full_path != "")) {
                #print(glue::glue("Using option haywoodcc.cfg.full_path [{opt_cfg_full_path}]"))
                cfg_full_path = opt_cfg_full_path
            } else if (!is.na(opt_cfg_fn) || !is.na(opt_cfg_path)) {
                if (!is.na(opt_cfg_fn)) {
                    #print(glue::glue("Using option haywoodcc.cfg.fn [{opt_cfg_fn}]"))
                    cfg_fn <- opt_cfg_fn
                } else if (!is.na(env_cfg_fn) && (env_cfg_fn != "")) {
                    #print(glue::glue("Using environtment HAYWOODCC_CFG_FN [{env_cfg_fn}]"))
                    cfg_fn <- env_cfg_fn
                } else {
                    #print(glue::glue("Using dflt_cfg_fn: {dflt_cfg_fn}"))
                    cfg_fn = dflt_cfg_fn
                }
                if (!is.na(opt_cfg_path)) {
                    #print(glue::glue("Using option haywoodcc.cfg.path [{opt_cfg_path}]"))
                    cfg_path <- opt_cfg_path
                } else if (!is.na(env_cfg_path) && (env_cfg_path != "")) {
                    #print(glue::glue("Using environtment HAYWOODCC_CFG_PATH [{env_cfg_path}]"))
                    cfg_path <- env_cfg_path
                } else {
                    #print(glue::glue("Using dflt_cfg_path: {dflt_cfg_path}"))
                    cfg_path <- dflt_cfg_path
                }

                cfg_full_path <- fs::path(cfg_path,cfg_fn)

            } else if (!is.na(env_cfg_full_path) && (env_cfg_full_path != "")) {
                #print(glue::glue("Using env_cfg_full_path: {env_cfg_full_path}"))
                cfg_full_path = env_cfg_full_path

            } else if (!is.na(env_cfg_fn) || !is.na(env_cfg_path)) {
                if (!is.na(env_cfg_fn) && (env_cfg_fn != "")) {
                    #print(glue::glue("Using environtment HAYWOODCC_CFG_FN [{env_cfg_fn}]"))
                    cfg_fn <- env_cfg_fn
                } else {
                    #print(glue::glue("Using dflt_cfg_fn: {dflt_cfg_fn}"))
                    cfg_fn = dflt_cfg_fn
                }

                if (!is.na(env_cfg_path) && (env_cfg_path != "")) {
                    #print(glue::glue("Using environtment HAYWOODCC_CFG_PATH [{env_cfg_path}]"))
                    cfg_path <- env_cfg_path
                } else {
                    #print(glue::glue("Using dflt_cfg_path: {dflt_cfg_path}"))
                    cfg_path <- dflt_cfg_path
                }

                cfg_full_path <- fs::path(cfg_path,cfg_fn)

            }
        } else {

            if (is.na(cfg_full_path)) {
                if (!is.na(cfg_fn)) {
                    #print(glue::glue("Using cfg_fn: {cfg_fn}"))
                } else if (!is.na(opt_cfg_fn)) {
                    #print(glue::glue("Using option haywoodcc.cfg.fn [{opt_cfg_fn}]"))
                    cfg_fn <- opt_cfg_fn
                } else if (!is.na(env_cfg_fn) && (env_cfg_fn != "")) {
                    #print(glue::glue("Using environtment HAYWOODCC_CFG_FN [{env_cfg_fn}]"))
                    cfg_fn <- env_cfg_fn
                } else {
                    #print(glue::glue("Using dflt_cfg_fn: {dflt_cfg_fn}"))
                    cfg_fn = dflt_cfg_fn
                }
                if (!is.na(cfg_path)) {
                    #print(glue::glue("Using cfg_path: {cfg_path}"))
                } else if (!is.na(opt_cfg_path)) {
                    #print(glue::glue("Using option haywoodcc.cfg.path [{opt_cfg_path}]"))
                    cfg_path <- opt_cfg_path
                } else if (!is.na(env_cfg_path) && (env_cfg_path != "")) {
                    #print(glue::glue("Using environtment HAYWOODCC_CFG_PATH [{env_cfg_path}]"))
                    cfg_path <- env_cfg_path
                } else {
                    #print(glue::glue("Using dflt_cfg_path: {dflt_cfg_path}"))
                    cfg_path <- dflt_cfg_path
                }

                cfg_fn <- dplyr::if_else(is.na(cfg_fn),dflt_cfg_fn,cfg_fn)
                cfg_path <- dplyr::if_else(is.na(cfg_path),dflt_cfg_path,cfg_path)

                #print(glue::glue("Using cfg_fn and cfg_path: {cfg_fn} and {cfg_path}"))

                cfg_full_path <- fs::path(cfg_path,cfg_fn)
            } else {
                #print(glue::glue("Using cfg_full_path: {cfg_full_path}"))
            }
        }

        print(glue::glue("Loading configuration from [{cfg_full_path}]"))

        if (fs::file_exists(cfg_full_path)) {
            cfg_l <- yaml::yaml.load_file(cfg_full_path)

            if (cfg_l$config$location != "self") {
                cfg_full_path <- fs::path(cfg_l$config$location,cfg_fn)

                if (fs::file_exists(cfg_full_path)) {
                    cfg <- yaml::yaml.load_file(cfg_full_path)
                    rlang::env_poke(pkg.env, "cfg_full_path", cfg_full_path)
                }
            } else {
                cfg = cfg_l
                rlang::env_poke(pkg.env, "cfg_full_path", cfg_full_path)
            }

            rlang::env_poke(pkg.env, "cfg", cfg)
        }
    } else {
        cfg_full_path <- rlang::env_get(pkg.env, "cfg_full_path", default=NA)
        #print(glue::glue("Using cached cfg: {cfg_full_path}"))
    }
    getCfg <- cfg
}

#' Set a new value in the cached YAML configuration file.
#'
#' @param section The section for the new variable
#' @param variable The name of the new variable
#' @param value The value for the new variable
#' @param cfg A YAML configuration file with sql section that includes driver, server, db, and schema_history.
#' @param cfg_full_path Provide the full path to the YAML configuration file. Defaults to NA.
#' @param cfg_fn The file name for the YAML configuration file. Defaults to config.yml.
#' @param cfg_path The file path to the YAML configuration file. Defaults to ".".
#' @param reload Force a reload of the config.
#' @importFrom rlang env_get
#' @export
#'
setCfg <- function( section, variable, value,
                    cfg=NA_character_, cfg_full_path=NA_character_,
                    cfg_fn=NA_character_, cfg_path=NA_character_,
                    reload=FALSE
                    ) {
    if (is.na(cfg) || is.null(cfg)) {
        cfg <- rlang::env_get(pkg.env, "cfg", default=NA)

        if (is.na(cfg) || is.null(cfg)) {
            cfg <- getCfg(cfg_full_path=cfg_full_path, cfg_fn=cfg_fn, cfg_path=cfg_path, reload=reload)
        }
    }

    t <- list(value)
    names(t) <- variable
    cfg[section] <- list(t)
    rlang::env_poke(pkg.env, "cfg", cfg)
}

#' Return a data from data warehouse Colleague tables.
#'
#' Return a data from of the IPEDS cohort data. Data will come either from the file ipeds_cohorts.csv of
#' from the CCDW_HIST SQL Server database.
#'
#' @param file The name of the Colleague file to return
#' @param schema Which schema should be used. Needed for non-Colleague tables.
#' @param version Specify which version to include. Default is for the latest data. Any other value will return the dated file.
#' @param from_file_path Specify path for file. This overrides the use of the database. Defaults to NA.
#' @param cfg A YAML configuration file with sql section that includes driver, server, db, and schema_history.
#' @param cfg_full_path Provide the full path to the YAML configuration file. Defaults to NA.
#' @param cfg_fn The file name for the YAML configuration file. Defaults to NA.
#' @param cfg_path The file path to the YAML configuration file. Defaults to NA.
#' @param sep The separator to use in the field names. Default is a '.' as in the original Colleague file.
#' @export
#' @importFrom stringr str_c
#' @importFrom fs path
#' @importFrom rlang env_get env_poke
#' @importFrom odbc dbConnect odbc
#' @importFrom dplyr tbl
#' @importFrom dbplyr in_schema
#' @importFrom readr read_csv
#'
getColleagueData <- function( file,
                              schema="history", version="latest",
                              from_file_path=NA_character_,
                              sep='.',
                              cfg=NA_character_, cfg_full_path=NA_character_,
                              cfg_fn=NA_character_, cfg_path=NA_character_,
                              reload=FALSE,
                              ...
                              ) {

    if (is.na(cfg) || is.null(cfg)) {
        cfg <- rlang::env_get(pkg.env, "cfg", default=NA)

        if (is.na(cfg) || is.null(cfg)) {
            cfg <- getCfg(cfg_full_path=cfg_full_path, cfg_fn=cfg_fn, cfg_path=cfg_path, reload=reload)
        }
    }

    if ("data_source" %nin% names(cfg)) {
        cfg$data_source$from_file_path <- NA_character_
    } else if ("from_file_path" %nin% names(cfg$data_source)) {
        cfg$data_source$from_file_path <- NA_character_
    }

    if (!is.na(from_file_path)) {
        cfg_from_file_path = from_file_path
    } else {
        cfg_from_file_path = cfg$data_source$from_file_path
    }

    if (is.na(cfg_from_file_path)) {
        conn_str <- stringr::str_c( glue::glue("Driver={{{cfg$sql$driver}}}"),
                                    glue::glue("Server={{{cfg$sql$server}}}"),
                                    glue::glue("Database={{{cfg$sql$db}}}"),
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
    } else {
        # Try to find <file> in a folder named <schema>
        if (file.exists(fs::path(cfg_from_file_path,schema,file,ext = "csv"))) {
            csvfile <- fs::path(schema,file,ext = "csv")
        } else {
            # ..., then look for a file named <schema><sep><file>.csv
            if (file.exists(fs::path(cfg_from_file_path,stringr::str_c(schema,file,sep=sep), ext="csv"))) {
                csvfile <- fs::path(glue::glue("{schema}{sep}{file}"), ext="csv")
            } else {
                # ..., then look for a file named <file>.csv
                if (file.exists(fs::path(cfg_from_file_path,file, ext="csv"))) {
                    csvfile <- fs::path(file, ext="csv")
                } else {
                    csvfile <- NA_character_
                    stop(glue::glue("ERROR: File not found: {file}"))
                }
            }
        }

        if (!exists("show_col_types") && is.na(show_col_types)) {
            show_col_types = FALSE
        }
        df <- readr::read_csv(fs::path(cfg_from_file_path,csvfile), show_col_types = show_col_types)
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
