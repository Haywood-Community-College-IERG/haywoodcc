cfg <- getCfg()

cfg$data_source$from_file_path <- '.'

getColleagueData("TEST_FILE", schema="history", version="current")
