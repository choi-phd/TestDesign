# Science dataset

itempool_science_raw    <- read.csv("inst/extdata/itempool_science_1000.csv",    header = TRUE)
itemattrib_science_raw  <- read.csv("inst/extdata/itemattrib_science_1000.csv",  header = TRUE)
constraints_science_raw <- read.csv("inst/extdata/constraints_science_1000.csv", header = TRUE)
itempool_science    <- loadItemPool("inst/extdata/itempool_science_1000.csv")
itemattrib_science  <- loadItemAttrib("inst/extdata/itemattrib_science_1000.csv",   itempool_science)
constraints_science <- loadConstraints("inst/extdata/constraints_science_1000.csv", itempool_science, itemattrib_science)

usethis::use_data(itempool_science_raw,    overwrite = TRUE)
usethis::use_data(itemattrib_science_raw,  overwrite = TRUE)
usethis::use_data(constraints_science_raw, overwrite = TRUE)
usethis::use_data(itempool_science,    overwrite = TRUE)
usethis::use_data(itemattrib_science,  overwrite = TRUE)
usethis::use_data(constraints_science, overwrite = TRUE)

# Reading dataset

itempool_reading_raw    <- read.csv("inst/extdata/itempool_reading_303.csv",    header = TRUE)
itemattrib_reading_raw  <- read.csv("inst/extdata/itemattrib_reading_303.csv",  header = TRUE)
stimattrib_reading_raw  <- read.csv("inst/extdata/stimattrib_reading_303.csv",  header = TRUE)
constraints_reading_raw <- read.csv("inst/extdata/constraints_reading_303.csv", header = TRUE)
itempool_reading    <- loadItemPool("inst/extdata/itempool_reading_303.csv")
itemattrib_reading  <- loadItemAttrib("inst/extdata/itemattrib_reading_303.csv",   itempool_reading)
stimattrib_reading  <- loadStAttrib("inst/extdata/stimattrib_reading_303.csv",     itemattrib_reading)
constraints_reading <- loadConstraints("inst/extdata/constraints_reading_303.csv", itempool_reading, itemattrib_reading, stimattrib_reading)

usethis::use_data(itempool_reading_raw,    overwrite = TRUE)
usethis::use_data(itemattrib_reading_raw,  overwrite = TRUE)
usethis::use_data(stimattrib_reading_raw,  overwrite = TRUE)
usethis::use_data(constraints_reading_raw, overwrite = TRUE)
usethis::use_data(itempool_reading,    overwrite = TRUE)
usethis::use_data(itemattrib_reading,  overwrite = TRUE)
usethis::use_data(stimattrib_reading,  overwrite = TRUE)
usethis::use_data(constraints_reading, overwrite = TRUE)

# Fatigue dataset

itempool_fatigue_raw    <- read.csv("inst/extdata/itempool_fatigue_95.csv",    header = TRUE)
itemattrib_fatigue_raw  <- read.csv("inst/extdata/itemattrib_fatigue_95.csv",  header = TRUE)
itemcontent_fatigue_raw <- read.csv("inst/extdata/itemcontent_fatigue_95.csv", header = TRUE)
constraints_fatigue_raw <- read.csv("inst/extdata/constraints_fatigue_95.csv", header = TRUE)
resp_fatigue_raw        <- read.csv("inst/extdata/resp_fatigue_95.csv",        header = FALSE)
itempool_fatigue    <- loadItemPool("inst/extdata/itempool_fatigue_95.csv")
itemattrib_fatigue  <- loadItemAttrib("inst/extdata/itemattrib_fatigue_95.csv",   itempool_fatigue)
constraints_fatigue <- loadConstraints("inst/extdata/constraints_fatigue_95.csv", itempool_fatigue, itemattrib_fatigue)

usethis::use_data(itempool_fatigue_raw,    overwrite = TRUE)
usethis::use_data(itemattrib_fatigue_raw,  overwrite = TRUE)
usethis::use_data(itemcontent_fatigue_raw, overwrite = TRUE)
usethis::use_data(constraints_fatigue_raw, overwrite = TRUE)
usethis::use_data(resp_fatigue_raw,    overwrite = TRUE)
usethis::use_data(itempool_fatigue,    overwrite = TRUE)
usethis::use_data(itemattrib_fatigue,  overwrite = TRUE)
usethis::use_data(constraints_fatigue, overwrite = TRUE)
