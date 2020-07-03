# Science dataset

itempool_science_data    <- read.csv("inst/extdata/itempool_science_1000.csv",    header = TRUE, as.is = TRUE)
itemattrib_science_data  <- read.csv("inst/extdata/itemattrib_science_1000.csv",  header = TRUE, as.is = TRUE)
constraints_science_data <- read.csv("inst/extdata/constraints_science_1000.csv", header = TRUE, as.is = TRUE)
constraints_science_data[["CONSTRAINT"]] <- as.character(constraints_science_data[["CONSTRAINT"]])
itempool_science    <- loadItemPool("inst/extdata/itempool_science_1000.csv")
itemattrib_science  <- loadItemAttrib("inst/extdata/itemattrib_science_1000.csv",   itempool_science)
constraints_science <- loadConstraints("inst/extdata/constraints_science_1000.csv", itempool_science, itemattrib_science)

usethis::use_data(itempool_science_data,    overwrite = TRUE)
usethis::use_data(itemattrib_science_data,  overwrite = TRUE)
usethis::use_data(constraints_science_data, overwrite = TRUE)
usethis::use_data(itempool_science,    overwrite = TRUE)
usethis::use_data(itemattrib_science,  overwrite = TRUE)
usethis::use_data(constraints_science, overwrite = TRUE)

# Reading dataset

itempool_reading_data    <- read.csv("inst/extdata/itempool_reading_303.csv",    header = TRUE, as.is = TRUE)
itemattrib_reading_data  <- read.csv("inst/extdata/itemattrib_reading_303.csv",  header = TRUE, as.is = TRUE)
stimattrib_reading_data  <- read.csv("inst/extdata/stimattrib_reading_303.csv",  header = TRUE, as.is = TRUE)
constraints_reading_data <- read.csv("inst/extdata/constraints_reading_303.csv", header = TRUE, as.is = TRUE)
constraints_reading_data[["CONSTRAINT"]] <- as.character(constraints_reading_data[["CONSTRAINT"]])
itempool_reading    <- loadItemPool("inst/extdata/itempool_reading_303.csv")
itemattrib_reading  <- loadItemAttrib("inst/extdata/itemattrib_reading_303.csv",   itempool_reading)
stimattrib_reading  <- loadStAttrib("inst/extdata/stimattrib_reading_303.csv",     itemattrib_reading)
constraints_reading <- loadConstraints("inst/extdata/constraints_reading_303.csv", itempool_reading, itemattrib_reading, stimattrib_reading)

usethis::use_data(itempool_reading_data,    overwrite = TRUE)
usethis::use_data(itemattrib_reading_data,  overwrite = TRUE)
usethis::use_data(stimattrib_reading_data,  overwrite = TRUE)
usethis::use_data(constraints_reading_data, overwrite = TRUE)
usethis::use_data(itempool_reading,    overwrite = TRUE)
usethis::use_data(itemattrib_reading,  overwrite = TRUE)
usethis::use_data(stimattrib_reading,  overwrite = TRUE)
usethis::use_data(constraints_reading, overwrite = TRUE)

# Fatigue dataset

itempool_fatigue_data    <- read.csv("inst/extdata/itempool_fatigue_95.csv",    header = TRUE , as.is = TRUE)
itemattrib_fatigue_data  <- read.csv("inst/extdata/itemattrib_fatigue_95.csv",  header = TRUE , as.is = TRUE)
itemcontent_fatigue_data <- read.csv("inst/extdata/itemcontent_fatigue_95.csv", header = TRUE , as.is = TRUE)
constraints_fatigue_data <- read.csv("inst/extdata/constraints_fatigue_95.csv", header = TRUE , as.is = TRUE)
resp_fatigue_data        <- read.csv("inst/extdata/resp_fatigue_95.csv",        header = FALSE, as.is = TRUE)
constraints_fatigue_data[["CONSTRAINT"]] <- as.character(constraints_fatigue_data[["CONSTRAINT"]])
itempool_fatigue    <- loadItemPool("inst/extdata/itempool_fatigue_95.csv")
itemattrib_fatigue  <- loadItemAttrib("inst/extdata/itemattrib_fatigue_95.csv",   itempool_fatigue)
constraints_fatigue <- loadConstraints("inst/extdata/constraints_fatigue_95.csv", itempool_fatigue, itemattrib_fatigue)

usethis::use_data(itempool_fatigue_data,    overwrite = TRUE)
usethis::use_data(itemattrib_fatigue_data,  overwrite = TRUE)
usethis::use_data(itemcontent_fatigue_data, overwrite = TRUE)
usethis::use_data(constraints_fatigue_data, overwrite = TRUE)
usethis::use_data(resp_fatigue_data,    overwrite = TRUE)
usethis::use_data(itempool_fatigue,    overwrite = TRUE)
usethis::use_data(itemattrib_fatigue,  overwrite = TRUE)
usethis::use_data(constraints_fatigue, overwrite = TRUE)

# Bayes dataset

itempool_bayes_data    <- read.csv("inst/extdata/itempool_bayes_320.csv",    header = TRUE, as.is = TRUE)
itempool_se_bayes_data <- read.csv("inst/extdata/itempool_se_bayes_320.csv", header = TRUE, as.is = TRUE)
itemattrib_bayes_data  <- read.csv("inst/extdata/itemattrib_bayes_320.csv",  header = TRUE, as.is = TRUE)
constraints_bayes_data <- read.csv("inst/extdata/constraints_bayes_320.csv", header = TRUE, as.is = TRUE)
constraints_bayes_data[["CONSTRAINT"]] <- as.character(constraints_bayes_data[["CONSTRAINT"]])
itempool_bayes    <- loadItemPool("inst/extdata/itempool_bayes_320.csv", "inst/extdata/itempool_se_bayes_320.csv")
itemattrib_bayes  <- loadItemAttrib("inst/extdata/itemattrib_bayes_320.csv",   itempool_bayes)
constraints_bayes <- loadConstraints("inst/extdata/constraints_bayes_320.csv", itempool_bayes, itemattrib_bayes)

usethis::use_data(itempool_bayes_data,    overwrite = TRUE)
usethis::use_data(itempool_se_bayes_data, overwrite = TRUE)
usethis::use_data(itemattrib_bayes_data,  overwrite = TRUE)
usethis::use_data(constraints_bayes_data, overwrite = TRUE)
usethis::use_data(itempool_bayes,    overwrite = TRUE)
usethis::use_data(itemattrib_bayes,  overwrite = TRUE)
usethis::use_data(constraints_bayes, overwrite = TRUE)
