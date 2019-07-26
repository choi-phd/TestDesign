# Science dataset

itempool.science.raw <- read.csv("data-raw/itempool.science.1000.csv", header = T)
itemattrib.science.raw <- read.csv("data-raw/itemattrib.science.1000.csv", header = T)
constraints.science.raw <- read.csv("data-raw/constraints.science.1000.csv", header = T)
itempool.science <- LoadItemPool("data-raw/itempool.science.1000.csv")
itemattrib.science <- LoadItemAttrib("data-raw/itemattrib.science.1000.csv", itempool.science)
constraints.science <- LoadConstraints("data-raw/constraints.science.1000.csv", itempool.science, itemattrib.science)

usethis::use_data(itempool.science.raw, overwrite = T)
usethis::use_data(itemattrib.science.raw, overwrite = T)
usethis::use_data(constraints.science.raw, overwrite = T)
usethis::use_data(itempool.science, overwrite = T)
usethis::use_data(itemattrib.science, overwrite = T)
usethis::use_data(constraints.science, overwrite = T)

# Reading dataset

itempool.reading.raw <- read.csv("data-raw/itempool.reading.303.csv", header = T)
itemattrib.reading.raw <- read.csv("data-raw/itemattrib.reading.303.csv", header = T)
stimattrib.reading.raw <- read.csv("data-raw/stimattrib.reading.303.csv", header = T)
constraints.reading.raw <- read.csv("data-raw/constraints.reading.303.csv", header = T)
itempool.reading <- LoadItemPool("data-raw/itempool.reading.303.csv")
itemattrib.reading <- LoadItemAttrib("data-raw/itemattrib.reading.303.csv", itempool.reading)
stimattrib.reading <- LoadStAttrib("data-raw/stimattrib.reading.303.csv", itemattrib.reading)
constraints.reading <- LoadConstraints("data-raw/constraints.reading.303.csv", itempool.reading, itemattrib.reading, stimattrib.reading)

usethis::use_data(itempool.reading.raw, overwrite = T)
usethis::use_data(itemattrib.reading.raw, overwrite = T)
usethis::use_data(stimattrib.reading.raw, overwrite = T)
usethis::use_data(constraints.reading.raw, overwrite = T)
usethis::use_data(itempool.reading, overwrite = T)
usethis::use_data(itemattrib.reading, overwrite = T)
usethis::use_data(stimattrib.reading, overwrite = T)
usethis::use_data(constraints.reading, overwrite = T)

# Fatigue dataset

itempool.fatigue.raw <- read.csv("data-raw/itempool.fatigue.95.csv", header = T)
itemattrib.fatigue.raw <- read.csv("data-raw/itemattrib.fatigue.95.csv", header = T)
itemcontent.fatigue.raw <- read.csv("data-raw/itemcontent.fatigue.95.csv", header = T)
constraints.fatigue.raw <- read.csv("data-raw/constraints.fatigue.95.csv", header = T)
resp.fatigue.raw <- read.csv("data-raw/resp.fatigue.95.csv", header = F)
itempool.fatigue <- LoadItemPool("data-raw/itempool.fatigue.95.csv")
itemattrib.fatigue <- LoadItemAttrib("data-raw/itemattrib.fatigue.95.csv", itempool.fatigue)
constraints.fatigue <- LoadConstraints("data-raw/constraints.fatigue.95.csv", itempool.fatigue, itemattrib.fatigue)

usethis::use_data(itempool.fatigue.raw, overwrite = T)
usethis::use_data(itemattrib.fatigue.raw, overwrite = T)
usethis::use_data(itemcontent.fatigue.raw, overwrite = T)
usethis::use_data(constraints.fatigue.raw, overwrite = T)
usethis::use_data(resp.fatigue.raw, overwrite = T)
usethis::use_data(itempool.fatigue, overwrite = T)
usethis::use_data(itemattrib.fatigue, overwrite = T)
usethis::use_data(constraints.fatigue, overwrite = T)
