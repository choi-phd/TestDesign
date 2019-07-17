# Science dataset

item.params.science.raw  = read.csv("data-raw/item.params.science.1000.csv", header = T)
item.attrib.science.raw  = read.csv("data-raw/item.attrib.science.1000.csv", header = T)
constraints.science.raw  = read.csv("data-raw/constraints.science.1000.csv", header = T)
itempool.science         = LoadItemPool("data-raw/item.params.science.1000.csv")
itemattrib.science       = LoadItemAttrib("data-raw/item.attrib.science.1000.csv", itempool.science)
constraints.science      = LoadConstraints("data-raw/constraints.science.1000.csv", itempool.science, itemattrib.science)
constraints2.science     = LoadConstraints("data-raw/constraints2.science.1000.csv", itempool.science, itemattrib.science)

usethis::use_data(item.params.science.raw, overwrite = T)
usethis::use_data(item.attrib.science.raw, overwrite = T)
usethis::use_data(constraints.science.raw, overwrite = T)
usethis::use_data(itempool.science       , overwrite = T)
usethis::use_data(itemattrib.science     , overwrite = T)
usethis::use_data(constraints.science    , overwrite = T)
usethis::use_data(constraints2.science   , overwrite = T)

# Reading dataset

item.params.reading.raw  = read.csv("data-raw/item.params.reading.303.csv", header = T)
item.attrib.reading.raw  = read.csv("data-raw/item.attrib.reading.303.csv", header = T)
stim.attrib.reading.raw  = read.csv("data-raw/stim.attrib.reading.303.csv", header = T)
constraints.reading.raw  = read.csv("data-raw/constraints.reading.303.csv", header = T)
itempool.reading         = LoadItemPool("data-raw/item.params.reading.303.csv")
itemattrib.reading       = LoadItemAttrib("data-raw/item.attrib.reading.303.csv", itempool.reading)
stimattrib.reading       = LoadStAttrib("data-raw/stim.attrib.reading.303.csv", itemattrib.reading)
constraints.reading      = LoadConstraints("data-raw/constraints.reading.303.csv", itempool.reading, itemattrib.reading, stimattrib.reading)

usethis::use_data(item.params.reading.raw, overwrite = T)
usethis::use_data(item.attrib.reading.raw, overwrite = T)
usethis::use_data(stim.attrib.reading.raw, overwrite = T)
usethis::use_data(constraints.reading.raw, overwrite = T)
usethis::use_data(itempool.reading       , overwrite = T)
usethis::use_data(itemattrib.reading     , overwrite = T)
usethis::use_data(stimattrib.reading     , overwrite = T)
usethis::use_data(constraints.reading    , overwrite = T)

# Fatigue dataset

item.params.fatigue.raw  = read.csv("data-raw/item.params.fatigue.95.csv", header = T)
item.attrib.fatigue.raw  = read.csv("data-raw/item.attrib.fatigue.95.csv", header = T)
item.content.fatigue.raw = read.csv("data-raw/item.content.fatigue.95.csv", header = T)
constraints.fatigue.raw  = read.csv("data-raw/constraints.fatigue.95.csv", header = T)
resp.fatigue.raw         = read.csv("data-raw/resp.fatigue.95.csv", header = F)
itempool.fatigue         = LoadItemPool("data-raw/item.params.fatigue.95.csv")
itemattrib.fatigue       = LoadItemAttrib("data-raw/item.attrib.fatigue.95.csv", itempool.fatigue)
constraints.fatigue      = LoadConstraints("data-raw/constraints.fatigue.95.csv", itempool.fatigue, itemattrib.fatigue)

usethis::use_data(item.params.fatigue.raw , overwrite = T)
usethis::use_data(item.attrib.fatigue.raw , overwrite = T)
usethis::use_data(item.content.fatigue.raw, overwrite = T)
usethis::use_data(constraints.fatigue.raw , overwrite = T)
usethis::use_data(resp.fatigue.raw        , overwrite = T)
usethis::use_data(itempool.fatigue        , overwrite = T)
usethis::use_data(itemattrib.fatigue      , overwrite = T)
usethis::use_data(constraints.fatigue     , overwrite = T)
