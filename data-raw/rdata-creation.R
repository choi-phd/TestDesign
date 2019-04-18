usethis::use_data_raw()

par_science              = read.csv("data-raw/par_Science_1000.csv", header = T)
item_attrib_science      = read.csv("data-raw/item_attrib_Science_1000.csv", header = T)
constraints_science      = read.csv("data-raw/constraints_Science_1000.csv", header = T)

par_reading              = read.csv("data-raw/par_reading_303.csv", header = T)
item_attrib_reading      = read.csv("data-raw/item_attrib_reading_303.csv", header = T)
stimulus_attrib_reading  = read.csv("data-raw/stimulus_attrib_reading_303.csv", header = T)
constraints_reading      = read.csv("data-raw/constraints_reading_303.csv", header = T)

par_fatigue              = read.csv("data-raw/par_fatigue_95.csv", header = T)
item_attrib_fatigue      = read.csv("data-raw/item_attrib_fatigue_95.csv", header = T)
item_content_fatigue     = read.csv("data-raw/item_content_fatigue_95.csv", header = T)
constraints_fatigue      = read.csv("data-raw/constraints_fatigue_95.csv", header = T)
raw_fatigue              = read.csv("data-raw/dat_fatigue_95.csv", header = F)


usethis::use_data(par_science            , overwrite = T)
usethis::use_data(item_attrib_science    , overwrite = T)
usethis::use_data(constraints_science    , overwrite = T)

usethis::use_data(par_reading            , overwrite = T)
usethis::use_data(item_attrib_reading    , overwrite = T)
usethis::use_data(stimulus_attrib_reading, overwrite = T)
usethis::use_data(constraints_reading    , overwrite = T)

usethis::use_data(par_fatigue            , overwrite = T)
usethis::use_data(item_attrib_fatigue    , overwrite = T)
usethis::use_data(item_content_fatigue   , overwrite = T)
usethis::use_data(constraints_fatigue    , overwrite = T)
usethis::use_data(raw_fatigue            , overwrite = T)

if (FALSE){
  
write.csv(par_science, "par_science.csv", row.names = F)
itempool.science = LoadItemPool("par_science.csv")
write.csv(item_attrib_science, "item_attrib_science.csv", row.names = F)
itemattrib.science = LoadItemAttrib("item_attrib_science.csv", itempool.science)
write.csv(constraints_science, "constraints_science.csv", row.names = F)
constraints.science = LoadConstraints("constraints_science.csv", itempool.science, itemattrib.science)


write.csv(par_reading, "par_reading.csv", row.names = F)
itempool.reading = LoadItemPool("par_reading.csv")
write.csv(item_attrib_reading, "item_attrib_reading.csv", row.names = F)
itemattrib.reading = LoadItemAttrib("item_attrib_reading.csv", itempool.reading)
write.csv(stimulus_attrib_reading, "stimulus_attrib_reading.csv", row.names = F)
stimattrib.reading = LoadStAttrib("stimulus_attrib_reading.csv", itemattrib.reading)
write.csv(constraints_reading, "constraints_reading.csv", row.names = F)
constraints.reading = LoadConstraints("constraints_reading.csv", itempool.reading, itemattrib.reading, stimattrib.reading)

write.csv(par_fatigue, "par_fatigue.csv", row.names = F)
itempool.fatigue = LoadItemPool("par_fatigue.csv")
write.csv(item_attrib_fatigue, "item_attrib_fatigue.csv", row.names = F)
itemattrib.fatigue = LoadItemAttrib("item_attrib_fatigue.csv", itempool.fatigue)
write.csv(constraints_fatigue, "constraints_fatigue.csv", row.names = F)
constraints.fatigue = LoadConstraints("constraints_fatigue.csv", itempool.fatigue, itemattrib.fatigue)
} 
 
