use_data_raw()


for (i in 1:3){
  li = LETTERS[i]
  set.seed(i)
  idx.shuffle = sample(300)

  attrib_pool = read.csv(paste0("data-raw/base/attrib_pool_", li, ".csv"))
  attrib_pool = attrib_pool[idx.shuffle,]

  levels(attrib_pool[,2]) = list(A = "EMI", B = "IOD", C = "SIN")
  levels(attrib_pool[,3]) = list(RD = "BIO", WRI = "CHE", LIS = "ESS", SPK = "PHY")

  attrib_pool[,1] = 1:300
  write.csv(attrib_pool, paste0("data-raw/item_attribs_", li, ".csv"), row.names = F)

  par_Pool = read.csv(paste0("data-raw/base/par_Pool_", li, ".csv"), header = F)
  par_Pool = par_Pool[idx.shuffle,]

  set.seed(i)
  par_Pool[,3] = par_Pool[,3] + runif(300, -.1, .1)
  par_Pool[,4] = par_Pool[,4] + runif(300, -.1, .1)
  par_Pool[,5] = qnorm(par_Pool[,5])
  par_Pool[,5] = par_Pool[,5] + runif(300, -.1, .1)
  par_Pool[,5] = pnorm(par_Pool[,5])
  par_Pool[,1] = 1:300

  write.table(par_Pool, paste0("data-raw/item_params_", li, ".csv"), row.names = F, col.names = F, sep = ",")

  se_Pool = read.csv(paste0("data-raw/base/se_Pool_", li, ".csv"), header = F)
  se_Pool = se_Pool[idx.shuffle,]
  se_Pool[,1] = 1:300
  set.seed(i)

  for (id in 3:5){
    se_Pool[,id] = se_Pool[,id] * runif(300, .95, 1.05)
  }

  write.csv(se_Pool, paste0("data-raw/item_se_", li, ".csv"), row.names = F)
}

constraints_0 = read.csv("data-raw/base/constraints_0.csv")
write.csv(constraints_0, "data-raw/constraints_0.csv", row.names = F, quote = F, na = '""')

constraints_1 = read.csv("data-raw/base/constraints_1.csv", as.is = T)

constraints_1[,4] = gsub("EMI", '"A"', constraints_1[,4])
constraints_1[,4] = gsub("IOD", '"B"', constraints_1[,4])
constraints_1[,4] = gsub("SIN", '"C"', constraints_1[,4])
constraints_1[,4] = gsub("BIO", '"RD"', constraints_1[,4])
constraints_1[,4] = gsub("CHE", '"WRI"', constraints_1[,4])
constraints_1[,4] = gsub("ESS", '"LIS"', constraints_1[,4])
constraints_1[,4] = gsub("PHY", '"SPK"', constraints_1[,4])
constraints_1[,4] = gsub("CV", '"CV"', constraints_1[,4])
constraints_1[,4] = gsub("DR", '"DR"', constraints_1[,4])
constraints_1[,4] = gsub("RS", '"RS"', constraints_1[,4])
constraints_1[,4] = paste0('"', constraints_1[,4], '"')
write.table(constraints_1, "data-raw/constraints_1.csv", row.names = F, quote = F, sep = ",")

## EXAMPLE DATASETS FOR STIMULUS-BASED ITEMS

attrib_pool = read.csv("data-raw/base/item_attrib_RD_45.csv")

attrib_pool[,2] = as.integer(as.factor(attrib_pool[,2])) + 1000

set.seed(42)
idx.shuffle = sample(514)
attrib_pool = attrib_pool[idx.shuffle,]
attrib_pool = attrib_pool[order(attrib_pool[,2]),]
attrib_pool[,1] = (1:514) + 10000

set.seed(42)
subcontent_map = sample(14)
attrib_pool[,10] = subcontent_map[attrib_pool[,10]]

attrib_pool[,7] = attrib_pool[,7] + 2

subid = attrib_pool[,7]
setid = attrib_pool[,10]
setnames = as.factor(setid)
levels(setnames) = rep(c("ALGEBRA", "GEOMETRY", "TRIGNOMETRY", "CALCULUS", "STATISTICS", "PROBABILITY", "CODING"), 2)

attrib_pool[,6] = paste0(setid, "-", subid, ": ", setnames)
write.csv(attrib_pool, "data-raw/item_attribs_stimbased.csv", row.names = F, quote = F)


par_pool = read.csv("data-raw/base/item_pool_RD_45.csv", header = F)
par_pool = par_pool[idx.shuffle,]
par_pool = par_pool[order(attrib_pool[,2]),]

par_pool[,1] = (1:514) + 10000

idx.gpc = which(par_pool[,2] == "GPC")

set.seed(1)
n = length(par_pool[-idx.gpc,3])
par_pool[-idx.gpc,3] = par_pool[-idx.gpc,3] + runif(n, -.1, .1)
par_pool[-idx.gpc,4] = par_pool[-idx.gpc,4] + runif(n, -.1, .1)

n = length(par_pool[idx.gpc,3])
par_pool[idx.gpc,3] = par_pool[idx.gpc,3] + runif(n, -.1, .1)
par_pool[idx.gpc,4] = par_pool[idx.gpc,4] + runif(n, -.1, .1)
par_pool[idx.gpc,5] = par_pool[idx.gpc,5] + runif(n, -.1, .1)

write.table(par_pool, "data-raw/item_params_stimbased.csv", row.names = F, col.names = F, sep = ",", quote = F)

# Stimulus attributes
st_attrib = read.csv("data-raw/base/stimulus_attrib_RD_45.csv")
st_attrib[,1]= as.integer(as.factor(st_attrib[,1])) + 1000
write.csv(st_attrib, "data-raw/stim_attribs_stimbased.csv", row.names = F)


# Constraints
const = read.csv("data-raw/base/constraints_RD_45.csv", as.is = T)

for(r in 7:13){

  for(i in 1:14){

    b = grep(paste0(i, ","), const[r,4], fixed = T)

    if (length(b) == 1){
      const[r,4] = gsub(paste0(i, ","), paste0(subcontent_map[i], ","), const[r,4], fixed = T)
    }

    if (length(b) == 1){
      break
    }
  }

  for(i in 1:14){

    b = grep(paste0(" ", i, ")"), const[r,4], fixed = T)

    if (length(b) == 1){
      const[r,4] = gsub(paste0(" ", i, ")"), paste0(" ", subcontent_map[i], ")"), const[r,4], fixed = T)
    }

    if (length(b) == 1){
      break
    }
  }
}

const[14:16,4] = gsub('"', '""', const[14:16,4])
const[7:16,4] = paste0('"', const[7:16,4], '"')
write.table(const, "data-raw/constraints_stimbased.csv", row.names = F, quote = F, sep = ",")

item_params_A    = read.csv("data-raw/item_params_A.csv", header = F)
item_params_B    = read.csv("data-raw/item_params_B.csv", header = F)
item_params_C    = read.csv("data-raw/item_params_C.csv", header = F)
item_se_A        = read.csv("data-raw/item_se_A.csv", header = T)
item_se_B        = read.csv("data-raw/item_se_B.csv", header = T)
item_se_C        = read.csv("data-raw/item_se_C.csv", header = T)
item_attribs_A   = read.csv("data-raw/item_attribs_A.csv", header = T)
item_attribs_B   = read.csv("data-raw/item_attribs_B.csv", header = T)
item_attribs_C   = read.csv("data-raw/item_attribs_C.csv", header = T)
constraints_0    = read.csv("data-raw/constraints_0.csv", header = T)
constraints_1    = read.csv("data-raw/constraints_1.csv", header = T)

usethis::use_data(item_params_A, overwrite = T)
usethis::use_data(item_params_B, overwrite = T)
usethis::use_data(item_params_C, overwrite = T)
#usethis::use_data(par_mixed, overwrite = T)
usethis::use_data(item_se_A, overwrite = T)
usethis::use_data(item_se_B, overwrite = T)
usethis::use_data(item_se_C, overwrite = T)
usethis::use_data(item_attribs_A, overwrite = T)
usethis::use_data(item_attribs_B, overwrite = T)
usethis::use_data(item_attribs_C, overwrite = T)
usethis::use_data(constraints_0, overwrite = T)
usethis::use_data(constraints_1, overwrite = T)

item_params_stimbased  = read.csv("data-raw/item_params_stimbased.csv", header = F)
item_attribs_stimbased = read.csv("data-raw/item_attribs_stimbased.csv", header = T)
stim_attribs_stimbased = read.csv("data-raw/stim_attribs_stimbased.csv", header = T)
constraints_stimbased  = read.csv("data-raw/constraints_stimbased.csv", header = T)
usethis::use_data(item_params_stimbased , overwrite = T)
usethis::use_data(item_attribs_stimbased, overwrite = T)
usethis::use_data(stim_attribs_stimbased, overwrite = T)
usethis::use_data(constraints_stimbased , overwrite = T)


item_params_fatigue    = read.csv("data-raw/item_params_fatigue.csv", header = F)
item_attribs_fatigue   = read.csv("data-raw/item_attribs_fatigue.csv", header = T)
item_contents_fatigue  = read.csv("data-raw/item_contents_fatigue.csv", header = T)
constraints_fatigue    = read.csv("data-raw/constraints_fatigue.csv", header = T)
raw_fatigue            = read.csv("data-raw/raw_fatigue.csv", header = F)

usethis::use_data(item_params_fatigue  , overwrite = T)
usethis::use_data(item_attribs_fatigue , overwrite = T)
usethis::use_data(item_contents_fatigue, overwrite = T)
usethis::use_data(constraints_fatigue  , overwrite = T)
usethis::use_data(raw_fatigue          , overwrite = T)
