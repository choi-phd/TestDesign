library(mvnfast)
use_data_raw()


for (i in 1:3){
  li = LETTERS[i]
  set.seed(i)
  idx.shuffle = sample(300)
  
  attrib_pool = read.csv(paste0("data-raw/base/attrib_pool_", li, ".csv"))
  attrib_pool = attrib_pool[idx.shuffle,]
  
  levels(attrib_pool[,2]) = list(A = "EMI", B = "IOD", C = "SIN")
  levels(attrib_pool[,3]) = list(RED = "BIO", WRI = "CHE", LIS = "ESS", SPK = "PHY")
  
  write.csv(attrib_pool, paste0("data-raw/attrib_pool_", li, ".csv"), row.names = F)

  par_Pool = read.csv(paste0("data-raw/base/par_Pool_", li, ".csv"), header = F)
  par_Pool = par_Pool[idx.shuffle,]

  set.seed(i)
  par_Pool[,3] = par_Pool[,3] + runif(300, -.1, .1)
  par_Pool[,4] = par_Pool[,4] + runif(300, -.1, .1)
  par_Pool[,5] = qnorm(par_Pool[,5])
  par_Pool[,5] = par_Pool[,5] + runif(300, -.1, .1)
  par_Pool[,5] = pnorm(par_Pool[,5])

  write.csv(par_Pool_A, paste0("data-raw/par_Pool_", li, ".csv"), row.names = F)

  se_Pool = read.csv(paste0("data-raw/base/se_Pool_", li, ".csv"), header = F)
  se_Pool = se_Pool[idx.shuffle,]
  
  set.seed(i)
  
  for (id in 3:5){
    se_Pool[,id] = se_Pool[,id] * runif(300, .95, 1.05)
  }
  
  write.csv(se_Pool, paste0("data-raw/se_Pool_", li, ".csv"), row.names = F)
}

constraints_0 = read.csv("data-raw/base/constraints_0.csv")
write.csv(constraints_0, "data-raw/constraints_0.csv", row.names = F, quote = F)

constraints_1 = read.csv("data-raw/base/constraints_1.csv", as.is = T)

constraints_1[,4] = gsub("EMI", "A", constraints_1[,4])
constraints_1[,4] = gsub("IOD", "B", constraints_1[,4])
constraints_1[,4] = gsub("SIN", "C", constraints_1[,4])
constraints_1[,4] = gsub("BIO", "RED", constraints_1[,4])
constraints_1[,4] = gsub("CHE", "WRI", constraints_1[,4])
constraints_1[,4] = gsub("ESS", "LIS", constraints_1[,4])
constraints_1[,4] = gsub("PHY", "SPK", constraints_1[,4])

write.csv(constraints_1, "data-raw/constraints_1.csv", row.names = F, quote = F)

attrib_pool = read.csv("data-raw/base/item_attrib_RD_45.csv")

set.seed(42)
idx.shuffle = sample(514)
attrib_pool = attrib_pool[idx.shuffle,]
attrib_pool[,1] = (1:514) + 10000

attrib_pool[,2] = as.integer(as.factor(attrib_pool[,2])) + 1000
tmp = as.integer(attrib_pool[,6])
subid = (tmp %% 8) + 1
setid = floor(tmp / 8) + 1
setnames = as.factor(setid)
levels(setnames) = c("ALGEBRA", "GEOMETRY", "TRIGNOMETRY", "CALCULUS")
as.character(setnames)
attrib_pool[,6] = paste0(setnames, " ", setid, "0", subid)

set.seed(1)
attrib_pool[,7] = attrib_pool[,7] + sample(c(0,1), 514, replace = T)

write.csv(attrib_pool, "data-raw/item_attrib_SET.csv")


par_pool = read.csv("data-raw/base/item_pool_RD_45.csv", header = F)
par_pool = par_pool[idx.shuffle,]
par_pool[,1] = attrib_pool[,1]

idx.gpc = which(par_pool[,2] == "GPC")

set.seed(1)
n = length(par_pool[-idx.gpc,3])
par_pool[-idx.gpc,3] = par_pool[-idx.gpc,3] + runif(n, -.1, .1)
par_pool[-idx.gpc,4] = par_pool[-idx.gpc,4] + runif(n, -.1, .1)

n = length(par_pool[idx.gpc,3])
par_pool[idx.gpc,3] = par_pool[idx.gpc,3] + runif(n, -.1, .1)
par_pool[idx.gpc,4] = par_pool[idx.gpc,4] + runif(n, -.1, .1)
par_pool[idx.gpc,5] = par_pool[idx.gpc,5] + runif(n, -.1, .1)

write.csv(par_pool, "data-raw/item_pool_SET.csv")


st_attrib = read.csv("data-raw/base/stimulus_attrib_RD_45.csv")
st_attrib[,1] = sort(unique(attrib_pool[,2]))
write.csv(st_attrib, "data-raw/st_attrib_SET.csv")

par_mixed     = read.csv("data-raw/base/par_mixed.csv", header = F)

usethis::use_data(attrib_pool_A, overwrite = T)
usethis::use_data(attrib_pool_B, overwrite = T)
usethis::use_data(attrib_pool_C, overwrite = T)
usethis::use_data(constraints_0, overwrite = T)
usethis::use_data(constraints_1, overwrite = T)
usethis::use_data(par_Pool_A, overwrite = T)
usethis::use_data(par_Pool_B, overwrite = T)
usethis::use_data(par_Pool_C, overwrite = T)
usethis::use_data(par_mixed, overwrite = T)
usethis::use_data(se_Pool_A, overwrite = T)
usethis::use_data(se_Pool_B, overwrite = T)
usethis::use_data(se_Pool_C, overwrite = T)

