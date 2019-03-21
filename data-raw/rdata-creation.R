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
  write.csv(attrib_pool, paste0("data-raw/attrib_pool_", li, ".csv"), row.names = F)

  par_Pool = read.csv(paste0("data-raw/base/par_Pool_", li, ".csv"), header = F)
  par_Pool = par_Pool[idx.shuffle,]

  set.seed(i)
  par_Pool[,3] = par_Pool[,3] + runif(300, -.1, .1)
  par_Pool[,4] = par_Pool[,4] + runif(300, -.1, .1)
  par_Pool[,5] = qnorm(par_Pool[,5])
  par_Pool[,5] = par_Pool[,5] + runif(300, -.1, .1)
  par_Pool[,5] = pnorm(par_Pool[,5])
  par_Pool[,1] = 1:300
  
  write.csv(par_Pool, paste0("data-raw/par_Pool_", li, ".csv"), row.names = F)

  se_Pool = read.csv(paste0("data-raw/base/se_Pool_", li, ".csv"), header = F)
  se_Pool = se_Pool[idx.shuffle,]
  se_Pool[,1] = 1:300
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
constraints_1[,4] = gsub("BIO", "RD", constraints_1[,4])
constraints_1[,4] = gsub("CHE", "WRI", constraints_1[,4])
constraints_1[,4] = gsub("ESS", "LIS", constraints_1[,4])
constraints_1[,4] = gsub("PHY", "SPK", constraints_1[,4])

write.csv(constraints_1, "data-raw/constraints_1.csv", row.names = F, quote = F)

## EXAMPLE DATASETS FOR STIMULUS-BASED ITEMS

attrib_pool = read.csv("data-raw/base/item_attrib_RD_45.csv")
set.seed(42)
idx.shuffle = sample(514)
attrib_pool = attrib_pool[idx.shuffle,]
attrib_pool[,1] = (1:514) + 10000

attrib_pool[,2] = as.integer(as.factor(attrib_pool[,2])) + 1000

set.seed(42)
subcontent_map = sample(14)
subcontent_map
attrib_pool[,10] = subcontent_map[attrib_pool[,10]]

attrib_pool[,7] = attrib_pool[,7] + 2

subid = attrib_pool[,7]
setid = attrib_pool[,10]
setnames = as.factor(setid)
levels(setnames) = rep(c("ALGEBRA", "GEOMETRY", "TRIGNOMETRY", "CALCULUS", "STATISTICS", "PROBABILITY", "CODING"), 2)

as.character(setnames)
attrib_pool[,6] = paste0(setid, "-", subid, ": ", setnames)
attrib_pool = attrib_pool[,-c(11,12)]
write.csv(attrib_pool, "data-raw/item_attrib_SET.csv", row.names = F)


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

write.table(par_pool, "data-raw/item_pool_SET.csv", row.names = F, col.names = F, sep = ",")

# Stimulus attributes
st_attrib = read.csv("data-raw/base/stimulus_attrib_RD_45.csv")
st_attrib[,1] = sort(unique(attrib_pool[,2]))
write.csv(st_attrib, "data-raw/st_attrib_SET.csv", row.names = F)


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

write.csv(const, "data-raw/constraints_SET.csv", row.names = F)

par_pool_A = read.csv("data-raw/par_pool_A.csv", header = T)
par_pool_B = read.csv("data-raw/par_pool_B.csv", header = T)
par_pool_C = read.csv("data-raw/par_pool_C.csv", header = T)
se_pool_A = read.csv("data-raw/se_pool_A.csv", header = T)
se_pool_B = read.csv("data-raw/se_pool_B.csv", header = T)
se_pool_C = read.csv("data-raw/se_pool_C.csv", header = T)
attrib_pool_A = read.csv("data-raw/attrib_pool_A.csv", header = T)
attrib_pool_B = read.csv("data-raw/attrib_pool_B.csv", header = T)
attrib_pool_C = read.csv("data-raw/attrib_pool_C.csv", header = T)
constraints_0 = read.csv("data-raw/constraints_0.csv", header = T)
constraints_1 = read.csv("data-raw/constraints_1.csv", header = T)

usethis::use_data(par_pool_A, overwrite = T)
usethis::use_data(par_pool_B, overwrite = T)
usethis::use_data(par_pool_C, overwrite = T)
#usethis::use_data(par_mixed, overwrite = T)
usethis::use_data(se_pool_A, overwrite = T)
usethis::use_data(se_pool_B, overwrite = T)
usethis::use_data(se_pool_C, overwrite = T)
usethis::use_data(attrib_pool_A, overwrite = T)
usethis::use_data(attrib_pool_B, overwrite = T)
usethis::use_data(attrib_pool_C, overwrite = T)
usethis::use_data(constraints_0, overwrite = T)
usethis::use_data(constraints_1, overwrite = T)

item_pool_SET = read.csv("data-raw/item_pool_SET.csv", header = F)
item_attrib_SET = read.csv("data-raw/item_attrib_SET.csv", header = T)
st_attrib_SET = read.csv("data-raw/st_attrib_SET.csv", header = T)
constraints_SET = read.csv("data-raw/constraints_SET.csv", header = T)
usethis::use_data(item_pool_SET, overwrite = T)
usethis::use_data(item_attrib_SET, overwrite = T)
usethis::use_data(st_attrib_SET, overwrite = T)
usethis::use_data(constraints_SET, overwrite = T)
