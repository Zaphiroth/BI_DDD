library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(purrr)
library(data.table)
library(stringr)
library(openxlsx)


##-- 注意如果确认你的数据时放在下面这个文件下的子文件夹：ffromall raw
##-- 下，那么你不需要修改任何代码就可以运行程序???
# setwd("//eu.boehringer.com//users//sha//users2//zhengli//Desktop//DATA//IMS DDD process//IMS DDD Data House")
setwd("//eu.boehringer.com//users//sha//users2//zhengli//Desktop//DATA//IMS DDD process//IMS DDD Data House")

##-- read in the raw ddd data
# ddd <- read.xlsx("./ffromall raw/ffromall raw.xlsx")
ddd <- read.csv("./final data/ffromall raw.csv",
                header = TRUE, stringsAsFactors = F,
                check.names = FALSE)

##-- brand rename
##-- 请将 "Brand Rename.xlsx" 文件与 "ffromall raw.xlsx" 文件放在同一文件夹路径下, 即 ”final data" 文件夹内
new_brand_data <- read.xlsx("./final data/Brand Rename.xlsx",
                            check.names = FALSE) %>% 
  mutate(PACK = as.character(PACK)) %>% 
  select("PACK", "new_brand" = "New.Brand_CN")

ddd <- ddd %>% 
  left_join(new_brand_data, by = c("PACK")) %>% 
  mutate(Brand_CN = ifelse(!is.na(new_brand),
                           new_brand,
                           Brand_CN)) %>% 
  select(-new_brand)

##--
ddd[] <- Map(
  function(x, y) {
    if (startsWith(y, "20") && !is.numeric(x)) {
      as.numeric(x)
    } else {
      x
    }
  },
  ddd, colnames(ddd))

##-- transpose the data to get the csv data
select_var <- colnames(ddd)[startsWith(colnames(ddd), "20")]
select_var <- format(as.numeric(select_var), nsmall = 2)
colnames(ddd)[startsWith(colnames(ddd), "20")] <- select_var

lm1 <- select_var[12]
lm2 <- select_var[24]

ddd_m <- data.table::dcast(setDT(ddd), 
                           ATC.1.Code + ATC.2.Code + ATC.3.Code + ATC.4.Code + 
                             Molecule.code + Molecule + Product + APP1 + Form1 + Manufactory + 
                             Corporation + Pack.Molecule + Main + Category + Category_CN + 
                             Category.type + Sub.category + Molecule_CN + Brand + Brand_CN + 
                             MANU_CN + Region + Province + Province_CN + City + City_CN + 
                             Veeva.code + Veeva.name + Decile + Note ~ Period + Measurement,
                           value.var = select_var,
                           fun = sum,
                           na.rm = TRUE)

colnames_tmp <- 
  str_split(colnames(ddd_m)[startsWith(colnames(ddd_m), "20")],
            "_", simplify = TRUE)

colnames_tmp <- paste(tolower(colnames_tmp[, 2]), 
                      toupper(colnames_tmp[, 3]),
                      colnames_tmp[, 1],
                      sep = "_")

colnames(ddd_m)[startsWith(colnames(ddd_m), "20")] <- colnames_tmp

##-- format the csv file
mth_colnames <- c(colnames(ddd_m)[grep("mth_RMB", colnames(ddd_m))],
                  colnames(ddd_m)[grep("mth_DOT", colnames(ddd_m))],
                  colnames(ddd_m)[grep("mth_UNIT", colnames(ddd_m))])

qtr_colnames <- c(colnames(ddd_m)[grep("qtr_RMB", colnames(ddd_m))],
                  colnames(ddd_m)[grep("qtr_DOT", colnames(ddd_m))],
                  colnames(ddd_m)[grep("qtr_UNIT", colnames(ddd_m))])

# ytd_colnames <- c(colnames(ddd_m)[grep("ytd_RMB", colnames(ddd_m))],
#                   colnames(ddd_m)[grep("ytd_DOT", colnames(ddd_m))],
#                   colnames(ddd_m)[grep("ytd_UNIT", colnames(ddd_m))]) 

ytd_colnames <- c(paste("ytd_RMB", lm1, sep = "_"),
                  paste("ytd_RMB", lm2, sep = "_"),
                  paste("ytd_DOT", lm1, sep = "_"),
                  paste("ytd_DOT", lm2, sep = "_"),
                  paste("ytd_UNIT", lm1, sep = "_"),
                  paste("ytd_UNIT", lm2, sep = "_"))

                  
# mat_colnames <- c(colnames(ddd_m)[grep("mat_RMB", colnames(ddd_m))],
#                   colnames(ddd_m)[grep("mat_DOT", colnames(ddd_m))],
#                   colnames(ddd_m)[grep("mat_UNIT", colnames(ddd_m))])

mat_colnames <- c(paste("mat_RMB", lm1, sep = "_"),
                  paste("mat_RMB", lm2, sep = "_"),
                  paste("mat_DOT", lm1, sep = "_"),
                  paste("mat_DOT", lm2, sep = "_"),
                  paste("mat_UNIT", lm1, sep = "_"),
                  paste("mat_UNIT", lm2, sep = "_"))

ddd_m <- setDF(ddd_m)

ddd_m <- ddd_m[,c(colnames(ddd_m)[!grepl("20", colnames(ddd_m))],
                  c(mth_colnames, qtr_colnames, ytd_colnames, mat_colnames))]

##-- output the csv file
output <- 
  lapply(unique(ddd_m$Main), 
         function(x) {
           tmp <- ddd_m %>%
             filter(Main == x)
           write.csv(tmp, paste("./final data/ddd_", x, "_csv_format.csv", sep = ""),
                     row.names = FALSE)
           print(paste(x, " finished!"))
           invisible()
         })












  
         
  







