#Author:Yaort
#Email:yaort@mail2.sysu.edu.cn
#Create Time:2025-04-22 20:28:04
#Last Modified by:Yaort
#Last Modified time:2025-04-22 20:44:55

rm(list = ls()); gc()
setwd("/Users/yaoruntian/HuangLab/PDPP")
if (!dir.exists("01_classify")) {dir.create("01_classify")}
setwd("01_classify")

library(openxlsx)
library(dplyr)
library(stringr)

#使用deepseek对02.country.xlsx的Country列添加注释行Type，因为原始数据中有空格，故使用excel替换函数删除空格后读入R语言去重。
country_data <- read.xlsx("02.country.xlsx")
unique_data <- country_data %>%
  distinct(Country, .keep_all = TRUE)
#save
write.xlsx(unique_data,"04.cleaned_country.xlsx")
#read
country_data <- read.xlsx("04.cleaned_country.xlsx")
df1 <- read.xlsx("../00_rawdata/01.cleaned_data.xlsx")
# df1$Country <- str_trim(df1$Country)
df1$Country <- gsub(" |\u00A0","",df1$Country)
res <- merge(country_data,df1,by = "Country",all = TRUE)
write.xlsx(res,"05.res.xlsx")
