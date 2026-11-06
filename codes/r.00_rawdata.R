#Author:Yaort
#Email:yaort@mail2.sysu.edu.cn
#Create Time:2025-04-22 19:10:42
#Last Modified by:Yaort
#Last Modified time:2025-04-22 20:28:21
rm(list = ls()); gc()
setwd("/Users/yaoruntian/HuangLab/PDPP")
if (!dir.exists("00_rawdata")) {dir.create("00_rawdata")}
setwd("00_rawdata")
library(openxlsx)
#读取数据并提取需要的行列，保存为清洗数据
df1 <- read.xlsx("rawdata.xlsx")
df1 <- df1[,c(2,3,6,9,12,23,24,26,40,41)]
colnames(df1)
# [1] "Location"                "DNA.nomenclature"        "Type.of.variant.DNA"     "Predicted.severity"      "CRIM.status"             "Allele.2.Location"      
# [7] "Allele.2.DNA"            "Phenotype.of.patient"    "No.of.patients.reported" "Country/Region"  
colnames(df1) <- c("A1_Location","A1_DNA","Mutation_Type","Predicted_severity","CRIM_status","A2_Location","A2_DNA","Phenotype","Patient_Count","Country")
write.xlsx(df1,"01.cleaned_data.xlsx")
#生成地区表格
dat1 <- as.data.frame(table(df1[,9]))
write.xlsx(dat1,"02.country.xlsx")

