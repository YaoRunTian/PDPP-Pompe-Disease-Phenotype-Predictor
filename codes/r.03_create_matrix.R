#Author:Yaort
#Email:yaort@mail2.sysu.edu.cn
#Create Time:2025-04-25 04:17:02
#Last Modified by:Yaort
#Last Modified time:2025-04-28 23:42:05
rm(list = ls()); gc()
setwd("/Users/yaoruntian/HuangLab/PDPP")
if (!dir.exists("03_create_matrix")) {dir.create("03_create_matrix")}
setwd("03_create_matrix")
library(openxlsx)
library(reshape2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(gtools)
#读取数据---------------------------------
# dat1 <- read.xlsx("00.res.xlsx")#来自02路径05.res.xlsx
dat1 <- read.xlsx("../01_classify/05.res.xlsx")#来自01路径05.res.xlsx
df <- dat1
df <- na.omit(df)
#重排列名的顺序---------------------------
df <- df %>%
  select(A1_Location,A1_DNA,A2_Location,A2_DNA,Phenotype,Patient_Count,
    Country,Type,Mutation_Type,Predicted_severity,CRIM_status) %>%
  rename(Population = Type)
#整理phenotype----------------------------
table(df$Phenotype)
# Adult                                Adult               Adult (1)/ Childhood (1)                             Adult (2) 
# 704                                     5                                     1                                     5 
# Adult (4)/ unknown (2)                      Adult/ Childhood                          asymptomatic                          Asymptomatic 
# 2                                     4                                    25                                     2 
# childhood                             Childhood                            Childhood               Childhood (1)/ Adult (1) 
# 2                                   505                                     1                                    12 
# Childhood (1)/ Adult (2)              Childhood (1)/ Adult (4) Childhood (1)/ Childhood or Adult (1)            Childhood (1)/ unknown (1) 
# 9                                     1                                     2                                     2 
# Childhood (1)/ unknown (2)              Childhood (2) /Adult (4)              Childhood (2)/ Adult (1)              Childhood (2)/ Adult (4) 
# 2                                     1                                     2                                     1 
# Childhood (2)/ Adult (5)                         Childhood (3)                  Childhood /Adult (2)                    Childhood or Adult 
# 2                                     3                                     2                                   126 
# Childhood or Adult/ Childhood                     Childhood/ Adult                      Classic infantile                     Classic Infantile 
# 2                                    15                                   584                                     2 
# Classic infantile (1) / Childhood (2)                 Classic infantile (2)                               mosaic                                    NBS 
# 1                                     2                                     1                                    31 
# phenotype not reported                               unknown                               Unknown                           unknown (2) 
# 1                                    55                                     4                                     2 
write.xlsx(df,"01.df.xlsx")##Childhood/Adult这一phenotype混杂，此处需人工整理,整理完后再读入该文件修改版,同时将“ ”替换为“”

df <- read.xlsx("01.df_manually_edited.xlsx")
processed_df <- df %>%
  # 记录原始 Phenotype 是否含有 "/"
  mutate(has_slash = str_detect(Phenotype, fixed("/"))) %>%
  # 拆分含 "/" 的行
  separate_rows(Phenotype, sep = "/") %>%
  mutate(
    # 提取括号内的数字
    bracket_num = str_extract(Phenotype, "\\(\\d+\\)") %>%
      str_remove_all("[()]") %>%
      as.numeric(),
    
    Patient_Count = case_when(
      has_slash & !is.na(bracket_num) ~ bracket_num,
      has_slash & is.na(bracket_num)  ~ 1,
      TRUE                            ~ Patient_Count
    ),
    # 移除括号及其中的内容
    Phenotype = str_remove(Phenotype, "\\s*\\(.*?\\)")
  ) %>%
  select(-has_slash, -bracket_num)

table(processed_df$Phenotype)
# Adult         asymptomatic         Asymptomatic            childhood            Childhood     ChildhoodorAdult     Classicinfantile 
# 751                   25                    2                    2                  553                  145                  587 
# ClassicInfantile               mosaic                  NBS phenotypenotreported              unknown              Unknown 
# 2                    1                   31                    1                   63                    4 
processed_df <- processed_df[-grep("know",processed_df$Phenotype),]
processed_df <- processed_df[-grep("mosaic",processed_df$Phenotype),]
processed_df <- processed_df[-grep("phenotypenotreported",processed_df$Phenotype),]
processed_df <- processed_df[-grep("NBS",processed_df$Phenotype),]
processed_df$Phenotype <- gsub("asymptomatic","Asymptomatic",processed_df$Phenotype)
processed_df$Phenotype <- gsub("Classicinfantile|Classic|ClassicInfantile","Classic infantile",processed_df$Phenotype)
processed_df$Phenotype <- gsub("childhood","Childhood",processed_df$Phenotype)
processed_df$Phenotype <- gsub("ChildhoodorAdult","Childhood or Adult",processed_df$Phenotype)
table(processed_df$Phenotype)
# Adult       Asymptomatic          Childhood Childhood or Adult  Classic infantile 
# 751                 27                555                145                589 

#清洗location---------------------------------------
#将GAA and part of CCDC40 和CCDC40 and GAA exon 1  改为exon 1
grep("GAA",processed_df$A1_Location)
processed_df$A1_Location[grep("GAA",processed_df$A1_Location)] <- "exon 1"
grep("GAA",processed_df$A2_Location)
processed_df$A2_Location[grep("GAA",processed_df$A2_Location)] <- "exon 1"
#整理Predicted_Severity列------------------------------------
table(processed_df$Predicted_severity)
processed_df <- processed_df %>%
  mutate(Severity = case_when(
    Predicted_severity == "less severe"~"Less severe",
    Predicted_severity == "Presumably non-pathogenic"~"Non-pathogenic",
    Predicted_severity == "potentially less severe"~"Less severe",
    Predicted_severity == "Potentially less severe"~"Less severe",
    Predicted_severity == "very severe"~"Very severe",
    TRUE ~ Predicted_severity
  ))
processed_df <- processed_df[-grep("Non-pathogenic",processed_df$Severity),]
table(processed_df$Severity)
# Less severe Potentially mild          Unknown      Very severe 
# 666              613               81              696 
#整理Mutation_Type列--------------------------
processed_df <- processed_df %>%
  mutate(Mutation_Type = case_when(
    Mutation_Type == "deletion"~"Deletion",
    Mutation_Type == "duplication"~"Duplication",
    Mutation_Type == "insertion"~"Insertion",
    Mutation_Type == "substitution"~"Substitution",
    Mutation_Type == "Deletion/ insertion"~"Delins",
    A1_DNA == "c.1585_1586delinsGT"~"Delins",
    TRUE ~ Mutation_Type
  ))
# processed_df <- processed_df[-grep("Deletion/ insertion",processed_df$Mutation_Type),]
table(processed_df$Mutation_Type)
# Deletion       Delins  Duplication    Insertion Substitution 
# 366           22           45            9         1614 

#2056个records

#添加A2_Mutation_Type列---------------------------------------
mut_num <- grep(">",processed_df$A2_DNA)
dup_num <- grep("dup",processed_df$A2_DNA)
ins_num <- grep("(?<!del)ins", processed_df$A2_DNA, perl = TRUE)
del_num <- grep("(?<!delins)del(?!ins)", processed_df$A2_DNA, perl = TRUE)
delins_num <- grep("delins",processed_df$A2_DNA)
processed_df$A2_Mutation_Type <- NA
processed_df[mut_num,"A2_Mutation_Type"] <- "Substitution"
processed_df[dup_num,"A2_Mutation_Type"] <- "Duplication"
processed_df[ins_num,"A2_Mutation_Type"] <- "Insertion"
processed_df[del_num,"A2_Mutation_Type"] <- "Deletion"
processed_df[delins_num,"A2_Mutation_Type"] <- "Delins"
#提取A1和A2相同的列-------------------------------
same_index <- processed_df$A1_DNA == processed_df$A2_DNA
df_same <- processed_df[same_index,]
df_diff <- processed_df[!same_index,]
write.xlsx(df_diff,"02.df_diff.xlsx")
write.xlsx(df_same,"02.df_same.xlsx")
#处理df_diff-------------------------------
# 下面的代码采用以下匹配规则：
# - 国家、Population、表型和 Patient_Count 相同；
# - A1、A2 的位置及 DNA 互相对调；
# - 如果满足条件的记录有多对，按它们在原表中的出现顺序配对；
# - 找不到对应行的记录仍然保留，并单独编号。

df_patient <- df_diff %>%
  # 保留原始行号，方便回查和维持顺序
  mutate(
    source_row = row_number(),
    # 用位置和 DNA 一起识别每个等位基因
    allele1 = paste(A1_Location, A1_DNA, sep = " | "),
    allele2 = paste(A2_Location, A2_DNA, sep = " | "),
    # 将两个等位基因按固定顺序排列，供对调记录匹配
    allele_low  = pmin(allele1, allele2),
    allele_high = pmax(allele1, allele2),
    # 标记当前行的排列方向
    direction = if_else(allele1 == allele_low, 1L, 2L)
  ) %>%
  # 同一方向重复出现时，按照原表顺序编号
  group_by(
    Country, Population, Phenotype, Patient_Count,
    allele_low, allele_high, direction
  ) %>%
  mutate(pair_number = row_number()) %>%
  ungroup() %>%
  # 按患者数复制行，patient_number 为该记录中的第几个患者
  uncount(weights = Patient_Count, .remove = FALSE, .id = "patient_number") %>%
  mutate(Patient_Count = 1L)  %>%
  # 对调的两行使用相同的分组和患者编号
  group_by(
    Country, Population, Phenotype, Patient_Count,
    allele_low, allele_high, pair_number, patient_number
  ) %>%
  mutate(Patient_ID = paste0("patient", cur_group_id())) %>%
  ungroup() %>%
  arrange(source_row, patient_number) %>%
  select(
    Patient_ID, everything(),
    -allele1, -allele2, -allele_low, -allele_high,
    -direction, -pair_number, -patient_number,-source_row
  )
write.xlsx(df_patient, "03.df_diff_with_patient_id.xlsx")
#single records
#363个patient id只出现了一次
df_single <- df_patient %>%
  group_by(Patient_ID) %>%
  filter(n() == 1) %>%
  ungroup()
write.xlsx(df_single, "04.single_patient_records.xlsx")

#paired的records
df_paired <- df_patient %>%
  group_by(Patient_ID) %>%
  filter(n() == 2) %>%
  ungroup()
write.xlsx(df_paired, "05.df_diff_paired_only.xlsx")
#处理df_same-------------------------------
# 找到已有的最大患者编号
max_patient_id <- max(
  c(0L, as.integer(sub("^patient", "", df_paired$Patient_ID))),
  na.rm = TRUE
)
df_same_patient <- df_same %>%
  # 根据 Patient_Count 展开，每行代表一位患者
  uncount(weights = Patient_Count) %>%
  mutate(
    Patient_ID = paste0("patient", max_patient_id + row_number()),
    Patient_Count = 1L
  ) %>%
  # 每位患者再复制一行，两行共享 Patient_ID
  uncount(weights = 2) %>%
  select(Patient_ID, everything())
write.xlsx(df_same_patient, "06.df_same_patient.xlsx")
#合并两个表生成df_final------------------
final_df <- rbind(df_paired,df_same_patient) #2624
#因为删除了df_single，所以重新编号patient id
old_ids <- mixedsort(unique(final_df$Patient_ID))
final_df <- final_df %>%
  mutate(
    Patient_ID = paste0("patient", match(Patient_ID, old_ids))
  ) %>%
  arrange(match(Patient_ID, mixedsort(unique(Patient_ID))))
#生成A1_SNP-----------
colnames(final_df)[colnames(final_df) == "Mutation_Type"] <- "A1_Mutation_Type"
final_df <- final_df %>%
  mutate(
    A1_DNA = if_else(
      A1_Mutation_Type == "Substitution",
      str_remove_all(A1_DNA, "[ \u00A0]"),
      A1_DNA
    ),
    A1_SNP = if_else(
      A1_Mutation_Type == "Substitution",
      str_sub(A1_DNA, -3),
      "E"
    )
  )
table(final_df$A1_SNP)
# A>C A>G A>T C>A C>G C>T   E G>A G>C G>T T>A T>C T>G 
# 15  46  20 172  79 286 565 392  89 105  31 144 680 
#生成A2_SNP-----------
final_df <- final_df %>%
  mutate(
    A2_DNA = if_else(
      A2_Mutation_Type == "Substitution",
      str_remove_all(A2_DNA, "[ \u00A0]"),
      A2_DNA
    ),
    A2_SNP = if_else(
      A2_Mutation_Type == "Substitution",
      str_sub(A2_DNA, -3),
      "E"
    )
  )
table(final_df$A2_SNP)
# A>C A>G A>T C>A C>G C>T   E G>A G>C G>T T>A T>C T>G 
# 15  46  20 172  79 286 565 392  89 105  31 144 680 
write.xlsx(final_df,"07.final_df.xlsx")
# #按7:3划分训练集和测试集-----------------------------
# set.seed(123)
# final_df <- read.xlsx("07.final_df.xlsx")
# final_df <- subset(final_df,Phenotype != "Asymptomatic") #2596个
# patient_ids <- unique(final_df$Patient_ID)
# train_ids <- sample(patient_ids, size = floor(length(patient_ids) * 0.7))
# train_df <- final_df %>% filter(Patient_ID %in% train_ids)
# test_df  <- final_df %>% filter(!Patient_ID %in% train_ids)
# write.xlsx(train_df,"08.train_df.xlsx")
# write.xlsx(test_df,"08.test_df.xlsx")
#生成count矩阵------------------------------------------
Population_mat <- acast(final_df,A1_DNA~Population,value.var = "Patient_Count",fun.aggregate = sum) %>% as.data.frame()
Population_mat2 <- acast(final_df,A1_Location~Population,value.var = "Patient_Count",fun.aggregate = sum) %>% as.data.frame()
Phenotype_mat <- acast(final_df,A1_DNA~Phenotype,value.var = "Patient_Count",fun.aggregate = sum) %>% as.data.frame()
Phenotype_mat2 <- acast(final_df,A1_Location~Phenotype,value.var = "Patient_Count",fun.aggregate = sum) %>% as.data.frame()
Population_Phenotype_mat <- acast(final_df,Population~Phenotype,value.var = "Patient_Count",fun.aggregate = sum) %>% as.data.frame()
write.xlsx(Population_mat,"09.Population_mat.xlsx",rowNames = TRUE)
write.xlsx(Phenotype_mat,"10.Phenotype_mat.xlsx",rowNames = TRUE)
write.xlsx(Population_mat2,"11.Population_mat_elements.xlsx",rowNames = TRUE)
write.xlsx(Phenotype_mat2,"12.Phenotype_mat_elements.xlsx",rowNames = TRUE)
write.xlsx(Population_Phenotype_mat,"13.Population_Phenotype_mat.xlsx",rowNames = TRUE)
#生成A1+A2信息
df_A1A2 <- final_df %>%
  rowwise() %>%
  mutate(A1A2_combined = ifelse(A1_DNA == A2_DNA, 
                                A1_DNA,
                                paste(sort(c(A1_DNA, A2_DNA)), collapse = "+"))) %>%
  ungroup()
A1A2Population_mat <- acast(df_A1A2,A1A2_combined~Population,value.var = "Patient_Count",fun.aggregate = sum) %>% as.data.frame()
A1A2Phenotype_mat <- acast(df_A1A2,A1A2_combined~Phenotype,value.var = "Patient_Count",fun.aggregate = sum) %>% as.data.frame()
write.xlsx(A1A2Population_mat,"14.A1A2Population_mat.xlsx",rowNames = TRUE)
write.xlsx(A1A2Phenotype_mat,"15.A1A2Phenotype_mat.xlsx",rowNames = TRUE)
# save.image("03_create_matrix_1103.RData")
