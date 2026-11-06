rm(list = ls()); gc()
setwd("/Users/yaoruntian/HuangLab/PDPP")
if (!dir.exists("08_severity")) {dir.create("08_severity")}
setwd("08_severity")
library(openxlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(RColorBrewer)
library(reshape2)
#01.Mutation_Type和Severity分布-----------------
df1 <- read.xlsx("../03_create_matrix/07.final_df.xlsx")
Mutation_Type_Severity_mat <- acast(df1,Mutation_Type~Severity,value.var = "Patient_Count",fun.aggregate = sum) %>% as.data.frame()
Mutation_Type_Severity_mat$Mutation_Type <- rownames(Mutation_Type_Severity_mat)
plot_data <- reshape2::melt(Mutation_Type_Severity_mat)
colnames(plot_data) <- c("Mutation_Type","Severity","Count")
plot_data$Severity <- factor(plot_data$Severity,levels = c("Potentially mild","Less severe","Very severe"))
#卡方检验
# 将数据转换为列联表格式
contingency_table <- xtabs(Count ~ Mutation_Type + Severity, data = plot_data)
print(contingency_table)
# 执行卡方检验
chi_test <- chisq.test(contingency_table)
print(chi_test)
p_value <- ifelse(chi_test$p.value < 0.001, "< 0.001", 
                  format(round(chi_test$p.value, 4), nsmall = 4))

p1 <- ggplot(plot_data, aes(x = Mutation_Type, y = Count, fill = Severity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste0("Mutation Type Distribution by Severity\n",
                      "Chi-square test p-value ", p_value),
       x = "Mutation Type",
       y = "Count",
       fill = "Severity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Less severe" = "#FFEE58", 
                               "Potentially mild" = "#66BB6A", 
                               "Very severe" = "#EF5350")) +
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3)
p1
ggsave("02.MutationType_Severity.pdf",p1,w=11,h=8.5)
ggsave("02.MutationType_Severity.png",p1,w=11,h=8.5,dpi = 600)
#02.SNP与Severity分析--------------------------------------
df1 <- read.xlsx("../03_create_matrix/07.final_df.xlsx")
df1 <- subset(df1,Mutation_Type == "Substitution")
df1$A1_DNA <- gsub(" |\u00A0","",df1$A1_DNA)
df1$SNP <- substr(df1$A1_DNA, nchar(df1$A1_DNA)-2, nchar(df1$A1_DNA))
table(df1$SNP)
SNP_Severity_Mat <- acast(df1,SNP~Severity,value.var = "Patient_Count",fun.aggregate = sum) %>% as.data.frame()
SNP_Severity_Mat$SNP <- rownames(SNP_Severity_Mat)
plot_data <- reshape2::melt(SNP_Severity_Mat)
colnames(plot_data) <- c("SNP","Severity","Count")
plot_data$Severity <- factor(plot_data$Severity,levels = c("Potentially mild","Less severe","Very severe"))
#卡方检验
# 将数据转换为列联表格式
contingency_table <- xtabs(Count ~ SNP + Severity, data = plot_data)
print(contingency_table)
# 执行卡方检验
chi_test <- chisq.test(contingency_table)
print(chi_test)
p_value <- ifelse(chi_test$p.value < 0.001, "< 0.001", 
                  format(round(chi_test$p.value, 4), nsmall = 4))

p2 <- ggplot(plot_data, aes(x = SNP, y = Count, fill = Severity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste0("SNP Type Distribution by Severity\n",
                      "Chi-square test p-value ", p_value),
       x = "SNP Type",
       y = "Count",
       fill = "Severity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Less severe" = "#FFEE58", 
                               "Potentially mild" = "#66BB6A", 
                               "Very severe" = "#EF5350")) +
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3)
p2
ggsave("03.SNP_Severity.pdf",p2,w=11,h=8.5)
ggsave("03.SNP_Severity.png",p2,w=11,h=8.5,dpi = 600)

mypalette <- colorRampPalette(brewer.pal(9,"Paired"))
p3 <- ggplot(plot_data, aes(x = Severity, y = Count, fill = SNP)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste0("Severity Distribution by SNP Type\n",
                      "Chi-square test p-value ", p_value),
       x = "Severity",
       y = "Count",
       fill = "SNP Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = mypalette(22)) +
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3)
p3
ggsave("04.Severity_SNP.pdf",p3,w=11,h=8.5)
ggsave("04.Severity_SNP.png",p3,w=11,h=8.5,dpi = 600)
write.xlsx(df1,"05.SNP_dat.xlsx")
#03.Mutation_Type和Phenotype-----------------------
df1 <- read.xlsx("../03_create_matrix/07.final_df.xlsx")
Mutation_Type_Phenotype_mat <- acast(df1,Mutation_Type~Phenotype,value.var = "Patient_Count",fun.aggregate = sum) %>% as.data.frame()
Mutation_Type_Phenotype_mat$Mutation_Type <- rownames(Mutation_Type_Phenotype_mat)
plot_data <- reshape2::melt(Mutation_Type_Phenotype_mat)
colnames(plot_data) <- c("Mutation_Type","Phenotype","Count")
plot_data$Phenotype <- factor(plot_data$Phenotype)
#卡方检验
# 将数据转换为列联表格式
contingency_table <- xtabs(Count ~ Mutation_Type + Phenotype, data = plot_data)
print(contingency_table)
# 执行卡方检验
chi_test <- chisq.test(contingency_table)
print(chi_test)
p_value <- ifelse(chi_test$p.value < 0.001, "< 0.001", 
                  format(round(chi_test$p.value, 4), nsmall = 4))

mypalette <- colorRampPalette(brewer.pal(6,"Paired"))
p4 <- ggplot(plot_data, aes(x = Mutation_Type, y = Count, fill = Phenotype)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste0("Mutation Type Distribution by Phenotype\n",
                      "Chi-square test p-value ", p_value),
       x = "Mutation Type",
       y = "Count",
       fill = "Phenotype") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#F29E94","#F8CBC9","#C1D09D","#A4C2B7","#445C46"))+
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3)
p4
ggsave("05.Phenotype_Mutation_Type.pdf",p4,w=11,h=8.5)
ggsave("05.Phenotype_Mutation_Type.png",p4,w=11,h=8.5,dpi = 600)
#SNP+E与Phenotype关联-------------------------------------------
df1 <- read.xlsx("../03_create_matrix/07.final_df.xlsx")
df2 <- subset(df1,Mutation_Type == "Substitution")
df2$A1_DNA <- gsub(" |\u00A0","",df2$A1_DNA)

df2$SNP <- substr(df2$A1_DNA, nchar(df2$A1_DNA)-2, nchar(df2$A1_DNA))
table(df2$SNP)
df3 <- subset(df1,Mutation_Type != "Substitution")
df3$SNP <- "E"
df1 <- rbind(df2,df3)
#添加A2_SNP
# df2 <- subset(df1,A2_Mutation_Type == "Substitution")
# df2$A2_DNA <- gsub("]","",df2$A2_DNA)
# df2$SNP <- substr(df2$A2_DNA, nchar(df2$A2_DNA)-2, nchar(df2$A2_DNA))
# table(df2$SNP)
# df3 <- subset(df1,Mutation_Type != "Substitution")
# df3$SNP <- "E"
# df1 <- rbind(df2,df3)

SNP_Phenotype_mat <- acast(df1,SNP~Phenotype,value.var = "Patient_Count",fun.aggregate = sum) %>% as.data.frame()
SNP_Phenotype_mat$SNP <- rownames(SNP_Phenotype_mat)
plot_data <- reshape2::melt(SNP_Phenotype_mat)
colnames(plot_data) <- c("SNP","Phenotype","Count")
plot_data$SNP <- factor(plot_data$SNP,levels =c("A>C", "A>G", "A>T", "C>A", "C>G", "C>T", "G>A", "G>C", 
                                                      "G>T", "T>A", "T>C", "T>G", "E"))
mypalette <- colorRampPalette(brewer.pal(13,"Paired"))
p5 <- ggplot(plot_data, aes(x = Phenotype, y = Count, fill = SNP)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste0("Phenotype Distribution by SNP Type\n",
                      "Chi-square test p-value ", p_value),
       x = "Phenotype",
       y = "Count",
       fill = "SNP Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = mypalette(22)) +
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3)
p5
ggsave("06.Phenotype_SNP.pdf",p5,w=11,h=8.5)
ggsave("06.Phenotype_SNP.png",p5,w=11,h=8.5,dpi = 600)
write.xlsx(df1,"05.SNP_dat.xlsx")
