rm(list = ls()); gc()
setwd("/Users/yaoruntian/HuangLab/PDPP")
if (!dir.exists("06_elements")) {dir.create("06_elements")}
setwd("06_elements")
library(pheatmap)
library(openxlsx)
library(dplyr)
library(data.table)
library(tibble)
library(reshape2)
library(ggplotify)
library(ggplot2)
library(stringr)
phenotype_dat <- read.xlsx("../03_create_matrix/12.Phenotype_mat_elements.xlsx")
colnames(phenotype_dat) <- c("Elements","Adult","Asymptomatic","Childhood","Childhood or Adult","Classic infantile")
b = str_order(phenotype_dat$Elements, numeric = TRUE)
phenotype_dat <- phenotype_dat[b,]
population_dat <- read.xlsx("../03_create_matrix/11.Population_mat_elements.xlsx")
colnames(population_dat)[1] <- "Elements"
colnames(population_dat)[8] <- "Other"
b = str_order(population_dat$Elements, numeric = TRUE)
population_dat <- population_dat[b,]

#卡方检验
chisq_test_function <- function(dat1){
  df1 <- reshape2::melt(dat1)
  colnames(df1) <- c("Elements","Type","Count")
  tab <- xtabs(Count ~ Elements + Type, data = df1)
  chisq_result <- chisq.test(tab, simulate.p.value = TRUE, B = 10000)
  # 获取标准化残差 (近似服从标准正态分布)
  std_residuals <- chisq_result$stdres %>% as.data.frame()
  # 将标准化残差转换为p值 (双侧检验)
  std_residuals$p.value <- 2 * pnorm(-abs(std_residuals$Freq))
  return(std_residuals)
}
population_res <- chisq_test_function(population_dat)
phenotype_res <- chisq_test_function(phenotype_dat)
#绘制热图数据--------------------------------------------
#population--------------------------------------
pmt <- acast(population_res, Elements ~ Type, value.var = "p.value")
# 创建p值矩阵
if (!is.null(pmt)){
  sssmt <- pmt < 0.001
  ssmt <- pmt < 0.01 & pmt >= 0.001
  smt <- pmt >= 0.01 & pmt <0.05
  pmt[sssmt] <- "***"
  pmt[ssmt] <-'**'
  pmt[smt] <- '*'
  pmt[!ssmt&!smt&!sssmt]<- ''
} else {
  pmt <- F
}
rownames(population_dat) <- population_dat[,1]
pmt <- pmt[b,]
cmt <- as.matrix(population_dat[,-1])
cmt <- log10(cmt+1)
#phenotype--------------------------------------
pmt2 <- acast(phenotype_res, Elements ~ Type, value.var = "p.value")
if (!is.null(pmt2)){
  sssmt <- pmt2 < 0.001
  ssmt <- pmt2 < 0.01 & pmt2 >= 0.001
  smt <- pmt2 >= 0.01 & pmt2 <0.05
  pmt2[sssmt] <- "***"
  pmt2[ssmt] <-'**'
  pmt2[smt] <- '*'
  pmt2[!ssmt&!smt&!sssmt]<- ''
} else {
  pmt2 <- F
}
rownames(phenotype_dat) <- phenotype_dat[,1]
pmt2 <- pmt2[b,]
cmt2 <- as.matrix(phenotype_dat[,-1])
cmt2 <- log10(cmt2+1)*(-1)

plot_data <- cbind(cmt,cmt2)
plot_pdata <- cbind(pmt,pmt2)
#注释条
annotation_col = data.frame(Elements = c(rep("exon",20),rep("intron",20)))
rownames(annotation_col) <- rownames(plot_data)
annotation_row = data.frame(Type = c(rep("Population",7),rep("Phenotype",5)))
rownames(annotation_row) <- colnames(plot_data)
elementcolor <- c("#89bed1","#e896ab")
names(elementcolor) <- c("intron","exon")
typecolor <- c("#b9e5ee","#ffe2de")
names(typecolor) <- c("Phenotype","Population")
anncolors <- list(Type = typecolor,Elements = elementcolor)
##热图
p1 <- pheatmap(t(plot_data),scale = "none",cluster_row = F, cluster_col = F, #border=TRUE,
               breaks=seq(-2, 2, length.out = 10),
               display_numbers = t(plot_pdata),
               fontsize_number = 14, number_color = "black",fontsize = 13,
               cellwidth = 20, cellheight =20,
               color=colorRampPalette(c("#61AACF", "#98CADD", "#EAEFF6","#F9EFEF", "#E9C6C6", "#DA9599"))(10),
               angle_col = 45,annotation_row = annotation_row,annotation_col = annotation_col,annotation_colors = anncolors,
               gaps_row = 7,gaps_col = 20)
as.ggplot(p1)
ggsave("01.heatmap.pdf",width = 15, height = 8.5)
ggsave("01.heatmap.png",dpi = 300,width = 15, height = 8.5)
