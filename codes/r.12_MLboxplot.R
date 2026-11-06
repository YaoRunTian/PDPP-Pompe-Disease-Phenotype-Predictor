rm(list = ls()); gc()
setwd("/Users/yaoruntian/HuangLab/PDPP")
if (!dir.exists("12_MLboxplot")) {dir.create("12_MLboxplot")}
setwd("12_MLboxplot")
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(gghalves)
model_list <- readRDS("../10b_MachineLearing/model_list.Rds")
results <- resamples(list(
  RF = model_list$RF,
  XGBoost = model_list$XGboost,
  SVM = model_list$SVM,
  GBM = model_list$GBM,
  KNN = model_list$KNN,
  NB = model_list$NB,
  LR = model_list$LR
))
summary(results)
df1 <- results$values
# 提取需要的指标列
df1 <- df1 %>%
  select(Resample, 
         # RF指标
         `RF~Accuracy`, `RF~Precision`, `RF~Recall`, `RF~F1`,
         # XGBoost指标
         `XGBoost~Accuracy`, `XGBoost~Precision`, `XGBoost~Recall`, `XGBoost~F1`,
         # SVM指标
         `SVM~Accuracy`, `SVM~Precision`, `SVM~Recall`, `SVM~F1`,
         # GBM指标
         `GBM~Accuracy`, `GBM~Precision`, `GBM~Recall`, `GBM~F1`,
         # KNN指标
         `KNN~Accuracy`, `KNN~Precision`, `KNN~Recall`, `KNN~F1`,
         # NB指标
         `NB~Accuracy`, `NB~Precision`, `NB~Recall`, `NB~F1`,
         # LR指标
         `LR~Accuracy`, `LR~Precision`, `LR~Recall`, `LR~F1`)
long_df <- df1 %>%
  pivot_longer(cols = -Resample, 
               names_to = "Metric_Model", 
               values_to = "Value") %>%
  separate(Metric_Model, into = c("Model", "Metric"), sep = "~")

models_order <- c("XGBoost","RF",  "SVM", "GBM", "KNN", "NB", "LR")
long_df$Model <- factor(long_df$Model, levels = models_order)
# 设置指标顺序
metrics_order <- c("Accuracy", "Precision", "Recall", "F1")
long_df$Metric <- factor(long_df$Metric, levels = metrics_order)

rect_data <- data.frame(Metric = metrics_order, 
                        xmin = 0.5, 
                        xmax = 2.5, 
                        ymin = -Inf, 
                        ymax = Inf)
# 绘制箱线图
ggplot(long_df) +
  geom_rect(data = rect_data, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#f57c6e", alpha = 0.2) +
  geom_half_violin(aes(x = Model, y = Value, fill = Model),side="r",width=0.9,color=NA)+
  geom_half_boxplot(aes(x = Model, y = Value, fill = Model),width=0.4,size=1.2,side="l",outlier.color=NA)+
  geom_jitter(aes(x = Model, y = Value, fill = Model),shape=21,size=1,position = position_jitter(width = 0.1))+
  facet_wrap(~ Metric, ncol = 2, scales = "free") +
  theme_bw() +
  theme(panel.grid = element_line(colour = "white"),
        axis.title= element_blank(),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15,angle = 30,hjust = 1),
        title = element_text(size = 20),
        legend.position = "none",
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 16, face = "bold"),
        strip.background = element_blank()) +
  scale_fill_manual(values = c("#f57c6e","#f2b56f","#fae69e","#84c3b7","#71b7ed","#b8aeeb","#f2afda"))
ggsave('01.boxplot.pdf',w=12,h=8)
ggsave('01.boxplot.png',w=12,h=8)
