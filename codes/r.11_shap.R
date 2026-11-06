rm(list = ls()); gc()
setwd("/Users/yaoruntian/HuangLab/PDPP")
if (!dir.exists("11_shap")) {dir.create("11_shap")}
setwd("11_shap")
library(fastshap)
library(shapviz)
library(ggplot2)
library(xgboost)
library(randomForest)
library(magrittr)
library(tidyverse)
library(pdp)
library(stringr)
library(openxlsx)
best_model <- readRDS("../10b_MachineLearing/best_model_all.rds")
train_data <- readRDS("../10b_MachineLearing/all_data.Rds")
train_target <- readRDS("../10b_MachineLearing/all_target.Rds")
final_rf <- best_model$finalModel
pfun <- function(object, newdata) {
  as.numeric(predict(object, newdata = as.matrix(newdata)))
}
set.seed(123)
shap_values <- fastshap::explain(
  final_rf,
  X = train_data,
  pred_wrapper = pfun,
  nsim = 100
)
# 创建 shapviz 对象
shap_obj <- shapviz(shap_values, X = train_data)
# 1. 特征重要性图（基于平均绝对SHAP值）
p1 <- sv_importance(shap_obj,fill =c("#f57c6e","#f2b56f","#fae69e","#84c3b7")) +
  labs(title = "All Features by Importance (All Data)",
       subtitle = "Based on SHAP Analysis with XGBoost Model") +
  theme_minimal()+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5,size = 18,face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,size = 15))
p1
# 2. 单个观测的SHAP力力图
# 选择第一个测试样本进行解释
set.seed(123)
sample_idx <- sample(1:nrow(train_data), 1)
shap_single <- fastshap::explain(
  final_rf,
  X = train_data[sample_idx, ],
  pred_wrapper = pfun,
  nsim = 100
)
shap_single_obj <- shapviz(shap_single, X = train_data[sample_idx, ])
sv_force(shap_single_obj) +
  ggtitle(paste("SHAP Force Plot for Observation", sample_idx))
# 3. 依赖图
# 依赖图
top_features <- head(names(sort(colMeans(abs(shap_obj$S)), decreasing = TRUE)),4)
features_df <- read.xlsx("../09_lasso/01.all_data.xlsx")
SNP_text <-  c("A>C", "A>G", "A>T", "C>A", "C>G", "C>T", "G>A", "G>C", 
               "G>T", "T>A", "T>C", "T>G","E")
Location_text <- str_sort(levels(as.factor(features_df$A1_Location)),numeric = TRUE)
p2 <- sv_dependence(shap_obj, v = top_features[1], color_var = top_features[1]) +
  theme_bw(base_size = 14)+
  geom_smooth(color = "gray40", se = FALSE)+
  scale_color_gradientn(colours = c("#f57c6e","#f2b56f","#fae69e","#84c3b7","#71b7ed","#b8aeeb","#f2afda"))+
  scale_x_continuous(labels = Location_text,breaks = c(1:40))+
  theme(axis.text.x = element_text(angle = 90,hjust = 0.5))
p3 <- sv_dependence(shap_obj, v = top_features[2], color_var = top_features[2]) +
  theme_bw(base_size = 14)+
  geom_smooth(color = "gray40", se = FALSE)+
  scale_color_gradientn(colours = c("#f57c6e","#f2b56f","#fae69e","#84c3b7","#71b7ed","#b8aeeb","#f2afda"))+
  scale_x_continuous(labels = Location_text,breaks = c(1:40))+
  theme(axis.text.x = element_text(angle = 90,hjust = 0.5))
p4 <- sv_dependence(shap_obj, v = top_features[3], color_var = top_features[3]) +
  theme_bw(base_size = 14)+
  geom_smooth(color = "gray40", se = FALSE)+
  scale_color_gradientn(colours = c("#f57c6e","#f2b56f","#fae69e","#84c3b7","#71b7ed","#b8aeeb","#f2afda"))+
  scale_x_continuous(labels = SNP_text,breaks = c(1:13))+
  theme(axis.text.x = element_text(angle = 30,hjust = 1))
p5 <- sv_dependence(shap_obj, v = top_features[4], color_var = top_features[4]) +
  theme_bw(base_size = 14)+
  geom_smooth(color = "gray40", se = FALSE)+
  scale_color_gradientn(colours = c("#f57c6e","#f2b56f","#fae69e","#84c3b7","#71b7ed","#b8aeeb","#f2afda"))+
  scale_x_continuous(labels = SNP_text,breaks = c(1:13))+
  theme(axis.text.x = element_text(angle = 30,hjust = 1))
p_all_dependence <- p2+p3+p5+p4
p_all_dependence
# 4. 所有特征的蜂群图
p6 <- sv_importance(shap_obj, kind = "beeswarm") +
  ggtitle("XGBoost-based SHAP Value Distribution for Phenotype Prediction Using All Data")+
  theme_minimal()+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5,size = 18,face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,size = 15),
        legend.position = "none")+
  scale_color_gradientn(colours = c("#f57c6e","#f2b56f","#fae69e","#84c3b7","#71b7ed","#b8aeeb","#f2afda"))
p6
#
dat1 <- shap_obj$S %>%as.data.frame()
dat1 <- dat1 %>%
  mutate(Total_score = A1_SNP + A1_Location + A2_SNP + A2_Location) %>%
  arrange(desc(Total_score)) %>%
  mutate(Sample = factor(rownames(dat1),levels = rownames(dat1)))
dat1$Category <- ifelse(dat1$Total_score > 0, "IOPD", "LOPD")
data_long <- dat1 %>%
  select(Sample, A1_SNP, A1_Location, A2_SNP, A2_Location) %>%
  reshape2::melt(id.vars = "Sample", variable.name = "Variable", value.name = "Value")

# 创建单独的注释条图层
annotation_plot <- ggplot(dat1, aes(x = Sample, y = 1, fill = Category)) +
  geom_tile() +
  scale_fill_manual(values = c("LOPD" = "#f57c6e", "IOPD" = "#71b7ed")) +
  theme_void(base_size = 14) +
  theme(legend.position = "right")

# 主图（去掉注释条）
p8_main <- ggplot(data_long, aes(x = Sample, y = Value, fill = Variable)) +
  geom_col(position = "stack") +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.2) +
  scale_fill_manual(
    values = c(
      "A1_SNP" = "#84c3b7", 
      "A1_Location" = "#f57c6e",
      "A2_SNP" = "#fae69e", 
      "A2_Location" = "#f2b56f"
    ),
    labels = c("A1 SNP", "A1 Location", "A2 SNP", "A2 Location")
  ) +
  labs(
    title = "XGBoost-Based on SHAP Force Plot for Phenotype Prediction Using All Data",
    x = "Sample",
    y = "SHAP value",
    fill = "Features"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "right",
    axis.title = element_text(size = 15),
    title = element_text(size = 15),
    axis.text.x = element_blank(),  # 隐藏x轴标签
    axis.ticks.x = element_blank()  # 隐藏x轴刻度
  ) +
  scale_x_discrete(breaks = c(0, 500, 1000, 1500))

# 使用patchwork组合图形
library(patchwork)
combined_plot <- p8_main / annotation_plot + 
  plot_layout(heights = c(10, 1),guides = "collect")&theme(legend.position = "top")
combined_plot
ggsave("barplot.png",w =12,h =6)
ggsave("barplot.pdf",w =12,h =6)
#
dat2 <- shap_obj$S %>%as.data.frame()
dat2$sample <- rownames(dat2)
dat2 <- dat2 %>%
  mutate(Total_score = A1_SNP + A1_Location + A2_SNP + A2_Location) %>%
  arrange(desc(Total_score))
head(dat2)
tail(dat2)
IOPD_FORCE <- sv_force(shap_obj, row_id = 1465,fill = c("#71b7ed","#f57c6e"))+ggtitle("Phenotype = IOPD")
LOPD_FORCE <- sv_force(shap_obj, row_id = 1916,fill = c("#71b7ed","#f57c6e"))+ggtitle("Phenotype = LOPD")
library(patchwork)
p7 <- LOPD_FORCE/IOPD_FORCE
p7
ggsave("force.png",w =10,h=8)
ggsave("force.pdf",w =10,h=8)

p1/p6/p_all_dependence+plot_layout(guides = "collect")&theme(legend.position = "none")
ggsave("res.png",w =16,h =20)
ggsave("res.pdf",w =16,h =20)
