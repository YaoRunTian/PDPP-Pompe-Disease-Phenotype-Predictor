rm(list = ls()); gc()
setwd("/Users/yaoruntian/HuangLab/PDPP")
if (!dir.exists("10_MachineLearing")) {dir.create("10_MachineLearing")}
setwd("10_MachineLearing")
# 加载必要的包
library(caret)
library(randomForest)
library(xgboost)
library(dplyr)
library(ggplot2)
library(MLmetrics)
library(kernlab)
library(gbm)
library(pROC)
library(mlr3)
library(mlr3measures)
library(openxlsx)
library(stringr)
library(ggDCA)
set.seed(123)
# 读取完整数据，以及已经划分好的训练集和测试集
df1 <- read.xlsx("../09_lasso/01.all_data.xlsx")
train_df <- read.xlsx("../09_lasso/01.train_df.xlsx")
test_df  <- read.xlsx("../09_lasso/01.test_df.xlsx")  # 按实际文件名调整
# 统一 Patient_ID 的类型
all_ids   <- as.character(df1$Patient_ID)
train_ids <- unique(as.character(train_df$Patient_ID))
test_ids  <- unique(as.character(test_df$Patient_ID))
# 检查 ID
if (anyNA(all_ids) || anyNA(train_ids) || anyNA(test_ids)) {
  stop("Patient_ID 中存在缺失值，请先检查。")
}
if (length(intersect(train_ids, test_ids)) > 0) {
  stop("训练集和测试集存在相同的 Patient_ID。")
}
if (!setequal(unique(all_ids), union(train_ids, test_ids))) {
  stop("完整数据与训练集、测试集的 Patient_ID 不完全一致。")
}
# 根据 Patient_ID 生成行索引
train_index <- all_ids %in% train_ids
test_index  <- all_ids %in% test_ids
# SNP 的分类顺序
snp_levels <- c(
  "A>C", "A>G", "A>T", "C>A", "C>G", "C>T",
  "G>A", "G>C", "G>T", "T>A", "T>C", "T>G", "E"
)
# 准备特征和结局
df1_ml <- df1 %>%
  select(A1_SNP, A1_Location, A2_SNP, A2_Location, Phenotype2) %>%
  mutate(
    across(c(A1_SNP, A2_SNP), ~ factor(.x, levels = snp_levels)),
    across(
      c(A1_Location, A2_Location),
      ~ factor(.x, levels = str_sort(unique(.x[!is.na(.x)]), numeric = TRUE))
    ),
    Phenotype2 = factor(Phenotype2)
  )
#将特征的因子水平转为数字
features <- df1_ml %>%
  select(-Phenotype2) %>%
  mutate(across(everything(), as.numeric))
target <- df1_ml$Phenotype2
# 使用同一组行索引拆分特征和结局
train_data   <- features[train_index, , drop = FALSE]
test_data    <- features[test_index, , drop = FALSE]
train_target <- target[train_index]
test_target  <- target[test_index]
# 保存
saveRDS(features, "all_data.Rds")
saveRDS(target, "all_target.Rds")
saveRDS(train_data, "train_data.Rds")
saveRDS(train_target, "train_target.Rds")
saveRDS(test_data, "test_data.Rds")
saveRDS(test_target, "test_target.Rds")
# 检查数量
cat("完整数据行数：", nrow(features), "\n")
cat("训练集行数：", nrow(train_data), "\n")
cat("测试集行数：", nrow(test_data), "\n")
#检查数据
nzv <- nearZeroVar(train_data)
if (length(nzv) > 0) {
  train_data <- train_data[, -nzv]
}
#02.定义训练控制参数（使用重复交叉验证）
ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,  #交叉折叠数
  repeats = 5,
  classProbs = TRUE,
  summaryFunction = multiClassSummary,
  savePredictions = "final"
)
#训练多个模型进行比较----------------------------------
#RandomForest
rf_model <- train(
  x = train_data,
  y = train_target,
  method = "rf",
  trControl = ctrl,
  # tuneLength = 5,
  importance = TRUE,
  metric = "Accuracy"
)

#XGBoost
xgb_grid <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(3, 4, 5),
  eta = c(0.01, 0.1, 0.3),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)
xgb_model <- train(
  x = train_data,
  y = train_target,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = xgb_grid,  # 使用自定义网格
  verbosity = 0,
  metric = "Accuracy"
)

#SVM
svm_model <- train(
  x = train_data,
  y = train_target,
  method = "svmRadial",
  trControl = ctrl,
  tuneLength = 5,
  metric = "Accuracy",
  preProcess = c("center", "scale")
)

#GBM
gbm_grid <- expand.grid(
  n.trees = c(50, 100, 150),
  interaction.depth = c(3, 5),
  shrinkage = c(0.01, 0.1),
  n.minobsinnode = 10
)
gbm_model <- train(
  x = train_data,
  y = train_target,
  method = "gbm",
  trControl = ctrl,
  tuneGrid = gbm_grid,
  metric = "Accuracy",
  verbose = FALSE,
  bag.fraction = 0.8
)

#K最近邻模型 (KNN)
knn_model <- train(
  x = train_data,
  y = train_target,
  method = "knn",
  trControl = ctrl,
  metric = "Accuracy",
  preProcess = c("center", "scale"),
  tuneLength = 10
)

#朴素贝叶斯模型 (NB)
nb_grid <- expand.grid(
  fL = c(0, 0.5, 1),       # 拉普拉斯平滑
  usekernel = c(TRUE, FALSE), # 是否使用核密度估计
  adjust = c(0.75, 1, 1.25)  # 带宽调整
)
nb_model <- train(
  x = train_data,
  y = train_target,
  method = "nb",
  trControl = ctrl,
  metric = "Accuracy",
  tuneGrid = nb_grid  # 使用自定义网格
)
#逻辑回归模型 (LR)
lr_model <- train(
  x = train_data,
  y = train_target,
  method = "glmnet",
  family = "binomial",
  trControl = ctrl,
  metric = "Accuracy",
  tuneLength = 10
)

# 收集所有模型结果
results <- resamples(list(
  RF = rf_model,
  XGBoost = xgb_model,
  SVM = svm_model,
  GBM = gbm_model,
  KNN = knn_model,
  NB = nb_model,
  LR = lr_model
))
summary(results)

# 计算并比较各模型的详细指标
model_list <- list(rf_model,xgb_model,svm_model,gbm_model,knn_model,nb_model,lr_model)
names(model_list) <- c("RF","XGboost","SVM","GBM","KNN","NB","LR")
# 初始化结果数据框
metrics_df <- data.frame(
  Model = character(),
  AUC = numeric(),
  Accuracy = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  F1 = numeric(),
  stringsAsFactors = FALSE
)
cm_list <- list()
# 为每个模型计算指标
df <- train_data
for (model_name in names(model_list)) {
  model <- model_list[[model_name]]
  pred <- predict(model, newdata = df)
  probs <- predict(model, newdata = df, type = "prob")[,1]
  # 计算混淆矩阵
  cm <- confusionMatrix(pred, train_target)
  cm_list[[model_name]] <- cm$table
  # 计算各项指标
  auc_val <- roc(train_target, probs)$auc
  acc <- cm$overall["Accuracy"]
  sens <- cm$byClass["Sensitivity"]
  spec <- cm$byClass["Specificity"]
  f1 <- cm$byClass["F1"]
  # 添加到结果数据框
  metrics_df <- rbind(metrics_df, data.frame(
    Model = model_name,
    AUC = auc_val,
    Accuracy = acc,
    Sensitivity = sens,
    Specificity = spec,
    F1 = f1
  ))
}
# 打印性能指标
print(metrics_df)
write.xlsx(metrics_df,"01.machine_res_train.xlsx")
saveRDS(model_list,"model_list.Rds")
# 找出最优模型（基于AUC）
best_model_index <- which.max(metrics_df$AUC)
best_model_name <- metrics_df$Model[best_model_index]
best_model <- model_list[[best_model_index]]
cat("\n最优模型:", best_model_name, "\n")
cat("AUC:", metrics_df$AUC[best_model_index], "\n\n")
#可视化混淆矩阵---------------------------------------
library(ggplot2)
library(reshape2)
for (model_name in names(model_list)[c(2,1,3:7)]){{
  confusion_matrix <- cm_list[[model_name]]
  confusion_melted <- melt(confusion_matrix)
  colnames(confusion_melted) <- c("Prediction", "Reference", "Count")
  # 计算百分比
  confusion_percent <- prop.table(confusion_matrix, margin = 2) * 100
  confusion_percent_melted <- melt(confusion_percent)
  colnames(confusion_percent_melted) <- c("Prediction", "Reference", "Percentage")
  # 合并计数和百分比
  confusion_combined <- merge(confusion_melted, confusion_percent_melted)
  confusion_combined$Label <- paste0(confusion_combined$Count, "\n(", 
                                     round(confusion_combined$Percentage, 1), "%)")
  # 创建带百分比的热图
  p_cm <- ggplot(confusion_combined, aes(x = Prediction, y = Reference, fill = Count)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Label), color = "black", size = 8) +
    scale_fill_gradient(low = "white", high = "brown") +
    # theme_bw() +
    labs(title = paste("Confusion Matrix of",model_name,"in Training Set"),
         x = "Predicted label",
         y = "True label") +
    theme(panel.grid.major = element_blank(),  # 取消主要网格线
          panel.grid.minor = element_blank(),  # 取消次要网格线
          panel.background = element_blank(),   # 取消背景框
          axis.line = element_blank(),           # 去除坐标轴线
          axis.ticks = element_blank(),           # 去除刻度线
          axis.text = element_text(size=20),
          axis.title = element_text(size =20),
          axis.title.y = element_text(angle = 90),
          axis.text.y = element_text(angle = 90,hjust = 1),
          plot.title = element_text(size= 20,hjust = 0.5,face = "bold"),
          legend.position = "none")
  png_filename <- paste0(model_name,"_cm_train.png")
  pdf_filename <- paste0(model_name,"_cm_train.pdf")
  ggsave(png_filename,p_cm,w = 8,h = 8)
  ggsave(pdf_filename,p_cm,w = 8,h = 8)
}}
#ROC------------------------------------
library(pROC)
library(ggplot2)
roc_curves <- NULL
for (model_name in names(model_list)[c(2,1,3:7)]){
  model <- model_list[[model_name]]
  probs <- predict(model, newdata = df, type = "prob")[,1]
  roc_curve <- roc(train_target, probs)
  roc_curves[[model_name]] <- roc_curve 
}
roc_curves
# 创建数据框用于ggplot
roc_data <- data.frame()
for (model_name in names(roc_curves)) {
  temp_df <- data.frame(
    Model = model_name,
    FPR = 1 - roc_curves[[model_name]]$specificities,
    TPR = roc_curves[[model_name]]$sensitivities
  )
  roc_data <- rbind(roc_data, temp_df)
}
roc_data$Model <- factor(roc_data$Model,levels = c("XGboost","RF","SVM","GBM","KNN","NB","LR"))
# 计算AUC值并保留3位小数
auc_values <- sapply(roc_curves, function(x) sprintf("%0.3f",x$auc))
legend_labels <- paste0(names(auc_values), " (AUC = ", auc_values, ")")
# 使用ggplot2绘制ROC曲线
p1 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
  labs(title = "ROC Curve Comparison(Training Set)",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)",
       color = "Models") +
  # scale_color_manual(values = c('#9dd0c7','#9180ac','#e58579','#8ab1d2','#DFAE36','#C87D98','#f5a65b'),
  #                    labels = legend_labels) +
  scale_color_manual(values = c("#f57c6e","#f2b56f","#fae69e","#84c3b7","#71b7ed","#b8aeeb","#f2afda"),
                     labels = legend_labels)+
  theme_bw(base_size = 14) +
  theme(
    axis.text = element_text(size=15),
    axis.title = element_text(size =15),
    legend.position = c(0.7, 0.2),
    legend.background = element_rect(fill = "white", color = "gray80"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  coord_equal()
p1
ggsave('02.train_ROC.pdf',p1,w=7,h=7)
ggsave('02.train_ROC.png',p1,w=7,h=7)
#DCA曲线--------------------------------
library(rmda)
library(ggDCA)
prob_df <- data.frame(obs = train_target)
for (model_name in names(model_list)) {
  prob_df[[model_name]] <- roc_curves[[model_name]]$predictor
}
prob_df$obs <- ifelse(prob_df$obs == "LOPD", 0, 1)
# 添加每个模型的 DCA 曲线
dca_df <- NULL
for(model in names(model_list)){
  dca_data <- decision_curve(
    as.formula(paste("obs ~", model)),
    data = prob_df
  )
  dca_df <- rbind(dca_df,dca_data$derived.data)
}
summary(dca_df)
dca_df <- dca_df %>%
  mutate(model = case_when(
    model == "obs ~ RF" ~ "RF",
    model == "obs ~ XGboost" ~ "XGboost",
    model == "obs ~ SVM" ~ "SVM",
    model == "obs ~ GBM" ~ "GBM",
    model == "obs ~ KNN" ~ "KNN",
    model == "obs ~ NB" ~ "NB",
    model == "obs ~ LR" ~ "LR",
    TRUE ~ as.character(model)
  ))
dca_df$model <- factor(dca_df$model,levels = c("XGboost","RF","All","None","SVM","GBM","KNN","NB","LR"))
p2 <- ggplot(dca_df, aes(x = thresholds,y = NB,color = model)) + 
  geom_line(linetype = "solid",size = 1.2)+
  ylim(0,1)+
  labs(
    title = "Decision Curve Analysis(Training Set)",
    x = "Threshold Probability",
    y = "Net Benefit",
    color = "Models"
  )+
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )+
  scale_color_manual(values = c("All" = "blue",
                                "None" = "red",
                                "RF" = '#f2b56f',
                                "XGboost" = '#f57c6e',
                                "SVM"= '#fae69e',
                                "GBM"='#84c3b7',
                                "KNN" = "#71b7ed",
                                "NB" = "#b8aeeb",
                                "LR" = "#f2afda"
                                ))+
  coord_equal()
p2
ggsave('03.train_DCA.pdf',p2,w=7,h=7)
ggsave('03.train_DCA.png',p2,w=7,h=7)
#打印最佳模型
best_model <- model_list[["RF"]]
var_importance <- varImp(best_model)$importance
cat("关键变量重要性:\n")
print(var_importance)
write.xlsx(var_importance,"06.importance_var.xlsx",rowNames = TRUE)
# 保存最优模型
saveRDS(best_model, "best_model_train.rds")

###测试集---------------------------------------------
# 计算并比较各模型的详细指标
model_list <- list(rf_model,xgb_model,svm_model,gbm_model,knn_model,nb_model,lr_model)
names(model_list) <- c("RF","XGboost","SVM","GBM","KNN","NB","LR")
# 初始化结果数据框
metrics_df <- data.frame(
  Model = character(),
  AUC = numeric(),
  Accuracy = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  F1 = numeric(),
  stringsAsFactors = FALSE
)
cm_list <- list()
# 为每个模型计算指标
df <- test_data
for (model_name in names(model_list)) {
  model <- model_list[[model_name]]
  pred <- predict(model, newdata = df)
  probs <- predict(model, newdata = df, type = "prob")[,1]
  # 计算混淆矩阵
  cm <- confusionMatrix(pred, test_target)
  cm_list[[model_name]] <- cm$table
  # 计算各项指标
  auc_val <- roc(test_target, probs)$auc
  acc <- cm$overall["Accuracy"]
  sens <- cm$byClass["Sensitivity"]
  spec <- cm$byClass["Specificity"]
  f1 <- cm$byClass["F1"]
  # 添加到结果数据框
  metrics_df <- rbind(metrics_df, data.frame(
    Model = model_name,
    AUC = auc_val,
    Accuracy = acc,
    Sensitivity = sens,
    Specificity = spec,
    F1 = f1
  ))
}
# 打印性能指标
print(metrics_df)
write.xlsx(metrics_df,"07.machine_res_test.xlsx")
# 找出最优模型（基于AUC）
best_model_index <- which.max(metrics_df$AUC)
best_model_name <- metrics_df$Model[best_model_index]
best_model <- model_list[[best_model_index]]
cat("\n最优模型:", best_model_name, "\n")
cat("AUC:", metrics_df$AUC[best_model_index], "\n\n")
#可视化混淆矩阵---------------------------------------
library(ggplot2)
library(reshape2)
for (model_name in names(model_list)[c(2,1,3:7)]){{
  confusion_matrix <- cm_list[[model_name]]
  confusion_melted <- melt(confusion_matrix)
  colnames(confusion_melted) <- c("Prediction", "Reference", "Count")
  # 计算百分比
  confusion_percent <- prop.table(confusion_matrix, margin = 2) * 100
  confusion_percent_melted <- melt(confusion_percent)
  colnames(confusion_percent_melted) <- c("Prediction", "Reference", "Percentage")
  # 合并计数和百分比
  confusion_combined <- merge(confusion_melted, confusion_percent_melted)
  confusion_combined$Label <- paste0(confusion_combined$Count, "\n(", 
                                     round(confusion_combined$Percentage, 1), "%)")
  # 创建带百分比的热图
  p_cm <- ggplot(confusion_combined, aes(x = Prediction, y = Reference, fill = Count)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Label), color = "black", size = 8) +
    scale_fill_gradient(low = "white", high = "brown") +
    # theme_bw() +
    labs(title = paste("Confusion Matrix of",model_name,"in Testing Set"),
         x = "Predicted label",
         y = "True label") +
    theme(panel.grid.major = element_blank(),  # 取消主要网格线
          panel.grid.minor = element_blank(),  # 取消次要网格线
          panel.background = element_blank(),   # 取消背景框
          axis.line = element_blank(),           # 去除坐标轴线
          axis.ticks = element_blank(),           # 去除刻度线
          axis.text = element_text(size=20),
          axis.title = element_text(size=20),
          axis.title.y = element_text(angle = 90),
          axis.text.y = element_text(angle = 90,hjust = 1),
          plot.title = element_text(size= 20,hjust = 0.5,face = "bold"),
          legend.position = "none")
  png_filename <- paste0(model_name,"_cm_test.png")
  pdf_filename <- paste0(model_name,"_cm_test.pdf")
  ggsave(png_filename,p_cm,w = 8,h = 8)
  ggsave(pdf_filename,p_cm,w = 8,h = 8)
}}
#ROC------------------------------------
library(pROC)
library(ggplot2)
roc_curves <- NULL
for (model_name in names(model_list)[c(2,1,3:7)]){
  model <- model_list[[model_name]]
  probs <- predict(model, newdata = df, type = "prob")[,1]
  roc_curve <- roc(test_target, probs)
  roc_curves[[model_name]] <- roc_curve 
}
roc_curves
# 创建数据框用于ggplot
roc_data <- data.frame()
for (model_name in names(roc_curves)) {
  temp_df <- data.frame(
    Model = model_name,
    FPR = 1 - roc_curves[[model_name]]$specificities,
    TPR = roc_curves[[model_name]]$sensitivities
  )
  roc_data <- rbind(roc_data, temp_df)
}
roc_data$Model <- factor(roc_data$Model,levels = c("XGboost","RF","SVM","GBM","KNN","NB","LR"))
# 计算AUC值并保留3位小数
auc_values <- sapply(roc_curves, function(x) sprintf("%0.3f",x$auc))
legend_labels <- paste0(names(auc_values), " (AUC = ", auc_values, ")")
# 使用ggplot2绘制ROC曲线
p3 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
  labs(title = "ROC Curve Comparison(Testing Set)",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)",
       color = "Models") +
  scale_color_manual(values = c("#f57c6e","#f2b56f","#fae69e","#84c3b7","#71b7ed","#b8aeeb","#f2afda"),
                     labels = legend_labels)+
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.7, 0.2),
    legend.background = element_rect(fill = "white", color = "gray80"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  coord_equal()
p3
ggsave('08.test_ROC.pdf',p3,w=7,h=7)
ggsave('08.test_ROC.png',p3,w=7,h=7)
#DCA曲线--------------------------------
library(rmda)
library(ggDCA)
prob_df <- data.frame(obs = test_target)
for (model_name in names(model_list)) {
  prob_df[[model_name]] <- roc_curves[[model_name]]$predictor
}
prob_df$obs <- ifelse(prob_df$obs == "LOPD", 0, 1)
# 添加每个模型的 DCA 曲线
dca_df <- NULL
for(model in names(model_list)){
  dca_data <- decision_curve(
    as.formula(paste("obs ~", model)),
    data = prob_df
  )
  dca_df <- rbind(dca_df,dca_data$derived.data)
}
summary(dca_df)
dca_df <- dca_df %>%
  mutate(model = case_when(
    model == "obs ~ RF" ~ "RF",
    model == "obs ~ XGboost" ~ "XGboost",
    model == "obs ~ SVM" ~ "SVM",
    model == "obs ~ GBM" ~ "GBM",
    model == "obs ~ KNN" ~ "KNN",
    model == "obs ~ NB" ~ "NB",
    model == "obs ~ LR" ~ "LR",
    TRUE ~ as.character(model)
  ))
dca_df$model <- factor(dca_df$model,levels = c("XGboost","RF","All","None","SVM","GBM","KNN","NB","LR"))
p4 <- ggplot(dca_df, aes(x = thresholds,y = NB,color = model)) + 
  geom_line(linetype = "solid",size = 1.2)+
  ylim(0,1)+
  labs(
    title = "Decision Curve Analysis(Testing Set)",
    x = "Threshold Probability",
    y = "Net Benefit",
    color = "Models"
  )+
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )+
  scale_color_manual(values = c("All" = "blue",
                                "None" = "red",
                                "RF" = '#f2b56f',
                                "XGboost" = '#f57c6e',
                                "SVM"= '#fae69e',
                                "GBM"='#84c3b7',
                                "KNN" = "#71b7ed",
                                "NB" = "#b8aeeb",
                                "LR" = "#f2afda"
  ))+
  coord_equal()
p4
ggsave('09.test_DCA.pdf',p4,w=7,h=7)
ggsave('09.test_DCA.png',p4,w=7,h=7)
#打印最佳模型
best_model <- model_list[["XGboost"]]
var_importance <- varImp(best_model)$importance
# 保存最优模型
saveRDS(best_model, "best_model_test.rds")
save.image("09_MachineLearning.RData")

###All_data-----------------------------------------
# 计算并比较各模型的详细指标
model_list <- list(rf_model,xgb_model,svm_model,gbm_model,knn_model,nb_model,lr_model)
names(model_list) <- c("RF","XGboost","SVM","GBM","KNN","NB","LR")
# 初始化结果数据框
metrics_df <- data.frame(
  Model = character(),
  AUC = numeric(),
  Accuracy = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  F1 = numeric(),
  stringsAsFactors = FALSE
)
cm_list <- list()
# 为每个模型计算指标
df <- features
for (model_name in names(model_list)) {
  model <- model_list[[model_name]]
  pred <- predict(model, newdata = df)
  probs <- predict(model, newdata = df, type = "prob")[,1]
  # 计算混淆矩阵
  cm <- confusionMatrix(pred, target)
  cm_list[[model_name]] <- cm$table
  # 计算各项指标
  auc_val <- roc(target, probs)$auc
  acc <- cm$overall["Accuracy"]
  sens <- cm$byClass["Sensitivity"]
  spec <- cm$byClass["Specificity"]
  f1 <- cm$byClass["F1"]
  # 添加到结果数据框
  metrics_df <- rbind(metrics_df, data.frame(
    Model = model_name,
    AUC = auc_val,
    Accuracy = acc,
    Sensitivity = sens,
    Specificity = spec,
    F1 = f1
  ))
}
# 打印性能指标
print(metrics_df)
write.xlsx(metrics_df,"10.machine_res_all_data.xlsx")
# 找出最优模型（基于AUC）
best_model_index <- which.max(metrics_df$AUC)
best_model_name <- metrics_df$Model[best_model_index]
best_model <- model_list[[best_model_index]]
cat("\n最优模型:", best_model_name, "\n")
cat("AUC:", metrics_df$AUC[best_model_index], "\n\n")
#可视化混淆矩阵---------------------------------------
library(ggplot2)
library(reshape2)
for (model_name in names(model_list)[c(2,1,3:7)]){{
  confusion_matrix <- cm_list[[model_name]]
  confusion_melted <- melt(confusion_matrix)
  colnames(confusion_melted) <- c("Prediction", "Reference", "Count")
  # 计算百分比
  confusion_percent <- prop.table(confusion_matrix, margin = 2) * 100
  confusion_percent_melted <- melt(confusion_percent)
  colnames(confusion_percent_melted) <- c("Prediction", "Reference", "Percentage")
  # 合并计数和百分比
  confusion_combined <- merge(confusion_melted, confusion_percent_melted)
  confusion_combined$Label <- paste0(confusion_combined$Count, "\n(", 
                                     round(confusion_combined$Percentage, 1), "%)")
  # 创建带百分比的热图
  p_cm <- ggplot(confusion_combined, aes(x = Prediction, y = Reference, fill = Count)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Label), color = "black", size = 8) +
    scale_fill_gradient(low = "white", high = "brown") +
    # theme_bw() +
    labs(title = paste("Confusion Matrix of",model_name,"in All Data Set"),
         x = "Predicted label",
         y = "True label") +
    theme(panel.grid.major = element_blank(),  # 取消主要网格线
          panel.grid.minor = element_blank(),  # 取消次要网格线
          panel.background = element_blank(),   # 取消背景框
          axis.line = element_blank(),           # 去除坐标轴线
          axis.ticks = element_blank(),           # 去除刻度线
          axis.text = element_text(size=20),
          axis.title = element_text(size =20),
          axis.title.y = element_text(angle = 90),
          axis.text.y = element_text(angle = 90,hjust = 1),
          plot.title = element_text(size= 20,hjust = 0.5,face = "bold"),
          legend.position = "none")
  png_filename <- paste0(model_name,"_cm_all.png")
  pdf_filename <- paste0(model_name,"_cm_all.pdf")
  ggsave(png_filename,p_cm,w = 8,h = 8)
  ggsave(pdf_filename,p_cm,w = 8,h = 8)
}}
#ROC------------------------------------
library(pROC)
library(ggplot2)
roc_curves <- NULL
for (model_name in names(model_list)[c(2,1,3:7)]){
  model <- model_list[[model_name]]
  probs <- predict(model, newdata = df, type = "prob")[,1]
  roc_curve <- roc(target, probs)
  roc_curves[[model_name]] <- roc_curve 
}
roc_curves
# 创建数据框用于ggplot
roc_data <- data.frame()
for (model_name in names(roc_curves)) {
  temp_df <- data.frame(
    Model = model_name,
    FPR = 1 - roc_curves[[model_name]]$specificities,
    TPR = roc_curves[[model_name]]$sensitivities
  )
  roc_data <- rbind(roc_data, temp_df)
}
roc_data$Model <- factor(roc_data$Model,levels = c("XGboost","RF","SVM","GBM","KNN","NB","LR"))
# 计算AUC值并保留3位小数
auc_values <- sapply(roc_curves, function(x) sprintf("%0.3f",x$auc))
legend_labels <- paste0(names(auc_values), " (AUC = ", auc_values, ")")
# 使用ggplot2绘制ROC曲线
p5 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
  labs(title = "ROC Curve Comparison(All Data)",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)",
       color = "Models") +
  scale_color_manual(values = c("#f57c6e","#f2b56f","#fae69e","#84c3b7","#71b7ed","#b8aeeb","#f2afda"),
                     labels = legend_labels)+
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.7, 0.2),
    legend.background = element_rect(fill = "white", color = "gray80"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  coord_equal()
p5
ggsave('11.all_ROC.pdf',p5,w=7,h=7)
ggsave('11.all_ROC.png',p5,w=7,h=7)
#DCA曲线--------------------------------
library(rmda)
library(ggDCA)
prob_df <- data.frame(obs = target)
for (model_name in names(model_list)) {
  prob_df[[model_name]] <- roc_curves[[model_name]]$predictor
}
prob_df$obs <- ifelse(prob_df$obs == "LOPD", 0, 1)
# 添加每个模型的 DCA 曲线
dca_df <- NULL
for(model in names(model_list)){
  dca_data <- decision_curve(
    as.formula(paste("obs ~", model)),
    data = prob_df
  )
  dca_df <- rbind(dca_df,dca_data$derived.data)
}
summary(dca_df)
dca_df <- dca_df %>%
  mutate(model = case_when(
    model == "obs ~ RF" ~ "RF",
    model == "obs ~ XGboost" ~ "XGboost",
    model == "obs ~ SVM" ~ "SVM",
    model == "obs ~ GBM" ~ "GBM",
    model == "obs ~ KNN" ~ "KNN",
    model == "obs ~ NB" ~ "NB",
    model == "obs ~ LR" ~ "LR",
    TRUE ~ as.character(model)
  ))
dca_df$model <- factor(dca_df$model,levels = c("XGboost","RF","All","None","SVM","GBM","KNN","NB","LR"))
p6 <- ggplot(dca_df, aes(x = thresholds,y = NB,color = model)) + 
  geom_line(linetype = "solid",size = 1.2)+
  ylim(0,1)+
  labs(
    title = "Decision Curve Analysis(All Data)",
    x = "Threshold Probability",
    y = "Net Benefit",
    color = "Models"
  )+
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )+
  scale_color_manual(values = c("All" = "blue",
                                "None" = "red",
                                "RF" = '#f2b56f',
                                "XGboost" = '#f57c6e',
                                "SVM"= '#fae69e',
                                "GBM"='#84c3b7',
                                "KNN" = "#71b7ed",
                                "NB" = "#b8aeeb",
                                "LR" = "#f2afda"
  ))+
  coord_equal()
p6
ggsave('12.all_DCA.pdf',p6,w=7,h=7)
ggsave('12.all_DCA.png',p6,w=7,h=7)
#打印最佳模型
best_model <- model_list[["XGboost"]]
var_importance <- varImp(best_model)$importance
# 保存最优模型
saveRDS(best_model, "best_model_all.rds")


#拼图-----------------
library(patchwork)
p_dca <- p2+p4+p6+plot_layout(guides = "collect")&theme(legend.position = "bottom")
p_dca
(p1+p3+p5)/p_dca
ggsave("res.png",w = 21,h = 16)
ggsave("res.pdf",w = 21,h = 16)

save.image("09_MachineLearning.RData")