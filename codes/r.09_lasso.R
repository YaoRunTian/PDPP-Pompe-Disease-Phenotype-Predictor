rm(list = ls()); gc()
setwd("/Users/yaoruntian/HuangLab/PDPP")
if (!dir.exists("09_lasso")) {dir.create("09_lasso")}
setwd("09_lasso")
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
library(viridis)
library(patchwork)
set.seed(123)
#00.数据整理-----------------------------
df1 <- read.xlsx("../03_create_matrix/07.final_df.xlsx")
df1 <- subset(df1,Phenotype != "Asymptomatic") #2596个
df1 <- df1 %>%
  mutate(Severity = case_when(
    Severity == "Less severe" ~"Less_severe",
    Severity == "Potentially mild" ~"Potentially_mild",
    Severity == "Very severe" ~"Very_severe",
    TRUE ~ Severity)
  )
df1 <- df1 %>%
  mutate(Phenotype2 = case_when(
    Phenotype == "Classic infantile" ~"IOPD",
    TRUE ~ "LOPD")
  )
#01.区分训练集和测试集---------------------
set.seed(123)
patient_ids <- unique(df1$Patient_ID)
train_ids <- sample(patient_ids, size = floor(length(patient_ids) * 0.7))
train_df <- df1 %>% filter(Patient_ID %in% train_ids)
test_df  <- df1 %>% filter(!Patient_ID %in% train_ids)
write.xlsx(train_df,"01.train_df.xlsx")
write.xlsx(test_df,"01.test_df.xlsx")
write.xlsx(df1,"01.all_data.xlsx")
#02.lasso回归进行特征选择------------------------------------
df1_ml <- df1 %>%
  select(A1_SNP,A1_Location,A2_SNP,A2_Location,Severity,CRIM_status,Population,Phenotype2) %>%
  mutate(across(c(A1_SNP,A1_Location,A2_SNP,A2_Location,Severity,CRIM_status,Population,Phenotype2), as.factor))
df1_ml <- df1_ml  %>% mutate_all(as.numeric)
x_train <- model.matrix(Phenotype2 ~ ., data = df1_ml)[,-1]
y_train <- df1_ml$Phenotype2
library("glmnet")
set.seed(123)
lasso_fit <- cv.glmnet(
  x = x_train,
  y = y_train,
  family = "gaussian",
  alpha = 1
)
lasso_fit
# Call:  cv.glmnet(x = x_train, y = y_train, family = "gaussian", alpha = 1) 
# 
# Measure: Mean-Squared Error 
# 
# Lambda Index Measure       SE Nonzero
# min 0.003054    44  0.1517 0.004481       6
# 1se 0.031259    19  0.1555 0.004104       6
lambda_min <- lasso_fit$lambda.min
lambda_1se <- lasso_fit$lambda.1se
set.seed(123)
la.eq <- glmnet(
  x = x_train,
  y = y_train,
  alpha = 1,
  family = "binomial",
  lambda = lambda_1se
)
cv_model  <- lasso_fit
best_lambda <- cv_model$lambda.min
lambda_values <- cv_model$lambda

coef_list <- lapply(lambda_values, function(lambda) coef(cv_model, s = lambda))
coef_matrices <- lapply(coef_list, function(coef) as.matrix(coef))
coef_df_list <- lapply(coef_matrices, function(matrix) as.data.frame(t(matrix)))
coef_df <- do.call(rbind, coef_df_list)
coef_df$lambda <- rep(lambda_values, each = nrow(coef_df_list[[1]]))
coef_df <- coef_df[!rownames(coef_df) %in% "(Intercept)", ]
colnames(coef_df) <- c(colnames(coef_df)[-ncol(coef_df)], "lambda")
library(reshape2)
coef_long <- reshape2::melt(coef_df, id.vars = "lambda", variable.name = "Variable", value.name = "Coefficient")
coef_long$log_lambda <- log(coef_long$lambda)
# 加载必要的包
library(ggplot2)
library(ggsci)
library(ggrepel)
library(RColorBrewer)
table(coef_long$Variable)
mypalette <- colorRampPalette(brewer.pal(9,"Paired"))
coef_long <- subset(coef_long,Variable != "(Intercept)")
sign <- NULL
for(i in 1:7){
  sign1 <- unique(coef_long$Variable)[i]
  sign2 <- c(as.character(sign1),rep(NA,62))
  sign <- c(sign,sign2)
}
sign <- sign[1:nrow(coef_long)]
coef_long$sign <- sign
p1 <- ggplot(coef_long, aes(x = log_lambda, y = Coefficient, color = Variable))+ 
  geom_vline(xintercept = log(best_lambda), size = 0.8, color = 'grey60', alpha = 0.8, linetype = 2) +
  #geom_vline(xintercept = log(cv_model$lambda.1se), size = 0.8, color = 'grey60', alpha = 0.8, linetype = 2) +
  geom_line(size = 1) +
  xlab("Log Lambda") + 
  ylab("Coefficients") + 
  theme_bw(base_rect_size = 2) +
  scale_x_continuous(expand = c(0.01, 0.01)) + 
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  theme(panel.grid = element_blank(), 
        axis.title = element_text(size = 15, color = 'black'), 
        axis.text = element_text(size = 15, color = 'black'), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 12, color = 'black'), 
        legend.position = 'none') +
  annotate('text', x = log(best_lambda)+0.5,
           y = -0.025, 
           label = paste0('log(lambda.min)\n', round(log(best_lambda), 3)), 
           color = 'black', size = 6)+
  scale_color_manual(values = mypalette(7))+
  geom_label_repel(aes(label=sign), fontface="bold",
                   box.padding=unit(0.35, "lines"), point.padding=unit(1, "lines"),
                   segment.colour = "grey50",size = 5,max.overlaps = 20)
p1
ggsave('01.lasso.Coef.pdf',p1,w=11,h=7)
ggsave('01.lasso.Coef.png',p1,w=11,h=7)
xx <- data.frame(
  lambda = lasso_fit$lambda,
  cvm = lasso_fit$cvm,
  cvsd = lasso_fit$cvsd,
  cvup = lasso_fit$cvup,
  cvlo = lasso_fit$cvlo,
  nozezo = lasso_fit$nzero
)
xx$ll <- log(xx$lambda)
xx$NZERO <- paste0(xx$nozezo, ' vars')
dev.off()
library(gtools)
xx$NZERO <- factor(xx$NZERO,levels=mixedsort(names(table(xx$NZERO))))
p2 <- ggplot(xx, aes(ll, cvm, color = NZERO)) +
  geom_errorbar(aes(ymin = cvlo, ymax = cvup), width = 0.05, size = 1) +
  geom_vline(xintercept = log(best_lambda), size = 0.8, color = 'grey60', alpha = 0.8, linetype = 2) +
  geom_point(size = 2) +
  xlab("Log Lambda") +
  ylab('Partial Likelihood Deviance') +
  theme_bw(base_rect_size = 1.5) +
  scale_x_continuous(expand = c(0.02, 0.02)) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15, color = 'black'),
    axis.text = element_text(size = 15, color = 'black'),
    legend.title = element_blank(),
    legend.text = element_text(size = 15, color = 'black'),
    legend.position = 'top'
  ) +
  scale_color_manual(values = mypalette(11))+
  annotate('text', x = log(best_lambda)+0.5, y = 0.165, 
           label = paste0('6 Vars\nlog(lambda.min)\n', round(log(best_lambda), 3)), 
           color = 'black', size = 6)+
  guides(col = guide_legend(ncol = 8))
print(p2)
ggsave('02.lasso.CV.pdf',p2,w=11,h=7)
ggsave('02.lasso.CV.png',p2,w=11,h=7)
#计算lasso结果
coefficients <- coef(lasso_fit,s = lasso_fit$lambda.min)
Active.Index <- which(as.numeric(coefficients) != 0)
active.coefficients <- as.numeric(coefficients)[Active.Index]
hubvars <- rownames(coefficients)[Active.Index]
hubvars
hubvarslist <- coefficients@Dimnames[[1]][Active.Index]
lasso_result <- data.frame(coef = coefficients@x)
rownames(lasso_result) <- hubvarslist
coef <- lasso_result[hubvars,]
lasso_result <- data.frame(coef = coef)
rownames(lasso_result) <- hubvars
lasso_result
write.table(lasso_result,"03.lassoresult.txt",row.names = TRUE,col.names = TRUE,quote = FALSE)
write.table(hubvars,"04.lassovarslist.txt",row.names = FALSE,col.names = FALSE,quote = FALSE)
#柱状图
lasso_plot_data <- lasso_result[-1, , drop = FALSE]  # 移除截距
# 按系数绝对值排序
lasso_plot_data <- lasso_result[-1, , drop = FALSE]
lasso_plot_data$variable <- rownames(lasso_plot_data)
lasso_plot_data$variable <- factor(lasso_plot_data$variable, 
                                   levels = lasso_plot_data$variable[order(lasso_plot_data$coef)])

p3 <- ggplot(lasso_plot_data,aes(x = coef, y = reorder(variable, coef), fill = coef))+
  geom_bar(stat = "identity",color = "black",linewidth = 0.8)+
  geom_text(aes(x = ifelse(coef>0,-.001,.001), y = reorder(variable, coef),
                label = reorder(variable, coef),hjust = ifelse(coef > 0, 1, 0)
                ),
            size = 5)+
  geom_text(aes(x = coef, y = reorder(variable, coef),
                label = round(coef,3),hjust = ifelse(coef > 0, -0.1, 1.1)
                ),
            size = 5)+
  geom_vline(xintercept=0,size=1,color="black")+
  scale_fill_viridis(begin = 0,end = 0.7,option = "D")+
  coord_cartesian(clip = "off")+
  scale_x_continuous(expand = expansion(add = c(.01,.01)))+
  labs(title = "LASSO Coefficients")+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text.y =  element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 15,face = "bold",color = "black",hjust = 0.6),
        axis.text.x = element_blank()
        )
p3
#patchwork
(p2+p1)/p3
ggsave("res.pdf",h = 10,w = 15)
ggsave("res.png",h = 10,w = 15)
# save.image("~/HuangLab/PDPP/09_lasso/09_lasso.RData")