rm(list = ls()); gc()
setwd("/Users/yaoruntian/HuangLab/PDPP")
if (!dir.exists("05_barplot")) {dir.create("05_barplot")}
setwd("05_barplot")
library(tidyr)
library(tibble)
library(openxlsx)
library(RColorBrewer)
library(ggplot2)
library(patchwork)
#01.mutation和phenotype------------------------------
#绘制堆积柱状图
dat1 <- read.xlsx("../03_create_matrix/10.Phenotype_mat.xlsx",rowNames = TRUE)
colnames(dat1) <- c("Adult","Asymptomatic","Childhood","Childhood or Adult","Classic infantile")
dat1$sum <- apply(dat1,1,sum)
ordered_df <- dat1[order(dat1$sum,decreasing = TRUE),]
#筛选sum>15的突变
rt <- subset(ordered_df,sum >= 15)
rt <- rt[,-ncol(rt)]
rt <- t(rt)
dat2 <- rt %>% as.data.frame() %>%
  rownames_to_column("Phenotype") %>% 
  gather(key = Mutation,value = Count,-Phenotype)
#Mutation Proportion
df1 <- data.frame("Phenotype" = rownames(rt),
                  "Number" = apply(rt,1,sum))
mypalette <- colorRampPalette(brewer.pal(9,"Paired"))
p1 <- ggplot(dat2,aes(Phenotype,Count,fill = Mutation)) + 
  geom_bar(stat = "identity",position = "fill") +
  geom_text(data=df1,
            aes(x=Phenotype,y=1,
                label=paste("n = ",Number)),
            inherit.aes = FALSE,
            vjust=-1,size = 5)+
  scale_fill_manual(values = mypalette(26))+
  labs(fill = "Mutation",x = "",y = " Mutation Proportion") + 
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid = element_line(colour = "white"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.position = "right") + 
  scale_y_continuous(expand = expansion(mult=c(0,0.1)),
                     labels = scales::percent_format())+
  guides(fill = guide_legend(ncol = 1,title.position = "top"))
p1
#Phenotype Count
p2 <- ggplot(dat2,aes(Mutation,Count,fill = Phenotype)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#F29E94","#F8CBC9","#C1D09D","#A4C2B7","#445C46"))+
  labs(fill = "Phenotype",x = "",y = " Phenotype Count") + 
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.position = "top") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02)))+
  guides(fill = guide_legend(title.position = "top",nrow = 1))
p2
##绘制饼图
top5_mutation_dat <- ordered_df[1:5,1:5]
mutation_names <- rownames(top5_mutation_dat)
plotPie <- function(mutation_name){
  plot_data <- t(top5_mutation_dat[mutation_name,]) %>%as.data.frame()
  colnames(plot_data) <- "value"
  plot_data$population <- rownames(plot_data)
  ggplot(plot_data, aes(x = "", y = value, fill = population)) +
    geom_col(width = 1, color = "white",size = 0.2) +       
    coord_polar("y", start = 0) +               
    theme_void()+
    scale_fill_manual(values = c("#F29E94","#F8CBC9","#C1D09D","#A4C2B7","#445C46"))+
    theme(legend.position = "none",
          axis.title.x = element_text(size = 12),
          plot.title = element_text(hjust = 0.5,size = 16))+
    ylab(mutation_name)+labs(fill = "Phenotype")
}
pieplot_list <- lapply(mutation_names, plotPie)
p3 <- wrap_plots(pieplot_list, byrow = T, ncol = 5)
p3
p4 <- p3/p2+plot_layout(heights = c(1, 2))
p4|p1+plot_layout(guides = "collect")&theme(legend.position = "right")
ggsave("01.Phenotype_barplot.pdf",width = 18, height = 10)
ggsave("01.Phenotype_barplot.png",dpi = 300,width = 18, height = 10)
#02.mutation和population------------------------------
rm(list = ls()); gc()
#绘制堆积柱状图
dat1 <- read.xlsx("../03_create_matrix/09.Population_mat.xlsx",rowNames = TRUE)
colnames(dat1)[7] <- "Other"
dat1$sum <- apply(dat1,1,sum)
ordered_df <- dat1[order(dat1$sum,decreasing = TRUE),]
#筛选sum>15的突变
rt <- subset(ordered_df,sum >= 15)
rt <- rt[,-ncol(rt)]
rt <- t(rt)
dat2 <- rt %>% as.data.frame() %>%
  rownames_to_column("Population") %>% 
  gather(key = Mutation,value = Count,-Population)
#Mutation Proportion
df1 <- data.frame("Population" = rownames(rt),
                  "Number" = apply(rt,1,sum))
mypalette <- colorRampPalette(brewer.pal(9,"Paired"))
dat2$Population <- factor(dat2$Population,levels = c("AFR","AMR","EAS","FIN","NFE","SAS","Other"))
# 重新定义种群顺序，排除EAS和FIN
selected_populations <- c("AFR","AMR","EAS","NFE","SAS","Other")

# 筛选数据
dat2_selected <- dat2 %>% 
  filter(Population %in% selected_populations) %>%
  mutate(Population = factor(Population, levels = selected_populations))

df1_selected <- df1 %>% 
  filter(Population %in% selected_populations) %>%
  mutate(Population = factor(Population, levels = selected_populations))

p1 <- ggplot(dat2_selected, aes(Population, Count, fill = Mutation)) + 
  geom_bar(stat = "identity", position = "fill") +
  geom_text(data = df1_selected,
            aes(x = Population, y = 1,
                label = paste("n = ", Number)),
            inherit.aes = FALSE,
            vjust = -1, size = 5) +
  scale_fill_manual(values = mypalette(26)) +
  labs(fill = "Mutation", x = "", y = "Mutation Proportion") + 
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid = element_line(colour = "white"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.position = "right") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                     labels = scales::percent_format()) +
  guides(fill = guide_legend(ncol = 1, title.position = "top"))
p1
#Population Count
head(dat2)
dat2$Population <- factor(dat2$Population,levels = c("AFR","AMR","EAS","FIN","NFE","SAS","Other"))
p2 <- ggplot(dat2,aes(Mutation,Count,fill = Population)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#f57c6e","#f2b56f","#fae69e","#84c3b7","#71b7ed","#b8aeeb","#f2afda"))+
  labs(fill = "Population",x = "",y = " Population Count") + 
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5),
        axis.title = element_text(size = 15),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.position = "top") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02)))+
  guides(fill = guide_legend(title.position = "top",nrow = 1))
p2
##绘制饼图
top5_mutation_dat <- ordered_df[1:5,1:7]
mutation_names <- rownames(top5_mutation_dat)

plotPie <- function(mutation_name){
  plot_data <- t(top5_mutation_dat[mutation_name,]) %>%as.data.frame()
  colnames(plot_data) <- "value"
  plot_data$population <- rownames(plot_data)
  plot_data$population <- factor(plot_data$population,levels = c("AFR","AMR","EAS","FIN","NFE","SAS","Other"))
  ggplot(plot_data, aes(x = "", y = value, fill = population)) +
    geom_col(width = 1, color = "white",size = 0.2) +       
    coord_polar("y", start = 0) +               
    theme_void()+
    scale_fill_manual(values = c("#f57c6e","#f2b56f","#fae69e","#84c3b7","#71b7ed","#b8aeeb","#f2afda"))+
    theme(legend.position = "none",
          axis.title.x = element_text(size = 12),
          plot.title = element_text(hjust = 0.5,size = 16))+
    ylab(mutation_name)+labs(fill = "Population")
}
pieplot_list <- lapply(mutation_names, plotPie)
p3 <- wrap_plots(pieplot_list, byrow = T, ncol = 5)
p3
p4 <- p3/p2+plot_layout(heights = c(1, 2))
p4|p1+plot_layout(guides = "collect")&theme(legend.position = "right")
ggsave("02.Population_barplot.pdf",width = 18, height = 10)
ggsave("02.Population_barplot.png",dpi = 300,width = 18, height = 10)

