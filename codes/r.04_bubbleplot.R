rm(list = ls()); gc()
setwd("/Users/yaoruntian/HuangLab/PDPP")
if (!dir.exists("04_bubbleplot")) {dir.create("04_bubbleplot")}
setwd("04_bubbleplot")
library(openxlsx)
library(ggplot2)
library(cowplot)
library(data.table)
library(dplyr)
library(lubridate)
library(scales)
library(magrittr)
library(tidyr)
library(reshape2)
library(ggpmisc)
library(ggbreak)
library(patchwork)
options(scipen = 200)
#绘制气泡主图
#phenotype_type
dat1 <- read.xlsx("../03_create_matrix/13.Population_Phenotype_mat.xlsx")
colnames(dat1) <- c("Population","Adult","Asymptomatic","Childhood","Childhood or Adult","Classic infantile")
#caculate the proportion
dat1[,2:ncol(dat1)] <- t(apply(dat1[,2:ncol(dat1)],1,function(row){row/sum(row)}))
plot_data <- reshape2::melt(dat1)
colnames(plot_data) <- c("Population","Phenotype","Proportion")
plot_data[plot_data == 0] <- NA
p1 <- ggplot(plot_data,
       aes(Phenotype, Population, fill = Population, size = Proportion)) +
  geom_point(shape = 21) +
  geom_hline(yintercept = seq(.5, 7.5, 1), size = .2) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev)+
  scale_radius(range = c(5, 15)) +
  scale_fill_manual(values = c("#f57c6e","#f2b56f","#fae69e","#84c3b7","#71b7ed","#b8aeeb","#f2afda"))+
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10)) +
  guides(fill = guide_legend(override.aes = list(size = 10),title.position = "top"),
         size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = .25),
                             label.position = "bottom",
                             title.position = "top"
                             #order = 2
         )
  ) +
  labs(size = "Population Proportion", 
       fill = "Population", y = NULL,x = NULL)
p1
#population柱状图
dat1 <- read.xlsx("../03_create_matrix/13.Population_Phenotype_mat.xlsx")
colnames(dat1) <- c("Population","Adult","Asymptomatic","Childhood","Childhood or Adult","Classic infantile")
population_count <- data.frame("population" = dat1[,1],
                               "Count" = apply(dat1[,2:ncol(dat1)],1,sum))

p2 <- ggplot(data=population_count,aes(x=population,y=Count,fill=population))+
  geom_col()+
  geom_text(aes(label=paste0(Count)),hjust=1,vjust=0.5)+
  coord_flip()+
  scale_y_reverse()+
  scale_x_discrete(limits = rev)+
  theme_classic()+
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom"
        )+
  scale_fill_manual(values = c("#f57c6e","#f2b56f","#fae69e","#84c3b7","#71b7ed","#b8aeeb","#f2afda"))+
  labs(x=NULL,y="Population Count")+
  guides(fill = "none")
p2
#phenotype柱状图
phenotype_count <- data.frame("Phenotype" = colnames(dat1[,2:ncol(dat1)]),
                              "Count" = apply(dat1[,2:ncol(dat1)],2,sum))

p3 <- ggplot(data=phenotype_count,aes(x=Phenotype,y=Count,fill=Phenotype))+
  geom_col(width = 0.5)+
  geom_text(aes(label=paste0(Count)),hjust=0.5,vjust=0)+
  theme_classic()+
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom"
  )+
  scale_fill_manual(values = c("#F29E94","#F8CBC9","#C1D09D","#A4C2B7","#445C46"))+
  labs(x=NULL,y="Phenotype Count")+
  guides(fill = guide_legend(override.aes = list(size = 5),title.position = "top",ncol = 3,order = 3))
p3
combined_plot <- (plot_spacer()+p3)/(p2+p1)+plot_layout(widths = c(1,2),guides = "collect")
legend_plot <- cowplot::get_legend(combined_plot)
final_plot <- (wrap_elements(panel = plot_spacer() + inset_element(legend_plot, 
                                                                  left = 0, right = 1, 
                                                                  bottom = 0, top = 1)) + p3)/(p2 + p1) + 
  plot_layout(widths = c(1, 2))
final_plot&theme(legend.position = "none")
ggsave(filename = '01.bubbleplot.pdf', w=12, h=8.5)
ggsave(filename = '01.bubbleplot.png', w=12, h=8.5, dpi = 600)
save.image("04_bubbleplot.RData")
