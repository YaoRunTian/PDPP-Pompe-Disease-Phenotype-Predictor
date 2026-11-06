#Author:Yaort
#Email:yaort@mail2.sysu.edu.cn
#Create Time:2025-04-24 22:37:12
#Last Modified by:Yaort
#Last Modified time:2025-04-25 04:14:30

rm(list = ls()); gc()
setwd("/Users/yaoruntian/HuangLab/PDPP")
if (!dir.exists("02_map")) {dir.create("02_map")}
setwd("02_map")
library(tidyverse)
library(maps)
library(ggplot2)
library(openxlsx)
volcano <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv") 
volcano %>% 
  select(primary_volcano_type, longitude, 
         latitude, population_within_100_km) %>%
  head()
#读取世界地图数据
world <- map_data("world")
write.table(unique(world$region),"01.region.txt",sep = "\t",quote = FALSE,row.names = FALSE,col.names = FALSE)
#读取subregion数据
region_dat <- read.xlsx("02.region_type.xlsx")
world1 <- merge(world[,1:5],region_dat,by = "region")
world1 <- world1[,c(2:5,1,6)]
world1 <-world1[order(world1$order, decreasing=FALSE),]
# world1$subregion <- gsub("Unknow",NA,world1$subregion)
#读取突变地区数据，根据01_classify中的05.res.xlsx
dat1 <- read.xlsx("../01_classify/05.res.xlsx")
dat1 <- dat1[,c(2,9,10)]
dat1 <- na.omit(dat1)
#随机取点
library(sf)
library(lwgeom)
long_lat_df <- data.frame()
for(i in 1:nrow(dat1)){
  print(i)
  df <- subset(world1,subregion == dat1[i,"Type"])
  # 提取经纬度并转换为矩阵
  coords <- as.matrix(df[, c("long", "lat")])
  coords[1, ] <- coords[nrow(coords), ]
  # 尝试反转坐标顺序以形成有效的逆时针多边形
  coords_reversed <- coords[nrow(coords):1, ]
  all(coords_reversed[1, ] == coords_reversed[nrow(coords_reversed), ])
  # 创建多边形并处理有效性
  polygon <- st_polygon(list(coords_reversed))
  polygon_sfc <- st_sfc(polygon,crs = 4326)
  valid_polygon <- st_make_valid(polygon_sfc)
  set.seed(i)
  random_point <- st_sample(valid_polygon, size = 1)
  random_coords <- st_coordinates(random_point)
  point_long <- random_coords[1,"X"]
  point_lat <- random_coords[1,"Y"]
  long_lat_df <- rbind(long_lat_df,data.frame(point_long,point_lat))
}
dat1 <- cbind(dat1,long_lat_df)

#可视化
ggplot() +
  geom_map(
    data = world1, map = world1,
    aes(long, lat, map_id = region,fill = subregion),color = "white",size = 0.1) +
  geom_point(
    data = dat1,
    aes(point_long, point_lat, 
        color = Phenotype,
        size=Patient_Count),
    alpha = 0.5
  ) +
  # labs(x = NULL, y = NULL, color = NULL)+
  theme_void() +
  theme(legend.position = "bottom")+
  labs(title="Pompe Disease Location")
