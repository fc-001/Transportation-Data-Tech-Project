library(tidyverse)
library(readxl)
library(ggplot2)
library(ggmap)
register_stadiamaps(key='ed870b84-66a8-47aa-bb45-dc22569ae3ca')  # stadiamaps注册之后的api
library(fpc)  # 聚类方法和聚类验证
library(factoextra)  # 各种分析法
library(cluster)  # 聚类
library(permute)  # 生成一系列受限制的排列设计
library(lattice)  # 画图的
library(vegan)  # 多元分析
library(NbClust)  # 评估聚类效果
data <- read_excel('E:\\Program Files\\TDA\\task5\\obike_1.xlsx')  # 读数据
# 预处理
rad <- pi/180  # 将经纬度转化为弧度制
olgt1 <- data$olgt * rad
olat1 <- data$olat * rad
dlgt1 <- data$dlgt * rad
dlat1 <- data$dlat * rad
earth_R <- 6371  # 地球平均半径(单位km)
data$distance <- earth_R * acos(cos(dlat1) * cos(olat1) * cos(dlgt1 - olgt1) + sin(dlat1)*sin(olat1))
# 上面是利用球面余弦定理和Haversin定理来计算地球上任意两点之间的最短距离
data$speed<-data$distance/(as.numeric(data$time)/3600)  #计算平均车速,单位km/h
for(i in 1:16386)
{
  if(data$speed[i]>=25)  # 设置阈值为25
    data$`bike ID`[i]<-NA
}
data<-na.omit(data)  #删除异常数据
# 特征提取
road <- ggmap::get_stadiamap(bbox = c(left = 121.3, bottom = 24.9, right = 121.7, top = 25.2),maptype = c("stamen_toner"), zoom = 13)
p1 <- ggmap(road) +  # 用get_stadiamap()而不是stamenmap()!
  geom_point(data = data, aes(x = olgt, y = olat),color = 'green', size = 2) + 
  theme_minimal() +
  labs(title = 'Start point sample distribution plot') + 
  xlab('Start Point Latitude') + 
  ylab('Start Point Longitude')  # 画O点地理位置分布图
p1


location <- data[,c('olgt','olat')]
# 根据相似度量来聚类
# (1)利用欧氏距离的k-means
# 定义聚类算法
my_kmeans_eculid <- function(data, k) {
  centroids <- data[sample(nrow(data), k), ]
  cluster_assignment <- rep(0, nrow(data))
  
  while(TRUE) {
    for(i in 1:nrow(data)) {
      distances <- sapply(1:k, function(j) {
        centroid <- as.vector(data[i, ])
        centroid_j <- as.vector(centroids[j, ])
        sqrt(sum((centroid - centroid_j)^2))
      })
      cluster_assignment[i] <- which.min(distances)
    }
    
    new_centroids <- sapply(1:k, function(j) {
      colMeans(data[cluster_assignment == j, ])
    })
    
    if(all(centroids == new_centroids)) {
      break
    }
    
    centroids <- new_centroids
  }
  
  data$cluster <- as.factor(cluster_assignment)
  
  ggplot(data, aes(x = olgt, y = olat, color = cluster)) +
    geom_point() +
    ggtitle(paste("K-means Clustering (k =", k, ")")) +
    theme_minimal()
}

my_kmeans_eculid(location, k = 5)


