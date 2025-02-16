library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(magrittr)
library(readxl)
library(lubridate)
xls1 <- read_excel("E:\\Program Files\\交通运输数据技术canvas\\作业一\\tmp001.xls")
xls2 <- read_excel("E:\\Program Files\\交通运输数据技术canvas\\作业一\\tmp002.xls")
xls3 <- read_excel("E:\\Program Files\\交通运输数据技术canvas\\作业一\\tmp003.xls")
xls4 <- read_excel("E:\\Program Files\\交通运输数据技术canvas\\作业一\\tmp004.xls")
xls5 <- read_excel("E:\\Program Files\\交通运输数据技术canvas\\作业一\\tmp005.xls")

#合并五份线圈数据，筛选，剔除第一列并从小到大排列
data <- bind_rows(xls1,xls2,xls3,xls4,xls5)
df <- filter(data,FSTR_LOOPGROUPID=='NHNX40(2)')
df <- df[order(df$FDT_TIME),]
df <- df[,-1]
#删除不需要的变量
rm(xls1,xls2,xls3,xls4,xls5,data)

#提取出时间冗余的行的序号index，包括第一次出现的行和后面的所有重复行
dup_index <- which(df$FDT_TIME %in% df$FDT_TIME[which(duplicated(df$FDT_TIME))])
dup_df <- df[dup_index, ] #显示冗余
#对于时间冗余的行需要做平均的列名
cols_need_mean <- c("FINT_VOLUME","FINT_SPEED","FINT_OCCUPY")
#按照时间分组，对不同的列进行不同的操作，在需要做平均的列名中的列取平均，其他列取第一个数
rows_to_add <- dup_df %>% group_by(FDT_TIME) %>%
  summarise(across(!any_of(cols_need_mean),first),across(all_of(cols_need_mean),mean))
df <- df[-dup_index, ]
df <- bind_rows(df,rows_to_add)
df <- arrange(df, FDT_TIME) #升序排序


# 构建完整的时刻序列
start_day = 18
end_day = 24
num_day = end_day - start_day + 1
time_df <- data.frame(base="2010-4-") %>%
  full_join(data.frame(day = start_day:end_day),by = character()) %>%
  full_join(data.frame(hour = 0:23),by = character()) %>%
  full_join(data.frame(minute = 0:59),by = character()) %>%
  full_join(data.frame(second = c("00","20","40")),by = character())
  
time_df$FDT_TIME <- str_c(
  str_c(time_df$base, time_df$day),
  str_c(time_df$hour,time_df$minute,time_df$second,sep=":"),
  sep=" ")


time_df$FDT_TIME <- strptime(time_df$FDT_TIME, format = "%Y-%m-%d %H:%M:%S")
time_df$FDT_TIME <- format(time_df$FDT_TIME, format = "%Y-%m-%d %H:%M:%S")
time_df <- arrange(time_df,FDT_TIME)# 升序

# 与原数据合并  
df$FDT_TIME<- as.character(strptime(df$FDT_TIME,format="%Y-%m-%d %H:%M:%S"))
df_full <- left_join(time_df["FDT_TIME"],df,by="FDT_TIME")
#显示每天每列的缺失情况
df_full$DATE<-as.Date(df_full$FDT_TIME)
missing_data<-df_full %>%
  group_by(DATE) %>%
  summarise(across(everything(), ~sum(is.na(.))))
#缺失补齐
index<-which(is.na(df_full$FINT_VOLUME))
#填补缺失项数据
for (i in index) {  if (i > 3) {
  df_full$FSTR_LOOPGROUPID[i] <- "NHNX40(2)"
  df_full$FINT_VOLUME[i] <- (df_full$FINT_VOLUME[i-1] + df_full$FINT_VOLUME[i-2] + df_full$FINT_VOLUME[i-3])/3
  df_full$FINT_SPEED[i] <- (df_full$FINT_SPEED[i-1] + df_full$FINT_SPEED[i-2] + df_full$FINT_SPEED[i-3])/3
  df_full$FINT_OCCUPY[i] <- (df_full$FINT_OCCUPY[i-1] + df_full$FINT_OCCUPY[i-2] + df_full$FINT_OCCUPY[i-3])/3
} else {
  df_full$FSTR_LOOPGROUPID[i] <- NA
  df_full$FINT_VOLUME[i] <- NA
  df_full$FINT_SPEED[i] <- NA
  df_full$FINT_OCCUPY[i] <- NA  }}

myfunction <- function(temp,x){
  temp <- arrange(temp, FDT_TIME)
  temp$id <- 1:nrow(temp)
  t<-x/20
  temp$group<-(temp$id-1)%/%t
  # 指定分组变量
  grouped <- group_by(.data = temp, group)
  # 聚合统计
  stats1<- summarise(.data= grouped, time=first(FDT_TIME),A_VOLUME= mean(FINT_VOLUME)*(3600/20) ,A_SPEED= sum(FINT_VOLUME*FINT_SPEED)/ sum(FINT_VOLUME),A_OCCUPY=mean(FINT_OCCUPY))
  stats1<-na.omit(stats1)
  return(stats1)
}

s1<-myfunction(df_full,20)
s2<-myfunction(df_full,300)
s3<-myfunction(df_full,900)

# 分别绘制时间间隔为20s、5min、15min下的v,q,o两两关系
library(cowplot)
p1<-ggplot(s1,aes(x=A_OCCUPY,y=A_SPEED))+geom_point(colour="blue")+ labs(title = "v-o")
p2<-ggplot(s1,aes(x=A_VOLUME,y=A_SPEED))+geom_point(colour="red")+ labs(title = "v-q")
p3<-ggplot(s1,aes(x=A_OCCUPY,y=A_VOLUME))+geom_point(colour="black")+ labs(title = "q-o")
p4<-cowplot::plot_grid(p1, p2, p3, nrow = 2)
p4
library(cowplot)
p1<-ggplot(s2,aes(x=A_OCCUPY,y=A_SPEED))+geom_point(colour="blue")+ labs(title = "v-o")
p2<-ggplot(s2,aes(x=A_VOLUME,y=A_SPEED))+geom_point(colour="red")+ labs(title = "v-q")
p3<-ggplot(s2,aes(x=A_OCCUPY,y=A_VOLUME))+geom_point(colour="black")+ labs(title = "q-o")
p4<-cowplot::plot_grid(p1, p2, p3, nrow = 2)
p4
library(cowplot)
p1<-ggplot(s3,aes(x=A_OCCUPY,y=A_SPEED))+geom_point(colour="blue")+ labs(title = "v-o")
p2<-ggplot(s3,aes(x=A_VOLUME,y=A_SPEED))+geom_point(colour="red")+ labs(title = "v-q")
p3<-ggplot(s3,aes(x=A_OCCUPY,y=A_VOLUME))+geom_point(colour="black")+ labs(title = "q-o")
p4<-cowplot::plot_grid(p1, p2, p3, nrow = 2)
p4
library(cowplot)
p1<-ggplot(s3,aes(x=A_OCCUPY,y=A_SPEED))+geom_point(colour="blue")+ labs(title = "v-o")
p2<-ggplot(s3,aes(x=A_VOLUME,y=A_SPEED))+geom_point(colour="red")+ labs(title = "v-q")
p3<-ggplot(s3,aes(x=A_OCCUPY,y=A_VOLUME))+geom_point(colour="black")+ labs(title = "q-o")
p4<-cowplot::plot_grid(p1, p2, p3, nrow = 2)
p4




#画时序图
SPEED_20s=ts(s1$A_SPEED,start=0,deltat=20)
SPEED_5min=ts(s2$A_SPEED,start=0,deltat=300)
SPEED_15min=ts(s3$A_SPEED,start=0,deltat=900)
plot(SPEED_20s, col = "blue", xlim = c(0, 86400), ylim = range(SPEED_20s, SPEED_5min, SPEED_15min),ylab = "SPEED")
lines(SPEED_5min, col = "red")
lines(SPEED_15min, col = "green")

VOLUME_20s=ts(s1$A_VOLUME,start=0,deltat=20)
VOLUME_5min=ts(s2$A_VOLUME,start=0,deltat=300)
VOLUME_15min=ts(s3$A_VOLUME,start=0,deltat=900)
plot(VOLUME_20s, col = "blue", xlim = c(0, 86400), ylim = range(VOLUME_20s, VOLUME_5min, VOLUME_15min), ylab = "VOLUME")
lines(VOLUME_5min, col = "red")
lines(VOLUME_15min, col = "green")

OCCUPY_20s=ts(s1$A_OCCUPY,start=0,deltat=20)
OCCUPY_5min=ts(s2$A_OCCUPY,start=0,deltat=300)
OCCUPY_15min=ts(s3$A_OCCUPY,start=0,deltat=900)
plot(OCCUPY_20s, col = "blue", xlim = c(0, 86400), ylim = range(OCCUPY_20s, OCCUPY_5min, OCCUPY_15min),ylab = "OCCUPY")
lines(OCCUPY_5min, col = "red")
lines(OCCUPY_15min, col = "green")




# 时间间隔为15min的流量和速度时变图
plot(SPEED_15min,col="white",xlim=c(0,661),ylim=c(0,750))
lines(s3$A_VOLUME/4,col="blue")
lines(10*s3$A_SPEED,col="green")
legend("topright", legend = c("A_VOLUME", "A_SPEED", "A_OCCUPY"),col = c("blue", "green", "red"), lty = 1)
lines(5*s3$A_OCCUPY,col="red")#加上占有率更好地体现v和o之间的同一性



# 求车长
veh_len <- s3$A_SPEED*s3$A_OCCUPY*20/(s3$A_VOLUME*3.6)
plot(veh_len, col = "blue", xlim = c(0, 700), ylim = c(0, 5), ylab = "vehicle_length_distribution")
legend("topright",legend = c ("veh_len"), col = c("blue"), pch = 1)
# 车长柱状分布图
hist(veh_len,main="length distribution",ylim = c(0,600),xlab="unit:m",ylab = "frequency")

# 平均车长
aver_len <- sum(veh_len)/length(veh_len)





