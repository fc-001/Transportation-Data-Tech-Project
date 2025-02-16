library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(zoo)
library(VIM)
library(Metrics)
# 任务一：故障数据检测
# 读取并整理数据一
xls1_sheet1 <- read_excel("E:\\Program Files\\TDA\\task3\\tmp001.xls",sheet = "SQL Results")
xls1_sheet2 <- read_excel("E:\\Program Files\\TDA\\task3\\tmp001.xls",sheet = "SQL Results (2)")
xls1_sheet3 <- read_excel("E:\\Program Files\\TDA\\task3\\tmp001.xls",sheet = "SQL Results (3)")
xls2 <- read_excel("E:\\Program Files\\TDA\\task3\\tmp002.xls",sheet = "SQL Results")
xls3_sheet1 <- read_excel("E:\\Program Files\\TDA\\task3\\tmp003.xls",sheet = "SQL Results")
xls3_sheet2 <- read_excel("E:\\Program Files\\TDA\\task3\\tmp003.xls",sheet = "SQL Results (2)")
xls3_sheet3 <- read_excel("E:\\Program Files\\TDA\\task3\\tmp003.xls",sheet = "SQL Results (3)")
xls4 <- read_excel("E:\\Program Files\\TDA\\task3\\tmp004.xls",sheet = "SQL Results")
xls5 <- read_excel("E:\\Program Files\\TDA\\task3\\tmp005.xls",sheet = "SQL Results")
data <- bind_rows(xls1_sheet1,xls1_sheet2,xls1_sheet3,xls2,xls3_sheet1,xls3_sheet2,xls3_sheet3,xls4,xls5)
data <- filter(data,FSTR_LOOPGROUPID == "NHNX39(1)")
data <- data[order(data$FDT_TIME),] # 按时间升序排序
data <- data[,c(2:6)] # 筛选出column2~6的数据
# (1)独立判断法
speed_limitation <- 80*1.3 # 限速80
occupation_limitation <- 95 # 占有率阈值95
volume_limitation <- 17 # 流量阈值17
data11 <- filter(data,FINT_SPEED > speed_limitation | FINT_OCCUPY > occupation_limitation | FINT_VOLUME > volume_limitation)
data1 <- anti_join(data,data11) 
m1 <- nrow(data) - nrow(data1) # 无效记录数,m2,m3也是
m1
# (2)联合判别法
data21 <- filter(data1,FINT_VOLUME > 0 & FINT_SPEED == 0) # 筛选出异常情况
data22 <- filter(data1,FINT_VOLUME == 0 & FINT_SPEED > 0)
data23 <- filter(data1,FINT_VOLUME > 0 & FINT_SPEED > 0 & FINT_OCCUPY == 0)
data24 <- filter(data1,FINT_VOLUME == 0 & FINT_SPEED == 0 & FINT_OCCUPY > 0)
data2 <- anti_join(data1,data21)%>%anti_join(data22)%>%anti_join(data23)%>%anti_join(data24)# 找到data中在data21、data22、data23、data24中都存在的行。
m2 <- nrow(data1) - nrow(data2)
m2
# (3)平均有效车长法
avg_length <- (data2$FINT_SPEED / 3.6) * (data2$FINT_OCCUPY / 100) / (data2$FINT_VOLUME / 20)  
# 把计算出的平均有效车长加到 data 中  
data3 <- cbind(data2,avg_length)
# 过滤平均有效车长不在 [3, 12] 范围内的数据  
data31 <- filter(data3, avg_length < 3 | avg_length > 12)
data3 <- anti_join(data2,data31)
m3 <- nrow(data2)-nrow(data3)
m3

# 分别计算20s数据的小时有效性和日有效性
# 小时有效性
data3$FDT_TIME <- as.POSIXct(data3$FDT_TIME, format = "%Y-%m-%d %H:%M:%S")
# 计算每小时的数据量hour_counts
hour_counts <- data3 %>%
  group_by(date_hour = format(FDT_TIME, "%Y-%m-%d %H", trim = FALSE)) %>%
  summarise(count = n())
hour_counts1 <- hour_counts$count/180
h_validity <- sum(hour_counts1)/24/7
h_validity
# 计算日有效性
data3$FDT_TIME <- as.character(data3$FDT_TIME)
day_counts <- data3 %>%  # 计算每日的数据量day_counts
  group_by(day = as.integer(strftime(data3$FDT_TIME, "%d"))) %>%  
  summarise(count = n())
day_counts1 <- day_counts$count / (180*24)
d_validity <- sum(day_counts1)/7
d_validity
# 时序图
plot(hour_counts1,col = 'black',xlab='hour',ylab='Validity',xlim=c(0,180))
plot(day_counts1,col='blue', xlab='day', ylab='Validity',xlim = c(0,8))


# 任务二：故障数据修复
# (1)随机清空100行
imputation_data <- read_csv('E:\\Program Files\\TDA\\task3\\data_for_imputation.csv')
imputation_data1 <- imputation_data
selected_rows <- sample(1:3597, 100) # 先保存被选中的行
origin_values <- imputation_data[selected_rows, "speed_391_23"] # 原值另存
origin_values <- unlist(origin_values)
imputation_data[selected_rows, "speed_391_23"] <- NA # 赋为空值
# (2)数据修复
# (2.1) KNN
# k=3
imputation_3 <- kNN(imputation_data, variable = 'speed_391_23', k = 3)
repaired_3 <- imputation_3[selected_rows, "speed_391_23"] # 提取被修复的值
# k=5
imputation_5 <- kNN(imputation_data,variable = 'speed_391_23', k = 5)
repaired_5 <- imputation_5[selected_rows,'speed_391_23'] # 同理
# 误差比较
rmse_3 <- rmse(origin_values,repaired_3) # 均方根误差
rmse_5 <- rmse(origin_values,repaired_5)
rmse_3
rmse_5
mape_3 <- mape(origin_values,repaired_3) # 平均绝对百分比误差
mape_5 <- mape(origin_values,repaired_5)
mape_3
mape_5
# (2.2)#相邻时间平均值法
# 窗口大小取值=5
fill_missing_values <- function(column) {
  column_na <- is.na(column)
  for (i in 1:length(column)) {
    if (column_na[i]) {
      neighbors <- c()
      k <- 1
      while (length(neighbors) < 5) {
        if (!is.na(column[i - k])) {
          neighbors <- c(column[i - k], neighbors)
        }
        k <- k + 1
      }
      k <- 1
      while (length(neighbors) < 10 & length(neighbors) >= 5) {
        if (!is.na(column[i + k])) {
          neighbors <- c(neighbors, column[i + k])
        }
        k <- k + 1
      }
      if (length(neighbors) == 10) {
        column[i] <- mean(neighbors)
      }
    }
  }
  return(column)
}
n_imputed_5 <- fill_missing_values(imputation_data$speed_391_23)  # 调用函数修复缺失值
n_imputed_5_col <- as.matrix(n_imputed_5)  # 转化为列向量
n_repaired_5 <- n_imputed_5_col[selected_rows]  # 提取对应行

# 窗口大小取值=3
fill_missing_values <- function(column) {
  column_na <- is.na(column)
  for(i in 1:length(column)){
    if(is.na(column[i])){
      neighbors<-c()
      k<-1
      while(length(neighbors)<3){
        if(!is.na(column[i-k])){
          neighbors<-c(column[i-k],neighbors)
        }
        k<-k+1
      }
      k<-1 
      while(length(neighbors)<6&length(neighbors)>=3){
        if(!is.na(column[i+k])){
          neighbors<-c(neighbors,column[i+k])
        }
        k<-k+1
      }
      if(length(neighbors)==6){
        column[i]<-mean(neighbors)
      }
    }
  }
  return(column)
}
n_imputed_3 <- fill_missing_values(imputation_data$speed_391_23)
n_imputed_3_col <- as.matrix(n_imputed_3)
n_repaired_3 <- n_imputed_3_col[selected_rows]
# 误差比较
rmse3 <- rmse(origin_values,n_repaired_3)
rmse5 <- rmse(origin_values,n_repaired_5)
rmse3
rmse5
mape3 <- mape(origin_values,n_repaired_3)
mape5 <- mape(origin_values,n_repaired_5)
mape3
mape5


# 任务三：基于数据一的数据平滑处理
# 选取检测器NHNX39(1), 20s间隔, 24日16:00~18:00的速度序列
NHNX39_speed <- data[data$FSTR_LOOPGROUPID == 'NHNX39(1)' & as.Date(data$FDT_TIME) == as.Date('2010-04-24') & format(data$FDT_TIME,"%H:%M:%S") >= "16:00:00" & format(data$FDT_TIME,"%H:%M:%S") <= "18:00:00", c("FDT_TIME","FINT_SPEED")]
ma_NHNX39 <- NHNX39_speed
# (3.1)移动平均法
library(forecast) # 导入forecast库以调用ma()函数
ma_NHNX39_3 <- forecast::ma(NHNX39_speed$FINT_SPEED,order = 3) # 窗口大小为3
ma_NHNX39_5 <- forecast::ma(NHNX39_speed$FINT_SPEED,order = 5) # 窗口大小为5
# 序列可视化
comparsion3 <- plot(NHNX39_speed$FDT_TIME,NHNX39_speed$FINT_SPEED,type='l',col = 'blue',xlab='时间',ylab='速度')
lines(NHNX39_speed$FDT_TIME,ma_NHNX39_3,col='red') # 窗口大小=3与原数据进行比较
legend('topright',legend = c('origin','ma3'),col=c('blue','red'),lty=1)
title(main='移动平均法(order=3)')
comparsion5 <- plot(NHNX39_speed$FDT_TIME,NHNX39_speed$FINT_SPEED,type='l',col = 'blue',xlab='时间',ylab='速度')
lines(NHNX39_speed$FDT_TIME,ma_NHNX39_5,col='red')
legend('topright',legend = c('origin','ma5'),col=c('blue','red'),lty=1) # 窗口大小=5与原数据进行比较
title(main='移动平均法(order=5)')
# 合在一起比较一下
comparsion_all <- plot(NHNX39_speed$FDT_TIME,NHNX39_speed$FINT_SPEED,type='l',col = 'black',xlab='时间',ylab='速度')
lines(NHNX39_speed$FDT_TIME,ma_NHNX39_3,col='green')
lines(NHNX39_speed$FDT_TIME,ma_NHNX39_5,col='red')
legend('topright',legend = c('origin','ma3','ma5'),col=c('black','green','red'),lty=1)
grid()
title(main = '移动平均法')

# (3.2)简单指数平滑
myfunction<-function(data,alpha)  # 形参分别是要平滑的数据和平滑因子alpha
{
  n<-length(data)
  smoothed_speed_data<-numeric(n)
  smoothed_speed_data[1]<-data[1]
  for(i in 2:n)
  {
    smoothed_speed_data[i]<-alpha*data[i]+(1-alpha)*smoothed_speed_data[i-1]
  }
  return(smoothed_speed_data)
}
# 调用函数并且画图
alpha1<-0.3
NHNX39_speed$smoothed_speed_0.3<-myfunction(NHNX39_speed$FINT_SPEED,alpha1)
alpha2<-0.6
NHNX39_speed$smoothed_speed_0.6<-myfunction(NHNX39_speed$FINT_SPEED,alpha2)
p4<-ggplot(NHNX39_speed,aes(x=FDT_TIME))+geom_line(aes(y=FINT_SPEED,color="Original Speed"))+geom_line(aes(y=smoothed_speed_0.3,color="alpha=0.3"))+geom_line(aes(y=smoothed_speed_0.6,color="alpha=0.6"))+scale_color_manual(values=c("green","red","black"))+labs(title="简单指数平滑",x="时间",y="速度")
p4



