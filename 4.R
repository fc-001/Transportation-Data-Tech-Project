library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2) 
library(sf)
library(lubridate)
library(ggmap)
#register_stadiamaps(key='ed870b84-66a8-47aa-bb45-dc22569ae3ca')  # stadiamaps注册之后的api
library(httr)
library(jsonlite)
library(png)
library(reshape2)

# 1.订单概况
# 读取数据转换格式
data <- read_excel('/Users/fancheng/Library/CloudStorage/OneDrive-tongji.edu.cn/作业/大二上/交通运输数据技术/task4/出租车订单可视化数据/taxi_order_21080418.xlsx')
data$stime <- as.POSIXct(data$stime,format="%Y-%m-%d %H:%M:%S")  # 将时间格式转化为POSIxct
data$etime <- as.POSIXct(data$etime,format="%Y-%m-%d %H:%M:%S")
# 进行筛选
data <- na.omit(data)  # 删去缺失值
data <- data[!duplicated(data), ]  # 剔除重复
data <- subset(data,etime > stime)  # 到达时间晚于出发时间
start_date <- as.POSIXct('2018-04-18',format='%Y-%m-%d')
data <- subset(data,stime >= start_date)
data <- subset(data,gcj_s_lng != gcj_e_lng | gcj_s_lat != gcj_e_lat)

data$start_hour <- floor_date(data$stime,unit="hour")
data$end_hour <- floor_date(data$etime,unit="hour")  # 计算订单开始和结束的时刻
start_counts <- data %>% group_by(start_hour)%>%summarise(total_start = n())  # 每小时的出发订单量
end_counts <- data %>% group_by(end_hour)%>%summarise(total_end = n())  # 到达
combined_counts <- full_join(start_counts, end_counts, by = c("start_hour" = "end_hour"))  # 合并数据
colnames(start_counts)[1]<-"hour"
colnames(end_counts)[1]<-"hour"
# 绘制上海市订单生成和完成数的小时变化的曲线图
p0 <- ggplot() +
  geom_line(data = combined_counts, aes(x = start_hour, y = total_start, color = "订单生成量")) +
  geom_point(data = combined_counts, aes(x = start_hour, y = total_start, color = "订单生成量")) +
  geom_line(data = combined_counts, aes(x = start_hour, y = total_end, color = "订单完成量")) +
  geom_point(data = combined_counts, aes(x = start_hour, y = total_end, color = "订单完成量")) +
  labs(x = "时间(小时)", y = "订单量(单)", color = "图例") + ggtitle('上海市订单生成和完成数的小时变化')
  scale_color_manual(values = c("出租车订单生成量" = "lightblue", "出租车订单完成量" = "red"))
p0
# 订单时长分布图
df_duration <- data.frame(duration <- difftime(data$etime, data$stime, units='mins'))
p1 <- ggplot(df_duration, aes(x = duration)) +
  geom_histogram(binwidth = 1, fill = "wheat", color = "black") +
  labs(x = "订单时长(分钟)", y = "订单数量", title = "订单时长分布")
p1
# 订单里程分布图
df_distance <- data.frame(distance <- data$dis)
p2 <- ggplot(df_distance, aes(x = data$dis)) +
  geom_histogram(binwidth = 0.5, fill = 'lightpink', color = 'black') + 
  labs(x = "订单里程(km)", y = "订单数量", title = "订单里程分布")
p2
df_distance$distance_category <- cut(df_distance$distance, breaks = c(0, 10, 30, Inf), labels = c("0-10km", "10-30km", ">30km"))
df_distance$prop <- with(df_distance, prop.table(table(distance_category)))
counts <- table(df_distance$distance_category)
props <- prop.table(counts)  # 计算每个类别的占比
labels <- paste(names(props), "\n", round(props * 100, 2), "%")
pie(counts, labels = labels, col = c("red", "purple", "blue"), main = "里程分布图") # 里程饼图

# 所有出租车每天完成的订单数量分布
data$date <- as.Date(data$etime)
daily_counts <- data %>% group_by(carID, date) %>% summarise(total = n())  # 每日订单量
p3 <- ggplot(daily_counts, aes(x = total)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(x = "车辆接受订单数", y = "频数", title = "所有出租车每天完成的订单数量分布") +
  scale_x_continuous(limits = c(1, 60)) + 
  scale_y_continuous(limits = c(0, 550))    
p3

# 2.地理可视化
# 绘制上海市不同行政区出发与完成订单数量分布图
# 热力图
shanghai_map<-st_read("/Users/fancheng/Library/CloudStorage/OneDrive-tongji.edu.cn/作业/大二上/交通运输数据技术/task4/出租车订单可视化数据/出租车订单可视化数据/shanghai_districts/shanghai_districts.shp")
order_count1<-data%>%group_by(s_district_name)%>%summarise(order_num1=n())  # 每个出发区的订单量
order_count2<-data%>%group_by(e_district_name)%>%summarise(order_num2=n())  # 每个到达区的订单量
sh_map_order1<-left_join(shanghai_map,order_count1,by=c("name"="s_district_name"))  # 以行政区名称为键将shanghai_map和order_count2进行连接
sh_map_order2<-left_join(shanghai_map,order_count2,by=c("name"="e_district_name"))
p5<-ggplot()+geom_sf(data=sh_map_order1,aes(fill=order_num1))+scale_fill_gradient(low="lightblue",high="blue")+labs(title="上海市各行政区出发订单数量分布图",fill="订单数量")
p5  # 出发
p6<-ggplot()+geom_sf(data=sh_map_order2,aes(fill=order_num2))+scale_fill_gradient(low="beige",high="orange")+labs(title="上海市各行政区完成订单数量分布图",fill="订单数量")
p6  # 完成  选点明显的颜色来更好地展现效果
# 条形图
order_counts <- full_join(order_count1,order_count2,  by = c("s_district_name"="e_district_name"))  # 还是以区名为键把两张表合并到一起
order_count <- reshape2::melt(order_counts,id.vars="s_district_name")
ggplot(order_count, aes(x = s_district_name, y = order_count$value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("order_num1" = "lightblue", "order_num2" = "purple"), 
                    labels = c("order_num1" = "出发量", "order_num2" = "到达量")) +
  labs(x = "行政区域", y = "订单量", title = "不同行政区出租车订单量条形分布图")

# 使用全天不同时段的OD的数据结合地图绘制热力图
# 先绘制小时分布图,观察分布情况
data$stime_hour <- format(as.POSIXct(data$stime, format = "%Y-%m-%d %H:%M:%S"), "%H")
data$etime_hour <- format(as.POSIXct(data$etime, format = "%Y-%m-%d %H:%M:%S"), "%H")
s_counts <- data.frame(hour = unique(data$stime_hour), count = table(data$stime_hour), type = "出发量")  # 每小时出发量
e_counts <- data.frame(hour = unique(data$etime_hour), count = table(data$etime_hour), type = "到达量")  # 每小时到达量
se_counts <- rbind(s_counts, e_counts)  # 合并数据
ggplot(se_counts, aes(x = se_counts$count.Var1, y = se_counts$count.Freq, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("出发量" = "darkgray", "到达量" = "pink")) +
  labs(x = "时间", y = "数量", fill = "类型", title = "全天订单量小时分布")
# 6:00~10:00早高峰时期
#map <- ggmap::get_stadiamap(maptype='stamen_toner')
data$stime_hour <- format(data$stime, "%H:%M:%S")
data$etime_hour <- format(data$etime, "%H:%M:%S")
morning_data <- data[data$stime_hour >= "06:00:00" & data$stime_hour <= "10:00:00",]
p7 <- ggplot() +
  geom_sf(data = sh_map_order1) +
  stat_density_2d(data = morning_data, aes(x = gcj_s_lng, y = gcj_s_lat, fill = ..level..), geom = "polygon") +
  geom_density_2d(data = morning_data, aes(x = gcj_s_lng, y = gcj_s_lat), size = 0.3) +  # 添加等高线
  scale_fill_gradient(low = "lightblue", high = "orange") +
  theme_classic()+
  ggtitle('6:00~10:00出租车订单出发量热力图')+
  xlab('经度')+
  ylab('纬度')
p7+xlim(121.25,121.65)+ylim(31.05,31.40)  # 出发量图
p7+xlim(121,122)+ylim(30.5,31.9) # 全域

morning_data1 <- data[data$etime_hour >= "06:00:00" & data$stime_hour <= "10:00:00",]
p8 <- ggplot() +
  geom_sf(data = sh_map_order1) +
  stat_density_2d(data = morning_data1, aes(x = gcj_s_lng, y = gcj_s_lat, fill = ..level..), geom = "polygon") +
  geom_density_2d(data = morning_data1, aes(x = gcj_s_lng, y = gcj_s_lat), size = 0.3) +
  scale_fill_gradient(low = "yellow", high = "purple") +
  theme_classic()+
  ggtitle('6:00~10:00出租车订单到达量热力图')+
  xlab('经度')+
  ylab('纬度')
p8+xlim(121.25, 121.65)+ylim(31.05, 31.40)  # 到达量图
p8+xlim(121,122)+ylim(30.5,31.9) # 全域

# 12:00~18:00下午时段
afternoon_data <- data[data$stime_hour >= "12:00:00" & data$stime_hour <= "18:00:00",]
p9 <- ggplot() +
  geom_sf(data = sh_map_order1) +
  stat_density_2d(data = afternoon_data, aes(x = gcj_s_lng, y = gcj_s_lat, fill = ..level..), geom = "polygon") +
  geom_density_2d(data = afternoon_data, aes(x = gcj_s_lng, y = gcj_s_lat), size = 0.3) +  # 添加等高线
  scale_fill_gradient(low = "lightblue", high = "orange") +
  theme_classic()+
  ggtitle('12:00~18:00出租车订单出发量热力图')+
  xlab('经度')+
  ylab('纬度')
p9+xlim(121.25,121.65)+ylim(31.05,31.40)  # 出发量图
p9+xlim(121,122)+ylim(30.5,31.9)  # 全域

afternoon_data1 <- data[data$etime_hour >= "12:00:00" & data$stime_hour <= "18:00:00",]
p10 <- ggplot() +
  geom_sf(data = sh_map_order1) +
  stat_density_2d(data = afternoon_data1, aes(x = gcj_s_lng, y = gcj_s_lat, fill = ..level..), geom = "polygon") +
  geom_density_2d(data = afternoon_data1, aes(x = gcj_s_lng, y = gcj_s_lat), size = 0.3) +
  scale_fill_gradient(low = "yellow", high = "purple") +
  theme_classic()+
  ggtitle('12:00~18:00出租车订单到达量热力图')+
  xlab('经度')+
  ylab('纬度')
p10+xlim(121.25, 121.65)+ylim(31.05, 31.40)  # 到达量图
p10+xlim(121,122)+ylim(30.5,31.9)  # 全域

# 18:00~24:00  晚间
evening_data <- data[data$stime_hour >= "18:00:00" & data$stime_hour <= "24:00:00",]
p11 <- ggplot() +
  geom_sf(data = sh_map_order1) +
  stat_density_2d(data = evening_data, aes(x = gcj_s_lng, y = gcj_s_lat, fill = ..level..), geom = "polygon") +
  geom_density_2d(data = evening_data, aes(x = gcj_s_lng, y = gcj_s_lat), size = 0.3) +  # 添加等高线
  scale_fill_gradient(low = "lightblue", high = "orange") +
  theme_classic()+
  ggtitle('18:00~24:00出租车订单出发量热力图')+
  xlab('经度')+
  ylab('纬度')
p11+xlim(121.25,121.65)+ylim(31.05,31.40)  # 出发量图
p11+xlim(121,122)+ylim(30.5,31.9)  # 重点是浦东机场

evening_data1 <- data[data$etime_hour >= "18:00:00" & data$stime_hour <= "24:00:00",]
p12 <- ggplot() +
  geom_sf(data = sh_map_order1) +
  stat_density_2d(data = evening_data1, aes(x = gcj_s_lng, y = gcj_s_lat, fill = ..level..), geom = "polygon") +
  geom_density_2d(data = evening_data1, aes(x = gcj_s_lng, y = gcj_s_lat), size = 0.3) +
  scale_fill_gradient(low = "yellow", high = "purple") +
  theme_classic()+
  ggtitle('18:00~24:00出租车订单到达量热力图')+
  xlab('经度')+
  ylab('纬度')
p12+xlim(121.3, 121.6)+ylim(31.10, 31.35)  # 到达量图
p12+xlim(121,122)+ylim(30.5,31.9)  # 浦东机场

