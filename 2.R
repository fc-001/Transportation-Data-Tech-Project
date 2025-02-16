library(tidyverse)
library(readxl)
library(stringr)
library(ggplot2)

data <- read_excel("E:\\Program Files\\TDA\\task2\\metro_ic_card.xlsx")
#构建完整时间序列
data$ftime <- paste(data$date,data$time, sep="")
data$ftime <- strptime(data$ftime, format = "%Y%m%d %H%M%S") #改一改时间格式年月日时分秒
#删除异常
data1 <- data[order(data$ID,data$ftime),]
diff1 <- diff(data1$ID) #差分卡号（乘客）
diff2 <- diff(data1$type) #区分交易类型
diff3 <- data.frame(diff1,diff2) #合并差分值
diff4 <- data.frame(1,1)
colnames(diff4)[1] = "diff1"
colnames(diff4)[2] = "diff2" #创建diff4,更改列名等待填充数据
diff5 <- rbind(diff3,diff4) #diff5将diff3&diff4按行合并
data2 <- filter(data1,diff5$diff1 !=0 | diff5$diff2 !=0) #在data1中筛选正常乘车记录

data3 <- arrange(summarise(group_by(data2,address),total = n()),desc(total))%>%head(3)
data4<-data2%>%filter(data2$address%in%data3$address)
#line20:将data2里的交易地址分组之后计算每个分组里的个数得到total,再把total降序排列后把前三列赋值给data3
#line21:筛选出data2里和data3中一样的地址，把结果赋值给data4

data4$ftime <- as.POSIXct(data4$ftime)#将data4中的时间改为POSIXct格式
data4$时刻 <- floor_date(data4$ftime,"5minutes")#将data4中的时间向下舍去，以5min为间隔
data5 <- data4%>%group_by(data4$address,data4$时刻,data4$type)%>%summarise(count=n(),.groups = "drop")
#line27:把data4中的地址时刻交易类型分组后计算组数
colnames(data5)[1]="站名"
colnames(data5)[2]="时刻"
colnames(data5)[3]="交易类型"
colnames(data5)[4]="人数"

#画出data3的三个最大的地铁站的进出站客流时变图
data6 <- filter(data5, 交易类型 == 21) # 筛选出进站的人
p1 <- ggplot(data = data6, aes(x = 时刻, y = 人数, group = 站名, color = factor(站名))) +
  ggtitle("进站客流时变曲线") +
  xlab("Time") +
  ylab("Number") +
  scale_y_continuous(breaks = seq(0, 600, 100)) + #y轴范围(0,600),间隔为100
  geom_line() + #折线图
  geom_point() + #散点图
  scale_color_manual(values = c("red", "blue", "green"))
p1

data7 <- filter(data5, 交易类型 == 22)
p2 <- ggplot(data = data7, aes(x = 时刻, y = 人数, group = 站名, color = factor(站名))) +
  ggtitle("出站客流时变曲线") +
  xlab("Time") +
  ylab("Number") +
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("red", "blue", "green"))
p2

#站点OD
#挑十个站点
records <- filter(data2,address == "1268001000"|address == "1268002000"|address == "1268003000"|address == "1268004000"|address == "1268005000"|address == "1268006000"|address == "1268007000"|address == "1268008000"|address == "1268009000"|address == "1268011000")
entry <- filter(records,type==21) #进站
exit <- filter(records,type==22) #出站
passenger_data <- inner_join(entry,exit,by="ID")#将乘客的进出站记录一一对应
as.POSIXct(passenger_data$ftime.x)
as.POSIXct(passenger_data$ftime.y)
passenger_data$difftime12 <- difftime(passenger_data$ftime.y,passenger_data$ftime.x,units = "secs")
valid_data <- filter(passenger_data,passenger_data$difftime12 >0) #这行和上一行是把出站时间减去进站时间，把正常的记录保留下来
v_data <- valid_data[order(valid_data$ID,valid_data$ftime.x,valid_data$ftime.y),] #把数据排序
diff6 <- diff(v_data$ID)
diff7 <- diff(v_data$ftime.x)
diff8 <- data.frame(diff6,diff7) #连续进站的ID和时间的差值，diff8将其一一对应
diff9 <- data.frame(1,1)
colnames(diff9)[1] = "diff6"
colnames(diff9)[2] = "diff7"
diff10 <- rbind(diff8,diff9)
v1_data <- filter(v_data,diff10$diff6 !=0 | diff10$diff7 !=0)
v2_data <- v1_data[v1_data$address.x!= v1_data$address.y,]
v2_data #将连续进站记录去掉
OD <- v2_data%>%group_by(address.x,address.y)%>%summarise(travel_count=n())%>%spread(address.y,travel_count,fill=0)
OD

# 1268006000和1268007000之间的,也就是6进站7出站的平均出行时间计算
st6 <- "1268006000"
st7 <- "1268007000"
records67 <- v2_data[(v2_data$address.x == st6 & v2_data$address.y == st7), ]
intervals <- difftime(records67$ftime.y,records67$ftime.x,units = "mins")
aver_interval = mean(intervals)
aver_interval


