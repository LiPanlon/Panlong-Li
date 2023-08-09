library(forestplot)
library(stringr)
library(survival)
library(rms)
# data process fro Air PRS Stroke
data1=read.csv("G:/UKB/Air_Gene_Stroke/Data49793.csv")
data2=read.csv("G:/UKB/Air_Gene_Stroke/PRS/StandPRS_ukb52369.csv")
# id=intersect(data1[,1],data2[,1])
n1=dim(data2)
data3=data1
id1=matrix(NA,nrow=n1[1])
for (i in 1:n1[1])
  {
  id1[i]=which(data1$eid==data2$eid[i])
}
data1=data1[id1,]
write.csv(data1,"G:/UKB/Air_Gene_Stroke/Datapipei.csv")
# 数据重新读入
data1=read.csv("G:/UKB/Air_Gene_Stroke/Datapipei.csv")
# 计算 stroke的年龄 length(id1) 表述stroke的人数
id1=is.na(data1$Date.of.iscaemic.stroke)
id2=which(id1==FALSE)
id3=is.na(data1$Source.of.stroke)
id4=which(id3==FALSE)
data2=data1

# 计算month unlist(gregexpr("/", l1, fixed = TRUE))
n3=length(id2)
m=matrix(NA,nrow=dim(data1)[1],ncol=1)
for (i in 1:n3)
{
  l1=unlist(gregexpr("/",data2$Date.of.iscaemic.stroke[id2[i]],fixed=TRUE))
  m1=as.numeric(substr(data2$Date.of.iscaemic.stroke[id2[i]],(l1[1]+1),(l1[2]-1)))
  l2=unlist(gregexpr("/",data2$Date.attending.UKB[id2[i]],fixed=TRUE))
  m2=as.numeric(substr(data2$Date.attending.UKB[id2[i]],(l2[1]+1),(l2[2]-1)))
  m[id2[i]]=m1-m2
}
data2$Date.of.iscaemic.stroke[id2]=substr(data2$Date.of.iscaemic.stroke[id2],1,4)
data2$Date.of.iscaemic.stroke=as.numeric(data2$Date.of.iscaemic.stroke)
data2$Stroke.age=data2$Date.of.iscaemic.stroke-data2$Birth
x=(data2$Stroke.age-data2$Age_baseline)*12 # *12 换成month
x=x+m
#
id5=which(x>0)
id6=which(x<0)
id=which(x==0)
ll=c(id6,id)
data2$time=x
data2=data2[-ll,]
id7=is.na(data2$time)
id8=which(id7==FALSE)
id9=which(id7==TRUE)
nrow=(length(id8)+length(id9))
matrix1=as.matrix(1:nrow,nrow=nrow,ncol=1)
data2$stats=matrix1
data2$stats[id8]=1
data2$stats[id9]=0
year=2022
month=1
id10=is.na(data2$time)
id11=which(id10==TRUE)
n4=length(id11)
m3=as.matrix(1,nrow=n4,1)
for (i in 1:n4)
{
  l3=unlist(gregexpr("/",data2$Date.attending.UKB[id11[i]],fixed=TRUE))
  m3[i]=as.numeric(substr(data2$Date.attending.UKB[id11[i]],(l3[1]+1),(l3[2]-1)))
}
t1=as.numeric(substr(data2$Date.attending.UKB[id11],1,4))
t=(year-t1)*12+m3-1
data2$time[id11]=t
write.csv(data2,"G:/UKB/Air_Gene_Stroke/Datapipeitime.csv")

# Aire population ---------------------------------------------------------
# air population process
data=read.csv("G:/UKB/Air_Gene_Stroke/Air49793.csv")
NO2=cbind(data$X24003.0.0,data$X24016.0.0,data$X24017.0.0,data$X24018.0.0)
L1=rowMeans(NO2,na.rm=TRUE)
PM10=cbind(data$X24005.0.0,data$X24019.0.0)
L2=rowMeans(PM10,na.rm=TRUE)
L3=data$X24004.0.0
L4=cbind(data$X24006.0.0,data$X24007.0.0,data$X24008.0.0)
data1=data.frame(matrix("",nrow=502415,ncol=1))
data1$eid=data$eid
data1$No=L3
data1$No2=L1
data1$PM2.5absorbance=data$X24007.0.0
data1$PM2.5=data$X24006.0.0
data1$PM2.510=data$X24008.0.0
data1[,8:14]=data[,8:14]
data1$PM10=L2
write.csv(data1,"G:/UKB/Air_Gene_Stroke/PM10.csv")
write.csv(data1,"G:/UKB/Air_Gene_Stroke/Aireprocess.csv")
## 合并 air Gene
air=read.csv("G:/UKB/Air_Gene_Stroke/Aireprocess.csv")
gen=read.csv("G:/UKB/Air_Gene_Stroke/Datapipeitimeprocess3.csv")
id=intersect(air$eid,gen$eid)
n=length(id)
id1=id
for (i in 1:n)
{
  id1[i]=which(air$eid==id[i])
}
air=air[id1,]
# 合并 air gen
data=cbind.data.frame(gen,air)
write.csv(data,"G:/UKB/Air_Gene_Stroke/data3.csv")
# 合并1598 与data3
data3=read.csv("G:/UKB/Air_Gene_Stroke/data3.csv")
data=read.csv("G:/UKB/Air_Gene_Stroke/Data49793_1598.csv")
id=intersect(data3$eid,data$eid)
n=length(id)
id1=id
for (i in 1:n)
{
  id1[i]=which(data$eid==id[i])
}
data=data[id1,]
# 合并 air gen
data=cbind.data.frame(data3,data)
write.csv(data,"G:/UKB/Air_Gene_Stroke/data4.csv")

# SCORE -------------------------------------------------------------------


## 计算 Diet score
data=read.csv("G:/UKB/Air_Gene_Stroke/data4_版本1_异常值替换为null.csv")
# data=data[,-1]
# 英国人口数量
 id1=which(data$Ethnic.background==1001)
# data=data[id1,]
n=dim(data)
Score=data.frame(matrix(nrow=n[1],ncol=1))
# 计算Diet score
Vegetable=data$Cooked.Vegetable+data$Salad.Vegetable
Score$Vegetable=Vegetable
Fruit=data$Fresh.Fruit+data$Dried.Fruit
Score$Fruit=Fruit
Fish=data$Oily.fish+data$no.oily.fish
Score$Fish=Fish
Processedmeat=data$Processed.meat
Score$Processedmeat=Processedmeat
Unprocessedmeat=data$Poulty.intake+data$Beef.intake+data$Lamb.mutton.intake+data$Pork.intake
Score$Unprocessedmeat=Unprocessedmeat
# vegetable
id2=length(Vegetable)
  l1=which(Vegetable>=4)
  l2=which(Vegetable<4)
Vegetablescore=Vegetable
Vegetablescore[l1]=1
Vegetablescore[l2]=0
# Fruit
id3=length(Fruit)
l3=which(Fruit>=3)
l4=which(Fruit<3)
Fruitscore=Fruit
Fruitscore[l3]=1
Fruitscore[l4]=0
# Fish
id3=length(Fish)
l5=which(Fish>=3)
l6=which(Fish<3)
Fishscore=Fish
Fishscore[l5]=1
Fishscore[l6]=0
# processedMeat
id4=length(Processedmeat)
l7=which(Processedmeat<=2)
l8=which(Processedmeat>2)
Processedmeatscore=Processedmeat
Processedmeatscore[l7]=1
Processedmeatscore[l8]=0
# 5 unprocessMeat
id5=length(Unprocessedmeat)
l9=which(Processedmeat<=2)
l10=which(Processedmeat>2)
Unprocessedmeatscore=Unprocessedmeat
Unprocessedmeatscore[l9]=1
Unprocessedmeatscore[l10]=0
# dietScore
dietScore=Vegetablescore+Fruitscore+Fishscore+Processedmeatscore+Unprocessedmeatscore
Score$DietScore=dietScore

# Calculation Score -------------------------------------------------------


## alchohol
# 估计每天饮酒量
weekAlcohol=data[,19:23]/7
weekAlcohol[,6]=data[,30]/7
monthAlcohol=data[,24:29]/30
# week 与month 合并
n1=length(weekAlcohol)
Alcohol=weekAlcohol[,1:2]
Alcohol[,1]=weekAlcohol[,1]
Alcohol[,2]=monthAlcohol[,1]
Alcohol1=rowMeans(Alcohol,na.rm=TRUE)

n1=length(weekAlcohol)
Alcohol=weekAlcohol[,1:2]
Alcohol[,1]=weekAlcohol[,2]
Alcohol[,2]=monthAlcohol[,2]
Alcohol2=rowMeans(Alcohol,na.rm=TRUE)

n1=length(weekAlcohol)
Alcohol=weekAlcohol[,1:2]
Alcohol[,1]=weekAlcohol[,3]
Alcohol[,2]=monthAlcohol[,3]
Alcohol3=rowMeans(Alcohol,na.rm=TRUE)

n1=length(weekAlcohol)
Alcohol=weekAlcohol[,1:2]
Alcohol[,1]=weekAlcohol[,4]
Alcohol[,2]=monthAlcohol[,4]
Alcohol4=rowMeans(Alcohol,na.rm=TRUE)

n1=length(weekAlcohol)
Alcohol=weekAlcohol[,1:2]
Alcohol[,1]=weekAlcohol[,5]
Alcohol[,2]=monthAlcohol[,5]
Alcohol5=rowMeans(Alcohol,na.rm=TRUE)

n1=length(weekAlcohol)
Alcohol=weekAlcohol[,1:2]
Alcohol[,1]=weekAlcohol[,6]
Alcohol[,2]=monthAlcohol[,6]
Alcohol6=rowMeans(Alcohol,na.rm=TRUE)

# AlcoholScore
AlcoholScore=1.5*Alcohol1+1.5*Alcohol2+3*Alcohol3+1*Alcohol4+1*Alcohol5+1.5*Alcohol6
Score$Alcohol1=Alcohol1
Score$Alcohol2=Alcohol2
Score$Alcohol3=Alcohol3
Score$Alcohol4=Alcohol4
Score$Alcohol5=Alcohol5
Score$Alcohol6=Alcohol6
Score$AlcoholScore=AlcoholScore
write.csv(Score,"G:/UKB/Air_Gene_Stroke/Score.csv")
# 手动合并data 与Score 之后生成 data4_score.csv
# data4_score 用SPSSPRO对一些协变量空值进行众数填充
# 下面用 missForest 进行多重预测填充
library("missForest")
data=read.csv("G:/UKB/Air_Gene_Stroke/data4_score_SPSSPRO1.csv")
# 重新计算diet score
# Vegetable
VegetableScore=data$vegetable
id1=which(data$Vegetable>=4)
id2=which(data$Vegetable<4)
VegetableScore[id1]=1
VegetableScore[id2]=0
#
FruitScore=data$Fruit
id3=which(data$Vegetable>=3)
id4=which(data$Vegetable<3)
FruitScore[id1]=1
FruitScore[id2]=0
# 
FishScore=data$Fish
id5=which(data$Fish>=3)
id6=which(data$Fish<3)
FishScore[id1]=1
FishScore[id2]=0
#
ProcessedScore=data$Processedmeat
id7=which(data$Processedmeat<=2)
id8=which(data$Processedmeat<2)
ProcessedScore[id1]=1
ProcessedScore[id2]=0
#
UnprocessedmeatScore=data$Unprocessedmeat
id9=which(data$Unprocessedmeat<=2)
id10=which(data$Unprocessedmeat<2)
UnprocessedmeatScore[id1]=1
UnprocessedmeatScore[id2]=0
DietScore=VegetableScore+FruitScore+FishScore+ProcessedScore+UnprocessedmeatScore
data$DietScore=DietScore

data1=data[,2:4]
data1[,4:7]=data[,7:10]
data1[,8:17]=data[,12:21]
data1[,18:29]=data[,29:40]
write.csv(data1,"G:/UKB/Air_Gene_Stroke/data4process.csv")
data2=missForest(data1,ntree=1000)
### 
data=read.csv("G:/UKB/Air_Gene_Stroke/data4_score_SPSSPRO1.csv")
# 重新计算diet score
# Vegetable
VegetableScore=data$Vegetable
id1=which(data$Vegetable>=4)
id2=which(data$Vegetable<4)
VegetableScore[id1]=1
VegetableScore[id2]=0
#
FruitScore=data$Fruit
id3=which(data$Vegetable>=3)
id4=which(data$Vegetable<3)
FruitScore[id3]=1
FruitScore[id4]=0
# 
FishScore=data$Fish
id5=which(data$Fish>=3)
id6=which(data$Fish<3)
FishScore[id5]=1
FishScore[id6]=0
#
ProcessedScore=data$Processedmeat
id7=which(data$Processedmeat<=2)
id8=which(data$Processedmeat>2)
ProcessedScore[id7]=1
ProcessedScore[id8]=0
#
UnprocessedmeatScore=data$Unprocessedmeat
id9=which(data$Unprocessedmeat<=2)
id10=which(data$Unprocessedmeat>2)
UnprocessedmeatScore[id9]=1
UnprocessedmeatScore[id10]=0
DietScore=VegetableScore+FruitScore+FishScore+ProcessedScore+UnprocessedmeatScore
data$DietScore=DietScore
# summary(data)
# 利用mice 工具包
#install.packages("mice")
library(mice)
imp=mice(data1,m=5)
# missForest
library(missForest)
data2=missForest(data1,ntree=200)
# PRS NA 16160
data1=data
summary(data)
l1=is.na(data$time)
l2=which(l1==TRUE)
data$time[l2]=166
# 删除PRS缺失的数据
l3=is.na(data$PRS)
l4=which(l3==FALSE)
data1=data1[l4,]
# 删除 缺失NO2的数据
l5=is.na(data1$No2)
l6=which(l5==FALSE)
data1=data1[l6,]
summary(data1)
# 删除 缺失NO的数据
l5=is.na(data1$No)
l6=which(l5==FALSE)
data1=data1[l6,]
summary(data1)
# PM2.5
l7=is.na(data1$PM2.5)
l8=which(l7==FALSE)
data1=data1[l8,]
summary(data1)
write.csv(data1,"G:/UKB/Air_Gene_Stroke/data5.csv")
# 利用随机森林对alcohol进行预测
data1=read.csv("G:/UKB/Air_Gene_Stroke/data5.csv")
data1=data1[,-1]
l9=is.na(data1$Alcohol1)
l10=which(l9==FALSE)
data1=data1[l10,]
summary(data1)
#
l11=is.na(data1$Alcohol2)
l12=which(l11==FALSE)
data1=data1[l12,]
#
summary(data1)
l13=is.na(data1$Alcohol3)
l14=which(l13==FALSE)
data1=data1[l14,]
#
summary(data1)
l15=is.na(data1$Alcohol4)
l16=which(l15==FALSE)
data1=data1[l16,]
#
summary(data1)
l17=is.na(data1$Alcohol5)
l18=which(l17==FALSE)
data1=data1[l18,]
#
write.csv(data1,"G:/UKB/Air_Gene_Stroke/data5_1.csv")
summary(data1)
## SPSSPRO 对alcoho6进行纵数填补
data=read.csv("G:/UKB/Air_Gene_Stroke/data5_2.csv")
summary(data)
AlcoholScore=1.5*data$Alcohol1+1.5*data$Alcohol2+2.5*data$Alcohol3+1*data$Alcohol4+1*data$Alcohol5+1.5*data$Alcohol6
data$AlcoholScore=AlcoholScore
write.csv(data,"G:/UKB/Air_Gene_Stroke/data5_3.csv")
# 
data2=data1
# 去除人种差异 ------------------------------------------------------------------

data=read.csv("G:/UKB/Air_Gene_Stroke/data5_3.csv")
# 人种 ethnic.background
data1=data
id1=which(data$Ethnic.background==1001)
data1=data[id1,]
data1$Center=data1$Center-11000
id2=which(data$Center<0)
data1$Center[id2]=11
id3=which(data$time<0)
## 将PM10添加进来
n=dim(data1)
for (i in c(1:n[1]))
     {
       id=which(data2$eid==data1$eid[i])
       data1$PM10[i]=data2$PM10[id]
}
write.csv(data1,"G:/UKB/Air_Gene_Stroke/data5_4.csv")
##
# 删除 education 为0人群
# id3=which(data1$Education==0)
# data1=data1[-id3,]

# data1$DietScore=as.factor(data1$DietScore)
## 计算 airpopulation score
AirPolluScore=(0.004*data1$NOx+0.049*data1$PM2.5_10
-0.006*data1$NO2-0.015*data1$PM10+
  0.124*data1$PM2.5)*5/(0.004+0.049-0.006-0.015+0.124)
data1$Air_Score=AirPolluScore
write.csv(data1,"G:/UKB/Air_Gene_Stroke/data5_5.csv")
# coxregression
data=read.csv("G:/UKB/Air_Gene_Stroke/data5_5_Q.csv")
data1=data
library(survival)
library(rms)
data1=data1[,-1]
# 生成factor
data1$Sex <- factor(data1$Sex, levels=c(1,2), labels=c("Female","Male"))
data1$Center <- as.factor(data1$Center)
data1$THI=as.factor(data1$THI)
data1$Education=as.factor(data1$Education)
data1$Smoking.Status=factor(data1$Smoking.Status,levels=c(1,2,3),labels=c("Never","Previous","Current"))
data1$stats=as.numeric(data1$stats)
# f1=psm(Surv(time,stats) ~., data=data1)
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
           BMI+Age_baseline+PRS+Air_Score+PRS*BMI+
           DietScore+AlcoholScore, data=data1)
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+PRS+PM2.5+PM2.5_10+PM10+NOx+NO2+
                DietScore+AlcoholScore, data=data1)
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+PRS+PM2.5+PM2.5_10+PM10+NOx+NO2+
                DietScore+AlcoholScore+PM2.5*PRS, data=data1)
summary(mul_cox)
# 画图
##画森林图的包
# install.packages("forestplot")
# install.packages("stringr")
library(forestplot)
library(stringr)

#一-2 multi1：提取：变量+HR+95%CI+95%CI
mul_cox1 <- summary(mul_cox)
colnames(mul_cox1$conf.int)
multi1<-as.data.frame(round(mul_cox1$conf.int[, c(1, 3, 4)], 2))
#一-3、multi2：提取：HR(95%CI)和P
#install.packages("tableone")
library(tableone)
multi2<-ShowRegTable(mul_cox, 
                     exp=TRUE, 
                     digits=2, 
                     pDigits =3,
                     printToggle = TRUE, 
                     quote=FALSE, 
                     ciFun=confint)
#一-4.将两次提取结果合并成表；取名result
result <-cbind(multi1,multi2);result
#一-5.行名转为表格第一列，并给予命名"Characteristics"
result<-tibble::rownames_to_column(result, var = "Characteristics");result
#
fig1<- forestplot(result[,c(1,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result[,3],  #告诉函数表格第3列为95%CI，
                  upper=result[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.3,       #设置小黑块的大小
                  graph.pos=2)       #森林图应插在图形第2列
fig1
# 图像美化
#1.删除部分变量名，只保留亚变量即："ER_Positive"变为"Positive"
result$Characteristics<-str_remove(result$Characteristics,"Sex|N|ER|HER2|G")

#2. 给参考变量插入空行
#2-1.这步代码不用改
ins <- function(x) {c(x, rep(NA, ncol(result)-1))}
##2-2：插入空行，形成一个新表
for(i in 5:6) {result[, i] = as.character(result[, i])}
result<-rbind(c("Characteristics", NA, NA, NA, "HR(95%CI)","p"),
              ins("Sex"),
              ins("Male"), 
              result[1, ],
              ins("THI"),
              ins("THI1"),
              result[2:5, ],
              ins("Education"),
              ins("Education0"),
              result[6:11, ],
              ins("Smoking.Status"),
              ins("Spmoking.StatusNever"),
              result[12:13, ],
              result[14:23,]
)
for(i in 2:4) {result[, i] = as.numeric(result[, i])}

fig1<- forestplot(result[,c(1,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result[,3],  #告诉函数表格第3列为95%CI，
                  upper=result[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.6,       #设置小黑块的大小
                  graph.pos=2)       #森林图应插在图形第2列
fig1
myVars=c("Sex","THI","Education","Smoking.Status")
catVars=c("Sex","THI","Education","Smoking.Status")

table1<- print(CreateTableOne(vars=myVars,
                              data = data1,
                              factorVars = catVars),
               showAllLevels=TRUE)
#2. 在基线表table1里插入空行，使它的行数和变量跟result一致
N<-rbind(c(NA,NA),
         c(NA,NA),
         table1[2:3, ],
         c(NA, NA),
         table1[4:8,],
         c(NA,NA),
         table1[9:15,],
         c(NA,NA), 
         table1[16:18,],
         c(NA,NA),
         c(NA,NA), 
         c(NA, NA),
         c(NA,NA),
         c(NA,NA),
         c(NA,NA), 
         c(NA, NA),
         c(NA,NA),
         c(NA,NA),
         c(NA,NA)
         )       
N<-N[,-1]
N<-data.frame(N)
#3.把N表和result表合在一起
result1<-cbind(result,N)
#调顺序。变为:变量-N-HR......顺序
result1<-result1[,c(1,7,2:6)]

#4.优化第一行。第一行行名中加入"Number(%)"
for(i in 2:7) {result1[, i] = as.character(result1[, i])}
result1<-rbind(c("Characteristics","Number (%)",NA,NA,NA,"HR (95%CI)","P.value"),
               result1[2:nrow(result1),])
for(i in 3:5) {result1[, i] = as.numeric(result1[, i])}

fig2<- forestplot(result1[,c(1,2,6,7)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result1[,3],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result1[,4],  #告诉函数表格第3列为95%CI，
                  upper=result1[,5],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.4,       #设置小黑块的大小
                  graph.pos=3)       #森林图应插在图形第3列
fig2


# 2 四分位COX模型 --------------------------------------------------------------
data=read.csv("G:/UKB/Air_Gene_Stroke/data5_5_Q1.csv")
data1=data
# 生成factor
data1$Sex <- factor(data1$Sex, levels=c(1,2), labels=c("Female","Male"))
data1$Center <- as.factor(data1$Center)
data1$THI=as.factor(data1$THI)
data1$Education=as.factor(data1$Education)
data1$Smoking.Status=factor(data1$Smoking.Status,levels=c(1,2,3),labels=c("Never","Previous","Current"))
# data1$AlcoholScore_Q=factor(data1$AlcoholScore_Q,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
# data1$DietScore_Q=factor(data1$DietScore_Q,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
data1$PM2.5_Q=factor(data1$PM2.5_QT,levels=c(1,2,3),labels=c("Q1","Q2","Q3"))
data1$PM2.5_10_Q=factor(data1$PM2.5_10_QT,levels=c(1,2,3),labels=c("Q1","Q2","Q3"))
data1$PM10_Q=factor(data1$PM10_QT,levels=c(1,2,3),labels=c("Q1","Q2","Q3"))
data1$NO2_Q=factor(data1$NO2_QT,levels=c(1,2,3),labels=c("Q1","Q2","Q3"))
data1$NOx_Q=factor(data1$NOx_QT,levels=c(1,2,3),labels=c("Q1","Q2","Q3"))
data1$PRS_Q=factor(data1$PRS_QT,levels=c(1,2,3),labels=c("Q1","Q2","Q3"))
data1$DietScore_Q=factor(data1$DietScore_QF,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
data1$AlcoholScore_Q=factor(data1$AlcoholScore_QF,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
data1$stats=as.numeric(data1$stats)
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+NOx_Q+NO2_Q+PM10_Q+PM2.5_Q+PM2.5_10_Q+
                DietScore_Q+AlcoholScore_Q, data=data1)
summary(mul_cox)
#一-2 multi1：提取：变量+HR+95%CI+95%CI
mul_cox1 <- summary(mul_cox)
colnames(mul_cox1$conf.int)
multi1<-as.data.frame(round(mul_cox1$conf.int[, c(1, 3, 4)], 2))
#一-3、multi2：提取：HR(95%CI)和P
#install.packages("tableone")
library(tableone)
multi2<-ShowRegTable(mul_cox, 
                     exp=TRUE, 
                     digits=2, 
                     pDigits =3,
                     printToggle = TRUE, 
                     quote=FALSE, 
                     ciFun=confint)
#一-4.将两次提取结果合并成表；取名result
result <-cbind(multi1,multi2);result
#一-5.行名转为表格第一列，并给予命名"Characteristics"
result<-tibble::rownames_to_column(result, var = "Characteristics");result
#
fig1<- forestplot(result[,c(1,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result[,3],  #告诉函数表格第3列为95%CI，
                  upper=result[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.3,       #设置小黑块的大小
                  graph.pos=2)       #森林图应插在图形第2列
fig1
# 合并 血糖、血压参数
data2=read.csv("G:/UKB/Air_Gene_Stroke/Data49793_diabeteshypertension.csv")
# 平均
b1=(data2$X93.0.0+data2$X93.0.1)/2
b2=(data2$X94.0.0+data2$X94.0.1)/2
b3=(data2$X4079.0.0+data2$X4079.0.1)/2
b4=(data2$X4080.0.0+data2$X4080.0.1)/2
id1=which(is.na(b3))
b3[id1]=b2[id1]
id2=which(is.na(b4))
b4[id2]=b1[id2]
data2$DBP=b3
data2$SBP=b4
n1=dim(data)
for (i in 1:n1[1])
{
  id3=which(data2$eid==data$eid[i])
  data$DBP[i]=data2$DBP[id3]
  data$SBP[i]=data2$SBP[id3]
  data$Glucose[i]=data2$X30740.0.0[id3]
  data$Diabetes[i]=data2$X2443.0.0[id3]
}
id4=which(is.na(data$DBP))
id5=which(is.na(data$Glucose))
write.csv(data,"G:/UKB/Air_Gene_Stroke/data6.csv")

# 3 -----------------------------------------------------------------------
# Q4
data=read.csv("G:/UKB/Air_Gene_Stroke/data6_1.csv")
data1=data
# 生成factor
data1$Sex <- factor(data1$Sex, levels=c(1,2), labels=c("Female","Male"))
data1$Center <- as.factor(data1$Center)
data1$THI=as.factor(data1$THI)
data1$Education=as.factor(data1$Education)
data1$Smoking.Status=factor(data1$Smoking.Status,levels=c(1,2,3),labels=c("Never","Previous","Current"))
# data1$AlcoholScore_Q=factor(data1$AlcoholScore_Q,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
# data1$DietScore_Q=factor(data1$DietScore_Q,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
data1$PM2.5_Q=factor(data1$PM2.5_QF,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
data1$PM2.5_10_Q=factor(data1$PM2.5_10_QF,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
data1$PM10_Q=factor(data1$PM10_QF,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
data1$NO2_Q=factor(data1$NO2_QF,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
data1$NOx_Q=factor(data1$NOx_QF,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
data1$PRS_Q=factor(data1$PRS_QT,levels=c(1,2,3),labels=c("Q1","Q2","Q3"))
data1$DietScore_Q=factor(data1$DietScore_QF,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
data1$AlcoholScore_Q=factor(data1$AlcoholScore_QF,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
data1$stats=as.numeric(data1$stats)
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+NOx+NO2+PM10+PM2.5+PM2.5_10+
                DietScore+AlcoholScore+DBP+SBP+Glucose+PRS, data=data1)
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline++PM2.5_Q+PM2.5_10_Q+PM10_Q+NO2_Q+NOx_Q+
                DietScore+AlcoholScore+SBP+Glucose+PRS, data=data1)
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+PM2.5_Q+
                DietScore+AlcoholScore+SBP+Glucose, data=data1)
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+PM2.5_Q+
                DietScore+AlcoholScore+SBP+Glucose+PRS, data=data1)
summary(mul_cox)
#一-2 multi1：提取：变量+HR+95%CI+95%CI
mul_cox1 <- summary(mul_cox)
colnames(mul_cox1$conf.int)
multi1<-as.data.frame(round(mul_cox1$conf.int[, c(1, 3, 4)], 2))
#一-3、multi2：提取：HR(95%CI)和P
#install.packages("tableone")
library(tableone)
multi2<-ShowRegTable(mul_cox, 
                     exp=TRUE, 
                     digits=2, 
                     pDigits =3,
                     printToggle = TRUE, 
                     quote=FALSE, 
                     ciFun=confint)
#一-4.将两次提取结果合并成表；取名result
result <-cbind(multi1,multi2);result
#一-5.行名转为表格第一列，并给予命名"Characteristics"
result<-tibble::rownames_to_column(result, var = "Characteristics");result
#
fig1<- forestplot(result[,c(1,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result[,3],  #告诉函数表格第3列为95%CI，
                  upper=result[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.3,       #设置小黑块的大小
                  graph.pos=2)       #森林图应插在图形第2列
fig1
# Q3
data=read.csv("G:/UKB/Air_Gene_Stroke/data6_1.csv")
data1=data
# 生成factor
data1$Sex <- factor(data1$Sex, levels=c(1,2), labels=c("Female","Male"))
data1$Center <- as.factor(data1$Center)
data1$THI=as.factor(data1$THI)
data1$Education=as.factor(data1$Education)
data1$Smoking.Status=factor(data1$Smoking.Status,levels=c(1,2,3),labels=c("Never","Previous","Current"))
# data1$AlcoholScore_Q=factor(data1$AlcoholScore_Q,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
# data1$DietScore_Q=factor(data1$DietScore_Q,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
data1$PM2.5_Q=factor(data1$PM2.5_QT,levels=c(1,2,3),labels=c("Q1","Q2","Q3"))
data1$PM2.5_10_Q=factor(data1$PM2.5_10_QT,levels=c(1,2,3),labels=c("Q1","Q2","Q3"))
data1$PM10_Q=factor(data1$PM10_QT,levels=c(1,2,3),labels=c("Q1","Q2","Q3"))
data1$NO2_Q=factor(data1$NO2_QT,levels=c(1,2,3),labels=c("Q1","Q2","Q3"))
data1$NOx_Q=factor(data1$NOx_QT,levels=c(1,2,3),labels=c("Q1","Q2","Q3"))
data1$PRS_Q=factor(data1$PRS_QT,levels=c(1,2,3),labels=c("Q1","Q2","Q3"))
data1$DietScore_Q=factor(data1$DietScore_QF,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
data1$AlcoholScore_Q=factor(data1$AlcoholScore_QF,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
data1$stats=as.numeric(data1$stats)
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+NOx+NO2+PM10+PM2.5+PM2.5_10+
                DietScore+AlcoholScore+DBP+SBP+Glucose+PRS, data=data1)
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline++PM2.5_Q+PM2.5_10_Q+PM10_Q+NO2_Q+NOx_Q+
                DietScore+AlcoholScore+SBP+Glucose+PRS, data=data1)
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+NOx_Q+
                DietScore+AlcoholScore+SBP+Glucose, data=data1)
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+PM2.5_Q+
                DietScore+AlcoholScore+SBP+Glucose+PRS, data=data1)
summary(mul_cox)
#一-2 multi1：提取：变量+HR+95%CI+95%CI
mul_cox1 <- summary(mul_cox)
colnames(mul_cox1$conf.int)
multi1<-as.data.frame(round(mul_cox1$conf.int[, c(1, 3, 4)], 2))
#一-3、multi2：提取：HR(95%CI)和P
#install.packages("tableone")
library(tableone)
multi2<-ShowRegTable(mul_cox, 
                     exp=TRUE, 
                     digits=2, 
                     pDigits =3,
                     printToggle = TRUE, 
                     quote=FALSE, 
                     ciFun=confint)
#一-4.将两次提取结果合并成表；取名result
result <-cbind(multi1,multi2);result
#一-5.行名转为表格第一列，并给予命名"Characteristics"
result<-tibble::rownames_to_column(result, var = "Characteristics");result
#
fig1<- forestplot(result[,c(1,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result[,3],  #告诉函数表格第3列为95%CI，
                  upper=result[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.3,       #设置小黑块的大小
                  graph.pos=2)       #森林图应插在图形第2列
fig1

# 4 五分位数分组 ----------------------------------------------------------------
data=read.csv("F:/UKB77740/Air_Gene_Stroke/data6_1.csv")
data1=data
# NOx
q1=as.data.frame(quantile(data1$NOx,probs = seq(0,1,0.2)))
breaks=q1[1:6,]
breaks[1]=breaks[1]-1
breaks[6]=breaks[6]+1
data1$NOx_Q=cut(data1$NOx,breaks=breaks,label=c("Q1","Q2","Q3","Q4","Q5"),right=TRUE)
#No2
q1=as.data.frame(quantile(data1$NO2,probs = seq(0,1,0.2)))
breaks=q1[1:6,]
breaks[1]=breaks[1]-1
breaks[6]=breaks[6]+1
data1$NO2_Q=cut(data1$NO2,breaks=breaks,label=c("Q1","Q2","Q3","Q4","Q5"),right=TRUE)
# PM2.5
q1=as.data.frame(quantile(data1$PM2.5,probs = seq(0,1,0.2)))
breaks=q1[1:6,]
breaks[1]=breaks[1]-1
breaks[6]=breaks[6]+1
data1$PM2.5_Q=cut(data1$PM2.5,breaks=breaks,label=c("Q1","Q2","Q3","Q4","Q5"),right=TRUE)
# PM2.5_10
q1=as.data.frame(quantile(data1$PM2.5_10,probs = seq(0,1,0.2)))
breaks=q1[1:6,]
breaks[1]=breaks[1]-1
breaks[6]=breaks[6]+1
data1$PM2.5_10_Q=cut(data1$PM2.5_10,breaks=breaks,label=c("Q1","Q2","Q3","Q4","Q5"),right=TRUE)
# PM10
q1=as.data.frame(quantile(data1$PM10,probs = seq(0,1,0.2)))
breaks=q1[1:6,]
breaks[1]=breaks[1]-1
breaks[6]=breaks[6]+1
data1$PM10_Q=cut(data1$PM10,breaks=breaks,label=c("Q1","Q2","Q3","Q4","Q5"),right=TRUE)
# COX
# 生成factor
data1$Sex <- factor(data1$Sex, levels=c(1,2), labels=c("Female","Male"))
data1$Center <- as.factor(data1$Center)
data1$THI=as.factor(data1$THI)
data1$Education=as.factor(data1$Education)
data1$Smoking.Status=factor(data1$Smoking.Status,levels=c(1,2,3),labels=c("Never","Previous","Current"))
data1$stats=as.numeric(data1$stats)
write.csv(data1,"G:/UKB/Air_Gene_Stroke/data6_2.csv")
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+NOx+NO2+PM10+PM2.5+PM2.5_10+
                DietScore+AlcoholScore+DBP+SBP+Glucose+PRS, data=data1)
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+PM2.5_Q+PM2.5_10_Q+PM10_Q+NO2_Q+NOx_Q+
                DietScore+AlcoholScore+SBP+Glucose+PRS, data=data1)
# 各个air pollution 单独
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+NOx_Q+
                DietScore+AlcoholScore+SBP+Glucose, data=data1)
summary(mul_cox)
# ptrend
data1$NOx_N=as.numeric(data1$NOx_Q)
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+NOx_N+
                DietScore+AlcoholScore+SBP+Glucose, data=data1)
# HR/10 UG/M3
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+NOx+
                DietScore+AlcoholScore+SBP+Glucose, data=data1)
mul_cox
summary(mul_cox)
# PM2.5
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+PM2.5_Q+
                DietScore+AlcoholScore+SBP+Glucose, data=data1)
# ptrend
data1$PM2.5_N=as.numeric(data1$PM2.5_Q)
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+PM2.5_N+
                DietScore+AlcoholScore+SBP+Glucose, data=data1)
# 
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+PM2.5+
                DietScore+AlcoholScore+SBP+Glucose, data=data1)


# PM2..5_10
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+PM2.5_10_Q+
                DietScore+AlcoholScore+SBP+Glucose, data=data1)
# PTREND
data1$PM2.5_10_N=as.numeric(data1$PM2.5_10_Q)

mul_cox1=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+PM2.5_10_N+
                DietScore+AlcoholScore+SBP+Glucose, data=data1)
# hr
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+PM2.5_10+
                 DietScore+AlcoholScore+SBP+Glucose, data=data1)
summary(mul_cox2)

# PM10
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+PM10_Q+
                DietScore+AlcoholScore+SBP+Glucose, data=data1)
summary(mul_cox1)
# PM10 HR
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+PM10+
                DietScore+AlcoholScore+SBP+Glucose, data=data1)
# PM10 PTREND
data1$PM10_N=as.numeric(data1$PM10_Q)
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+PM10_N+
                DietScore+AlcoholScore+SBP+Glucose, data=data1)

# NO2
mul_cox=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+NO2_Q+
                DietScore+AlcoholScore+SBP+Glucose, data=data1)
# NO2 HR
mul_cox1=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                BMI+Age_baseline+NO2+
                DietScore+AlcoholScore+SBP+Glucose, data=data1)
# NO2 PTREND
data1$NO2_N=as.numeric(data1$NO2_Q)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+NO2_N+
                 DietScore+AlcoholScore+SBP+Glucose, data=data1)
summary(mul_cox)
# 计算air pollution score
data1$Air_Score=(0.0074537*data1$PM2.5+0.0243*data1$PM2.5_10+0.01136*data1$PM10+
                   0.003263*data1$NO2+0.004454*data1$NOx)*(5/(0.004454+0.0074537+0.0243+0.01136+0.003263))
q1=as.data.frame(quantile(data1$Air_Score,probs = seq(0,1,0.2)))
breaks=q1[1:6,]
breaks[1]=breaks[1]-1
breaks[6]=breaks[6]+1
data1$Air_Score_Q=cut(data1$Air_Score,breaks=breaks,label=c("Q1","Q2","Q3","Q4","Q5"),right=TRUE)
n=dim(data1)
for (i in 1:n[1])
{
  id1=which(target_col$eid==data1$eid[i])
  data1$Genotyping[i]=target_col$`22000-0.0`[id1]
  data1$GenPCA1[i]=target_col$`22009-0.1`[id1]
  data1$GenPCA2[i]=target_col$`22009-0.2`[id1]
  data1$GenPCA3[i]=target_col$`22009-0.3`[id1]
  data1$GenPCA4[i]=target_col$`22009-0.4`[id1]
  data1$GenPCA5[i]=target_col$`22009-0.5`[id1]
  data1$GenPCA6[i]=target_col$`22009-0.6`[id1]
  data1$GenPCA7[i]=target_col$`22009-0.7`[id1]
  data1$GenPCA8[i]=target_col$`22009-0.8`[id1]
  data1$GenPCA9[i]=target_col$`22009-0.9`[id1]
  data1$GenPCA10[i]=target_col$`22009-0.10`[id1]
}
write.csv(data1,"G:/UKB/Air_Gene_Stroke/data7.1.csv")
###
# 计算 air score 的HR： 三个model: 1 adjusted for age, sex;
# 2 adjusted for age, sex, education, total inhourse income...
# 3 adjusted for age, sex, educaiton, THI..., PRS, Gentyping, 
# 计算各组case N
id1=which(data1$stats==1)
length(id1)
l=data1$stats[id1]
id2=which(l==1)
length(id2)
# 1 adjusted for age, sex
data2=read.csv("F:/UKB77740/Air_Gene_Stroke/data8.csv")
#
# air polluant correlation
air=matrix(NA,307304,5)
air[,1]=data2$NO2
air[,2]=data2$NOx
air[,3]=data2$PM2.5
air[,4]=data2$PM2.5_10
air[,5]=data2$PM10
colnames(air)=c("NO2","NOx", "PM2.5","PM2.5-10","PM10")
cormatrix=cor(air,method='spearman')
library(corrplot)
corrplot(cormatrix,method='number')
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score_Q, data=data2)
summary(mul_cox1)
mul_cox1
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score, data=data2)
data1$Air_Score_N=as.numeric(data1$Air_Score_Q)
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score_N, data=data1)
# 2 adjust for .....
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_Q+
                 DietScore+AlcoholScore+SBP+Glucose, data=data2)
summary(mul_cox2)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score+
                DietScore+AlcoholScore+SBP+Glucose, data=data2)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_N+
                 DietScore+AlcoholScore+SBP+Glucose, data=data2)
mul_cox2

# 3 adjusted for 
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_Q+
                 DietScore+AlcoholScore+SBP+Glucose+PRS+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data2)
summary(mul_cox3)
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score+
                 DietScore+AlcoholScore+SBP+Glucose+PRS+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data2)
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_N+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)
mul_cox3
# PRS
q1=as.data.frame(quantile(data1$PRS,probs = seq(0,1,0.2)))
breaks=q1[1:6,]
breaks[1]=breaks[1]-1
breaks[6]=breaks[6]+1
# 1 adjusted for age, sex
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+PRS_Q+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data2)
summary(mul_cox1)
mul_cox1
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+PRS+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)
data1$PRS_N=as.numeric(data1$PRS_Q)
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+PRS_N+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)
# 2 adjust for .....
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+PRS_Q+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)
summary(mul_cox2)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+PRS+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+PRS_N+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)
mul_cox2

# 3 adjusted for 
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+PRS_Q+Air_Score_Q+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)
summary(mul_cox2)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+PRS++Air_Score_Q+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+PRS_N++Air_Score_Q+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)
mul_cox2
## low genetic risk #  medium geneti risk # high genetic risk 
# low genetic risk
q1=as.data.frame(quantile(data1$PRS,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+2
data1$PRS_Q=cut(data1$PRS,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
n1=dim(data1)
l1=as.numeric(data1$PRS_Q)
l2=as.numeric(data1$Air_Score_Q)
l3=as.character(l1)
l4=as.character(l2)
l5=paste(l3,l4,sep="")
data1$Air_Score_QQ=as.factor(l5)
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_QQ+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)


#一-2 multi1：提取：变量+HR+95%CI+95%CI
mul_cox2 <- summary(mul_cox1)
colnames(mul_cox2$conf.int)
multi1<-as.data.frame(round(mul_cox2$conf.int[, c(1, 3, 4)], 4))
#一-3、multi2：提取：HR(95%CI)和P
#install.packages("tableone")
library(tableone)
multi2<-ShowRegTable(mul_cox1, 
                     exp=TRUE, 
                     digits=2, 
                     pDigits =3,
                     printToggle = TRUE, 
                     quote=FALSE, 
                     ciFun=confint)
#一-4.将两次提取结果合并成表；取名result
result <-cbind(multi1,multi2);result
#一-5.行名转为表格第一列，并给予命名"Characteristics"
result<-tibble::rownames_to_column(result, var = "Characteristics");result
#
fig1<- forestplot(result[,c(1,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result[,3],  #告诉函数表格第3列为95%CI，
                  upper=result[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.3,       #设置小黑块的大小
                  graph.pos=2)       #森林图应插在图形第2列
fig1
# 结果美化
#2. 给参考变量插入空行
#2-1.这步代码不用改
ins <- function(x) {c(x, rep(NA, ncol(result)-1))}
##2-2：插入空行，形成一个新表
for(i in 5:6) {result[, i] = as.character(result[, i])}
result<-rbind(c("Characteristics", NA, NA, NA, "HR(95%CI)","p"),
              ins("Low genetic risk"),
              ins("Air pollution score Q1"), 
              result[8:11, ],
              ins("Medium genetic risk"),
              result[12:16, ],
              ins("High genetic risk"),
              result[17:21, ]
)
for(i in 2:4) {result[, i] = as.numeric(result[, i])}

fig1<- forestplot(result[,c(1,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result[,3],  #告诉函数表格第3列为95%CI，
                  upper=result[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.6,       #设置小黑块的大小
                  graph.pos=2)       #森林图应插在图形第2列
fig1
myVars=c("Air_Score_QQ11","Air_Score_QQ12","Air_Score_QQ13","Air_Score_QQ14",
         "Air_Score_QQ15","Air_Score_QQ21","Air_Score_QQ22","Air_Score_QQ23","Air_Score_QQ24",
         "Air_Score_QQ25","Air_Score_QQ31","Air_Score_QQ32","Air_Score_QQ33","Air_Score_QQ34",
         "Air_Score_QQ35")
catVars=c("Air_Score_QQ11","Air_Score_QQ12","Air_Score_QQ13","Air_Score_QQ14",
           "Air_Score_QQ15","Air_Score_QQ21","Air_Score_QQ22","Air_Score_QQ23","Air_Score_QQ24",
           "Air_Score_QQ25","Air_Score_QQ31","Air_Score_QQ32","Air_Score_QQ33","Air_Score_QQ34",
           "Air_Score_QQ35")
myVars=c("Air_Score_QQ")
catVars=c("Air_Score_QQ")

table1<- print(CreateTableOne(vars=myVars,
                              data = data1,
                              factorVars = catVars),
               showAllLevels=TRUE)
#2. 在基线表table1里插入空行，使它的行数和变量跟result一致
N<-rbind(c(NA,NA),
         c(NA,NA),
         table1[2:6, ],
         c(NA, NA),
         table1[7:11,],
         c(NA,NA),
         table1[12:16,])       
N<-N[,-1]
N<-data.frame(N)
#3.把N表和result表合在一起
result1<-cbind(result,N)
#调顺序。变为:变量-N-HR......顺序
result1<-result1[,c(1,7,2:6)]

#4.优化第一行。第一行行名中加入"Number(%)"
for(i in 2:7) {result1[, i] = as.character(result1[, i])}
result1<-rbind(c("Characteristics","Number (%)",NA,NA,NA,"HR (95%CI)","P.value"),
               result1[2:nrow(result1),])
for(i in 3:5) {result1[, i] = as.numeric(result1[, i])}
#计算各组人数
id1=which(data1$Air_Score_QQ=="11")
length(id1)
id2=which(data1$stats[id1]==1)
length(id2)
result1[1,2]="Cases/N"
result1[2,2]=NA
result1[3,2]=paste(as.character(length(id2)),"/",as.character(length(id1)))
result1[3,3]=1
#
id1=which(data1$Air_Score_QQ=="12")
length(id1)
id2=which(data1$stats[id1]==1)
length(id2)
result1[4,1]="Air pollution score Q2"
result1[4,2]=paste(as.character(length(id2)),"/",as.character(length(id1)))
#
id1=which(data1$Air_Score_QQ=="13")
length(id1)
id2=which(data1$stats[id1]==1)
length(id2)
result1[5,1]="Air pollution score Q3"
result1[5,2]=paste(as.character(length(id2)),"/",as.character(length(id1)))
#
id1=which(data1$Air_Score_QQ=="14")
length(id1)
id2=which(data1$stats[id1]==1)
length(id2)
result1[6,1]="Air pollution score Q4"
result1[6,2]=paste(as.character(length(id2)),"/",as.character(length(id1)))
#
id1=which(data1$Air_Score_QQ=="15")
length(id1)
id2=which(data1$stats[id1]==1)
length(id2)
result1[7,1]="Air pollution score Q5"
result1[7,2]=paste(as.character(length(id2)),"/",as.character(length(id1)))
#
id1=which(data1$Air_Score_QQ=="21")
length(id1)
id2=which(data1$stats[id1]==1)
length(id2)
result1[9,1]="Air pollution score Q1"
result1[9,2]=paste(as.character(length(id2)),"/",as.character(length(id1)))
#
id1=which(data1$Air_Score_QQ=="22")
length(id1)
id2=which(data1$stats[id1]==1)
length(id2)
result1[10,1]="Air pollution score Q2"
result1[10,2]=paste(as.character(length(id2)),"/",as.character(length(id1)))
#
id1=which(data1$Air_Score_QQ=="23")
length(id1)
id2=which(data1$stats[id1]==1)
length(id2)
result1[11,1]="Air pollution score Q3"
result1[11,2]=paste(as.character(length(id2)),"/",as.character(length(id1)))
#
id1=which(data1$Air_Score_QQ=="24")
length(id1)
id2=which(data1$stats[id1]==1)
length(id2)
result1[12,1]="Air pollution score Q4"
result1[12,2]=paste(as.character(length(id2)),"/",as.character(length(id1)))
#
id1=which(data1$Air_Score_QQ=="25")
length(id1)
id2=which(data1$stats[id1]==1)
length(id2)
result1[13,1]="Air pollution score Q5"
result1[13,2]=paste(as.character(length(id2)),"/",as.character(length(id1)))
#
id1=which(data1$Air_Score_QQ=="31")
length(id1)
id2=which(data1$stats[id1]==1)
length(id2)
result1[15,1]="Air pollution score Q1"
result1[15,2]=paste(as.character(length(id2)),"/",as.character(length(id1)))
#
id1=which(data1$Air_Score_QQ=="32")
length(id1)
id2=which(data1$stats[id1]==1)
length(id2)
result1[16,1]="Air pollution score Q2"
result1[16,2]=paste(as.character(length(id2)),"/",as.character(length(id1)))
#
id1=which(data1$Air_Score_QQ=="33")
length(id1)
id2=which(data1$stats[id1]==1)
length(id2)
result1[17,1]="Air pollution score Q3"
result1[17,2]=paste(as.character(length(id2)),"/",as.character(length(id1)))

#
id1=which(data1$Air_Score_QQ=="34")
length(id1)
id2=which(data1$stats[id1]==1)
length(id2)
result1[18,1]="Air pollution score Q4"
result1[18,2]=paste(as.character(length(id2)),"/",as.character(length(id1)))
#
id1=which(data1$Air_Score_QQ=="35")
length(id1)
id2=which(data1$stats[id1]==1)
length(id2)
result1[19,1]="Air pollution score Q5"
result1[19,2]=paste(as.character(length(id2)),"/",as.character(length(id1)))

fig2<- forestplot(result1[,c(1,2,6,7)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result1[,3],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result1[,4],  #告诉函数表格第3列为95%CI，
                  upper=result1[,5],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.4,       #设置小黑块的大小
                  graph.pos=3)       #森林图应插在图形第3列
fig2
## sensitivity analysis
# 1 air pollution score without PM2.5_10, PM10
# 2 with PM2.5 absorbance
# 3 IS > 2 years
# 4 living current address >5 years
# 5 road environment
# 1 without PM2.5_10, PM10
data2$Air_Score1=(0.0074537*data2$PM2.5+
                   0.003263*data2$NO2+0.004454*data2$NOx)*(3/(0.004454+0.0074537+0.003263))
q1=as.data.frame(quantile(data1$Air_Score1,probs = seq(0,1,0.2)))
breaks=q1[1:6,]
breaks[1]=breaks[1]-1
breaks[6]=breaks[6]+1
data1$Air_Score1_Q=cut(data1$Air_Score1,breaks=breaks,label=c("Q1","Q2","Q3","Q4","Q5"),right=TRUE)
data1$Air_Score1_N=as.numeric(data1$Air_Score1_Q)
# 1 adjusted for age, sex

mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score1_Q, data=data2)
summary(mul_cox1)
mul_cox1
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score1, data=data2)
data1$Air_Score_N1=as.numeric(data1$Air_Score1)
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score1_N, data=data1)
# 2 adjust for .....
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score1_Q+
                 DietScore+AlcoholScore+SBP+Glucose, data=data1)
summary(mul_cox2)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score1+
                 DietScore+AlcoholScore+SBP+Glucose, data=data2)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score1_N+
                 DietScore+AlcoholScore+SBP+Glucose, data=data1)
mul_cox2

# 3 adjusted for 
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score1_Q+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)
summary(mul_cox3)
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score1+PRS+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data2)
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score1_N+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)
mul_cox3
# 2 including pm2.5 absorbance
q1=as.data.frame(quantile(data1$PM2.5absorbance,probs = seq(0,1,0.2)))
breaks=q1[1:6,]
breaks[1]=breaks[1]-1
breaks[6]=breaks[6]+1
data1$PM2.5absorbance_Q=cut(data1$PM2.5absorbance,breaks=breaks,label=c("Q1","Q2","Q3","Q4","Q5"),right=TRUE)
data1$PM2.5absorbance_N=as.numeric(data1$PM2.5absorbance_Q)
# 人数
id1=which(data1$Air_Score2_Q=="Q5")
id2=which(data1$stats[id1]==1)
length(id2)
length(id1)


# 1 adjusted for age, sex

mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+PM2.5absorbance_Q, data=data1)
summary(mul_cox1)
mul_cox1
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+PM2.5absorbance, data=data1)
data1$Air_Score_N1=as.numeric(data1$Air_Score1)
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+PM2.5absorbance_N, data=data1)
# 2 adjust for .....
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_Q1+
                 DietScore+AlcoholScore+SBP+Glucose, data=data1)
summary(mul_cox2)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score1+
                 DietScore+AlcoholScore+SBP+Glucose, data=data1)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_N1+
                 DietScore+AlcoholScore+SBP+Glucose, data=data1)
mul_cox2

# 3 adjusted for 
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_Q1+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)
summary(mul_cox3)
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+PM2.5absorbance+
                 DietScore+AlcoholScore+SBP+Glucose, data=data1)
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_N1+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)
mul_cox3
#
# sensativity 2 add PM2.5absorbance
data1$Air_Score2=(0.0074537*data1$PM2.5+0.0243*data1$PM2.5_10+0.01136*data1$PM10+
                    0.003263*data1$NO2+0.004454*data1$NOx+0.133*data1$PM2.5absorbance)*(6/(0.004454+0.0074537+0.0243+0.01136+0.003263+0.133))

q1=as.data.frame(quantile(data1$Air_Score2,probs = seq(0,1,0.2)))
breaks=q1[1:6,]
breaks[1]=breaks[1]-1
breaks[6]=breaks[6]+1
data1$Air_Score2_Q=cut(data1$Air_Score2,breaks=breaks,label=c("Q1","Q2","Q3","Q4","Q5"),right=TRUE)
data1$Air_Score2_N=as.numeric(data1$Air_Score2_Q)

mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score2_Q, data=data2)
summary(mul_cox1)
mul_cox1
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score2, data=data2)
data1$Air_Score2_N=as.numeric(data1$Air_Score2_Q)
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score2_N, data=data1)
# 2 adjust for .....
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score2_Q+
                 DietScore+AlcoholScore+SBP+Glucose, data=data1)
summary(mul_cox2)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score2+
                 DietScore+AlcoholScore+SBP+Glucose, data=data2)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score2_N+
                 DietScore+AlcoholScore+SBP+Glucose, data=data1)
mul_cox2

# 3 adjusted for 
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score2_Q+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)
summary(mul_cox3)
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score2+
                 DietScore+AlcoholScore+SBP+Glucose+PRS+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data2)
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score2_N+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)
mul_cox3
#
# 3 IS > 2 years
id1=which(data2$time>24)
length(id1)
data3=data2[id1,]
q1=as.data.frame(quantile(data3$Air_Score,probs = seq(0,1,0.2)))
breaks=q1[1:6,]
breaks[1]=breaks[1]-1
breaks[6]=breaks[6]+1
data3$Air_Score_Q=cut(data3$Air_Score,breaks=breaks,label=c("Q1","Q2","Q3","Q4","Q5"),right=TRUE)
data3$Air_Score_N=as.numeric(data3$Air_Score_Q)
id2=which(data3$Air_Score_Q=="Q5")
id3=which(data3$stats[id2]==1)
length(id2)
length(id3)
# adjust for age and sex
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score_Q, data=data3)
summary(mul_cox1)
mul_cox1
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score, data=data2)
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score_N, data=data2)
# 2 adjust for .....
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_Q+
                 DietScore+AlcoholScore+SBP+Glucose, data=data2)
summary(mul_cox2)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score+
                 DietScore+AlcoholScore+SBP+Glucose, data=data3)
data2$Air_Score_N=as.numeric(data2$Air_Score_Q)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_N+
                 DietScore+AlcoholScore+SBP+Glucose, data=data2)
mul_cox2

# 3 adjusted for 
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_Q+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data2)
summary(mul_cox3)
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score+
                 DietScore+AlcoholScore+SBP+Glucose+PRS+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data3)
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_N+
                 DietScore+AlcoholScore+SBP+Glucose+PRS+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data1)
mul_cox3
#
# sensitivity living current adrees for 5 years
#
address=read.csv("G:/UKB/Air_Gene_Stroke/Data49793_livingaddress.csv")
n=dim(data1)
for (i in 1:n[1])
{
  id1=which(address$eid==data1$eid[i])
  data1$address[i]=address$X699.0.0[id1]
}
write.csv(data1,"G:/UKB/Air_Gene_Stroke/data8.csv")

data2=read.csv('F:/UKB77740/Air_Gene_Stroke/data8.csv')
id2=which(data2$address>5)
data2=data2[id2,]
q1=as.data.frame(quantile(data2$Air_Score,probs = seq(0,1,0.2)))
breaks=q1[1:6,]
breaks[1]=breaks[1]-1
breaks[6]=breaks[6]+1
data2$Air_Score_Q=cut(data2$Air_Score,breaks=breaks,label=c("Q1","Q2","Q3","Q4","Q5"),right=TRUE)
data2$Air_Score_N=as.numeric(data2$Air_Score_Q)

id1=which(data2$Air_Score_Q=="Q5")
di2=which(data2$stats[id1]==1)
length(id1)
length(di2)



# adjust for age and sex
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score_Q, data=data2)
summary(mul_cox1)
mul_cox1
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score, data=data2)
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score_N, data=data2)
# 2 adjust for .....
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_Q+
                 DietScore+AlcoholScore+SBP+Glucose, data=data2)
summary(mul_cox2)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score+
                 DietScore+AlcoholScore+SBP+Glucose, data=data2)
# data2$Air_Score_N=as.numeric(data2$Air_Score_Q)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_N+
                 DietScore+AlcoholScore+SBP+Glucose, data=data2)
mul_cox2

# 3 adjusted for 
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_Q+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data2)
summary(mul_cox3)
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score+
                 DietScore+AlcoholScore+SBP+Glucose+PRS+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data2)
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_N+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data2)
mul_cox3


# 4 -----------------------------------------------------------------------
# road
data2=read.csv('F:/UKB77740/Air_Gene_Stroke/data8.csv')
# adjust for age and sex
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score_Q
                 , data=data2)
summary(mul_cox1)
mul_cox1
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score, data=data2)
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score_N, data=data2)
# 2 adjust for .....
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_Q+
                 Distance.to.the.nearst.major.road+
                 Distance.to.the.nearst.road+
                 Sum.of.raod.length.of.major.roads.within.100m+
                 Trafic.intensity.on.the.nearest.major.road+
                 Traffic.intensity.on.the.nearst.road+
                 DietScore+AlcoholScore+SBP+Glucose, data=data2)
summary(mul_cox2)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score+
                 Distance.to.the.nearst.major.road+
                 Distance.to.the.nearst.road+
                 Sum.of.raod.length.of.major.roads.within.100m+
                 Trafic.intensity.on.the.nearest.major.road+
                 Traffic.intensity.on.the.nearst.road+
                 DietScore+AlcoholScore+SBP+Glucose, data=data2)
# data2$Air_Score_N=as.numeric(data2$Air_Score_Q)
mul_cox2=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_N+
                 Distance.to.the.nearst.major.road+
                 Distance.to.the.nearst.road+
                 Sum.of.raod.length.of.major.roads.within.100m+
                 Trafic.intensity.on.the.nearest.major.road+
                 Traffic.intensity.on.the.nearst.road+
                 DietScore+AlcoholScore+SBP+Glucose, data=data2)
mul_cox2

# 3 adjusted for 
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_Q+
                 Distance.to.the.nearst.major.road+
                 Distance.to.the.nearst.road+
                 Sum.of.raod.length.of.major.roads.within.100m+
                 Trafic.intensity.on.the.nearest.major.road+
                 Traffic.intensity.on.the.nearst.road+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data2)
summary(mul_cox3)
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score+
                 Distance.to.the.nearst.major.road+
                 Distance.to.the.nearst.road+
                 Sum.of.raod.length.of.major.roads.within.100m+
                 Trafic.intensity.on.the.nearest.major.road+
                 Traffic.intensity.on.the.nearst.road+
                 DietScore+AlcoholScore+SBP+Glucose+PRS+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data2)
mul_cox3=coxph(Surv(time,stats) ~Sex+THI+Education+Smoking.Status+
                 BMI+Age_baseline+Air_Score_N+
                 Distance.to.the.nearst.major.road+
                 Distance.to.the.nearst.road+
                 Sum.of.raod.length.of.major.roads.within.100m+
                 Trafic.intensity.on.the.nearest.major.road+
                 Traffic.intensity.on.the.nearst.road+
                 DietScore+AlcoholScore+SBP+Glucose+
                 Genotyping+GenPCA1+GenPCA2+GenPCA3+GenPCA4+GenPCA5+GenPCA6+
                 GenPCA7+GenPCA8+GenPCA9+GenPCA10, data=data2)
mul_cox3

# 十折交叉验证：
data2=read.csv('F:/UKB77740/Air_Gene_Stroke/data8.csv')
K=10
m=nrow(data2)
kfold <- sample(rep(1:K, length.out=m), size=m, replace=F)
#result1=as.matrix(NA, nrow=11)
#result1=as.table(result1)
kresults=as.table(kresults)
kresults1=as.matrix(NA,ncol=11)
kresults2=as.matrix(NA,ncol=11)
kresults3=as.matrix(NA,ncol=11)
kresults4=as.matrix(NA,ncol=11)
kresults5=as.matrix(NA,ncol=11)
kresults6=as.matrix(NA,ncol=11)
train_set=data2[kfold !=1,]
test_set=data2[kfold ==1,]
mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score, data=test_set)
mul_cox2 <- summary(mul_cox1)
colnames(mul_cox2$conf.int)
multi1<-as.data.frame(round(mul_cox2$conf.int[, c(1, 3, 4)], 4))
SD=sd(test_set$Air_Score)
multi1=exp(log(multi1)*SD)
#一-3、multi2：提取：HR(95%CI)和P
#install.packages("tableone")
# library(tableone)
multi2<-ShowRegTable(mul_cox1, 
                     exp=TRUE, 
                     digits=2, 
                     pDigits =3,
                     printToggle = TRUE, 
                     quote=FALSE, 
                     ciFun=confint)
#一-4.将两次提取结果合并成表；取名result
result <-cbind(multi1,multi2);result
#一-5.行名转为表格第一列，并给予命名"Characteristics"
result<-tibble::rownames_to_column(result, var = "Characteristics");result
result1=result
for (i in 1:K)
{
  train_set=data2[kfold !=i,]
  test_set=data2[kfold ==i,]
  mul_cox1=coxph(Surv(time,stats) ~Sex+Age_baseline+Air_Score, data=test_set)
  mul_cox2 <- summary(mul_cox1)
  colnames(mul_cox2$conf.int)
  multi1<-as.data.frame(round(mul_cox2$conf.int[, c(1, 3, 4)], 4))
  SD=sd(test_set$Air_Score)
  multi1=exp(log(multi1)*SD)
  #一-3、multi2：提取：HR(95%CI)和P
  #install.packages("tableone")
  # library(tableone)
  multi2<-ShowRegTable(mul_cox1, 
                       exp=TRUE, 
                       digits=2, 
                       pDigits =3,
                       printToggle = TRUE, 
                       quote=FALSE, 
                       ciFun=confint)
  #一-4.将两次提取结果合并成表；取名result
  result <-cbind(multi1,multi2);result
  #一-5.行名转为表格第一列，并给予命名"Characteristics"
  result<-tibble::rownames_to_column(result, var = "Characteristics");result
  #
  result1[i,]=result[3,]
}
for (i in 1:10){result1[i,1]=paste("fold",as.character(i))}
  
  fig1<- forestplot(result1[,c(1,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                    mean=result1[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                    lower=result1[,3],  #告诉函数表格第3列为95%CI，
                    upper=result1[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                    zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                    boxsize=0.4,       #设置小黑块的大小
                    graph.pos=2)       #森林图应插在图形第2列
  fig1
  install.packages("rmeta")
library(rmeta)
  install.packages("meta")
  install.packages("metafor")
  library(meta)
  library(metafor)
   se=(result1[,4]-result1[,3])/(2*1.96)
   result1=cbind(result1,se)
  meta3=metagen(result1[,2],result1[,7],comb.fixed=TRUE)
  x=summary(meta3)
  result1[11,1]="Common effect"
  result1[11,2]=x$common$TE
  result1[11,3]=x$common$lower
  result1[11,4]=x$common$upper
  for (i in 1:11)
  {
  result1[i,5]=paste(as.character(round(result1[i,2],2)),'[',
                      as.character(round(result1[i,3],2)),',',
                      as.character(round(result1[i,4],2)),']')
  }
  result1[11,6]="<0.0001"
  
        
  forest(meta3,col.square = "black",col.diamond = "black",col.diamond.lines = "black",hetstat = TRUE,leftcols = "studlab")
kresults=cbind(kresults1,kresults2,kresults3,kresults4,kresults5,kresults6)
for (i in 3:5){kresults[,i]=as.numeric(kresults[,i])}
fig2<- forestplot(result1[,c(1,2,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=kresults[,3],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=kresults[,4],  #告诉函数表格第3列为95%CI，
                  upper=kresults[,5],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.4,       #设置小黑块的大小
                  graph.pos=3)       #森林图应插在图形第3列
fig2


