# delirium

# 1 extraction outcome: delirium ------------------------------------------
setwd("E:/UKB/UKB94885/ThreeParts/")
filepath = "ukb674470.csv"
filepath = "ukb674529.csv"
filepath = "ukb674659.csv" # PRS # img
filepath="E:/UKB/UKB94885/ukb671960.csv"
filepath="E:/UKB/UKB94885/ukb673997.csv"
# get column names
con = file(filepath,'rt')
con.names = colnames(read.csv(con, nrow = 1,head=TRUE,check.names = FALSE))
close(con)
name <- as.data.frame(con.names)
# 变量FID
f1='E:/UKB/UKB94885/Delirium/Mental and behavioural disorders FID.csv'
f1='E:/UKB/UKB94885/Delirium/FID.csv'
f1="E:/UKB/UKB94885/Delirium/Medicalinfectiousparasitic.csv" #173
f1="E:/UKB/UKB94885/Delirium/Medical_blood_immune.csv"
f1="E:/UKB/UKB94885/Delirium/thyroid.csv"
f1="E:/UKB/UKB94885/Delirium/diabetes.csv"
f1="E:/UKB/UKB94885/Delirium/Nutiondeficiency.csv"
f1="E:/UKB/UKB94885/Delirium/obesity.csv"
f1="E:/UKB/UKB94885/Delirium/othermetabolic.csv"
f1="E:/UKB/UKB94885/Delirium/Medical_eye_adnexa.csv"
f1="E:/UKB/UKB94885/Delirium/Ear_mastoid.csv"
f1="E:/UKB/UKB94885/Delirium/Circulatory system.csv"
f1="E:/UKB/UKB94885/Delirium/Respiratory.csv"
f1="E:/UKB/UKB94885/Delirium/digestive.csv"
f1="E:/UKB/UKB94885/Delirium/skin.csv"
f1="E:/UKB/UKB94885/Delirium/Musculoskeletal.csv"
f1="E:/UKB/UKB94885/Delirium/genitourinary.csv"
f1="E:/UKB/UKB94885/Delirium/pregenacy.csv"
f2=read.csv(f1)
id=f2$FieldID
id1=as.character(id)
id2='-0.0'
id3=paste(id1,id2,sep='')
choose=id3
choose=c("34-","52-")
choose=c("53-0")
choose=c("21000")
choose=c("1369-","1299-")
choose=c("4462-0.0","5364-0.0","4429-0.0","4418-0.0","4451-0.0","4407-0.0",
         "4440-0.0","1588-0.0","1578-0.0","1608-0.0","1568-0.0","1598-0.0") # alcohol
choose=c("1767-0","1687-0","1777-0") # 补充early life
choose=c("42014-","42016-","42018-","42026-","42000-","42002-","42004-","42030-","42006-") # medical
choose=c("130814-0")
choose=c("1468-0")
choose=c("1707-0")
choose=c("24014-0","24012-0","24010-0","24009-0","24011-0","24013-0")
choose=c("21103-0")
# social
choose=c("6160-0","1031-0","2110-0")
choose=c("1289-0","1299-0","1309-0","1329-0","1339-0","1349-0","1359-0","1369-0","1389-0")
# alcohol
choose=c("1558-","1588-","1578-","1608-","1568-","1598-","4440-","4407-","4451-","4418-","4429")
# physical measures
choose=c("23099-0","23100-0","23101-0","23124-0","23120-0","23123-0","23119-0",
         "23125-0","23121-0","23116-0","23112-0","23115-0","23111-0","23117-0",
         "23113-0","23128-0","23127-0","23129-0","23434-0","23104-0","22427-0",
         "22409-0","22410-0")
choose=c("22409-0","22427-0","22410-0")
choose=c("699")
index <- NULL
for (i in 1:length(choose)) {
  b <- nchar(choose[i])
  a <- which(substr(name$con.names,1,b)==choose[i])
  if(length(a)<1){cat(choose[i],'\n')}
  index <- c(index,a)
}
# set target column name
target_colname =c("eid",
                  name$con.names[index]
)
#target_colname =c("eid","40000-0.0"
#                 )
#get column postion or index
target_colindex = which(con.names%in%target_colname)
# use colClasses parameter to read specify column data
colClasses  = rep("NULL",length(con.names))
colClasses[target_colindex]=NA
target_col = read.csv(filepath, head=TRUE,check.names = FALSE,colClasses=colClasses)
target_col1=target_col
target_col=merge(target_col2,target_col1,by="eid")
names=colnames(target_col)
result1=choose %in% names
id=which(result1==FALSE)
write.csv(target_col,"E:/UKB/UKB94885/Delirium/variable1.csv")
target_col=read.csv("E:/UKB/UKB94885/Delirium/participants1.csv")
# 提取时间和状态
id1=which(target_col$X130846.0.0 =="")
id2=is.na(target_col$X130847.0.0)
id3=which(id2==FALSE) # id3 发生delirium的人
id5=which(target_col$X130846.0.0=="2037-07-07")
# year1=2023
# month1=3
# 先把状态 计算出来
data1=target_col[,1:3]
colnames(data1)=c("eid","time","status")
data1$status[id1]=1  # 1 表示未发病
data1$status[-id1]=2
# data1$time[id1]=(2023-target_col$`34-0.0`)*12+(3-target_col$`52-0.0`)

# 计算month unlist(gregexpr("/", l1, fixed = TRUE))
# data1=target_col
n3=length(id3)
m1=matrix(NA,nrow=dim(data1)[1],ncol=1)  #患病的月
y1=matrix(NA,nrow=dim(data1)[1],ncol=1) #患病的年
m2=matrix(NA,nrow=dim(data1)[1],ncol=1)  #入组的月
y2=matrix(NA,nrow=dim(data1)[1],ncol=1)   #入组的年
for (i in 1:n3)
{
  l1=unlist(gregexpr("-",target_col$X130846.0.0[id3[i]],fixed=TRUE))
  m=as.numeric(substr(target_col$X130846.0.0[id3[i]],(l1[1]+1),(l1[2]-1)))
  y=as.numeric(substr(target_col$X130846.0.0[id3[i]],(1),(l1[1]-1)))
  m1[id3[i]]=m
  y1[id3[i]]=y
}
# 入组年月
for (i in 1:dim(data1)[1])
{
  l2=unlist(gregexpr("-",target_col$`53-0.0`[i],fixed=TRUE))
  m=as.numeric(substr(target_col$`53-0.0`[i],(l2[1]+1),(l2[2]-1)))
  y=as.numeric(substr(target_col$`53-0.0`[i],(1),(l2[1]-1)))
  m2[i]=m
  y2[i]=y
}
data1$time[id3]=(y1[id3]-y2[id3])*12+(m1[id3]-m2[id3])
data1$time[-id3]=(2023-y2[-id3])*12+(3-m2[-id3])
write.csv(data1,"E:/UKB/UKB94885/Delirium/participants2.csv")
data1=read.csv("E:/UKB/UKB94885/Delirium/participants2.csv")
data1=data1[,-1]
id=which(data1$time<0)
data1=data1[-id,]
id1=which(data1$status==2)
write.csv(data1,"E:/UKB/UKB94885/Delirium/participants3.csv")

# 2 data preprocess -------------------------------------------------------
variable=read.csv("E:/UKB/UKB94885/Delirium/variable1.csv")
# 合并 blood pressure
SBP1=variable$X93.0.0
DBP1=variable$X94.0.0
SBP2=variable$X4080.0.0
DBP2=variable$X4079.0.0
x1=data.frame(SBP1,SBP2)
y1=data.frame(DBP1,DBP2)
SBP=rowMeans(x1,na.rm=TRUE)
DBP=rowMeans(y1,na.rm=TRUE)
variable$SBP=SBP
variable$DBP=DBP
#  合并pulse rate
PR1=variable$X95.0.0
PR2=variable$X102.0.0
x1=data.frame(PR1,PR2)
PR=rowMeans(x1,na.rm=TRUE)
variable$pulse_rate=PR
var=c("X93.0.0","X94.0.0","X4080.0.0","X4079.0.0","X95.0.0","X102.0.0")
names=colnames(variable)
id= names %in% var
id1=which(id==TRUE)
variable1=variable[,-id1]
variable1=variable
## 删除缺失率大于20%的变量
n=dim(variable1)
names=colnames(variable1)
names=0
for (i in 1:n[2])
{
  id1=is.na(variable[,i])
  id2=which(id1==TRUE)
  rate=length(id2)/n[1]
  if (rate>0.2)
  {
    names[i]=1
  }
}
id=which(names==1)
variable1=variable1[,-id]
write.csv(variable1,"E:/UKB/UKB94885/Delirium/variable1_3.csv")
x1=variable1$X4526.0.0
id=is.na(x1)
id1=which(id==FALSE)
id2=which(id==TRUE)
variable=read.csv("E:/UKB/UKB94885/Delirium/variable1_3.csv")
n=dim(variable)
for (i in 1:n[2])
{
  id=which(variable[,i]==-10)
  variable[id,i]=0
}
write.csv(variable,"E:/UKB/UKB94885/Delirium/variable1_4.csv")
# 异常值处理
n=dim(variable)
x1=variable$Health_score
summary(x1)
id1=is.na(x1)
id2=which(id1==TRUE)
variable$Health_score[id2]=median(x1, na.rm=TRUE)
x2=variable$Crime_score
id1=is.na(x2)
id2=which(id1==TRUE)
variable$Crime_score[id2]=median(x2,na.rm=TRUE)
summary(x2)
variable=read.csv("E:/UKB/UKB94885/Delirium/variable1_4.csv")
n=dim(variable)
library(prettyR)
for (i in 1:(n[2]-2))
{
id1=which(variable[,i]<0)
if (is.integer(variable[1,i])&max(variable[,i],na.rm=TRUE)<10)
{
  variable[id1,i]=as.numeric(Mode(variable[,i]))
}
else{variable[id1,i]=median(variable[,i],na.rm=TRUE)}

}
##
# NA的处理 ：众数和中位数填充
for (i in 1:(n[2]-2))
{
  id1=is.na(variable[,i])
  id2=which(id1==TRUE)
  if (is.integer(variable[1,i])&max(variable[,i],na.rm=TRUE)<10)
  {
    variable[id2,i]=as.numeric(Mode(variable[,i]))
  }
  else{variable[id2,i]=median(variable[,i],na.rm=TRUE)}
  
}
# NA处理方法2：multi imputation 
library(mice)
data=read.csv("E:/UKB/UKB94885/Delirium/variable1_4.csv")
data1=data
eid=data[,1]
data=data[,-1]
data$Water.hardness=as.factor(data$Water.hardness)
data$Mg.concentration=as.factor(data$Mg.concentration)
data$Sex=as.factor(data$Sex)
data$Center=as.factor(data$Center)
data$Birth_weight=as.factor(data$Birth_weight)
data$Number_household=as.factor(data$Number_household)
data$Vehicles_household=as.factor(data$Vehicles_household)
data$Income=as.factor(data$Income)
data$outdoor_winter=as.factor(data$outdoor_winter)
data$sleep=as.factor(data$sleep)
data$Nap_day=as.factor(data$Nap_day)
data$Snoring=as.factor(data$Snoring)
data$Smoking_status=as.factor(data$Smoking_status)
data$smoke_home=as.factor(data$smoke_home)
data$Bread_type=as.factor(data$Bread_type)
data$Waterintake=as.factor(data$Waterintake)
data$Breastfead=as.factor(data$Breastfead)
data$skin_colour=as.factor(data$skin_colour)
data$Maternal_smoking=as.factor(data$Maternal_smoking)
data$mother_alive=as.factor(data$mother_alive)
data$tenseness.restlessness=as.factor(data$tiredness.lethargy)
data$tiredness.lethargy=as.factor(data$tiredness.lethargy)
data$solarium.sunlamp=as.factor(data$solarium.sunlamp)
data$Cancer=as.factor(data$Cancer)
data$Alcohol_status=as.factor(data$Alcohol_status)
# data1=data
# data=data1
mice_data=mice(data,m=5,method="rf")
mice_data1 <- complete(mice_data)
write.csv(mice_data1,"F:/UKB77740/Air_Gene_Stroke/R2/data10_2.csv")
mice_data1$eid=eid
write.csv(variable,"E:/UKB/UKB94885/Delirium/variable1_5.csv")
d1=read.csv("E:/UKB/UKB94885/Delirium/participants3.csv")
d1=d1[,-1]
data=merge(d1,variable,by="eid")
write.csv(data,"E:/UKB/UKB94885/Delirium/data1.csv")
#
# air pollutant
L1=cbind(data$No2_2010,data$No2_2005,data$No2_2006,data$No2_2007)
No2=rowMeans(L1,na.rm=TRUE)
data$No2=No2
L2=cbind(data$PM10_2007,data$PM10_2010)
PM10=rowMeans(L2,na.rm=TRUE)
data$PM10=PM10
data$PM2.5=data$PM2.5_2010
data$PM2.5_10=data$PM2.5_10_2010
data$NOx=data$Nox_2010
data=subset(data,select=-c(No2_2010,No2_2005,No2_2006,No2_2007))
data=subset(data,select=-c(PM10_2007,PM10_2010))
data=subset(data,select=-c(Nox_2010,PM2.5_2010))
##
# diet score
data=merge(data,target_col,by="eid")
id=which(data$`1299-0.0`<0)
data$`1299-0.0`[id]=median(data$`1299-0.0`,na.rm=TRUE)
id=which(data$`1369-0.0`<0)
data$`1369-0.0`[id]=median(data$`1369-0.0`,na.rm=TRUE)
vegetable=data$vegetable_cooked+data$`1299-0.0`
fruit=data$Freshfruit+data$Driedfruit
fish=data$Oilyfish+data$nonoilyfish
processmeat=data$Processedmeat
unprocessmeat=data$Poultry+data$`1369-0.0`
# vegetable
id2=length(vegetable)
l1=which(vegetable>=4)
l2=which(vegetable<4)
Vegetablescore=vegetable
Vegetablescore[l1]=1
Vegetablescore[l2]=0
# Fruit
id3=length(fruit)
l3=which(fruit>=3)
l4=which(fruit<3)
Fruitscore=fruit
Fruitscore[l3]=1
Fruitscore[l4]=0
# Fish
id3=length(fish)
l5=which(fish>=3)
l6=which(fish<3)
Fishscore=fish
Fishscore[l5]=1
Fishscore[l6]=0
# processedMeat
id4=length(processmeat)
l7=which(processmeat<=2)
l8=which(processmeat>2)
Processedmeatscore=processmeat
Processedmeatscore[l7]=1
Processedmeatscore[l8]=0
# 5 unprocessMeat
id5=length(unprocessmeat)
l9=which(unprocessmeat<=2)
l10=which(unprocessmeat>2)
Unprocessedmeatscore=unprocessmeat
Unprocessedmeatscore[l9]=1
Unprocessedmeatscore[l10]=0
# dietScore
dietScore=Vegetablescore+Fruitscore+Fishscore+Processedmeatscore+Unprocessedmeatscore
data$DietScore=dietScore
data=subset(data,select=-c(`1299-0.0`,`1369-0.0`,vegetable_cooked,Freshfruit,Freshfruit,
                           nonoilyfish, Oilyfish,Processedmeat,Poultry))
# alcohol
## alchohol
# 估计每天饮酒量
target_col=target_col2
n1=dim(target_col)
weekAlcohol=target_col[,2:6]
weekAlcohol[,6]=target_col[,13]
monthAlcohol=target_col[,7:12]
monthAlcohol=monthAlcohol/4
#
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
target_col$AlcoholScore=AlcoholScore
score=target_col
score=score[,-c(2:13)]
id=which(score[,2]<0)
score[id,2]=median(score[,2],na.rm=TRUE)
id1=is.na(score[,2])
id2=which(id1==TRUE)
score[id2,2]=median(score[,2],na.rm=TRUE)
data=merge(data,score, by="eid")

## 删除缺失率大于20%的变量
n=dim(data)
names=colnames(data)
names=0
for (i in 1:n[2])
{
  id1=is.na(data[,i])
  id2=which(id1==TRUE)
  rate=length(id2)/n[1]
  if (rate>0.2)
  {
    names[i]=1
  }
}
id=which(names==1)
data=data[,-id]
write.csv(data,"E:/UKB/UKB94885/Delirium/participants4.csv")
# 添加earlife variable
colnames(target_col)=c("eid","body_size_10","adopted","multiple_birth")
summary(target_col)
n=dim(target_col)
for (i in c(1:n[2]))
{
  id1=which(target_col[,i]<0)
  target_col[id1,i]=median(target_col[,i],na.rm=TRUE)
  id2=is.na(target_col[,i])
  id3=which(id2==TRUE)
  target_col[id3,i]=median(target_col[,i],na.rm=TRUE)
}
data=read.csv("E:/UKB/UKB94885/Delirium/participants4.csv")
data=merge(data,target_col,by="eid")
write.csv(data,"E:/UKB/UKB94885/Delirium/participants5.csv")
# 添加medical history
target_col2=target_col
target_col=target_col2
n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
y2=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) # 入组的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
 # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
# t=y2
# y2[,1:9]=t
y1=y1-y2
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]<0)
  y1[id,i]=2
  y1[-id,i]=1
}
  
target_col2[,2:10]=y1
data=read.csv("E:/UKB/UKB94885/Delirium/participants5.csv")
data=merge(data,target_col2,by="eid")
write.csv(data,"E:/UKB/UKB94885/Delirium/participants6.csv")
###
# medical: Certain infectious and parasitic diseases
n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
y2=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) # 入组的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
# target_col$`53-0.0`=y2
# write.csv(target_col,"E:/UKB/UKB94885/Delirium/time_ruzu.csv")
n=dim(y1)
for (i in 1:n[2])
{
  y2[,i]=t
}

y1=y1-y2
y3=y1
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
target_col$infectious_parasitic=rowSums(y1)
target_col=target_col[,-c(2:161)]
data=read.csv("E:/UKB/UKB94885/Delirium/participants6.csv")
data=merge(data,target_col,by="eid")
write.csv(data,"E:/UKB/UKB94885/Delirium/participants7.csv")
## medical blood, immune
n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
n=dim(y1)
y2=y2[,1:n[2]]
y1=y1-y2
y3=y1
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
target_col$infectious_parasitic=rowSums(y1)
target_col=target_col[,-c(2:35)]
data=merge(data,target_col,by="eid")
##
# thyroid
n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
n=dim(y1)
# y4=y2
y2=y4[,1:n[2]]
y1=y1-y2
y3=y1
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
target_col$thyroid=rowSums(y1)
target_col=target_col[,-c(2:8)]
data=merge(data,target_col,by="eid")
##
# pancreas / diabetes
n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
n=dim(y1)
# y4=y2
y2=y4[,1:n[2]]
y1=y1-y2
y3=y1
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
target_col$pancres=rowSums(y1)
target_col=target_col[,-2]
data=merge(data,target_col,by="eid")
##
# nutrition deficiency
n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
n=dim(y1)
# y4=y2
y2=y4[,1:n[2]]
y1=y1-y2
y3=y1
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
target_col$nutrition_deficiences=rowSums(y1)
target_col=target_col[,-c(2:14)]
data=merge(data,target_col,by="eid")
##
# obesity
n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
n=dim(y1)
# y4=y2
y2=y4[,1:n[2]]
y1=y1-y2
y3=y1
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
summary(test)
target_col$obesity=rowSums(y1)
target_col=target_col[,-c(2:5)]
data=merge(data,target_col,by="eid")
##
# lipoprotein
n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
n=dim(y1)
# y4=y2
y2=y4[,1:n[2]]
y1=y1-y2
y3=y1
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
summary(test)
target_col$lipoprotein=rowSums(y1)
target_col=target_col[,-c(2)]
data=merge(data,target_col,by="eid")
##
# other metabolic
n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
n=dim(y1)
# y4=y2
y2=y4[,1:n[2]]
y1=y1-y2
y3=y1
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
summary(test)
target_col$other_metabolic_diorder=rowSums(y1)
target_col=target_col[,-c(2:10)]
data=merge(data,target_col,by="eid")
write.csv(data,"E:/UKB/UKB94885/Delirium/participants7.csv")
##
# eye
 data=read.csv("E:/UKB/UKB94885/Delirium/participants7.csv")
y2=read.csv("E:/UKB/UKB94885/Delirium/time_ruzu.csv")
y2=y2[,-1]
n1=dim(target_col)
y4=matrix(NA,nrow=n1[1],ncol=100)
for (i in 1:n1[2])
{
  y4[,i]=y2$X53.0.0
}

n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
n=dim(y1)
# y4=y2
y2=y4[,1:n[2]]
y1=y1-y2
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
summary(test)
target_col$eye_adnexa=rowSums(y1)
target_col=target_col[,-c(2:49)]
data=merge(data,target_col,by="eid")
##
# Ear mastoid

n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
n=dim(y1)
# y4=y2
y2=y4[,1:n[2]]
y1=y1-y2
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
summary(test)
target_col$ear_mastoid=rowSums(y1)
target_col=target_col[,-c(2:22)]
data=merge(data,target_col,by="eid")
##
# Circulatory system disorders
n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
n=dim(y1)
# y4=y2
y2=y4[,1:n[2]]
y1=y1-y2
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
summary(test)
target_col$circulatory=rowSums(y1)
target_col=target_col[,-c(2:77)]
data=merge(data,target_col,by="eid")
###
# respiratory
n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
n=dim(y1)
# y4=y2
y2=y4[,1:n[2]]
y1=y1-y2
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
summary(test)
target_col$respiratory=rowSums(y1)
target_col=target_col[,-c(2:63)]
data=merge(data,target_col,by="eid")
##
# digestive system
n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
n=dim(y1)
# y4=y2
y2=y4[,1:n[2]]
y1=y1-y2
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
summary(test)
target_col$digestive=rowSums(y1)
target_col=target_col[,-c(2:63)]
data=merge(data,target_col,by="eid")
##
# skin
n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
n=dim(y1)
# y4=y2
y2=y4[,1:n[2]]
y1=y1-y2
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
summary(test)
target_col$skin=rowSums(y1)
target_col=target_col[,-c(2:72)]
data=merge(data,target_col,by="eid")
##
# Musculoskeletal
n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
n=dim(y1)
# y4=y2
y2=y4[,1:n[2]]
y1=y1-y2
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
summary(test)
target_col$musculoskeletal=rowSums(y1)
target_col=target_col[,-c(2:9)]
data=merge(data,target_col,by="eid")#
###
# Genitourinary
n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
n=dim(y1)
# y4=y2
y2=y4[,1:n[2]]
y1=y1-y2
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
summary(test)
target_col$genitourinary=rowSums(y1)
target_col=target_col[,-c(2:9)]
data=merge(data,target_col,by="eid")
##
# pregenacy
n1=dim(target_col)
y1=matrix(NA,nrow=dim(target_col)[1],ncol=n1[2]-1) #患病的年
for (i in 2:n1[2])
{
  id1=which(target_col[,i]=="")
  y1[id1,i-1]=2023
  id2=is.na(y1[,i-1])
  id3=which(id2==TRUE)
  # x=target_col[,i-1]
  y1[id3,i-1]=as.numeric(substr(target_col[id3,i],(1),(4)))
}
n1=dim(y1)
for (i in 1:n1[2])
{
  id=which(y1[,i]<1950)
  y1[id,i]=2023
}
n=dim(y1)
# y4=y2
y2=y4[,1:n[2]]
y1=y1-y2
n=dim(y1)
for (i in 1:n[2])
{
  id=which(y1[,i]>0)
  y1[id,i]=0
  y1[-id,i]=1
}
test=rowSums(y1)
summary(test)
target_col$pregenacy=rowSums(y1)
target_col=target_col[,-c(2:9)]
data=merge(data,target_col,by="eid")
##
# 
write.csv(data,"E:/UKB/UKB94885/Delirium/participants8.csv")
# 添加 cereal type FID:1468
colnames(target_col)=c("eid","Cereal type")
data=merge(data,target_col,by="eid")
# 添加 handedness
colnames(target_col)=c("eid","Handedness")

data=merge(data,target_col,by="eid")
write.csv(data,"E:/UKB/UKB94885/Delirium/participants8.csv")
## 添加 road信息
colnames(target_col)=c("eid","Trafic intensity 1", "Inverse distence to the nearst road", "Trafic intensity 2", "Inverse distance to the nearest major road", "Total trafic load","close to major road")
data=merge(data,target_col,by="eid")
write.csv(data,"E:/UKB/UKB94885/Delirium/participants8.csv")
# 添加 water_CaCO3
colnames(target_col)=c("eid","Water_CaCO3")
data=merge(data,target_col,by="eid")
write.csv(data,"E:/UKB/UKB94885/Delirium/participants8.csv")
# 添加 social factor
target_col[,4]=rowMeans(target_col1[,3:7],na.rm=TRUE)
target_col=target_col[,1:4]
colnames(target_col)=c("eid","Friend/family visits","Confide","Leisure/social activities")
data=merge(data,target_col,by="eid")
write.csv(data,"E:/UKB/UKB94885/Delirium/participants8.csv")
## 添加 diet 
data=read.csv("E:/UKB/UKB94885/Delirium/participants8.csv")
colnames(target_col)=c("eid","Cooked vegetable","Raw vegetable","Fresh fruit","Oily fish","Non-oily fish","Processed mat","Poultry","Beef","Pork")
data=merge(data,target_col,by="eid")
write.csv(data,"E:/UKB/UKB94885/Delirium/participants8.csv")
## 添加 alcohol
target_col1=target_col[,1:12]
colnames(target_col1)=c("eid","1558","1568","1578","1588","1598","1608","4407","4418","4429","4440","4451")
target_col1[,2]=rowMeans(target_col[,2:5],na.rm=TRUE)
target_col1[,3]=rowMeans(target_col[,6:9],na.rm=TRUE)
target_col1[,4]=rowMeans(target_col[,10:13],na.rm=TRUE)
target_col1[,5]=rowMeans(target_col[,14:17],na.rm=TRUE)
target_col1[,6]=rowMeans(target_col[,18:21],na.rm=TRUE)
target_col1[,7]=rowMeans(target_col[,22:25],na.rm=TRUE)
target_col1[,8]=rowMeans(target_col[,26:29],na.rm=TRUE)
target_col1[,9]=rowMeans(target_col[,30:33],na.rm=TRUE)
target_col1[,10]=rowMeans(target_col[,34:37],na.rm=TRUE)
target_col1[,11]=rowMeans(target_col[,38:41],na.rm=TRUE)
target_col1[,12]=rowMeans(target_col[,42:45],na.rm=TRUE)
target_col1[,8:12]=target_col1[,8:12]/4
x1=target_col1[,c(3,8)]
target_col1$`1568`=rowMeans(target_col1[,c(3,8)],na.rm=TRUE)
target_col1$`1578`=rowMeans(target_col1[,c(4,9)],na.rm=TRUE)
target_col1$`1588`=rowMeans(target_col1[,c(5,10)],na.rm=TRUE)
target_col1$`1598`=rowMeans(target_col1[,c(6,11)],na.rm=TRUE)
target_col1$`1608`=rowMeans(target_col1[,c(7,12)],na.rm=TRUE)
target_col1=target_col1[,1:7]
colnames(target_col1)=c("eid","Alcohol intake", "Red wine","White wine","Beer/cider","Spirits","Fortified wine")
data=merge(data,target_col1,by="eid")
write.csv(data,"E:/UKB/UKB94885/Delirium/participants8.csv")
data1=read.csv("E:/UKB/UKB94885/Delirium/participants8_lifestyle.csv")
data1=data1[,c(-37:-41)]
data1=merge(data1,target_col1,by="eid")
write.csv(data1,"E:/UKB/UKB94885/Delirium/participants8_lifestyle.csv")
id1=is.na(data$White.wine)
id2=which(id1==0)
# physical measures
write.csv(target_col,"E:/UKB/UKB94885/Delirium/lings.csv")
data=read.csv("E:/UKB/UKB94885/Delirium/participants8_physical measures.csv")
data1=read.csv("E:/UKB/UKB94885/Delirium/lings.csv")
data=merge(data,data1,by="eid")
write.csv(data,"E:/UKB/UKB94885/Delirium/lings.csv")
x1=data[,c(22,25)]
x1=na.omit(x1)
cor(x1[,1],x1[,2])
data$Leg_fat_percentage=rowMeans(data[,c(22,25)],na.rm=TRUE)
x1=data[,c(23,26)]
x1=na.omit(x1)
cor(x1[,1],x1[,2])
data$Leg_fat=rowMeans(data[,c(23,26)],na.rm=TRUE)
x1=data[,c(24,27)]
x1=na.omit(x1)
cor(x1[,1],x1[,2])
data$Leg_fat_free=rowMeans(data[,c(24,27)],na.rm=TRUE)
x1=data[,c(28,31)]
x1=na.omit(x1)
cor(x1[,1],x1[,2])
data$arm_fat_percentage=rowMeans(data[,c(28,31)],na.rm=TRUE)
x1=data[,c(29,32)]
x1=na.omit(x1)
cor(x1[,1],x1[,2])
data$arm_fat=rowMeans(data[,c(29,32)],na.rm=TRUE)

x1=data[,c(30,33)]
x1=na.omit(x1)
cor(x1[,1],x1[,2])
data$arm_fat_free=rowMeans(data[,c(30,33)],na.rm=TRUE)

write.csv(data, "E:/UKB/UKB94885/Delirium/lings.csv")
cor(data$Hand_grip_1, data$Hand_grip_2)
## medical history
data=read.csv("E:/UKB/UKB94885/Delirium/participants9_Medical history.csv")
id1=which(data$Cancer==1)
id2=which(data$Asthma==2)
id3=which(data$infectious_parasitic.x==2)
id4=which(data$pancres==2)
##
# early life
data1=read.csv("E:/UKB/UKB94885/Delirium/participants8_earlylife.csv")
summary(data1)
# 异常值 -3 -1
n=dim(data1)
for (i in 1:n[2])
{
  id1=which(data1[,i]==-3)
  id2=which(data1[,i]==-1)
  if (length(id1)>0) {data1[id1,i]=NA}
  if (length(id2)>0) {data1[id2,i]=NA}
}
for (i in 5:n[2])
{
  data1[,i]=as.factor(data1[,i])
}
# environment
data2=read.csv("E:/UKB/UKB94885/Delirium/participants8_environment.csv")
n=dim(data2)
for (i in 1:n[2])
{
  id1=which(data2[,i]==-3)
  id2=which(data2[,i]==-1)
  if (length(id1)>0) {data2[id1,i]=NA}
  if (length(id2)>0) {data2[id2,i]=NA}
}
data2$Water_hardness=as.factor(data2$Water_hardness)
data2$Close_to_major_road=as.factor(data2$Close_to_major_road)
data=merge(data1,data2,by="eid")
# social factors
data3=read.csv("E:/UKB/UKB94885/Delirium/participants8_social.csv")
n=dim(data3)
for (i in 1:n[2])
{
  id1=which(data3[,i]==-3)
  id2=which(data3[,i]==-1)
  if (length(id1)>0) {data3[id1,i]=NA}
  if (length(id2)>0) {data3[id2,i]=NA}
}
data3$Leisure.social.activities=as.integer(data3$Leisure.social.activities)
for (i in 2:10)
{
  data3[,i]=as.factor(data3[,i])
}
data=merge(data,data3,by="eid")
# Life style
data4=read.csv("E:/UKB/UKB94885/Delirium/participants8_lifestyle.csv")
#data4=data4[,-1]
n=dim(data4)
for (i in 1:n[2])
{
  id1=which(data4[,i]==-3)
  id2=which(data4[,i]==-1)
  id3=which(data4[,i]==-10)
  if (length(id1)>0) {data4[id1,i]=NA}
  if (length(id2)>0) {data4[id2,i]=NA}
  if (length(id3)>0) {data4[id3,i]=0}
}
for (i in 32:37)
{data4[,i]=as.integer(data4[,i])}
for (i in 2:5)
{
  data4[,i]=as.factor(data4[,i])
}
data=merge(data,data4,by="eid")
# physical measures
data5=read.csv("E:/UKB/UKB94885/Delirium/participants8_physical measures.csv")
n=dim(data5)
for (i in 1:n[2])
{
  id1=which(data5[,i]==-3)
  id2=which(data5[,i]==-1)
  if (length(id1)>0) {data5[id1,i]=NA}
  if (length(id2)>0) {data5[id2,i]=NA}
}
data=merge(data,data5,by="eid")
# blood assary
data6=read.csv("E:/UKB/UKB94885/Delirium/participants8_blood assary.csv")
n=dim(data6)
for (i in 1:n[2])
{
  id1=which(data6[,i]==-3)
  id2=which(data6[,i]==-1)
  if (length(id1)>0) {data6[id1,i]=NA}
  if (length(id2)>0) {data6[id2,i]=NA}
}
data=merge(data,data6,by="eid")
# medical history
data7=read.csv("E:/UKB/UKB94885/Delirium/participants9_Medical history.csv")
n=dim(data7)
for (i in 1:n[2])
{
  id1=which(data7[,i]==-3)
  id2=which(data7[,i]==-1)
  if (length(id1)>0) {data7[id1,i]=NA}
  if (length(id2)>0) {data7[id2,i]=NA}
}
for (i in 2:n[2])
{
  data7[,i]=as.factor(data7[,i])
}
data=merge(data,data7,by="eid")
write.csv(data,"E:/UKB/UKB94885/Delirium/participants8withimissingvalue.csv")
data=read.csv("E:/UKB/UKB94885/Delirium/participants8withimissingvalue.csv")
# missing value process
data=read.csv("E:/UKB/UKB94885/Delirium/participants8missbucong.csv")
data1=data[,-c(1:5)]
library(mice)
data1=data[,4:201]
mice_data=mice(data1,m=5,method="rf")
mice_data1 <- complete(mice_data)
data[,4:201]=mice_data1
write.csv(data,"E:/UKB/UKB94885/Delirium/participants8missbucong.csv")
# 对数据进缺失值插补
data$Sex=as.factor(data$Sex)
data[,6]=as.factor(data[,6])
data[,7:8]=as.factor(data[,7:8])
# 2 构建初级cox 模型--------------------------------------------------------------------
library(survival)
library(rms)
library(tableone)
data=read.csv("E:/UKB/UKB94885/Delirium/participants8missbucong.csv")
data=data[,c(-1,-2,-3)]
data=na.omit(data)
for (i in c(4:14))
{
 data[,i]=as.factor(data[,i]) 
}
data[,27]=as.factor(data[,27])
#data[,29]=as.factor(data[,29])
for (i in c(45:47))
{
  data[,i]=as.factor(data[,i]) 
}
for (i in c(56:59))
{
  data[,i]=as.factor(data[,i])
}
for (i in c(62:63))
{
  data[,i]=as.factor(data[,i])
}
for (i in c(171:197))
{
  data[,i]=as.factor(data[,i])
}

# data$multiple_birth=as.factor(data$multiple_birth)
# data$myocardial.infarction=as.factor(data$myocardial.infarction)

## 一些连续型参数按照分位数变成分类变量
data1=data
# Trafic_intensity
q1=as.data.frame(quantile(data1$Trafic_intensity_2,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+1
data1$Trafic_intensity_2=cut(data1$Trafic_intensity_2,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
id=which(colnames(data1)=="obesity")
data1=data[,-id]
# Employment score
q1=as.data.frame(quantile(data1$Empolyment.score,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+1
data1$Empolyment.score=cut(data1$Empolyment.score,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
 # income score
q1=as.data.frame(quantile(data1$Income.score,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+1
data1$Income.score=cut(data1$Income.score,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
# 	Immature.reticulocyte.fraction
q1=as.data.frame(quantile(data1$Immature.reticulocyte.fraction,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+1
data1$Immature.reticulocyte.fraction=cut(data1$Immature.reticulocyte.fraction,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
## High.light.scatter.reticulocyte.count
q1=as.data.frame(quantile(data1$High.light.scatter.reticulocyte.count,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+1
data1$High.light.scatter.reticulocyte.count=cut(data1$High.light.scatter.reticulocyte.count,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
#  # 将trafic intensity 变为三分类
q1=as.data.frame(quantile(data1$Trafic_intensity_2,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+1
data1$Trafic_intensity_2=cut(data1$Trafic_intensity_2,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)

write.csv(data1,"E:/UKB/UKB94885/Delirium/participants9.csv")
# ####
data=read.csv("E:/UKB/UKB94885/Delirium/participants9.csv") # 194 个变量
 data=data[,-1]
 data=data1
 # 调整body  size at age 10
 id1=which(data$Body.size.at.10==1)
 id2=which(data$Body.size.at.10==2)
 id3=which(data$Body.size.at.10==3)
 data$Body.size.at.10[id1]=3
 data$Body.size.at.10[id3]=1
 data$Body.size.at.10=as.factor(data$Body.size.at.10)
 library(survival)
 multi=coxph(Surv(time,status) ~Age+Sex+Center+data$Body.size.at.10, data=data)
 summary(multi)
 for (i in c(4:14))
 {
   data[,i]=as.factor(data[,i]) 
 }
 data[,27]=as.factor(data[,27])
 #data[,29]=as.factor(data[,29])
 for (i in c(45:47))
 {
   data[,i]=as.factor(data[,i]) 
 }
 for (i in c(56:59))
 {
   data[,i]=as.factor(data[,i])
 }
 for (i in c(62:63))
 {
   data[,i]=as.factor(data[,i])
 }
 for (i in c(171:196))
 {
   data[,i]=as.factor(data[,i])
 }
 
 
 # # Immature.reticulocyte.fraction
 # q1=as.data.frame(quantile(data$Immature.reticulocyte.fraction,probs = seq(0,1,0.3333)))
 # breaks=q1[1:4,]
 # breaks[1]=breaks[1]-1
 # breaks[4]=breaks[4]+1
 # data$Immature.reticulocyte.fraction=cut(data$Immature.reticulocyte.fraction,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
# data$Immature.reticulocyte.fraction=
 #data=data1
n1=dim(data)
vname=names(data)
beta=vname
multi3=data[1:n1[2],1:2]
#data$Center=as.factor(data$Center)
multi=coxph(Surv(time,status) ~Age+Sex+Center+data[,vname[7]], data=data)
schoenfeld_test <- cox.zph(multi)
##
multi1=coxph(Surv(time,status) ~Age+Sex+Center+data[,vname[7]]+Age*Center, data=data)
schoenfeld_test1 <- cox.zph(multi1)
multi2=summary(multi)
colnames(multi3)=c("eid","p")
# colnames(multi3)=colnames(multi2$conf.int)
k=1
id1=sapply(data,is.factor) 
id2=which(id1==1)
for (i in 1:n1[2])
{
is_in= i %in% id2
if (is_in) {
 x1=data[,i]
 x2=length(levels(x1))
 k=k+x2
} else {
k=k+1
}
}
# 构建一个空的dataframe
# 创建空数据框
Pvalue <- data.frame(matrix(ncol = 6, nrow = 290))
x_p=data.frame(matrix(ncol = 1, nrow = 196))
# 指定列名
colnames(Pvalue) <- c("Vname", "P", "HR","95%lower","95%uper","HR[95%]")
k=28
for (i in 7:n1[2])
{
  mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+skin_colour+data[,i], data=data)
 mul_cox1 <- summary(mul_cox) 
 schoenfeld_test1 <- cox.zph(mul_cox)
  x_p[i,1]=schoenfeld_test1$table[4,3]
  if (x_p[i,1]<0.0001)
  {
    mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data[,i]+Age*data[,i], data=data)
    mul_cox1 <- summary(mul_cox)
  }
  t=mul_cox1[["coefficients"]]
  colnames(mul_cox1$conf.int)
  multi1<-as.data.frame(round(mul_cox1$conf.int[, c(1, 3, 4)], 7))
  multi2<-ShowRegTable(mul_cox, 
                       exp=TRUE, 
                       digits=4, 
                       pDigits =20,
                       printToggle = TRUE, 
                       quote=FALSE, 
                       ciFun=confint)
  n=dim(t)
  if (n[1]>29)
  {
    k=k+1
  Pvalue$`HR[95%]`[k:(k+n[1]-29)]=multi2[29:n[1],1]
  Pvalue$P[k:(k+n[1]-29)]=t[29:n[1],5]
  l=paste(vname[i],"L",sep="")
  name=paste(l,2:(n[1]-27),sep="")
  Pvalue$Vname[k:(k+n[1]-29)]=name
  Pvalue$HR[k:(k+n[1]-29)]=t[29:n[1],2]
  Pvalue$`95%lower`[k:(k+n[1]-29)]=multi1[29:n[1],2]
  Pvalue$`95%uper`[k:(k+n[1]-29)]=multi1[29:n[1],3]
  k=k+n[1]-29
  }
  else
  {
    k=k+1
    Pvalue$`HR[95%]`[k]=multi2[n[1],1]
    Pvalue$P[k]=t[n[1],5]
    Pvalue$Vname[k]=vname[i]
    Pvalue$HR[k]=t[n[1],2]
    Pvalue$`95%lower`[k]=multi1[n[1],2]
    Pvalue$`95%uper`[k]=multi1[n[1],3]
  }
}
# multi3$vname=names(data)
# multi3$p=as.numeric(multi3$p)
write.csv(Pvalue,"E:/UKB/UKB94885/Delirium/cox_singleresult1.csv")
write.csv(data,"E:/UKB/UKB94885/Delirium/participants9.csv")
# 画曼哈顿图
data1=read.csv("E:/UKB/UKB94885/Delirium/cox_singleresult.csv")
data1=read.csv("E:/UKB/UKB94885/Delirium/cox_singleresult1_1.csv")
data1=na.omit(data1)
# library(qqman)
# manhattan(data1,
#           main = "Manhattan Plot",  #设置主标题
#           ylim = c(0,100),   #设置Y轴范围
#           cex = 1,    #设置点的大小
#           cex.axis = 0.9,  #设置坐标轴字体大小
#           col = c("blue4","dark magenta","orange3","green","red","cyan","light pink"),   #设置散点的颜色
#           suggestiveline = -log10(2.5e-5), genomewideline = F,   #remove the suggestive and genome-wide significance lines
#           chrlabs = c(paste("chr",c(1:20)),"P","Q")  #设置X轴染色体标签签名
# )
## ggplot
library(ggplot2)
library(dplyr)
library(ggrepel)
CHR=c(1:7)
LEN=c(10,20,20,20,20,20,20)
chr.len=data.frame(CHR,LEN)
chr.len %>% pull(LEN) %>% cumsum() -> x1
chr.len %>% pull(LEN) -> x2
#
head(x1,-1)
temp.df<-data.frame(chromo=chr.len %>% pull(CHR),
                    chr_len=c(0,head(x1,-1)))
gwas.results=data1
gwas.results %>% 
  left_join(temp.df,by=c("CHR"="chromo")) %>% 
  mutate(new_pos=BP+chr_len) -> new.gwas.results
# 画图
cols=c("#008087","dark magenta","#f98177","#74BC56","#E53636","#3D7ABB","yellow")
cols<-c("#008087","dark magenta","#f98177","#80b3d4","green","#3D7ABB","#EE8B15")
# cols<-c("#bdbadb","#fdb363","#f98177","#80b3d4","gray")
new.gwas.results$CHR=as.factor(new.gwas.results$CHR)
cols=c("#008087","dark magenta","#f98177","#74BC56","#E53636","#3D7ABB","dodgerblue")
ggplot(data=new.gwas.results,
       aes(x=new_pos,y=-log10(P)))+
  geom_point(aes(color=CHR),show.legend = FALSE)+
  # scale_x_continuous(breaks =c(0,head(x1,-1)) + x2/2 ,
  #                    labels =temp.df %>% pull(chromo))+
  scale_x_continuous(breaks =c(0,head(x1,-1)) + x2/2 ,
                     labels =c("Ear life","Envoriment","Social factor",
                               "Life style","Physical measurement","Blood assay","Medical history"))+
  theme_bw()+
   theme(panel.grid.major = element_line(colour="gray",linewidth=0.3),
         panel.grid.minor=element_blank(),
         panel.grid.major.x=element_blank(),
         panel.background = element_rect(fill=rgb(250, 240, 230, maxColorValue = 255)),
        panel.border = element_blank(),
         axis.line = element_line(),
        axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.5))+
  scale_color_manual(values = cols)+
  scale_y_continuous(expand = expansion(mult = c(0,0)),
                     limits = c(0,300),
                     breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200))+
    geom_hline(yintercept = 3.6,lty="dashed")
  # theme(
  #   panel.background = element_rect(fill = "lightblue")
  # )
id=which(data1$P<0.00025)
##################################################################
### 给点添加标注
id1=which(data1$P<1e-10)
label=data1$SNP
label[-id1]=NA
ggplot(data=new.gwas.results,
       aes(x=new_pos,y=-log10(P)))+
  geom_point(aes(color=CHR),show.legend = FALSE)+
  geom_label(aes(label = label), vjust = -0.5, na.rm=TRUE,
             label.padding = unit(0.1,"lines"))+
  scale_x_continuous(breaks =c(0,head(x1,-1)) + x2/2 ,
                     labels =c("Ear life","Envoriment","Social factor",
                               "Life style","Physical measurement","Blood assay","Medical history"))+
  
  theme_bw()+
  theme(panel.grid.major = element_line(colour="gray",linewidth=0.7),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.background = element_rect(fill=rgb(250, 240, 230, maxColorValue = 255)),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.5))+
  scale_color_manual(values = cols)+
  scale_y_continuous(expand = expansion(mult = c(0,0)),
                     limits = c(0,200),
                     breaks = c(0,10,20,30,40,50,60,70,80,90,200,300,400))+
  geom_hline(yintercept = 3.6,lty="dashed")
##
# vjust=-0.5
# vjust[1:183]=-0.5
# hjust=0.5
vjust=data1$vjust
hjust=data1$hjust
ggplot(data=new.gwas.results,
       aes(x=new_pos,y=-log10(P)))+
  geom_point(aes(color=CHR),show.legend = FALSE)+
  geom_text(aes(label = label), vjust = vjust, hjust=hjust, na.rm=TRUE,
            check_overlap = FALSE, inherit.aes = TRUE, size=1.8,
             label.padding = unit(0.1,"lines"))+
  scale_x_continuous(breaks =c(0,head(x1,-1)) + x2/2 ,
                     labels =c("Ear life","Envoriment","Social factor",
                               "Life style","Physical measurement","Blood assay","Medical history"))+
  
  theme_bw()+
  theme(panel.grid.major = element_line(colour="gray",linewidth=0.3),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.background = element_rect(fill=rgb(250, 240, 230, maxColorValue = 255)),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.3))+
  scale_color_manual(values = cols)+
  scale_y_continuous(expand = expansion(mult = c(0,0)),
                     limits = c(0,200),
                     breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200))+
  geom_hline(yintercept = 3.6,lty="dashed")
# 画森林图
data2=read.csv("E:/UKB/UKB94885/Delirium/cox_singleresult1_1.csv")
data3=data2
data1$SNP=data1$Vname
vname=data1$SNP
n=dim(data1)
for (i in 1:n[1])
{
  id=which(data2$Vname==vname[i])
  data3[i,]=data2[id,]
}
write.csv(data3,"E:/UKB/UKB94885/Delirium/cox_singleresult2.csv")
#############
# 
data3=read.csv("E:/UKB/UKB94885/Delirium/cox_singleresult2.csv")
data3$Vname=data3$SNP
#data3$Vname=as.factor(data3$Vname)
# High.light.scatter.reticulocyte.count
# id=which(vname=="High.light.scatter.reticulocyte.count")
# data3$HR[id]=1
# data3$X95.lower[id]=1
# data3$X95.uper[id]=1
# early life
data3=data3[,-1]
data4=data3[1:70,]
data4=data3[71:140,]
data4=data3[141:210,]
 #data4=data3[157:210,]
data4=data4[nrow(data4):1,]
# data5=data4
# data4 <- data4[order(data4$Vname, decreasing = TRUE), ]
#  data4=rev(data4)
data4$Vname=factor(data4$Vname,levels=unique(data4$Vname))
#data4$Number=c(1:188)
p1<- ggplot(data4,aes(x=HR,y=Vname))+
  theme_bw()+
  theme(legend.position = "none")+ #去掉图注
  geom_point()
  # geom_rect(aes(xmin = -Inf, ymin = 0,
  #               xmax = Inf, ymax = 8),#填充0-3.5的棕色色块
  #           fill = "#008087", color = "#008087", size =1.5)
p1
p1+ theme(axis.text = element_text(color = "black"))
#
# p2<- p1+ geom_rect(aes(xmin = -Inf, ymin = 3.4,
#                        xmax = Inf, ymax = 7.5),
#                    fill = "#BFD6ED", color = "#BFD6ED", size =1.5)+
#   geom_rect(aes(xmin = -Inf, ymin = 0,
#                 xmax = Inf, ymax = 9.5),
#             fill = "#C7DFB5", color = "#C7DFB5", size =1.5)
# p2
##
data4$col="gray"
id1=which(data4$CHR==1)
data4$col[id1]="#008087"
id2=which(data4$CHR==2)
data4$col[id2]="dark magenta"
id3=which(data4$CHR==3)
data4$col[id3]="#f98177"
id4=which(data4$CHR==4)
data4$col[id4]="#74BC56"
id5=which(data4$CHR==5)
data4$col[id5]="#E53636"
id6=which(data4$CHR==6)
data4$col[id6]="#3D7ABB"
id7=which(data4$CHR==7)
data4$col[id7]="dodgerblue"
id=which(data4$P>0.00027)
data4$col[id]='gray'
p3<- p1+geom_point(size=2,col=data4$col)+#画点
  scale_fill_manual(values = c("#008087","dark magenta",
                               "#f98177","#74BC56","#E53636","#3D7ABB","dodgerblue"))+
  scale_color_manual(values = c("#008087","dark magenta",
                                "#f98177","#74BC56","#E53636","#3D7ABB","dodgerblue"))+
geom_curve(aes(x = data4$X95.lower, y = data4$Vname, xend = data4$X95.uper, yend = data4$Vname),
           #arrow = arrow(length = unit(0.02, "npc"), type="closed"),
           colour = data4$col, size = 1, angle = 0)+
  theme_bw()+
  theme(panel.grid.major = element_line(colour="white",linewidth=0.7),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.background = element_rect(fill=rgb(250, 240, 230, maxColorValue = 255)),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x=element_text(angle=0,hjust=1,vjust=1))+
  geom_vline(aes(xintercept=1), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=0.8), colour="gray", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=1.2), colour="gray", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=1.4), colour="gray", linetype="dashed", size=0.3)
p3+ theme(axis.text = element_text(color = "black"))
##
# 将HR 按照从大到小排序
HR=order(data3$P)
data3[HR[1:10],]
###################################
# HR在各组中的系数
# total group
data1=read.csv("E:/UKB/UKB94885/Delirium/participants9.csv") # 194 个变量
#data=data[,-1]
data=data1
# 调整body  size at age 10
id1=which(data$Body.size.at.10==1)
id2=which(data$Body.size.at.10==2)
id3=which(data$Body.size.at.10==3)
data$Body.size.at.10[id1]=3
data$Body.size.at.10[id3]=1
data$Body.size.at.10=as.factor(data$Body.size.at.10)
library(survival)
multi=coxph(Surv(time,status) ~Age+Sex+Center+data$Body.size.at.10, data=data)
summary(multi)
for (i in c(4:14))
{
  data[,i]=as.factor(data[,i]) 
}
data[,27]=as.factor(data[,27])
#data[,29]=as.factor(data[,29])
for (i in c(45:47))
{
  data[,i]=as.factor(data[,i]) 
}
for (i in c(56:59))
{
  data[,i]=as.factor(data[,i])
}
for (i in c(62:63))
{
  data[,i]=as.factor(data[,i])
}
for (i in c(171:196))
{
  data[,i]=as.factor(data[,i])
}
# age <60
id1=which(data$Age<60)
data3=data
data1=data[id1,]
data2=data[-id1,]
id=which(data$time>60)
data=data3[id,]

n1=dim(data)
vname=names(data)
beta=vname
multi3=data[1:n1[2],1:2]
#data$Center=as.factor(data$Center)
multi=coxph(Surv(time,status) ~Age+Sex+Center+data[,vname[7]], data=data)
schoenfeld_test <- cox.zph(multi)
##
multi1=coxph(Surv(time,status) ~Age+Sex+Center+data[,vname[7]]+Age*Center, data=data)
schoenfeld_test1 <- cox.zph(multi1)
multi2=summary(multi)
colnames(multi3)=c("eid","p")
# colnames(multi3)=colnames(multi2$conf.int)
k=1
id1=sapply(data,is.factor) 
id2=which(id1==1)
for (i in 1:n1[2])
{
  is_in= i %in% id2
  if (is_in) {
    x1=data[,i]
    x2=length(levels(x1))
    k=k+x2
  } else {
    k=k+1
  }
}
# 构建一个空的dataframe
# 创建空数据框
Pvalue <- data.frame(matrix(ncol = 6, nrow = 290))
x_p=data.frame(matrix(ncol = 1, nrow = 196))
# 指定列名
colnames(Pvalue) <- c("Vname", "P", "HR","95%lower","95%uper","HR[95%]")
k=28
for (i in 7:n1[2])
{
  mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+skin_colour+data[,i], data=data)
  mul_cox1 <- summary(mul_cox) 
  schoenfeld_test1 <- cox.zph(mul_cox)
  x_p[i,1]=schoenfeld_test1$table[4,3]
  if (x_p[i,1]<0.0001)
  {
    mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data[,i]+Age*data[,i], data=data)
    mul_cox1 <- summary(mul_cox)
  }
  t=mul_cox1[["coefficients"]]
  colnames(mul_cox1$conf.int)
  multi1<-as.data.frame(round(mul_cox1$conf.int[, c(1, 3, 4)], 7))
  multi2<-ShowRegTable(mul_cox, 
                       exp=TRUE, 
                       digits=4, 
                       pDigits =20,
                       printToggle = TRUE, 
                       quote=FALSE, 
                       ciFun=confint)
  n=dim(t)
  if (n[1]>29)
  {
    k=k+1
    Pvalue$`HR[95%]`[k:(k+n[1]-29)]=multi2[29:n[1],1]
    Pvalue$P[k:(k+n[1]-29)]=t[29:n[1],5]
    l=paste(vname[i],"L",sep="")
    name=paste(l,2:(n[1]-27),sep="")
    Pvalue$Vname[k:(k+n[1]-29)]=name
    Pvalue$HR[k:(k+n[1]-29)]=t[29:n[1],2]
    Pvalue$`95%lower`[k:(k+n[1]-29)]=multi1[29:n[1],2]
    Pvalue$`95%uper`[k:(k+n[1]-29)]=multi1[29:n[1],3]
    k=k+n[1]-29
  }
  else
  {
    k=k+1
    Pvalue$`HR[95%]`[k]=multi2[n[1],1]
    Pvalue$P[k]=t[n[1],5]
    Pvalue$Vname[k]=vname[i]
    Pvalue$HR[k]=t[n[1],2]
    Pvalue$`95%lower`[k]=multi1[n[1],2]
    Pvalue$`95%uper`[k]=multi1[n[1],3]
  }
}
P1=Pvalue # P1 whole population
P2=Pvalue # P2 age<60
P3=Pvalue #p3 Age>=60
P4=Pvalue # p4 female
P5=Pvalue # p5 male
P6=Pvalue # p6 follow up time > 5 years
# p7 local 居住大于5年
data=read.csv("E:/UKB/UKB94885/Delirium/participants8missbucong.csv")
data=data[,-c(1:2,27,31,191)]
target_col=target_col[,c(1,2)]
data=merge(data,target_col,by="eid")
Score=Score[,c(1,2)]
data=merge(data,Score, by="eid")
data=data[,-c(1,199)]
id=which(data$`699-0.0`<5)
data=data[-id,-c(197)]
for (i in c(4:14))
{
  data[,i]=as.factor(data[,i]) 
}
data[,27]=as.factor(data[,27])
#data[,29]=as.factor(data[,29])
for (i in c(46:48))
{
  data[,i]=as.factor(data[,i]) 
}
data[,79]=as.factor(data[,79])

for (i in c(85:88))
{
  data[,i]=as.factor(data[,i])
}
for (i in c(62:63))
{
  data[,i]=as.factor(data[,i])
}
for (i in c(171:196))
{
  data[,i]=as.factor(data[,i])
}

n1=dim(data)
vname=names(data)
beta=vname
multi3=data[1:n1[2],1:2]
#data$Center=as.factor(data$Center)
multi=coxph(Surv(time,status) ~Age+Sex+Center+data[,vname[7]], data=data)
schoenfeld_test <- cox.zph(multi)
##
multi1=coxph(Surv(time,status) ~Age+Sex+Center+data[,vname[7]]+Age*Center, data=data)
schoenfeld_test1 <- cox.zph(multi1)
multi2=summary(multi)
colnames(multi3)=c("eid","p")
# colnames(multi3)=colnames(multi2$conf.int)
k=1
id1=sapply(data,is.factor) 
id2=which(id1==1)
for (i in 1:n1[2])
{
  is_in= i %in% id2
  if (is_in) {
    x1=data[,i]
    x2=length(levels(x1))
    k=k+x2
  } else {
    k=k+1
  }
}
# 构建一个空的dataframe
# 创建空数据框
Pvalue <- data.frame(matrix(ncol = 6, nrow = 290))
x_p=data.frame(matrix(ncol = 1, nrow = 196))
# 指定列名
colnames(Pvalue) <- c("Vname", "P", "HR","95%lower","95%uper","HR[95%]")
k=28
for (i in 7:n1[2])
{
  mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+skin_colour+data[,i], data=data)
  mul_cox1 <- summary(mul_cox) 
  schoenfeld_test1 <- cox.zph(mul_cox)
  x_p[i,1]=schoenfeld_test1$table[4,3]
  if (x_p[i,1]<0.0001)
  {
    mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data[,i]+Age*data[,i], data=data)
    mul_cox1 <- summary(mul_cox)
  }
  t=mul_cox1[["coefficients"]]
  colnames(mul_cox1$conf.int)
  multi1<-as.data.frame(round(mul_cox1$conf.int[, c(1, 3, 4)], 7))
  multi2<-ShowRegTable(mul_cox, 
                       exp=TRUE, 
                       digits=4, 
                       pDigits =20,
                       printToggle = TRUE, 
                       quote=FALSE, 
                       ciFun=confint)
  n=dim(t)
  if (n[1]>29)
  {
    k=k+1
    Pvalue$`HR[95%]`[k:(k+n[1]-29)]=multi2[29:n[1],1]
    Pvalue$P[k:(k+n[1]-29)]=t[29:n[1],5]
    l=paste(vname[i],"L",sep="")
    name=paste(l,2:(n[1]-27),sep="")
    Pvalue$Vname[k:(k+n[1]-29)]=name
    Pvalue$HR[k:(k+n[1]-29)]=t[29:n[1],2]
    Pvalue$`95%lower`[k:(k+n[1]-29)]=multi1[29:n[1],2]
    Pvalue$`95%uper`[k:(k+n[1]-29)]=multi1[29:n[1],3]
    k=k+n[1]-29
  }
  else
  {
    k=k+1
    Pvalue$`HR[95%]`[k]=multi2[n[1],1]
    Pvalue$P[k]=t[n[1],5]
    Pvalue$Vname[k]=vname[i]
    Pvalue$HR[k]=t[n[1],2]
    Pvalue$`95%lower`[k]=multi1[n[1],2]
    Pvalue$`95%uper`[k]=multi1[n[1],3]
  }
}
#  去除participants9不显著的变量 （手动），保存为participnats10

###
# 去共线性
data=read.csv("E:/UKB/UKB94885/Delirium/participants10.csv")
data=data[,-1]
# fat_leg, fat_arm
data$Fat_arm=(data$Fat_arm_l+data$Fat_arm_r)/2
data$Fat_leg=(data$Fat_leg.r+data$Fat_leg_l)/2
data$Hand_grip=(data$Hand_grip_1+data$Hand_grip_2)/2
id1=which(names(data)=="Fat_arm_l")
id2=which(names(data)=="Fat_arm_r")
id3=which(names(data)=="Fat_leg_l")
id4=which(names(data)=="Fat_leg.r")
data=data[,c(-id1,-id2,-id3,-id4)]
id5=which(names(data)=="Hand_grip_1")
id6=which(names(data)=="Hand_grip_2")
data=data[,c(-id5,-id6)]
# write.csv(data,"E:/UKB/UKB94885/Delirium/participants11.csv")
# data=read.csv("E:/UKB/UKB94885/Delirium/participants11.csv")
id=which(colnames(data)=="No2")
data1=data[,1:47]
data=data[,48:112]
cormatrix=cor(data,method="spearman")
library(corrplot)
n=dim(cormatrix)
for (i in 1:n[1])
{cormatrix[i,i]=0
}
write.csv(cormatrix,"E:/UKB/UKB94885/Delirium/cormatrix.csv")
commatrix=read.csv("E:/UKB/UKB94885/Delirium/cormatrix.csv")
cormatrix=commatrix[,-1]
corrplot(cormatrix, tl.pos ='n')
heatmap(cormatrix)
Air=data[,c(1,2,68,67,66,65,64)]
# install.packages("matrixStats")
# library(matrixStats)
library(data.table)
Air= as.data.table(Air)
coomatrix=colCor(Air,method="kendall")
cormatrix=cor(Air, method="kendall")
corrplot(cormatrix,tl.cex=0.4,tl.col="black",method="shade",
         shade.col=NA,col= colorRampPalette(c("blue", "white", "red"))(100))
#corrplot.mixed(cormatrix,tl.cex=0.4,lower="number",upper="color",cl.pos="n")
##
max(cormatrix)
min(cormatrix)
library(caret)
highly_correlated <- findCorrelation(cormatrix, cutoff = 0.9)
colnames(data)[highly_correlated]
less_correlated <- data[, -highly_correlated]
data1[,48:100]=less_correlated
write.csv(less_correlated,"E:/UKB/UKB94885/Delirium/participant11.csv")
###
# PRS 的计算
# 2 读取FinGen 的GWAS 文件 -----------------------------------------------------
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("readr")
library(data.table)
library(dplyr)
library(readr)
data=read.delim("E:/文章/Delirium/GWAS summary statistics/summary_stats_finngen_R9_F5_DELIRIUM")
id=which(data$pval<5E-8)
data1=data[id,]
##
weight4=read.table("F:/GWAS/ukb22418_c1_22_v2_merged.bim")
colnames(weight4)=c("V1","rsids","V3","V4","V5","V6")
x1=data1$rsids
x2=weight4$V2
n1=length(x1)
id=x1
weight=merge(data1,weight4,by="rsids")
which(x1=="rs10119")
weight5=weight[,c(1,2,3,4,5,9)]
write.table(weight5,"F:/GWAS/summary_stats_finngen_R9_F5_DELIRIUM_new.txt",quote=FALSE,row.names=FALSE)
##
# 上传到 ukb analysis platfrom 计算PRS
PRS=read.table("F:/GWAS/plink_PRS_delirium.txt")
colnames(PRS)[1]="eid"
data1=merge(data1,PRS, by="eid")
write.csv(data1,"E:/UKB/UKB94885/Delirium/participant12.csv")
data1=read.csv("E:/UKB/UKB94885/Delirium/participant12.csv")

# 2 计算各个领域的score ----------------------------------------------------------
library(survival)
# early life
data=read.csv("E:/UKB/UKB94885/Delirium/participant12eralylifefactors.csv")
data$body_size_10=as.factor(data$body_size_10)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data$body_size_10, data=data)
mul_cox1 <- summary(mul_cox) 
# reverse code
id1=which(data$body_size_10==1)
id2=which(data$body_size_10==2)
id3=which(data$body_size_10==3)
data$body_size_10[id1]=2
data$body_size_10[id2]=3
data$body_size_10[id3]=1
data$body_size_10=as.factor(data$body_size_10)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data$body_size_10, data=data)
mul_cox1 <- summary(mul_cox) 
Score=data[,c(1:6,8)]
Score$earlylife=data$body_size_10
write.csv(Score,"E:/UKB/UKB94885/Delirium/score.csv")
##
# life style
data1=read.csv("E:/UKB/UKB94885/Delirium/participant12lifestyle.csv")
# data2=read.csv("E:/UKB/UKB94885/Delirium/participants9.csv")
# id=which(colnames(data2)=="DietScore")
# id1=which(colnames(data2)=="Age")
# x=data2[,c(2,id,id1)]
# data1=merge(data1,x,by="eid")
# write.csv(data1,"E:/UKB/UKB94885/Delirium/participant12lifestyle.csv")
Score1=data1
# stair climbing frequency
data1$stair_climbing=as.factor(data1$stair_climbing)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$stair_climbing, data=data1)

data1$stair_climbing=as.numeric(data1$stair_climbing)
id1=which(data1$stair_climbing==1)
id2=which(data1$stair_climbing==2)
id3=which(data1$stair_climbing==3)
id4=which(data1$stair_climbing==4)
id5=which(data1$stair_climbing==5)
id6=which(data1$stair_climbing==6)
data1$stair_climbing[id1]=5
data1$stair_climbing[id2]=4
data1$stair_climbing[id3]=3
data1$stair_climbing[id4]=2
data1$stair_climbing[id5]=1
data1$stair_climbing[id6]=0
data1$stair_climbing=as.factor(data1$stair_climbing)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$stair_climbing, data=data1)


# cheese intake frequency 奶酪
data1$Cheese=as.factor(data1$Cheese)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$Cheese, data=data1)

data1$Cheese=as.numeric(data1$Cheese)
id1=which(data1$Cheese==1)
id2=which(data1$Cheese==2)
id3=which(data1$Cheese==3)
id4=which(data1$Cheese==4)
id5=which(data1$Cheese==5)
id6=which(data1$Cheese==6)
data1$Cheese[id1]=5
data1$Cheese[id2]=4
data1$Cheese[id3]=3
data1$Cheese[id4]=2
data1$Cheese[id5]=0
data1$Cheese[id6]=1
data1$Cheese=as.factor(data1$Cheese)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$Cheese, data=data1)

# id1=which(data1$Cheese==0)
# id2=which(data1$Cheese==1)
# id3=which(data1$Cheese==2)
# id4=which(data1$Cheese==3)
# id5=which(data1$Cheese==4)
# id6=which(data1$Cheese==5)
# Score1$Cheese=as.numeric(Score1$Cheese)
# Score1$Cheese[id1]=0
# Score1$Cheese[id2]=mul_cox$coefficients[4]
# Score1$Cheese[id3]=mul_cox$coefficients[5]
# Score1$Cheese[id4]=mul_cox$coefficients[6]
# Score1$Cheese[id5]=mul_cox$coefficients[7]
# Score1$Cheese[id6]=mul_cox$coefficients[8]

# bread type
#data=read.csv("E:/UKB/UKB94885/Delirium/participant12lifestyle.csv")
# id=which(colnames(data)=="Bread_type")
# data=data[,c(1,id)]
# id=which(colnames(data1)=="Bread_type")
# data1=data1[,-id]
# data1=merge(data1,data,by="eid")
data1$Bread_type=Score1$Bread_type
data1$Bread_type=as.factor(data1$Bread_type)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$Bread_type, data=data1)

data1$Bread_type=as.numeric(data1$Bread_type)
id1=which(data1$Bread_type==1)
id2=which(data1$Bread_type==2)
id3=which(data1$Bread_type==3)
id4=which(data1$Bread_type==4)
data1$Bread_type[id1]=3
data1$Bread_type[id2]=2
data1$Bread_type[id3]=0
data1$Bread_type[id4]=1

data1$Bread_type=as.factor(data1$Bread_type)
# mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$Cheese, data=data1)
# mul_cox1 <- summary(mul_cox) 
# data1$Cheese=as.numeric(data1$Cheese)
# id1=which(data1$Cheese==1)
# id2=which(data1$Cheese==2)
# id3=which(data1$Cheese==3)
# id4=which(data1$Cheese==4)
# 
# Score1$Bread_type=as.numeric(Score1$Bread_type)
# Score1$Bread_type[id1]=0
# Score1$Bread_type[id2]=mul_cox$coefficients[4]
# Score1$Bread_type[id3]=mul_cox$coefficients[5]
# Score1$Bread_type[id4]=mul_cox$coefficients[6]
# write.csv(Score1,"E:/UKB/UKB94885/Delirium/Score1.csv")
# health Diet score
# Score1=read.csv("E:/UKB/UKB94885/Delirium/Score1.csv")
# Score1=Score1[,-c(1,2,4,5)]
# Score1=na.omit(Score1)
# data1=Score1 # 4880048
Score1=na.omit(Score1)
data1$DietScore=Score1$DietScore
data1=na.omit(data1) # 521
data1$DietScore=as.factor(data1$DietScore)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$DietScore, data=data1)
data1$DietScore=as.numeric(data1$DietScore)
id1=which(data1$DietScore==1)
id2=which(data1$DietScore==2)
id3=which(data1$DietScore==3)
id4=which(data1$DietScore==4)
id5=which(data1$DietScore==5)
id6=which(data1$DietScore==6)
data1$DietScore[id1]=5
data1$DietScore[id2]=4
data1$DietScore[id3]=3
data1$DietScore[id4]=2
data1$DietScore[id5]=0
data1$DietScore[id6]=1
data1$DietScore=as.factor(data1$DietScore)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$DietScore, data=data1)

# id1=which(data1$Cheese==0)
# id2=which(data1$Cheese==1)
# id3=which(data1$Cheese==2)
# id4=which(data1$Cheese==3)
# id5=which(data1$Cheese==4)
# id6=which(data1$Cheese==5)
# Score1$DietScore=as.numeric(Score1$DietScore)
# Score1$DietScore[id1]=0
# Score1$DietScore[id2]=mul_cox$coefficients[4]
# Score1$DietScore[id3]=mul_cox$coefficients[5]
# Score1$DietScore[id4]=mul_cox$coefficients[6]
# Score1$DietScore[id5]=mul_cox$coefficients[7]
# Score1$DietScore[id6]=mul_cox$coefficients[8]
# Score1=Score1[,-c(19,20)]
# write.csv(Score1,"E:/UKB/UKB94885/Delirium/Score1.csv")
# ##
# out door in winter
# Score1=read.csv("E:/UKB/UKB94885/Delirium/Score1.csv")
# data1=Score1
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$outdoor_winter, data=data1)
mul_cox1 <- summary(mul_cox) 
Score1$outdoor_winter=data1$outdoor_winter*mul_cox$coefficients[4]
## 
# frequency of Nap in a day
data1$Nap_day=as.factor(data1$Nap_day)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$Nap_day, data=data1)

# data1$Nap_day=as.numeric(data1$Nap_day)
# id1=which(data1$Nap_day==1)
# id2=which(data1$Nap_day==2)
# id3=which(data1$Nap_day==3)
# # data1$Cheese[id1]=3
# # data1$Cheese[id2]=2
# # data1$Cheese[id3]=1
# data1$Cheese=as.factor(data1$Cheese)
# mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$Cheese, data=data1)
# id1=which(data1$Cheese==1)
# id2=which(data1$Cheese==2)
# id3=which(data1$Cheese==3)
# Score1$Nap_day[id1]=0
# Score1$Nap_day[id2]=mul_cox$coefficients[4]
# Score1$Nap_day[id3]=mul_cox$coefficients[5]
# 
# data1$Cheese=as.factor(data1$Cheese)
# mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$Cheese, data=data1)
# mul_cox1 <- summary(mul_cox) 
# id1=which(data1$Cheese==0)
# id2=which(data1$Cheese==1)
# id3=which(data1$Cheese==2)
# id4=which(data1$Cheese==3)
# id5=which(data1$Cheese==4)
# id6=which(data1$Cheese==5)
# Score1$DietScore=as.numeric(Score1$DietScore)
# Score1$DietScore[id1]=0
# Score1$DietScore[id2]=mul_cox$coefficients[4]
# Score1$DietScore[id3]=mul_cox$coefficients[5]
# Score1$DietScore[id4]=mul_cox$coefficients[6]
# Score1$DietScore[id5]=mul_cox$coefficients[7]
# Score1$DietScore[id6]=mul_cox$coefficients[8]
write.csv(Score1,"E:/UKB/UKB94885/Delirium/Score1.csv")
##
# Snoring
data1$Snoring=as.factor(data1$Snoring)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$Snoring, data=data1)
# 
# id1=which(data1$Snoring==1)
# id2=which(data1$Snoring==2)
# Score1$Snoring[id1]=0
# Score1$Snoring[id2]=mul_cox$coefficients[4]
##
# Bread
# data1$Bread=as.numeric(data1$Bread)
# mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$Bread, data=data1)
# mul_cox1 <- summary(mul_cox) 
# Score1$Bread=data1$Bread*mul_cox$coefficients[4]
##
# Bowel of Cereal to eat a week
data1$Cereal=as.numeric(data1$Cereal)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$Cereal, data=data1)
mul_cox1 <- summary(mul_cox) 
# 连续型变量按分位数分为三组
q1=as.data.frame(quantile(data1$Cereal,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+1
data1$Cereal=cut(data1$Cereal,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
data1$Cereal=as.factor(data1$Cereal)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$Cereal, data=data1)
id1=which(data1$Cereal=="Q1")
id2=which(data1$Cereal=="Q2")
id3=which(data1$Cereal=="Q3")
data1$Cereal[id1]="Q3"
data1$Cereal[id2]="Q1"
data1$Cereal[id3]="Q2"
data1$Cereal[-c(id1,id2,id3)]="Q2"
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$Cereal, data=data1)
data1$Cereal=as.numeric(data1$Cereal)
data1$Cereal=as.factor(data1$Cereal)
# id1=which(data1$Cereal=="Q1")
# id2=which(data1$Cereal=="Q2")
# id3=which(data1$Cereal=="Q3")
# Score1$Cereal[id1]=0
# Score1$Cereal[id2]=mul_cox$coefficients[4]
# Score1$Cereal[id3]=mul_cox$coefficients[5]
# write.csv(Score1,"E:/UKB/UKB94885/Delirium/Score1.csv")
##
# Hot drink
# data=read.csv("E:/UKB/UKB94885/Delirium/participant12lifestyle.csv")
# id=which(colnames(data)=="Hotdrink")
# data=data[,c(1,id)]
# id=which(colnames(data1)=="Hotdrink")
# data1=data1[,-id]
# data1=merge(data1,data,by="eid")
# data1$Hotdrink=Score1$Hotdrink
id=which(colnames(Score1)=="Hotdrink")
x=Score1[,c(1,id)]
id=which(colnames(data1)=="Hotdrink")
data1=data1[,-id]
data1=merge(data1,x,by="eid")

data1$Hotdrink=as.factor(data1$Hotdrink)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$Hotdrink, data=data1)
data1$Hotdrink=as.numeric(data1$Hotdrink)
id1=which(data1$Hotdrink==1)
id2=which(data1$Hotdrink==2)
id3=which(data1$Hotdrink==3)
data1$Hotdrink[id1]=0
data1$Hotdrink[id2]=1
data1$Hotdrink[id3]=2
data1$Hotdrink=as.factor(data1$Hotdrink)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+data1$Hotdrink, data=data1)
# id1=which(data1$Hotdrink==1)
# id2=which(data1$Hotdrink==2)
# id3=which(data1$Hotdrink==3)
# Score1$Hotdrink[id1]=0
# Score1$Hotdrink[id2]=mul_cox$coefficients[4]
# Score1$Hotdrink[id3]=mul_cox$coefficients[5]
#
##
# Score1=read.csv("E:/UKB/UKB94885/Delirium/Score1.csv")
# data1=Score1
# smoking status
data1$Smoking_status=as.factor(data1$Smoking_status)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+Smoking_status, data=data1)
# id1=which(data1$Smoking_status==0)
# id2=which(data1$Smoking_status==1)
# id3=which(data1$Smoking_status==2)
# Score1$Smoking_status[id1]=0
# Score1$Smoking_status[id2]=mul_cox$coefficients[4]
# Score1$Smoking_status[id3]=mul_cox$coefficients[5]
##
# Alcohol status
# data1$Alcohol_status=Score1$Alcohol_status
data1$Alcohol_status=as.factor(data1$Alcohol_status)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+Alcohol_status, data=data1)
id1=which(data1$Alcohol_status==0)
id2=which(data1$Alcohol_status==1)
id3=which(data1$Alcohol_status==2)
data1$Alcohol_status=as.numeric(data1$Alcohol_status)
data1$Alcohol_status[id1]=1
data1$Alcohol_status[id2]=2
data1$Alcohol_status[id3]=0
data1$Alcohol_status=as.factor(data1$Alcohol_status)
# id1=which(data1$Alcohol_status==0)
# id2=which(data1$Alcohol_status==1)
# id3=which(data1$Alcohol_status==2)
# Score1$Alcohol_status[id1]=0
# Score1$Alcohol_status[id2]=mul_cox$coefficients[4]
# Score1$Alcohol_status[id3]=mul_cox$coefficients[5]
# write.csv(Score1,"E:/UKB/UKB94885/Delirium/Score1.csv")
##
# water intake
data1$Waterintake=as.numeric(data1$Waterintake)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+Waterintake, data=data1)
# 转换成分类数据
# 
# q1=as.data.frame(quantile(data1$Waterintake,probs = seq(0,1,0.3333)))
# breaks=q1[1:4,]
# breaks[1]=breaks[1]-1
# breaks[4]=breaks[4]+1
# data1$Waterintake=cut(data1$Waterintake,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
# data1$Waterintake=as.factor(data1$Waterintake)
# mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+Waterintake, data=data1)
# id1=which(data1$Waterintake=="Q1")
# id2=which(data1$Waterintake=="Q2")
# id3=which(data1$Waterintake=="Q3")
# Score1$Waterintake[id1]=0
# Score1$Waterintake[id2]=mul_cox$coefficients[4]
# Score1$Waterintake[id3]=mul_cox$coefficients[5]
write.csv(data1,"E:/UKB/UKB94885/Delirium/data1_1.csv")
##
#data1=read.csv("E:/UKB/UKB94885/Delirium/data1_1.csv")
id=which(colnames(data1)=="PRS")
data2=data1[,-c(1,id)]
mul_cox=coxph(Surv(time,status) ~., data=data2)
write.csv(data1,"E:/UKB/UKB94885/Delirium/data1_1.csv")
write.csv(mul_cox$coefficients,"E:/UKB/UKB94885/Delirium/liefcoeeficient.csv")
Score=read.csv("E:/UKB/UKB94885/Delirium/liefcoeeficient.csv")
id1=which(data1$stair_climbing==0)
data2=data1
data2$stair.Score[id1]=0
id2=which(data1$stair_climbing==1)
data2$stair.Score[id2]=1*Score$score[3]
id3=which(data1$stair_climbing==2)
data2$stair.Score[id3]=2*Score$score[4]
id4=which(data1$stair_climbing==3)
data2$stair.Score[id4]=3*Score$score[5]
id5=which(data1$stair_climbing==4)
data2$stair.Score[id5]=4*Score$score[6]
id6=which(data1$stair_climbing==5)
data2$stair.Score[id6]=5*Score$score[7]
#
data2$outdoor.Score=data1$outdoor_winter*Score$score[8]
#
id1=which(data1$Nap_day==1)
data2$nap.Score[id1]=0
id2=which(data1$Nap_day==2)
data2$nap.Score[id2]=1*Score$score[9]
id3=which(data1$Nap_day==3)
data2$nap.Score[id3]=2*Score$score[10]
#
id1=which(data1$Snoring==1)
data2$snoring.Score[id1]=0
id2=which(data1$Snoring==2)
data2$snoring.Score[id2]=1*Score$score[11]
write.csv(data2,"E:/UKB/UKB94885/Delirium/liefscore.csv")
data2=read.csv("E:/UKB/UKB94885/Delirium/liefscore.csv")
#
# cheese
data2$cheese.Score=0
id1=which(data2$Cheese==0)
data2$cheese.Score[id1]=0
id2=which(data1$Cheese==1)
data2$cheese.Score[id2]=1*Score$score[12]
id3=which(data1$Cheese==2)
data2$cheese.Score[id3]=2*Score$score[13]
id4=which(data1$Cheese==3)
data2$cheese.Score[id4]=3*Score$score[14]
id5=which(data1$Cheese==4)
data2$cheese.Score[id5]=4*Score$score[15]
id6=which(data1$Cheese==5)
data2$cheese.Score[id6]=5*Score$score[16]
#
# bread
data2$bread.Score=data1$Bread*Score$score[17]
# bread type
data2$breadtype.Score=0
id1=which(data1$Bread_type==0)
data2$breadtype.Score[id1]=0
id2=which(data1$Bread_type==1)
data2$breadtype.Score[id2]=Score$score[18]
id3=which(data1$Bread_type==2)
data2$breadtype.Score[id3]=2*Score$score[19]
id4=which(data1$Bread_type==3)
data2$breadtype.Score[id4]=3*Score$score[20]
# cereal
data2$cereal.Score=0
id1=which(data1$cereal==1)
data2$cereal.Score[id1]=Score$score[21]
id2=which(data1$cereal==2)
data2$cereal.Score[id2]=2*Score$score[22]
# hot dringking
data2$hotdrink.Score=0
id1=which(data1$Hotdrink==0)
data2$hotdrink.Score[id1]=0
id2=which(data1$Hotdrink==1)
data2$hotdrink.Score[id2]=Score$score[23]
id3=which(data1$Hotdrink==2)
data2$hotdrink.Score[id3]=2*Score$score[24]
# water intake
data2$waterintake.Score=data1$Waterintake*Score$score[25]
# smoking status
data2$smoking.Score=0
id1=which(data1$Smoking_status==0)
data2$smoking.Score[id1]=0
id2=which(data1$Smoking_status==1)
data2$smoking.Score[id2]=Score$score[26]
id3=which(data1$Smoking_status==2)
data2$smoking.Score[id3]=2*Score$score[27]

# alcohol status
data2$alcohol.Score=0
id1=which(data1$Alcohol_status==0)
data2$alcohol.Score[id1]=0
id2=which(data1$Alcohol_status==1)
data2$alcohol.Score[id2]=Score$score[28]
id3=which(data1$Alcohol_status==2)
data2$alcohol.Score[id3]=2*Score$score[29]
# Diet score
data2$diet.Score=0
id1=which(data1$DietScore==0)
data2$diet.Score[id1]=0
id2=which(data1$DietScore==1)
data2$diet.Score[id2]=Score$score[30]
id3=which(data1$DietScore==2)
data2$diet.Score[id3]=2*Score$score[31]
id4=which(data1$DietScore==3)
data2$diet.Score[id4]=3*Score$score[32]
id5=which(data1$DietScore==4)
data2$diet.Score[id5]=4*Score$score[33]
id6=which(data1$DietScore==5)
data2$diet.Score[id6]=5*Score$score[34]
# 
data2$Lifestyle.Score=rowSums(data2[,23:33])*32/sum(Score$score[3:34])
library(survival)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+Lifestyle.Score, data=data2)
write.csv(data2,"E:/UKB/UKB94885/Delirium/liefscore.csv")
## 分成三组
q1=as.data.frame(quantile(data2$Lifestyle.Score,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+1
data2$Lifestyle.Score_Q=cut(data2$Lifestyle.Score,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
data2$Lifestyle.Score_Q=as.factor(data2$Lifestyle.Score_Q)
mul_cox1=coxph(Surv(time,status) ~Age+Sex+Center+Lifestyle.Score_Q, data=data2)
# 


# 3 local environment -----------------------------------------------------
data=read.csv("E:/UKB/UKB94885/Delirium/participant12localenvironment.csv")
data$Water.hardness=as.factor(data$Water.hardness)
data$Sex=as.factor(data$Sex)
data=data[,-1]
data1=data[,-18]
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+Natural_environment_2, data=data1)
mul_cox=coxph(Surv(time,status) ~., data=data1)
mul_cox
id1=which(data1$Water.hardness==0)
id2=which(data1$Water.hardness==1)
id3=which(data1$Water.hardness==2)
id4=which(data1$Water.hardness==3)
data1$Water.hardness[id1]=3
data1$Water.hardness[id2]=2
data1$Water.hardness[id3]=0
data1$Water.hardness[id4]=1
mul_cox=coxph(Surv(time,status) ~., data=data1)
mul_cox
data1$Environment_Score=0
id1=which(data1$Water.hardness==0)
data1$water.Score=0
id2=which(data1$Water.hardness==1)
data1$water.Score[id2]=mul_cox$coefficients[4]
id3=which(data1$Water.hardness==2)
data1$water.Score[id3]=2*mul_cox$coefficients[5]
id4=which(data1$Water.hardness==3)
data1$water.Score[id4]=3*mul_cox$coefficients[6]
# PM2.5
data1$PM2.5.Score=data$PM2.5*mul_cox$coefficients[10]
# gree space
data1$greenspace.Score=data$Greenspace_2*mul_cox$coefficients[16]
#
data1$Environment_Score=rowSums(data1[,19:21])*5/(sum(mul_cox$coefficients[c(4:6,10,16)]))

mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+Environment_Score, data=data1)
mul_cox
## 分成三组
q1=as.data.frame(quantile(data1$Environment_Score,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+1
data1$Environment_Score_Q=cut(data1$Environment_Score,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
data1$Environment_Score_Q=as.factor(data1$Environment_Score_Q)
mul_cox1=coxph(Surv(time,status) ~Age+Sex+Center+Environment_Score_Q, data=data1)
# 
write.csv(data1,"E:/UKB/UKB94885/Delirium/environmentscore.csv")
# 

# 4  ----------------------------------------------------------------------
data=read.csv("E:/UKB/UKB94885/Delirium/participant12socialfactor.csv")
data1=data[,c(-1,-7)]
data1$Number_household=data$Number_household
data1$mother_alive=data$mother_alive
data1$Sex=as.factor(data1$Sex)
mul_cox=coxph(Surv(time,status) ~., data=data1)
q1=as.data.frame(quantile(data1$Number_household,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+50
data1$Number_household_Q=cut(data1$Number_household,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
q1=as.data.frame(quantile(data1$Vehicles_household,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+50
data1$Vehicles_household_Q=cut(data1$Vehicles_household,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
data1$Income=as.factor(data1$Income)
data1$mother_alive=as.factor(data1$mother_alive)
# mul_cox=coxph(Surv(time,status)~., data=data1)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+Number_household_Q+Vehicles_household_Q+Income+mother_alive+Crime_score+Number_household_Q, data=data1)
# score
# Number household
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+Number_household_Q, data=data1)
id1=which(data1$Number_household_Q=="Q1")
id2=which(data1$Number_household_Q=="Q2")
id3=which(data1$Number_household_Q=="Q3")
data1$Number_household_Q[id1]="Q3"
data1$Number_household_Q[id2]="Q2"
data1$Number_household_Q[id3]="Q1"
# vehicles household
id1=which(data1$Vehicles_household_Q=="Q1")
id2=which(data1$Vehicles_household_Q=="Q2")
id3=which(data1$Vehicles_household_Q=="Q3")
data1$Vehicles_household_Q[id1]="Q3"
data1$Vehicles_household_Q[id2]="Q2"
data1$Vehicles_household_Q[id3]="Q1"
# income
id1=which(data1$Income==1)
id2=which(data1$Income==2)
id3=which(data1$Income==3)
id4=which(data1$Income==4)
id5=which(data1$Income==5)
data1$Income[id1]=4
data1$Income[id2]=3
data1$Income[id3]=2
data1$Income[id4]=1
data1$Income[id5]=0
data1$Income=as.numeric(data1$Income)
id=is.na(data1$Income)
id1=which(id==1)
data1$Income[id1]=0
data1$Income=as.factor(data1$Income)
id1=which(data1$mother_alive==0)
data1$mother_alive[id1]=1
id2=which(data1$mother_alive==1)
data1$mother_alive[id2]=0
data1$mother_alive=as.factor(data1$mother_alive)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+Number_household_Q+Vehicles_household_Q+Income+mother_alive+Crime_score+Number_household_Q, data=data1)
# number household
id1=which(data1$Number_household_Q=="Q1")
id2=which(data1$Number_household_Q=="Q2")
id3=which(data1$Number_household_Q=="Q3")
data1$Number_household_Q[id1]="Q1"
data1$Number_household_Q[id2]="Q3"
data1$Number_household_Q[id3]="Q2"
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+Number_household_Q+Vehicles_household_Q+Income+mother_alive+Crime_score+Number_household_Q, data=data1)
# number household score
id1=which(data1$Number_household_Q=="Q1")
data1$Number_household.score[id1]=0
id2=which(data1$Number_household_Q=="Q2")
data1$Number_household.score[id2]=1*mul_cox$coefficients[4]
id3=which(data1$Number_household_Q=="Q3")
data1$Number_household.score[id3]=2*mul_cox$coefficients[5]
# Vehicles_household
id1=which(data1$Vehicles_household_Q=="Q1")
data1$Vehicles_household.score=0
id2=which(data1$Vehicles_household_Q=="Q2")
data1$Vehicles_household.score[id2]=mul_cox$coefficients[6]
id3=which(data1$Vehicles_household_Q=="Q3")
data1$Vehicles_household.score[id3]=2*mul_cox$coefficients[7]
# Income
id1=which(data1$Income==0)
data1$Income.score=0
id2=which(data1$Income==1)
data1$Income.score[id2]=mul_cox$coefficients[8]
id3=which(data1$Income==2)
data1$Income.score[id3]=2*mul_cox$coefficients[9]
id4=which(data1$Income==3)
data1$Income.score[id4]=3*mul_cox$coefficients[10]
id5=which(data1$Income==4)
data1$Income.score[id5]=4*mul_cox$coefficients[11]
# mother alieve
id1=which(data1$mother_alive==0)
data1$mother_alive.score=0
id2=which(data1$mother_alive==1)
data1$mother_alive.score[id2]=mul_cox$coefficients[12]
# crime score
data1$Crime.score=data1$Crime_score*mul_cox$coefficients[13]
###
data1$Social.Score=rowSums(data1[,13:17])*5/sum(mul_cox$coefficients[4:13]) # 应该乘以7 在后边校正
write.csv(data1,"E:/UKB/UKB94885/Delirium/social_score.csv")
data=read.csv("E:/UKB/UKB94885/Delirium/social_score.csv")
data$Social.Score=data$Social.Score*10/5
write.csv(data,"E:/UKB/UKB94885/Delirium/social_score.csv")
### physical measurement
data=read.csv("E:/UKB/UKB94885/Delirium/participant12physicalmeasurement.csv")
data=na.omit(data)
data1=data
data1=data1[,c(-1,-49)]
data1$Sex=as.factor(data1$Sex)
data1$tiredness.lethargy=as.factor(data1$tiredness.lethargy)
data1=na.omit(data1)
mul_cox1=coxph(Surv(time,status) ~Age+Sex+Center+tiredness.lethargy+tenseness.restlessness, data=data1)
mul_cox=coxph(Surv(time,status) ~., data=data1)
id1=which(data1$tiredness.lethargy==1)
id2=which(data1$tiredness.lethargy==2)
id3=which(data1$tiredness.lethargy==3)
id4=which(data1$tiredness.lethargy==4)
data1$tiredness.Score=0
data1$tiredness.Score[id1]=0
data1$tiredness.Score[id2]=mul_cox$coefficients[4]
data1$tiredness.Score[id3]=2*mul_cox$coefficients[5]
data1$tiredness.Score[id4]=3*mul_cox$coefficients[6]
data1$physical.Score=data1$tiredness.Score
data1$physical.Score=data1$physical.Score+data1$Waist*mul_cox$coefficients[11]
# hip 三分类
q1=as.data.frame(quantile(data1$Hip,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
data1$Hip_Q=cut(data1$Hip,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
mul_cox1=coxph(Surv(time,status) ~Age+Sex+Center+Hip_Q, data=data1) 
id=which(colnames(data1)=="Hip")
data1=data1[,-id]
id1=which(data1$Hip_Q=="Q1")
id2=which(data1$Hip_Q=="Q2")
id3=which(data1$Hip_Q=="Q3")
data1$Hip_Q[id1]="Q3"
data1$Hip_Q[id3]="Q1"
# trunk fat
q1=as.data.frame(quantile(data1$Trunk_fat,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
data1$Trunk_fat_Q=cut(data1$Trunk_fat,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
mul_cox1=coxph(Surv(time,status) ~Age+Sex+Center+Trunk_fat_Q, data=data1) 
# Lymphocyte.percentage
q1=as.data.frame(quantile(data1$Lymphocyte.percentage,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
data1$Lymphocyte.percentage_Q=cut(data1$Lymphocyte.percentage,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
mul_cox1=coxph(Surv(time,status) ~Age+Sex+Center+Lymphocyte.percentage_Q, data=data1) 
id1=which(data1$Lymphocyte.percentage_Q=="Q1")
id2=which(data1$Lymphocyte.percentage_Q=="Q2")
id3=which(data1$Lymphocyte.percentage_Q=="Q3")
data1$Lymphocyte.percentage_Q[id1]="Q3"
data1$Lymphocyte.percentage_Q[id3]="Q1"
id=which(colnames(data1)=="Lymphocyte.percentage")
data1=data1[,-id]
# Cholesterol
mul_cox1=coxph(Surv(time,status) ~Age+Sex+Center+Cholesterol_Q, data=data1) 
q1=as.data.frame(quantile(data1$Cholesterol,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
data1$Cholesterol_Q=cut(data1$Cholesterol,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
id1=which(data1$Cholesterol_Q=="Q1")
id2=which(data1$Cholesterol_Q=="Q2")
id3=which(data1$Cholesterol_Q=="Q3")
data1$Cholesterol_Q[id1]="Q3"
data1$Cholesterol_Q[id3]="Q1"
id=which(colnames(data1)=="Cholesterol")
data1=data1[,-id]
## Creatinine
# 
mul_cox1=coxph(Surv(time,status) ~Age+Sex+Center+Creatinine_Q, data=data1) 
q1=as.data.frame(quantile(data1$Creatinine,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
data1$Creatinine_Q=cut(data1$Creatinine,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
id1=which(data1$Creatinine_Q=="Q1")
id2=which(data1$CreatinineQ=="Q2")
id3=which(data1$Creatinine_Q=="Q3")
data1$Creatinine_Q[id1]="Q3"
data1$Creatinine_Q[id3]="Q1"
id=which(colnames(data1)=="Creatinine")
data1=data1[,-id]
## VitaminD
mul_cox1=coxph(Surv(time,status) ~Age+Sex+Center+VitaminD_Q, data=data1) 
q1=as.data.frame(quantile(data1$VitaminD,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
data1$VitaminD_Q=cut(data1$VitaminD,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
id1=which(data1$VitaminD_Q=="Q1")
id2=which(data1$VitaminD_Q=="Q2")
id3=which(data1$VitaminD_Q=="Q3")
data1$VitaminD_Q[id1]="Q3"
data1$VitaminD_Q[id3]="Q1"
id=which(colnames(data1)=="VitaminD")
data1=data1[,-id]
# Hand_grip
mul_cox1=coxph(Surv(time,status) ~Age+Sex+Center+Hand_grip, data=data1) 
q1=as.data.frame(quantile(data1$Hand_grip,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
data1$Hand_grip_Q=cut(data1$Hand_grip,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
id1=which(data1$Hand_grip_Q=="Q1")
id2=which(data1$Hand_grip_Q=="Q2")
id3=which(data1$Hand_grip_Q=="Q3")
data1$Hand_grip_Q[id1]="Q3"
data1$Hand_grip_Q[id3]="Q1"
id=which(colnames(data1)=="Hand_grip")
data1=data1[,-id]
##
library(survival)
mul_cox=coxph(Surv(time,status) ~., data=data1) 
data1$eid=data$eid
write.csv(data1,"E:/UKB/UKB94885/Delirium/participant12physicalmeasurement_Q.csv")
#data=read.csv("E:/UKB/UKB94885/Delirium/participant12physicalmeasurement_Q.csv")
y1=summary(mul_cox)
write.csv(y1$coefficients,"E:/UKB/UKB94885/Delirium/physicalmeasurementCoefficient.csv")
## calculation score 只纳入p<0.001的变量
data1=read.csv("E:/UKB/UKB94885/Delirium/participant12physicalmeasurement_Q.csv")
data1$tiredness.lethargy=as.factor(data1$tiredness.lethargy)
data1=na.omit(data1)
eid=data1$eid
data2=data1[,-1]
data2$Sex=as.factor(data2$Sex)
data2$Center=as.factor(data2$Center)
mul_cox=coxph(Surv(time,status) ~., data=data2)
y1=summary(mul_cox)
write.csv(y1$coefficients,"E:/UKB/UKB94885/Delirium/physicalmeasurementCoefficient.csv")
data1$physical.Score=0
data1$blood.Score=0
score=data1$physical.Score
score1=data1$blood.Score
# impedence leg 反向编码
l1=coxph(Surv(time,status)~Age+Sex+Center+Impedence_leg,data=data2)
q1=as.data.frame(quantile(data2$Impedence_leg,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
data2$Impedence_leg=cut(data2$Impedence_leg,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
id1=which(data2$Impedence_leg=="Q1")
id2=which(data2$Impedence_leg=="Q3")
data2$Impedence_leg[id1]="Q3"
data2$Impedence_leg[id2]="Q1"
# trunk fat 
data2=data2[,-11]
id1=which(data2$Trunk_fat_Q=="Q1")
id2=which(data2$Trunk_fat_Q=="Q3")
data2$Trunk_fat_Q[id1]="Q3"
data2$Trunk_fat_Q[id2]="Q1"
mul_cox=coxph(Surv(time,status) ~., data=data2)
y1=summary(mul_cox)
write.csv(y1$coefficients,"E:/UKB/UKB94885/Delirium/physicalmeasurementCoefficient.csv")
# tiredness.lethargy
id1=which(data1$tiredness.lethargy==2)
id2=which(data1$tiredness.lethargy==3)
id3=which(data1$tiredness.lethargy==4)
score[id1]=1*mul_cox$coefficients[24]
score[id2]=2*mul_cox$coefficients[25]
score[id3]=3*mul_cox$coefficients[26]
data1$physical.Score=data1$physical.Score+score
# waist
score=data1$Waist*mul_cox$coefficients[27]
data1$physical.Score=data1$physical.Score+score
# education
# score=data1$Education_score*mul_cox$coefficients[16]
# data1$physical.Score=data1$physical.Score+score
# red blood cell erythro
# score=data1$Red.blood.cell..erythrocyte..distribution.width*mul_cox$coefficients[23]
# data1$physical.Score=data1$physical.Score+score
# # monocyte.count
# score=data1$Monocyte.count*mul_cox$coefficients[26]
# data1$physical.Score=data1$physical.Score+score
# # moncyte.count1
# score=data1$Monocyte.count.1*mul_cox$coefficients[27]
# data1$physical.Score=data1$physical.Score+score
# # mean sphered cell volum
# score=data1$Mean.sphered.cell.volume*mul_cox$coefficients[31]
# data1$physical.Score=data1$physical.Score+score
# # Cystatin.C
# score=data1$Cystatin.C*mul_cox$coefficients[33]
# data1$physical.Score=data1$physical.Score+score
# # Gamma.glutamyltransfera
# score=data1$Gamma.glutamyltransferase*mul_cox$coefficients[34]
# data1$physical.Score=data1$physical.Score+score
# # HbA1c
# score=data1$HbA1c*mul_cox$coefficients[36]
# data1$physical.Score=data1$physical.Score+score
# impedence leg
# l1=coxph(Surv(time,status) ~Age+Sex+Center, data=data2)
# pulse_rate
score=data1$pulse_rate*mul_cox$coefficients[32]
data1$physical.Score=data1$physical.Score+score
# Hip_Q
id1=which(data1$Hip_Q=="Q1")
id2=which(data1$Hip_Q=="Q2")
id3=which(data1$Hip_Q=="Q3")
score[id1]=0
score[id2]=1*mul_cox$coefficients[34]
score[id3]=2*mul_cox$coefficients[35]
data1$physical.Score=data1$physical.Score+score
# # Lymphocyte.percentage_Q
# id1=which(data1$Lymphocyte.percentage_Q=="Q1")
# id2=which(data1$Lymphocyte.percentage_Q=="Q2")
# id3=which(data1$Lymphocyte.percentage_Q=="Q3")
# score[id1]=0
# score[id2]=1*mul_cox$coefficients[48]
# score[id3]=2*mul_cox$coefficients[49]
# data1$physical.Score=data1$physical.Score+score
# # Cholesterol_Q
# id1=which(data1$Cholesterol_Q=="Q1")
# id2=which(data1$Cholesterol_Q=="Q2")
# id3=which(data1$Cholesterol_Q=="Q3")
# score[id1]=0
# score[id2]=1*mul_cox$coefficients[50]
# score[id3]=2*mul_cox$coefficients[51]
# data1$physical.Score=data1$physical.Score+score
# # Creatinine
# id1=which(data1$Creatinine_Q=="Q1")
# id2=which(data1$Creatinine_Q=="Q2")
# id3=which(data1$Creatinine_Q=="Q3")
# score[id1]=0
# score[id2]=1*mul_cox$coefficients[52]
# score[id3]=2*mul_cox$coefficients[53]
# data1$physical.Score=data1$physical.Score+score
# # VitaminD
# id1=which(data1$VitaminD_Q=="Q1")
# id2=which(data1$VitaminD_Q=="Q2")
# id3=which(data1$VitaminD_Q=="Q3")
# score[id1]=0
# score[id2]=1*mul_cox$coefficients[54]
# score[id3]=2*mul_cox$coefficients[55]
# data1$physical.Score=data1$physical.Score+score
# Hand_grip
id1=which(data1$Hand_grip_Q=="Q1")
id2=which(data1$Hand_grip_Q=="Q2")
id3=which(data1$Hand_grip_Q=="Q3")
score[id1]=0
score[id2]=1*mul_cox$coefficients[38]
score[id3]=2*mul_cox$coefficients[39]
data1$physical.Score=data1$physical.Score+score
# 标准化
data1$physical.Score=data1$physical.Score*9/2.71
write.csv(data1,"E:/UKB/UKB94885/Delirium/physical_Score.csv")
#
mul_cox1=coxph(Surv(time,status) ~Age+Sex+Center+physical.Score, data=data1) 
####
# blood assay socre
data=read.csv("E:/UKB/UKB94885/Delirium/participant12bloodmeasure_Q.csv")
data1=data[,-1]
data1$Sex=as.factor(data1$Sex)
data1$Center=as.factor(data1$Center)
mul_cox1=coxph(Surv(time,status) ~., data=data1) 
y1=summary(mul_cox1)
write.csv(y1$coefficients,"E:/UKB/UKB94885/Delirium/bloodassayCoefficient.csv")
q1=as.data.frame(quantile(data1$IGF.1,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
data1$IGF.1=cut(data1$IGF.1,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
id1=which(data1$IGF.1=="Q1")
id2=which(data1$IGF.1=="Q3")
data1$IGF.1[id1]="Q3"
data1$IGF.1[id2]="Q1"
#
q1=as.data.frame(quantile(data1$Platelet.crit,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
data1$Platelet.crit=cut(data1$Platelet.crit,breaks=breaks,label=c("Q1","Q2","Q3"),right=TRUE)
id1=which(data1$IGF.1=="Q1")
id2=which(data1$IGF.1=="Q3")
data1$Platelet.crit[id1]="Q3"
data1$Platelet.crit[id2]="Q1"
mul_cox1=coxph(Surv(time,status) ~., data=data1) 
y1=summary(mul_cox1)
write.csv(y1$coefficients,"E:/UKB/UKB94885/Delirium/bloodassayCoefficient.csv")
## calculation score
data1$bloodassay.Score=0
Score=data1$bloodassay.Score
# high.lights.catt.retculocyte.percentage
id1=which(data1$High.lights.catter.reticulocyte.percentage=="Q2")
id2=which(data1$High.lights.catter.reticulocyte.percentage=="Q3")
Score[id1]=1*mul_cox1$coefficients[26]
Score[id2]=2*mul_cox1$coefficients[27]
data1$bloodassay.Score=Score
# Red.blood.cell..erythrocyte..distribution.width
Score=0
Score=data1$Red.blood.cell..erythrocyte..distribution.width*mul_cox1$coefficients[33]
data1$bloodassay.Score=data1$bloodassay.Score+Score
# Monocyte.count
Score=0
Score=data1$Monocyte.count*mul_cox1$coefficients[37]
data1$bloodassay.Score=data1$bloodassay.Score+Score
# Monocyte.count.1
Score=0
Score=data1$Monocyte.count.1*mul_cox1$coefficients[38]
data1$bloodassay.Score=data1$bloodassay.Score+Score
# Mean.sphered.cell.volume
Score=0
Score=data1$Mean.sphered.cell.volume*mul_cox1$coefficients[42]
data1$bloodassay.Score=data1$bloodassay.Score+Score
# C creatinine
Score=0
Score=data1$Cystatin.C*mul_cox1$coefficients[44]
data1$bloodassay.Score=data1$bloodassay.Score+Score
# Gamma.glutamyltransferase
Score=0
Score=data1$Gamma.glutamyltransferase*mul_cox1$coefficients[45]
data1$bloodassay.Score=data1$bloodassay.Score+Score
#Glucose
Score=0
Score=data1$Glucose*mul_cox1$coefficients[46]
data1$bloodassay.Score=data1$bloodassay.Score+Score
# HbA1c
Score=0
Score=data1$HbA1c*mul_cox1$coefficients[47]
data1$bloodassay.Score=data1$bloodassay.Score+Score
# lym
Score=data1$bloodassay.Score
Score[1:488023]=0
id1=which(data1$Lymphocyte.percentage_Q=="Q2")
id2=which(data1$Lymphocyte.percentage_Q=="Q3")
Score[id1]=1*mul_cox1$coefficients[54]
Score[id2]=2*mul_cox1$coefficients[55]
data1$bloodassay.Score=data1$bloodassay.Score+Score
# choles
Score[1:488023]=0
id1=which(data1$Cholesterol_Q=="Q2")
id2=which(data1$Cholesterol_Q=="Q3")
Score[id1]=1*mul_cox1$coefficients[56]
Score[id2]=2*mul_cox1$coefficients[57]
data1$bloodassay.Score=data1$bloodassay.Score+Score
# creatinie
Score[1:488023]=0
id1=which(data1$Creatinine_Q=="Q2")
id2=which(data1$Creatinine_Q=="Q3")
Score[id1]=1*mul_cox1$coefficients[58]
Score[id2]=2*mul_cox1$coefficients[59]
data1$bloodassay.Score=data1$bloodassay.Score+Score
# VitaminD_QQ2
Score[1:488023]=0
id1=which(data1$VitaminD_Q=="Q2")
id2=which(data1$VitaminD_Q=="Q3")
Score[id1]=1*mul_cox1$coefficients[60]
Score[id2]=2*mul_cox1$coefficients[61]
data1$bloodassay.Score=data1$bloodassay.Score+Score
# 标准化
data1$bloodassay.Score=data1$bloodassay.Score*18/2.527
data1$eid=data$eid
write.csv(data1,"E:/UKB/UKB94885/Delirium/bloodassay_Score.csv")

# medical score calculating
data=read.csv("E:/UKB/UKB94885/Delirium/participant12medical.csv")
data1=data
data1=na.omit(data1)
data1=data1[,-1]
data1$Sex=as.factor(data1$Sex)
data1[,6]=as.factor(data1[,6])
for (i in 6:24)
{
  data1[,i]=as.factor(data1[,i])
}
data1[,24]=as.numeric(data1[,24])
mul_cox1=coxph(Surv(time,status) ~., data=data1) 
#
data$score=0
# stroke
id1=which(data1$stroke==2)
data$score[id1]=data$score[id1]+0.3564
# asthma 0.1876
id2=which(data1$asthma==2)
data$score[id2]=data$score[id2]+0.1876
# chronic.obstructive.pulmonary.disease 0.461
id3=which(data1$chronic.obstructive.pulmonary.disease==2)
data$score[id3]=data$score[id3]+0.461
# pancres 0.723
id4=which(data1$pancres==2)
data$score[id4]=data$score[id4]+0.723
# lipoprotein 0.15
id5=which(data1$lipoprotein==2)
data$score[id5]=data$score[id5]+0.15
# other_metabolic_diorder
id6=which(data1$other_metabolic_diorder==2)
data$score[id6]=data$score[id6]+0.967
# circulatory 0.426
id7=which(data1$circulatory==2)
data$score[id7]=data$score[id7]+0.426
# digestive 0.15
id8=which(data1$digestive==2)
data$score[id8]=data$score[id8]+0.15
# 
write.csv(data,"E:/UKB/UKB94885/Delirium/participant12medicalscore.csv")
# 标准化 medical score
data=read.csv("E:/UKB/UKB94885/Delirium/participant12medicalscore.csv")
data$score=(data$score)*8/(0.15+0.426+0.967+0.15+0.723+0.461+0.1876+0.3564)
write.csv(data,"E:/UKB/UKB94885/Delirium/participant12medicalscore.csv")
mul_xoc2=coxph(Surv(time,status) ~Age+Center+Cancer+score, data=data)
## 合并score
data1=read.csv("E:/UKB/UKB94885/Delirium/environmentscore.csv")
data2=read.csv("E:/UKB/UKB94885/Delirium/social_score.csv")
data3=read.csv("E:/UKB/UKB94885/Delirium/physical_Score.csv")
data4=read.csv("E:/UKB/UKB94885/Delirium/liefscore.csv")
data5=read.csv("E:/UKB/UKB94885/Delirium/participant12medicalscore.csv")
Score=data1[,1:6]
Score$Environment_score=data1$Environment_Score
Score$social_score=data2$Social.Score
Score$medical_score=data5$score
x=data3[,50:51]
Score=merge(Score,x,by="eid")
x=data4[,c(1,21)]
Score=merge(Score,x,by="eid")
data6=read.csv("E:/UKB/UKB94885/Delirium/participant12eralylifefactors.csv")
data6$Sex=as.factor(data6$Sex)
data6$body_size_10=as.factor(data6$body_size_10)
mul_cox1=coxph(Surv(time,status) ~Sex+Center+Age+body_size_10, data=data6) 
id1=which(data6$body_size_10==1)
id2=which(data6$body_size_10==2)
id3=which(data6$body_size_10==3)
data6$body_size_10[id2]=3
data6$body_size_10[id3]=2
id1=which(data6$body_size_10==1)
id2=which(data6$body_size_10==2)
id3=which(data6$body_size_10==3)
data6$earlylifescore=0
data6$earlylifescore[id2]=0.094
data6$earlylifescore[id3]=2*0.231
data6$earlylifescore=2*data6$earlylifescore/(0.094+0.231)
x=data6[,c(1,9)]
Score=merge(Score,x,by="eid")
write.csv(Score,"E:/UKB/UKB94885/Delirium/Score.csv")
Score=read.csv("E:/UKB/UKB94885/Delirium/score.csv")
Score=Score[,c(-1,-12,-13)]
## 2024.3.22 将原来的physiclameansurement score 分为physical score 和 blood assay score
l1=read.csv("E:/UKB/UKB94885/Delirium/physical_Score.csv")
l2=read.csv("E:/UKB/UKB94885/Delirium/bloodassay_Score.csv")
l3=l1[,c(2,19)]
l4=l2[,c(37,38)]
Score=merge(Score,l3,by="eid")
Score=merge(Score,l4,by="eid")
write.csv(Score,"E:/UKB/UKB94885/Delirium/Score.csv")
Score=read.csv("E:/UKB/UKB94885/Delirium/Score.csv")
Score$Sex=as.factor(Score$Sex)
Score$Center=as.factor(Score$Center)
data=Score[,c(-1,-2)]
# 将各领域Score作为连续型变量，建立cox regression model
mul_cox2=coxph(Surv(time,status) ~., data=data) 
# 将Score 按分位数分类
Score1=Score
# early life
q1=as.data.frame(quantile(Score$Earlylife.score,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
Score1$Earlylife.score_Q=cut(Score1$Earlylife.score,breaks=breaks,label=c("Favourable","Intermediate","Unfavourable"),right=TRUE)
# Environment_score
q1=as.data.frame(quantile(Score$Environment.score,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
Score1$Environment.score_Q=cut(Score1$Environment.score,breaks=breaks,label=c("Favourable","Intermediate","Unfavourable"),right=TRUE)
# social score
q1=as.data.frame(quantile(Score$Social.score,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
Score1$Social.score_Q=cut(Score1$Social.score,breaks=breaks,label=c("Favourable","Intermediate","Unfavourable"),right=TRUE)
# Lifestyle.Score
q1=as.data.frame(quantile(Score$Lifestyle.Score,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
Score1$Lifestyle.Score_Q=cut(Score1$Lifestyle.Score,breaks=breaks,label=c("Favourable","Intermediate","Unfavourable"),right=TRUE)
# physical.Score
q1=as.data.frame(quantile(Score$physical.Score,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
Score1$Physical.Score_Q=cut(Score1$physical.Score,breaks=breaks,label=c("Favourable","Intermediate","Unfavourable"),right=TRUE)
# bloodassay.Score
q1=as.data.frame(quantile(Score$bloodassay.Score,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
Score1$Bloodassay.Score_Q=cut(Score1$bloodassay.Score,breaks=breaks,label=c("Favourable","Intermediate","Unfavourable"),right=TRUE)
# medical_score
q1=as.data.frame(quantile(Score$medical_score,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
Score1$Medical.Score_Q=cut(Score1$medical_score,breaks=breaks,label=c("Favourable","Intermediate","Unfavourable"),right=TRUE)
data=Score1[,c(3:7,15:21)]
mul_cox3=coxph(Surv(time,status) ~., data=data) 

#  Figure -----------------------------------------------------------------
#一-2 multi1：提取：变量+HR+95%CI+95%CI
mul_3<- summary(mul_cox3)
colnames(mul_3$conf.int)
multi1<-as.data.frame(round(mul_3$conf.int[, c(1, 3, 4)], 4))
#一-3、multi2：提取：HR(95%CI)和P
#install.packages("tableone")
library(tableone)
multi2<-ShowRegTable(mul_cox3, 
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
library(forestplot)
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
result=result[c(24:37),]
##2-2：插入空行，形成一个新表
for(i in 5:6) {result[, i] = as.character(result[, i])}
result<-rbind(c("Characteristics", NA, NA, NA, "HR(95%CI)","p"),
              ins("Early life"),
              ins("Favourable"), 
              result[1:2, ],
              ins("Environment"), 
              ins("Favourable"),
              result[3:4, ],
              ins("Social factors"), 
              ins("Favourable"),
              result[5:6, ],
              ins("Life style"), 
              ins("Favourable"),
              result[7:8, ],
              ins("Physical measures"), 
              ins("Favourable"),
              result[9:10, ],
              ins("Blood assay"), 
              ins("Favourable"),
              result[11:12, ],
              ins("Medical history"), 
              ins("Favourable"),
              result[13:14, ]
)
result[3,2]=1
result[3,3:4]=1
result[7,2]=1
result[7,3:4]=1
result[11,2]=1
result[11,3:4]=1
result[15,2]=1
result[15,3:4]=1
result[19,2]=1
result[19,3:4]=1
result[23,2]=1
result[23,3:4]=1
result[27,2]=1
result[27,3:4]=1
for(i in 2:4) {result[, i] = as.numeric(result[, i])}
#
result[4,1]="Intermediate"
result[5,1]="Qunfavourable"
result[8,1]="Intermediate"
result[9,1]="Qunfavourable"
result[12,1]="Intermediate"
result[13,1]="Qunfavourable"
result[16,1]="Intermediate"
result[17,1]="Qunfavourable"
result[20,1]="Intermediate"
result[21,1]="Qunfavourable"
result[24,1]="Intermediate"
result[25,1]="Qunfavourable"
result[27,1]="Favourable"
result[28,1]="Intermediate"
result[29,1]="Qunfavourable"
#
# 加入cases control
result$casesofdelirium=NA
result$totalparticipants=NA
# early life score
id1=which(Score1$Earlylife.score_Q=="Favourable")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[3]=length(id2)
result$totalparticipants[3]=length(id1)

id1=which(Score1$Earlylife.score_Q=="Intermediate")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[4]=length(id2)
result$totalparticipants[4]=length(id1)

id1=which(Score1$Earlylife.score_Q=="Unfavourable")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[5]=length(id2)
result$totalparticipants[5]=length(id1)
# Environment_score
id1=which(Score1$Environment.score_Q=="Favourable")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[7]=length(id2)
result$totalparticipants[7]=length(id1)

id1=which(Score1$Environment.score_Q=="Intermediate")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[8]=length(id2)
result$totalparticipants[8]=length(id1)

id1=which(Score1$Environment.score_Q=="Unfavourable")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[9]=length(id2)
result$totalparticipants[9]=length(id1)
## psychosocial factors
id1=which(Score1$Physical.Score_Q=="Favourable")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[7]=length(id2)
result$totalparticipants[7]=length(id1)

id1=which(Score1$Physical.Score_Q=="Intermediate")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[8]=length(id2)
result$totalparticipants[8]=length(id1)

id1=which(Score1$Physical.Score_Q=="Unfavourable")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[9]=length(id2)
result$totalparticipants[9]=length(id1)
###social factors
id1=which(Score1$Social.score_Q=="Favourable")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[11]=length(id2)
result$totalparticipants[11]=length(id1)

id1=which(Score1$Social.score_Q=="Intermediate")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[12]=length(id2)
result$totalparticipants[12]=length(id1)

id1=which(Score1$Social.score_Q=="Unfavourable")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[13]=length(id2)
result$totalparticipants[13]=length(id1)
### life style score
id1=which(Score1$Lifestyle.Score_Q=="Favourable")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[15]=length(id2)
result$totalparticipants[15]=length(id1)

id1=which(Score1$Lifestyle.Score_Q=="Intermediate")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[16]=length(id2)
result$totalparticipants[16]=length(id1)

id1=which(Score1$Lifestyle.Score_Q=="Unfavourable")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[17]=length(id2)
result$totalparticipants[17]=length(id1)
### physical measurements
id1=which(Score1$Physical.Score_Q=="Favourable")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[19]=length(id2)
result$totalparticipants[19]=length(id1)

id1=which(Score1$Physical.Score_Q=="Intermediate")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[20]=length(id2)
result$totalparticipants[20]=length(id1)

id1=which(Score1$Physical.Score_Q=="Unfavourable")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[21]=length(id2)
result$totalparticipants[21]=length(id1)
### Blood Assay
id1=which(Score1$Bloodassay.Score_Q=="Favourable")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[23]=length(id2)
result$totalparticipants[23]=length(id1)

id1=which(Score1$Bloodassay.Score_Q=="Intermediate")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[24]=length(id2)
result$totalparticipants[24]=length(id1)

id1=which(Score1$Bloodassay.Score_Q=="Unfavourable")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[25]=length(id2)
result$totalparticipants[25]=length(id1)
# Medical history
id1=which(Score1$Medical.Score_Q=="Favourable")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[27]=length(id2)
result$totalparticipants[27]=length(id1)

id1=which(Score1$Medical.Score_Q=="Intermediate")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[28]=length(id2)
result$totalparticipants[28]=length(id1)

id1=which(Score1$Medical.Score_Q=="Unfavourable")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[29]=length(id2)
result$totalparticipants[29]=length(id1)
#
result$casesofdelirium[1]="Cases"
result$totalparticipants[1]="Total"
result[9,6]=0.005
#
fig1<- forestplot(result[,c(1,7,8,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result[,3],  #告诉函数表格第3列为95%CI，
                  upper=result[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.6,       #设置小黑块的大小
                  graph.pos=4,
                  xticks=c(0.5,1,1.5,2,2.5),
                  lwd.xaxis=1.5,
                  lwd.zero=1.5,
                  lwd.ci=2,
                  txt_gp=fpTxtGp(ticks=gpar(cex=0.9)),
                  col=fpColors(box=c("brown"),lines = "grey",zero = "gray50"),
                  # is.summary=c(TRUE,rep(FALSE,14),TRUE), #是否突出显示。传入一个长度等于图表行数的向量，下标为TRUE的行会被加粗，且该行上下会添加一条直线，但在未设置颜色时不显示。
                  # hrzl_lines = gpar(col="red"))#线的颜色。默认作用于summary上下。)       #森林图应插在图形第2列
                  hrzl_lines = list( "2"=gpar(lty=1,lwd=2),
                                     "6" = gpar(lty=2,columns=c(1),col='grey'),
                                    "10" = gpar(lty=2,columns=c(1),col='grey'),
                                    "14" = gpar(lty=2, columns=c(1), col = "grey"),
                                    "18" = gpar(lty=2, columns=c(1), col = "grey"),
                                    "22" = gpar(lty=2, columns=c(1), col = "grey")))
fig1
####

# 3.1 score 与基因交互作用 ---------------------------------------------------------;
write.csv(Score1,"E:/UKB/UKB94885/Delirium/Score1.csv")
lings=read.csv("E:/UKB/UKB94885/Delirium/participant12socialfactor.csv")
lings=lings[,c(1,7)]
Score1=merge(Score1,lings,by="eid")
#
Score1=read.csv("E:/UKB/UKB94885/Delirium/Score1.csv")
Score1=Score1[,c(-1,-2)]
# 乘积项
PRS=Score1[,20]
Score=Score1[,c(6:12)]
Score=Score*PRS
colnames(Score)=c("early_life*PRS","environment*PRS","social*PRS","life_style*PRS","medical*PRS","physicla*PRS","blood*PRS")
Score1[,c(21:27)]=Score
colnames(Score1)[21:27]=colnames(Score)
write.csv(Score1,"E:/UKB/UKB94885/Delirium/Score2.csv")
#Score1=Score1[,-1]
#mul_cox1=coxph(Surv(time,status) ~., data=Score1) 
mul_cox1=coxph(Surv(time,status) ~Age+Sex+Center+Earlylife.score+PRS+`early_life*PRS`, data=Score1)
mul_cox2=coxph(Surv(time,status) ~Age+Sex+Center+Environment.score+PRS+`environment*PRS`, data=Score1)
mul_cox3=coxph(Surv(time,status) ~Age+Sex+Center+Social.score+PRS+`social*PRS`, data=Score1)
mul_cox4=coxph(Surv(time,status) ~Age+Sex+Center+Lifestyle.Score+PRS+`life_style*PRS`, data=Score1)
mul_cox7=coxph(Surv(time,status) ~Age+Sex+Center+medical_score+PRS+`medical*PRS`, data=Score1)
mul_cox5=coxph(Surv(time,status) ~Age+Sex+Center+physical.Score+PRS+`physicla*PRS`, data=Score1)
mul_cox6=coxph(Surv(time,status) ~Age+Sex+Center+bloodassay.Score+PRS+`blood*PRS`,data=Score1)
####

# 3.2 交互作用 ------------------------------------------------------------------
Score1=read.csv("E:/UKB/UKB94885/Delirium/Score1.csv")
Score1=Score1[,c(-1,-2)]
q1=as.data.frame(quantile(Score1$PRS,probs = seq(0,1,0.3333)))
breaks=q1[1:4,]
breaks[1]=breaks[1]-1
breaks[4]=breaks[4]+100
Score1$PRS_Q=cut(Score1$PRS,breaks=breaks,label=c("Low genetic risk","Intermediate genetic risk","High genetic risk"),right=TRUE)
id1=which(Score1$PRS_Q=="Low genetic risk")
data1=Score1[id1,]
id2=which(Score1$PRS_Q=="Intermediate genetic risk")
data2=Score1[id2,]
id3=which(Score1$PRS_Q=="High genetic risk")
data3=Score1[id3,]
# Environment 
id1=which(Score1$Environment.score_Q=="Favourable")
data1=Score1[id1,]
id2=which(Score1$Environment.score_Q=="Intermediate")
data2=Score1[id2,]
id3=which(Score1$Environment.score_Q=="Unfavourable")
data3=Score1[id3,]
library(survival)
mul_cox1=coxph(Surv(time,status) ~Age+Sex+Center+PRS_Q, data=data1)
mul_cox2=coxph(Surv(time,status) ~Age+Sex+Center+PRS_Q, data=data2)
mul_cox3=coxph(Surv(time,status) ~Age+Sex+Center+PRS_Q, data=data3)
mul_3<- summary(mul_cox1)
colnames(mul_3$conf.int)
multi1<-as.data.frame(round(mul_3$conf.int[, c(1, 3, 4)], 4))
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
##
mul_3<- summary(mul_cox2)
colnames(mul_3$conf.int)
multi1<-as.data.frame(round(mul_3$conf.int[, c(1, 3, 4)], 4))
#一-3、multi2：提取：HR(95%CI)和P
#install.packages("tableone")
library(tableone)
multi2<-ShowRegTable(mul_cox2, 
                     exp=TRUE, 
                     digits=2, 
                     pDigits =3,
                     printToggle = TRUE, 
                     quote=FALSE, 
                     ciFun=confint)
#一-4.将两次提取结果合并成表；取名result
result1 <-cbind(multi1,multi2);result1
##
mul_3<- summary(mul_cox3)
colnames(mul_3$conf.int)
multi1<-as.data.frame(round(mul_3$conf.int[, c(1, 3, 4)], 4))
#一-3、multi2：提取：HR(95%CI)和P
#install.packages("tableone")
library(tableone)
multi2<-ShowRegTable(mul_cox3, 
                     exp=TRUE, 
                     digits=2, 
                     pDigits =3,
                     printToggle = TRUE, 
                     quote=FALSE, 
                     ciFun=confint)
#一-4.将两次提取结果合并成表；取名result
result2 <-cbind(multi1,multi2);result2
#一-5.行名转为表格第一列，并给予命名"Characteristics"
result<-tibble::rownames_to_column(result, var = "Characteristics");result
#
result[7:8,2:6]=result1[4:5,]
result[9:10,2:6]=result2[4:5,]
library(forestplot)
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
result<-rbind(c("Subgroup", NA, NA, NA, "HR(95%CI)","p"),
              ins("Low genetic risk"),
              ins("Favourable environment score"), 
              result[4:5, ],
              ins("Intermediate genetic risk"), 
              ins("Favourable environment score"),
              result[7:8, ],
              ins("High genetic risk"), 
              ins("Favourable environment score"),
              result[9:10, ]
)
result[3,2:4]=1
result[7,2:4]=1
result[11,2:4]=1

for(i in 2:4) {result[, i] = as.numeric(result[, i])}
#
result[4,1]="Intermediate environment score"
result[5,1]="Unfavourable environment score"
result[8,1]="Intermediate environment score"
result[9,1]="Qunfavourable environment score"
result[12,1]="Intermediate environment score"
result[13,1]="Qunfavourable environment score"

#
# 加入cases control
result$casesofdelirium=NA
result$totalparticipants=NA
# Environment_score
id1=which(data1$Environment.score_Q=="Favourable")
data=data1[id1,]
id2=which(data$status==2)
result$casesofdelirium[3]=length(id2)
result$totalparticipants[3]=length(id1)

id1=which(data1$Environment.score_Q=="Intermediate")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[4]=length(id2)
result$totalparticipants[4]=length(id1)

id1=which(data1$Environment.score_Q=="Unfavourable")
data=data1[id1,]
id2=which(data$status==2)
result$casesofdelirium[5]=length(id2)
result$totalparticipants[5]=length(id1)
## intemiddle low genetic risk
id1=which(data2$Environment.score_Q=="Favourable")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[7]=length(id2)
result$totalparticipants[7]=length(id1)

id1=which(data2$Environment.score_Q=="Intermediate")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[8]=length(id2)
result$totalparticipants[8]=length(id1)

id1=which(data2$Environment.score_Q=="Unfavourable")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[9]=length(id2)
result$totalparticipants[9]=length(id1)
### high genetic risk 
id1=which(data3$Environment.score_Q=="Favourable")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[11]=length(id2)
result$totalparticipants[11]=length(id1)

id1=which(data3$Environment.score_Q=="Intermediate")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[12]=length(id2)
result$totalparticipants[12]=length(id1)

id1=which(data3$Environment.score_Q=="Unfavourable")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[13]=length(id2)
result$totalparticipants[13]=length(id1)

#
fig1<- forestplot(result[,c(1,7,8,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result[,3],  #告诉函数表格第3列为95%CI，
                  upper=result[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.6,       #设置小黑块的大小
                  graph.pos=4,
                  xticks=c(0.5,1,1.5,2,2.5),
                  lwd.xaxis=1.5,
                  lwd.zero=1.5,
                  lwd.ci=2,
                  txt_gp=fpTxtGp(ticks=gpar(cex=0.9)),
                  col=fpColors(box=c("brown"),lines = "grey",zero = "gray50"),
                  # is.summary=c(TRUE,rep(FALSE,14),TRUE), #是否突出显示。传入一个长度等于图表行数的向量，下标为TRUE的行会被加粗，且该行上下会添加一条直线，但在未设置颜色时不显示。
                  # hrzl_lines = gpar(col="red"))#线的颜色。默认作用于summary上下。)       #森林图应插在图形第2列
                  hrzl_lines = list( "2"=gpar(lty=1,lwd=2),
                                     "6" = gpar(lty=2,columns=c(1),col='grey'),
                                     "10" = gpar(lty=2,columns=c(1),col='grey')
                                   ))
fig1

# 以 low genetic risk favrouable early life score 为参考--------------
Score1$inter_Q=NA
id1=which(Score1$PRS_Q=="Low genetic risk" & Score1$Earlylife.score_Q=="Favourable")
Score1$inter_Q[id1]="Q1"
id2=which(Score1$PRS_Q=="Low genetic risk" & Score1$Earlylife.score_Q=="Intermediate")
Score1$inter_Q[id2]="Q2"
id3=which(Score1$PRS_Q=="Low genetic risk" & Score1$Earlylife.score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q3"
id1=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Earlylife.score_Q=="Favourable")
Score1$inter_Q[id1]="Q4"
id2=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Earlylife.score_Q=="Intermediate")
Score1$inter_Q[id2]="Q5"
id3=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Earlylife.score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q6"
id1=which(Score1$PRS_Q=="High genetic risk" & Score1$Earlylife.score_Q=="Favourable")
Score1$inter_Q[id1]="Q7"
id2=which(Score1$PRS_Q=="High genetic risk" & Score1$Earlylife.score_Q=="Intermediate")
Score1$inter_Q[id2]="Q8"
id3=which(Score1$PRS_Q=="High genetic risk" & Score1$Earlylife.score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q9"
Score1$inter_Q=as.factor(Score1$inter_Q)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+inter_Q, data=Score1)
#
mul_3<- summary(mul_cox)
colnames(mul_3$conf.int)
multi1<-as.data.frame(round(mul_3$conf.int[, c(1, 3, 4)], 4))
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
result1 <-cbind(multi1,multi2);result1
result[4:5,2:6]=result1[4:5,]
result[7:9,2:6]=result1[6:8,]
result[11:13,2:6]=result1[9:11,]
result[1,7]="Cases"
result[1,8]="Total"
#
result[3,1]="Favourable early life score"
result[4,1]="Intermediate early life score"
result[5,1]="Unfavourable early life score"
result[7,1]="Favourable early life score"
result[8,1]="Intermediate early life score"
result[9,1]="Qunfavourable early life score"
result[11,1]="Favourable early life score"
result[12,1]="Intermediate early life score"
result[13,1]="Qunfavourable early life score"
# 统计人数
id1=which(data1$Earlylife.score_Q=="Favourable")
data=data1[id1,]
id2=which(data$status==2)
result$casesofdelirium[3]=length(id2)
result$totalparticipants[3]=length(id1)

id1=which(data1$Earlylife.score_Q=="Intermediate")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[4]=length(id2)
result$totalparticipants[4]=length(id1)

id1=which(data1$Earlylife.score_Q=="Unfavourable")
data=data1[id1,]
id2=which(data$status==2)
result$casesofdelirium[5]=length(id2)
result$totalparticipants[5]=length(id1)
## intemiddle low genetic risk
id1=which(data2$Earlylife.score_Q=="Favourable")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[7]=length(id2)
result$totalparticipants[7]=length(id1)

id1=which(data2$Earlylife.score_Q=="Intermediate")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[8]=length(id2)
result$totalparticipants[8]=length(id1)

id1=which(data2$Earlylife.score_Q=="Unfavourable")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[9]=length(id2)
result$totalparticipants[9]=length(id1)
### high genetic risk 
id1=which(data3$Earlylife.score_Q=="Favourable")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[11]=length(id2)
result$totalparticipants[11]=length(id1)

id1=which(data3$Earlylife.score_Q=="Intermediate")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[12]=length(id2)
result$totalparticipants[12]=length(id1)

id1=which(data3$Earlylife.score_Q=="Unfavourable")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[13]=length(id2)
result$totalparticipants[13]=length(id1)

fig1<- forestplot(result[,c(1,7,8,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result[,3],  #告诉函数表格第3列为95%CI，
                  upper=result[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.6,       #设置小黑块的大小
                  graph.pos=4,
                  xticks=c(0.5,1,1.5,2,2.5),
                  lwd.xaxis=1.5,
                  lwd.zero=1.5,
                  lwd.ci=2,
                  txt_gp=fpTxtGp(ticks=gpar(cex=0.9)),
                  col=fpColors(box=c("brown"),lines = "grey",zero = "gray50"),
                  # is.summary=c(TRUE,rep(FALSE,14),TRUE), #是否突出显示。传入一个长度等于图表行数的向量，下标为TRUE的行会被加粗，且该行上下会添加一条直线，但在未设置颜色时不显示。
                  # hrzl_lines = gpar(col="red"))#线的颜色。默认作用于summary上下。)       #森林图应插在图形第2列
                  hrzl_lines = list( "2"=gpar(lty=1,lwd=2),
                                     "6" = gpar(lty=2,columns=c(1),col='grey'),
                                     "10" = gpar(lty=2,columns=c(1),col='grey')
                  ))
fig1
# 3.3 以low genetic risk favourable environment score 为参考 ------------------
Score1$inter_Q=NA
id1=which(Score1$PRS_Q=="Low genetic risk" & Score1$Environment.score_Q=="Favourable")
Score1$inter_Q[id1]="Q1"
id2=which(Score1$PRS_Q=="Low genetic risk" & Score1$Environment.score_Q=="Intermediate")
Score1$inter_Q[id2]="Q2"
id3=which(Score1$PRS_Q=="Low genetic risk" & Score1$Environment.score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q3"
id1=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Environment.score_Q=="Favourable")
Score1$inter_Q[id1]="Q4"
id2=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Environment.score_Q=="Intermediate")
Score1$inter_Q[id2]="Q5"
id3=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Environment.score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q6"
id1=which(Score1$PRS_Q=="High genetic risk" & Score1$Environment.score_Q=="Favourable")
Score1$inter_Q[id1]="Q7"
id2=which(Score1$PRS_Q=="High genetic risk" & Score1$Environment.score_Q=="Intermediate")
Score1$inter_Q[id2]="Q8"
id3=which(Score1$PRS_Q=="High genetic risk" & Score1$Environment.score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q9"
Score1$inter_Q=as.factor(Score1$inter_Q)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+inter_Q, data=Score1)
#
mul_3<- summary(mul_cox)
colnames(mul_3$conf.int)
multi1<-as.data.frame(round(mul_3$conf.int[, c(1, 3, 4)], 4))
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
result1 <-cbind(multi1,multi2);result1
result[4:5,2:6]=result1[4:5,]
result[7:9,2:6]=result1[6:8,]
result[11:13,2:6]=result1[9:11,]
result[1,7]="Cases"
result[1,8]="Total"
#
result[3,1]="Favourable environment score"
result[4,1]="Intermediate environment score"
result[5,1]="Unfavourable environment score"
result[7,1]="Favourable environment score"
result[8,1]="Intermediate environment score"
result[9,1]="Qunfavourable environment score"
result[11,1]="Favourable environment score"
result[12,1]="Intermediate environment score"
result[13,1]="Qunfavourable environment score"
# 统计人数
id1=which(data1$Environment.score_Q=="Favourable")
data=data1[id1,]
id2=which(data$status==2)
result$casesofdelirium[3]=length(id2)
result$totalparticipants[3]=length(id1)

id1=which(data1$Environment.score_Q=="Intermediate")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[4]=length(id2)
result$totalparticipants[4]=length(id1)

id1=which(data1$Environment.score_Q=="Unfavourable")
data=data1[id1,]
id2=which(data$status==2)
result$casesofdelirium[5]=length(id2)
result$totalparticipants[5]=length(id1)
## intemiddle low genetic risk
id1=which(data2$Environment.score_Q=="Favourable")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[7]=length(id2)
result$totalparticipants[7]=length(id1)

id1=which(data2$Environment.score_Q=="Intermediate")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[8]=length(id2)
result$totalparticipants[8]=length(id1)

id1=which(data2$Environment.score_Q=="Unfavourable")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[9]=length(id2)
result$totalparticipants[9]=length(id1)
### high genetic risk 
id1=which(data3$Environment.score_Q=="Favourable")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[11]=length(id2)
result$totalparticipants[11]=length(id1)

id1=which(data3$Environment.score_Q=="Intermediate")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[12]=length(id2)
result$totalparticipants[12]=length(id1)

id1=which(data3$Environment.score_Q=="Unfavourable")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[13]=length(id2)
result$totalparticipants[13]=length(id1)
#
fig1<- forestplot(result[,c(1,7,8,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result[,3],  #告诉函数表格第3列为95%CI，
                  upper=result[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.6,       #设置小黑块的大小
                  graph.pos=4,
                  xticks=c(0.5,1,1.5,2,2.5),
                  lwd.xaxis=1.5,
                  lwd.zero=1.5,
                  lwd.ci=2,
                  txt_gp=fpTxtGp(ticks=gpar(cex=0.9)),
                  col=fpColors(box=c("brown"),lines = "grey",zero = "gray50"),
                  # is.summary=c(TRUE,rep(FALSE,14),TRUE), #是否突出显示。传入一个长度等于图表行数的向量，下标为TRUE的行会被加粗，且该行上下会添加一条直线，但在未设置颜色时不显示。
                  # hrzl_lines = gpar(col="red"))#线的颜色。默认作用于summary上下。)       #森林图应插在图形第2列
                  hrzl_lines = list( "2"=gpar(lty=1,lwd=2),
                                     "6" = gpar(lty=2,columns=c(1),col='grey'),
                                     "10" = gpar(lty=2,columns=c(1),col='grey')
                  ))
fig1
## forespler
# install.packages("forestploter")
# library(forestploter)

####
## social_score_Q AND prs
Score1$inter_Q=NA
id1=which(Score1$PRS_Q=="Low genetic risk" & Score1$Social.score_Q=="Favourable")
Score1$inter_Q[id1]="Q1"
id2=which(Score1$PRS_Q=="Low genetic risk" & Score1$Social.score_Q=="Intermediate")
Score1$inter_Q[id2]="Q2"
id3=which(Score1$PRS_Q=="Low genetic risk" & Score1$Social.score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q3"
id1=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Social.score_Q=="Favourable")
Score1$inter_Q[id1]="Q4"
id2=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Social.score_Q=="Intermediate")
Score1$inter_Q[id2]="Q5"
id3=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Social.score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q6"
id1=which(Score1$PRS_Q=="High genetic risk" & Score1$Social.score_Q=="Favourable")
Score1$inter_Q[id1]="Q7"
id2=which(Score1$PRS_Q=="High genetic risk" & Score1$Social.score_Q=="Intermediate")
Score1$inter_Q[id2]="Q8"
id3=which(Score1$PRS_Q=="High genetic risk" & Score1$Social.score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q9"
Score1$inter_Q=as.factor(Score1$inter_Q)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+inter_Q, data=Score1)
#
mul_3<- summary(mul_cox)
colnames(mul_3$conf.int)
multi1<-as.data.frame(round(mul_3$conf.int[, c(1, 3, 4)], 4))
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
result1 <-cbind(multi1,multi2);result1
result[4:5,2:6]=result1[4:5,]
result[7:9,2:6]=result1[6:8,]
result[11:13,2:6]=result1[9:11,]
result[1,7]="Cases"
result[1,8]="Total"
result[3,1]="Favourable social score"
result[4,1]="Intermediate social score"
result[5,1]="Unfavourable social score"
result[7,1]="Favourable social score"
result[8,1]="Intermediate social score"
result[9,1]="Unfavourable social score"
result[11,1]="Favourable social score"
result[12,1]="Intermediate social score"
result[13,1]="Unfavourable social score"
# 统计人数
id1=which(data1$Social.score_Q=="Favourable")
data=data1[id1,]
id2=which(data$status==2)
result$casesofdelirium[3]=length(id2)
result$totalparticipants[3]=length(id1)

id1=which(data1$Social.score_Q=="Intermediate")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[4]=length(id2)
result$totalparticipants[4]=length(id1)

id1=which(data1$Social.score_Q=="Unfavourable")
data=data1[id1,]
id2=which(data$status==2)
result$casesofdelirium[5]=length(id2)
result$totalparticipants[5]=length(id1)
## intemiddle low genetic risk
id1=which(data2$Social.score_Q=="Favourable")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[7]=length(id2)
result$totalparticipants[7]=length(id1)

id1=which(data2$Social.score_Q=="Intermediate")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[8]=length(id2)
result$totalparticipants[8]=length(id1)

id1=which(data2$Social.score_Q=="Unfavourable")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[9]=length(id2)
result$totalparticipants[9]=length(id1)
### high genetic risk 
id1=which(data3$Social.score_Q=="Favourable")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[11]=length(id2)
result$totalparticipants[11]=length(id1)

id1=which(data3$Social.score_Q=="Intermediate")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[12]=length(id2)
result$totalparticipants[12]=length(id1)

id1=which(data3$Social.score_Q=="Unfavourable")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[13]=length(id2)
result$totalparticipants[13]=length(id1)

fig1<- forestplot(result[,c(1,7,8,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result[,3],  #告诉函数表格第3列为95%CI，
                  upper=result[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.6,       #设置小黑块的大小
                  graph.pos=4,
                  xticks=c(0.5,1,1.5,2,2.5,3.5,4.5),
                  lwd.xaxis=1.5,
                  lwd.zero=1.5,
                  lwd.ci=2,
                  txt_gp=fpTxtGp(ticks=gpar(cex=0.9)),
                  col=fpColors(box=c("brown"),lines = "grey",zero = "gray50"),
                  # is.summary=c(TRUE,rep(FALSE,14),TRUE), #是否突出显示。传入一个长度等于图表行数的向量，下标为TRUE的行会被加粗，且该行上下会添加一条直线，但在未设置颜色时不显示。
                  # hrzl_lines = gpar(col="red"))#线的颜色。默认作用于summary上下。)       #森林图应插在图形第2列
                  hrzl_lines = list( "2"=gpar(lty=1,lwd=2),
                                     "6" = gpar(lty=2,columns=c(1),col='grey'),
                                     "10" = gpar(lty=2,columns=c(1),col='grey')
                  ))
fig1
###
# Lifestyle.Score_Q AND prs
Score1$inter_Q=NA
id1=which(Score1$PRS_Q=="Low genetic risk" & Score1$Lifestyle.Score_Q=="Favourable")
Score1$inter_Q[id1]="Q1"
id2=which(Score1$PRS_Q=="Low genetic risk" & Score1$Lifestyle.Score_Q=="Intermediate")
Score1$inter_Q[id2]="Q2"
id3=which(Score1$PRS_Q=="Low genetic risk" & Score1$Lifestyle.Score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q3"
id1=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Lifestyle.Score_Q=="Favourable")
Score1$inter_Q[id1]="Q4"
id2=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Lifestyle.Score_Q=="Intermediate")
Score1$inter_Q[id2]="Q5"
id3=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Lifestyle.Score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q6"
id1=which(Score1$PRS_Q=="High genetic risk" & Score1$Lifestyle.Score_Q=="Favourable")
Score1$inter_Q[id1]="Q7"
id2=which(Score1$PRS_Q=="High genetic risk" & Score1$Lifestyle.Score_Q=="Intermediate")
Score1$inter_Q[id2]="Q8"
id3=which(Score1$PRS_Q=="High genetic risk" & Score1$Lifestyle.Score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q9"
Score1$inter_Q=as.factor(Score1$inter_Q)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+inter_Q, data=Score1)
#
mul_3<- summary(mul_cox)
colnames(mul_3$conf.int)
multi1<-as.data.frame(round(mul_3$conf.int[, c(1, 3, 4)], 4))
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
result1 <-cbind(multi1,multi2);result1
result[4:5,2:6]=result1[4:5,]
result[7:9,2:6]=result1[6:8,]
result[11:13,2:6]=result1[9:11,]
result[1,7]="Cases"
result[1,8]="Total"
result[3,1]="Favourable lifestyle score"
result[4,1]="Intermediate lifestyle score"
result[5,1]="Unfavourable lifestyle score"
result[7,1]="Favourable lifestyle score"
result[8,1]="Intermediate lifestyle score"
result[9,1]="Unfavourable lifestyle score"
result[11,1]="Favourable lifestyle score"
result[12,1]="Intermediate lifestyle score"
result[13,1]="Unfavourable lifestyle score"
# 统计人数
id1=which(data1$Lifestyle.Score_Q=="Favourable")
data=data1[id1,]
id2=which(data$status==2)
result$casesofdelirium[3]=length(id2)
result$totalparticipants[3]=length(id1)

id1=which(data1$Lifestyle.Score_Q=="Intermediate")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[4]=length(id2)
result$totalparticipants[4]=length(id1)

id1=which(data1$Lifestyle.Score_Q=="Unfavourable")
data=data1[id1,]
id2=which(data$status==2)
result$casesofdelirium[5]=length(id2)
result$totalparticipants[5]=length(id1)
## intemiddle low genetic risk
id1=which(data2$Lifestyle.Score_Q=="Favourable")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[7]=length(id2)
result$totalparticipants[7]=length(id1)

id1=which(data2$Lifestyle.Score_Q=="Intermediate")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[8]=length(id2)
result$totalparticipants[8]=length(id1)

id1=which(data2$Lifestyle.Score_Q=="Unfavourable")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[9]=length(id2)
result$totalparticipants[9]=length(id1)
### high genetic risk 
id1=which(data3$Lifestyle.Score_Q=="Favourable")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[11]=length(id2)
result$totalparticipants[11]=length(id1)

id1=which(data3$Lifestyle.Score_Q=="Intermediate")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[12]=length(id2)
result$totalparticipants[12]=length(id1)

id1=which(data3$Lifestyle.Score_Q=="Unfavourable")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[13]=length(id2)
result$totalparticipants[13]=length(id1)
fig1<- forestplot(result[,c(1,7,8,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result[,3],  #告诉函数表格第3列为95%CI，
                  upper=result[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.6,       #设置小黑块的大小
                  graph.pos=4,
                  xticks=c(0.5,1.5,2.5,3.5),
                  lwd.xaxis=1.5,
                  lwd.zero=1.5,
                  lwd.ci=2,
                  txt_gp=fpTxtGp(ticks=gpar(cex=0.9)),
                  col=fpColors(box=c("brown"),lines = "grey",zero = "gray50"),
                  # is.summary=c(TRUE,rep(FALSE,14),TRUE), #是否突出显示。传入一个长度等于图表行数的向量，下标为TRUE的行会被加粗，且该行上下会添加一条直线，但在未设置颜色时不显示。
                  # hrzl_lines = gpar(col="red"))#线的颜色。默认作用于summary上下。)       #森林图应插在图形第2列
                  hrzl_lines = list( "2"=gpar(lty=1,lwd=2),
                                     "6" = gpar(lty=2,columns=c(1),col='grey'),
                                     "10" = gpar(lty=2,columns=c(1),col='grey')
                  ))
fig1
##
#physical.Score AND prs
Score1$inter_Q=NA
id1=which(Score1$PRS_Q=="Low genetic risk" & Score1$Physical.Score_Q=="Favourable")
Score1$inter_Q[id1]="Q1"
id2=which(Score1$PRS_Q=="Low genetic risk" & Score1$Physical.Score_Q=="Intermediate")
Score1$inter_Q[id2]="Q2"
id3=which(Score1$PRS_Q=="Low genetic risk" & Score1$Physical.Score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q3"
id1=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Physical.Score_Q=="Favourable")
Score1$inter_Q[id1]="Q4"
id2=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Physical.Score_Q=="Intermediate")
Score1$inter_Q[id2]="Q5"
id3=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Physical.Score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q6"
id1=which(Score1$PRS_Q=="High genetic risk" & Score1$Physical.Score_Q=="Favourable")
Score1$inter_Q[id1]="Q7"
id2=which(Score1$PRS_Q=="High genetic risk" & Score1$Physical.Score_Q=="Intermediate")
Score1$inter_Q[id2]="Q8"
id3=which(Score1$PRS_Q=="High genetic risk" & Score1$Physical.Score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q9"
Score1$inter_Q=as.factor(Score1$inter_Q)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+inter_Q, data=Score1)
#
mul_3<- summary(mul_cox)
colnames(mul_3$conf.int)
multi1<-as.data.frame(round(mul_3$conf.int[, c(1, 3, 4)], 4))
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
result1 <-cbind(multi1,multi2);result1
result[4:5,2:6]=result1[4:5,]
result[7:9,2:6]=result1[6:8,]
result[11:13,2:6]=result1[9:11,]
result[1,7]="Cases"
result[1,8]="Total"
result[3,1]="Favourable physical score"
result[4,1]="Intermediate physical score"
result[5,1]="Unfavourable physical score"
result[7,1]="Favourable physical score"
result[8,1]="Intermediate physical score"
result[9,1]="Unfavourable physical score"
result[11,1]="Favourable physical score"
result[12,1]="Intermediate physical score"
result[13,1]="Unfavourable physical score"
# 统计人数
id1=which(data1$Physical.Score_Q=="Favourable")
data=data1[id1,]
id2=which(data$status==2)
result$casesofdelirium[3]=length(id2)
result$totalparticipants[3]=length(id1)

id1=which(data1$Physical.Score_Q=="Intermediate")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[4]=length(id2)
result$totalparticipants[4]=length(id1)

id1=which(data1$Physical.Score_Q=="Unfavourable")
data=data1[id1,]
id2=which(data$status==2)
result$casesofdelirium[5]=length(id2)
result$totalparticipants[5]=length(id1)
## intemiddle low genetic risk
id1=which(data2$Physical.Score_Q=="Favourable")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[7]=length(id2)
result$totalparticipants[7]=length(id1)

id1=which(data2$Physical.Score_Q=="Intermediate")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[8]=length(id2)
result$totalparticipants[8]=length(id1)

id1=which(data2$Physical.Score_Q=="Unfavourable")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[9]=length(id2)
result$totalparticipants[9]=length(id1)
### high genetic risk 
id1=which(data3$Physical.Score_Q=="Favourable")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[11]=length(id2)
result$totalparticipants[11]=length(id1)

id1=which(data3$Physical.Score_Q=="Intermediate")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[12]=length(id2)
result$totalparticipants[12]=length(id1)

id1=which(data3$Physical.Score_Q=="Unfavourable")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[13]=length(id2)
result$totalparticipants[13]=length(id1)

fig1<- forestplot(result[,c(1,7,8,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result[,3],  #告诉函数表格第3列为95%CI，
                  upper=result[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.6,       #设置小黑块的大小
                  graph.pos=4,
                  xticks=c(0.5,1.5,2.5,3.5,4.5,5.5),
                  lwd.xaxis=1.5,
                  lwd.zero=1.5,
                  lwd.ci=2,
                  txt_gp=fpTxtGp(ticks=gpar(cex=0.9)),
                  col=fpColors(box=c("brown"),lines = "grey",zero = "gray50"),
                  # is.summary=c(TRUE,rep(FALSE,14),TRUE), #是否突出显示。传入一个长度等于图表行数的向量，下标为TRUE的行会被加粗，且该行上下会添加一条直线，但在未设置颜色时不显示。
                  # hrzl_lines = gpar(col="red"))#线的颜色。默认作用于summary上下。)       #森林图应插在图形第2列
                  hrzl_lines = list( "2"=gpar(lty=1,lwd=2),
                                     "6" = gpar(lty=2,columns=c(1),col='grey'),
                                     "10" = gpar(lty=2,columns=c(1),col='grey')
                  ))
fig1
##
## blood assay AND prs
Score1$inter_Q=NA
id1=which(Score1$PRS_Q=="Low genetic risk" & Score1$Bloodassay.Score_Q=="Favourable")
Score1$inter_Q[id1]="Q1"
id2=which(Score1$PRS_Q=="Low genetic risk" & Score1$Bloodassay.Score_Q=="Intermediate")
Score1$inter_Q[id2]="Q2"
id3=which(Score1$PRS_Q=="Low genetic risk" & Score1$Bloodassay.Score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q3"
id1=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Bloodassay.Score_Q=="Favourable")
Score1$inter_Q[id1]="Q4"
id2=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Bloodassay.Score_Q=="Intermediate")
Score1$inter_Q[id2]="Q5"
id3=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Bloodassay.Score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q6"
id1=which(Score1$PRS_Q=="High genetic risk" & Score1$Bloodassay.Score_Q=="Favourable")
Score1$inter_Q[id1]="Q7"
id2=which(Score1$PRS_Q=="High genetic risk" & Score1$Bloodassay.Score_Q=="Intermediate")
Score1$inter_Q[id2]="Q8"
id3=which(Score1$PRS_Q=="High genetic risk" & Score1$Bloodassay.Score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q9"
Score1$inter_Q=as.factor(Score1$inter_Q)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+inter_Q, data=Score1)
#
mul_3<- summary(mul_cox)
colnames(mul_3$conf.int)
multi1<-as.data.frame(round(mul_3$conf.int[, c(1, 3, 4)], 4))
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
result1 <-cbind(multi1,multi2);result1
result[4:5,2:6]=result1[4:5,]
result[7:9,2:6]=result1[6:8,]
result[11:13,2:6]=result1[9:11,]
result[1,7]="Cases"
result[1,8]="Total"
result[3,1]="Favourable blood assay score"
result[4,1]="Intermediate blood assay score"
result[5,1]="Unfavourable blood assay score"
result[7,1]="Favourable blood assay score"
result[8,1]="Intermediate blood assay score"
result[9,1]="Unfavourable blood assay score"
result[11,1]="Favourable blood assay score"
result[12,1]="Intermediate blood assay score"
result[13,1]="Unfavourable blood assay score"
# 统计人数
id1=which(data1$Bloodassay.Score_Q=="Favourable")
data=data1[id1,]
id2=which(data$status==2)
result$casesofdelirium[3]=length(id2)
result$totalparticipants[3]=length(id1)

id1=which(data1$Bloodassay.Score_Q=="Intermediate")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[4]=length(id2)
result$totalparticipants[4]=length(id1)

id1=which(data1$Bloodassay.Score_Q=="Unfavourable")
data=data1[id1,]
id2=which(data$status==2)
result$casesofdelirium[5]=length(id2)
result$totalparticipants[5]=length(id1)
## intemiddle low genetic risk
id1=which(data2$Bloodassay.Score_Q=="Favourable")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[7]=length(id2)
result$totalparticipants[7]=length(id1)

id1=which(data2$Bloodassay.Score_Q=="Intermediate")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[8]=length(id2)
result$totalparticipants[8]=length(id1)

id1=which(data2$Bloodassay.Score_Q=="Unfavourable")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[9]=length(id2)
result$totalparticipants[9]=length(id1)
### high genetic risk 
id1=which(data3$Bloodassay.Score_Q=="Favourable")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[11]=length(id2)
result$totalparticipants[11]=length(id1)

id1=which(data3$Bloodassay.Score_Q=="Intermediate")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[12]=length(id2)
result$totalparticipants[12]=length(id1)

id1=which(data3$Bloodassay.Score_Q=="Unfavourable")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[13]=length(id2)
result$totalparticipants[13]=length(id1)
fig1<- forestplot(result[,c(1,7,8,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result[,3],  #告诉函数表格第3列为95%CI，
                  upper=result[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.6,       #设置小黑块的大小
                  graph.pos=4,
                  xticks=c(0.5,1,1.5,2,2.5),
                  lwd.xaxis=1.5,
                  lwd.zero=1.5,
                  lwd.ci=2,
                  txt_gp=fpTxtGp(ticks=gpar(cex=0.9)),
                  col=fpColors(box=c("brown"),lines = "grey",zero = "gray50"),
                  # is.summary=c(TRUE,rep(FALSE,14),TRUE), #是否突出显示。传入一个长度等于图表行数的向量，下标为TRUE的行会被加粗，且该行上下会添加一条直线，但在未设置颜色时不显示。
                  # hrzl_lines = gpar(col="red"))#线的颜色。默认作用于summary上下。)       #森林图应插在图形第2列
                  hrzl_lines = list( "2"=gpar(lty=1,lwd=2),
                                     "6" = gpar(lty=2,columns=c(1),col='grey'),
                                     "10" = gpar(lty=2,columns=c(1),col='grey')
                  ))
fig1
# medical_score_Q AND prs
Score1$inter_Q=NA
id1=which(Score1$PRS_Q=="Low genetic risk" & Score1$Medical.Score_Q=="Favourable")
Score1$inter_Q[id1]="Q1"
id2=which(Score1$PRS_Q=="Low genetic risk" & Score1$Medical.Score_Q=="Intermediate")
Score1$inter_Q[id2]="Q2"
id3=which(Score1$PRS_Q=="Low genetic risk" & Score1$Medical.Score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q3"
id1=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Medical.Score_Q=="Favourable")
Score1$inter_Q[id1]="Q4"
id2=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Medical.Score_Q=="Intermediate")
Score1$inter_Q[id2]="Q5"
id3=which(Score1$PRS_Q=="Intermediate genetic risk" & Score1$Medical.Score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q6"
id1=which(Score1$PRS_Q=="High genetic risk" & Score1$Medical.Score_Q=="Favourable")
Score1$inter_Q[id1]="Q7"
id2=which(Score1$PRS_Q=="High genetic risk" & Score1$Medical.Score_Q=="Intermediate")
Score1$inter_Q[id2]="Q8"
id3=which(Score1$PRS_Q=="High genetic risk" & Score1$Medical.Score_Q=="Unfavourable")
Score1$inter_Q[id3]="Q9"
Score1$inter_Q=as.factor(Score1$inter_Q)
mul_cox=coxph(Surv(time,status) ~Age+Sex+Center+inter_Q, data=Score1)
#
mul_3<- summary(mul_cox)
colnames(mul_3$conf.int)
multi1<-as.data.frame(round(mul_3$conf.int[, c(1, 3, 4)], 4))
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
result1 <-cbind(multi1,multi2);result1
result[4:5,2:6]=result1[4:5,]
result[7:9,2:6]=result1[6:8,]
result[11:13,2:6]=result1[9:11,]
result[1,7]="Cases"
result[1,8]="Total"
result[3,1]="Favourable medical score"
result[4,1]="Intermediate medical score"
result[5,1]="Unfavourable medical score"
result[7,1]="Favourable medical score"
result[8,1]="Intermediate medical score"
result[9,1]="Unfavourable medical score"
result[11,1]="Favourable medical score"
result[12,1]="Intermediate medical score"
result[13,1]="Unfavourable medical score"
# 统计人数
id1=which(data1$Medical.Score_Q=="Favourable")
data=data1[id1,]
id2=which(data$status==2)
result$casesofdelirium[3]=length(id2)
result$totalparticipants[3]=length(id1)

id1=which(data1$Medical.Score_Q=="Intermediate")
data=Score1[id1,]
id2=which(data$status==2)
result$casesofdelirium[4]=length(id2)
result$totalparticipants[4]=length(id1)

id1=which(data1$Medical.Score_Q=="Unfavourable")
data=data1[id1,]
id2=which(data$status==2)
result$casesofdelirium[5]=length(id2)
result$totalparticipants[5]=length(id1)
## intemiddle low genetic risk
id1=which(data2$Medical.Score_Q=="Favourable")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[7]=length(id2)
result$totalparticipants[7]=length(id1)

id1=which(data2$Medical.Score_Q=="Intermediate")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[8]=length(id2)
result$totalparticipants[8]=length(id1)

id1=which(data2$Medical.Score_Q=="Unfavourable")
data=data2[id1,]
id2=which(data$status==2)
result$casesofdelirium[9]=length(id2)
result$totalparticipants[9]=length(id1)
### high genetic risk 
id1=which(data3$Medical.Score_Q=="Favourable")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[11]=length(id2)
result$totalparticipants[11]=length(id1)

id1=which(data3$Medical.Score_Q=="Intermediate")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[12]=length(id2)
result$totalparticipants[12]=length(id1)

id1=which(data3$Medical.Score_Q=="Unfavourable")
data=data3[id1,]
id2=which(data$status==2)
result$casesofdelirium[13]=length(id2)
result$totalparticipants[13]=length(id1)
fig1<- forestplot(result[,c(1,7,8,5,6)], #告诉函数，合成的表格result的第1，5，6列还是显示数字
                  mean=result[,2],   #告诉函数，表格第2列为HR，它要变成森林图的小方块
                  lower=result[,3],  #告诉函数表格第3列为95%CI，
                  upper=result[,4],  #表格第5列为95%CI，它俩要化作线段，穿过方块
                  zero=1,            #告诉函数，零线或参考线为HR=1即x轴的垂直线
                  boxsize=0.6,       #设置小黑块的大小
                  graph.pos=4,
                  xticks=c(0.5,1,1.5,2,2.5,3.5),
                  lwd.xaxis=1.5,
                  lwd.zero=1.5,
                  lwd.ci=2,
                  txt_gp=fpTxtGp(ticks=gpar(cex=0.9)),
                  col=fpColors(box=c("brown"),lines = "grey",zero = "gray50"),
                  # is.summary=c(TRUE,rep(FALSE,14),TRUE), #是否突出显示。传入一个长度等于图表行数的向量，下标为TRUE的行会被加粗，且该行上下会添加一条直线，但在未设置颜色时不显示。
                  # hrzl_lines = gpar(col="red"))#线的颜色。默认作用于summary上下。)       #森林图应插在图形第2列
                  hrzl_lines = list( "2"=gpar(lty=1,lwd=2),
                                     "6" = gpar(lty=2,columns=c(1),col='grey'),
                                     "10" = gpar(lty=2,columns=c(1),col='grey')
                  ))
fig1
###

#####

####

# 

# 4 PAF -------------------------------------------------------------------
# PCA
Score1=read.csv("E:/UKB/UKB94885/Delirium/Score1.csv")
Score1=Score1[,c(-1,-2,-3)]
library(psych)
# data 存放
data=Score1[,c(6:12)]
result <- principal(data, scores=TRUE)
communalities <- result$communality
weights=result$loadings
# model 1: 将有利和中间因素组合为1组
Score1$earlylife_score_Q1=Score1$Earlylife.score_Q
id6=which(Score1$Earlylife.score_Q=="Intermediate")
Score1$earlylife_score_Q1[id6]="Favourable"
#
Score1$Environment_score_Q1=Score1$Environment.score_Q
id1=which(Score1$Environment.score_Q=="Intermediate")
Score1$Environment_score_Q1[id1]="Favourable"
#
Score1$social_score_Q1=Score1$Social.score_Q
id2=which(Score1$Social.score_Q=="Intermediate")
Score1$social_score_Q1[id2]="Favourable"
#
Score1$lifestyle_score_Q1=Score1$Lifestyle.Score_Q
id5=which(Score1$Lifestyle.Score_Q=="Intermediate")
Score1$lifestyle_score_Q1[id5]="Favourable"
#
Score1$physical_score_Q1=Score1$Physical.Score_Q
id4=which(Score1$Physical.Score_Q=="Intermediate")
Score1$physical_score_Q1[id4]="Favourable"
#
Score1$bloodassay_score_Q1=Score1$Bloodassay.Score_Q
id6=which(Score1$Bloodassay.Score_Q=="Intermediate")
Score1$bloodassay_score_Q1[id6]="Favourable"
#
Score1$medical_score_Q1=Score1$Medical.Score_Q
id3=which(Score1$Medical.Score_Q=="Intermediate")
Score1$medical_score_Q1[id3]="Favourable"
# data1 包含time, status, age, sex, center, 七个领域得分Q1
data1=Score1[,c(1:5,20:27)]
library(survival)
data1$Sex=as.factor(data1$Sex)
data1$Center=as.factor(data1$Center)
#data1[,7:12]=as.factor(data1[,7:12])
mode1=coxph(Surv(time,status) ~., data=data1)
# PAF = P(E) * (RR-1) / [1 + P(E) * (RR-1)], P(E)暴露的患病率
l=summary(mode1)
HR=l$conf.int[25:31,c(1,3,4)]
#
P=HR[,1]
id1=which(data1$earlylife_score_Q1=="Unfavourable")
id2=which(Score1$status[id1]==2)
P[1]=length(id1)/487502
#
id1=which(data1$Environment_score_Q1=="Unfavourable")
id2=which(Score1$status[id1]==2)
P[2]=length(id1)/487502
#
id1=which(data1$social_score_Q1=="Unfavourable")
id2=which(Score1$status[id1]==2)
P[3]=length(id1)/487502
#
id1=which(data1$lifestyle_score_Q1=="Unfavourable")
id2=which(Score1$status[id1]==2)
P[4]=length(id1)/487502
#
id1=which(data1$physical_score_Q1=="Unfavourable")
id2=which(Score1$status[id1]==2)
P[5]=length(id1)/487502
#
id1=which(data1$bloodassay_score_Q1=="Unfavourable")
id2=which(Score1$status[id1]==2)
P[6]=length(id1)/487502
#
id1=which(data1$medical_score_Q1=="Unfavourable")
id2=which(Score1$status[id1]==2)
P[7]=length(id1)/487502
# unweight PAF
PAF=HR[,1]
PAF=P*(HR-1)/(1+P*(HR-1))
#
# PAF1=HR[,1]
# PAF1=P*(HR-1)/HR
write.csv(PAF,"E:/UKB/UKB94885/Delirium/Results/PAF.csv")
#
# data1$Sex=as.numeric(data1$Sex)
# data1[,7]=as.factor(data1[,7])
# data1[,8]=as.factor(data1[,8])
# data1[,9]=as.factor(data1[,9])
# data1[,10]=as.factor(data1[,10])
# data1[,11]=as.factor(data1[,11])
# data1[,12]=as.factor(data1[,12])
# data1[,7]=as.numeric(data1[,7])
# data1[,8]=as.numeric(data1[,8])
# data1[,9]=as.numeric(data1[,9])
# data1[,10]=as.numeric(data1[,10])
# data1[,11]=as.numeric(data1[,11])
# data1[,12]=as.numeric(data1[,12])
# result <- principal(data1[,7:12], scores=TRUE)
# commnality=result$communality
# weight=result$loadings
data=data1[,c(7:13)]
for (i in c(1:7))
{
  data[,i]=as.factor(data[,i])
  data[,i]=as.numeric(data[,i])
}
result <- principal(data, scores=TRUE)
communalities <- result$communality
weights=result$loadings
weightPAF=PAF
for (i in c(1:3))
{weightPAF[,i]=weights*PAF[,i]}
write.csv(weightPAF,"E:/UKB/UKB94885/Delirium/Results/weightPAF.csv")
####
# model 2: 将不利因素和中间因素组合为1组 
Score1$earlylife_score_Q1=Score1$Earlylife.score_Q
id6=which(Score1$Earlylife.score_Q=="Intermediate")
Score1$earlylife_score_Q1[id6]="Unfavourable"
#
Score1$Environment_score_Q1=Score1$Environment.score_Q
id1=which(Score1$Environment.score_Q=="Intermediate")
Score1$Environment_score_Q1[id1]="Unfavourable"
#
Score1$social_score_Q1=Score1$Social.score_Q
id2=which(Score1$Social.score_Q=="Intermediate")
Score1$social_score_Q1[id2]="Unfavourable"
#
Score1$lifestyle_score_Q1=Score1$Lifestyle.Score_Q
id5=which(Score1$Lifestyle.Score_Q=="Intermediate")
Score1$lifestyle_score_Q1[id5]="Unfavourable"
#
Score1$physical_score_Q1=Score1$Physical.Score_Q
id4=which(Score1$Physical.Score_Q=="Intermediate")
Score1$physical_score_Q1[id4]="Unfavourable"
#
Score1$bloodassay_score_Q1=Score1$Bloodassay.Score_Q
id6=which(Score1$Bloodassay.Score_Q=="Intermediate")
Score1$bloodassay_score_Q1[id6]="Unfavourable"
#
Score1$medical_score_Q1=Score1$Medical.Score_Q
id3=which(Score1$Medical.Score_Q=="Intermediate")
Score1$medical_score_Q1[id3]="Unfavourable"
#
# data1 包含time, status, age, sex, center, 七个领域得分Q1
data1=Score1[,c(1:5,20:27)]
library(survival)
data1$Sex=as.factor(data1$Sex)
data1$Center=as.factor(data1$Center)
#data1[,7:12]=as.factor(data1[,7:12])
mode1=coxph(Surv(time,status) ~., data=data1)
# PAF = P(E) * (RR-1) / [1 + P(E) * (RR-1)], P(E)暴露的患病率
l=summary(mode1)
HR=l$conf.int[25:31,c(1,3,4)]
#
id1=which(data1$earlylife_score_Q1=="Unfavourable")
id2=which(Score1$status[id1]==2)
P[1]=length(id1)/487502
#
id1=which(data1$Environment_score_Q1=="Unfavourable")
id2=which(Score1$status[id1]==2)
P=HR[,1]
P[2]=length(id1)/487502
#
id1=which(data1$social_score_Q1=="Unfavourable")
id2=which(Score1$status[id1]==2)
P[3]=length(id1)/487502
#
id1=which(data1$lifestyle_score_Q1=="Unfavourable")
id2=which(Score1$status[id1]==2)
P[4]=length(id1)/487502
#
id1=which(data1$physical_score_Q1=="Unfavourable")
id2=which(Score1$status[id1]==2)
P[5]=length(id1)/487502
#
id1=which(data1$bloodassay_score_Q1=="Unfavourable")
id2=which(Score1$status[id1]==2)
P[6]=length(id1)/487502
#
id1=which(data1$medical_score_Q1=="Unfavourable")
id2=which(Score1$status[id1]==2)
P[7]=length(id1)/487502
# unweight PAF
PAF=HR[,1]
PAF=P*(HR-1)/(1+P*(HR-1))
#
# PAF1=HR[,1]
# PAF1=P*(HR-1)/HR
write.csv(PAF,"E:/UKB/UKB94885/Delirium/Results/mode2PAF.csv")
#
data=data1[,c(7:13)]
for (i in c(1:7))
{
  data[,i]=as.factor(data[,i])
  data[,i]=as.numeric(data[,i])
}

result <- principal(data, scores=TRUE)
communalities <- result$communality
weights=result$loadings
weightPAF=PAF
for (i in c(1:3))
{weightPAF[,i]=weights*PAF[,i]}
write.csv(weightPAF,"E:/UKB/UKB94885/Delirium/Results/mode2weightPAF.csv")
# 

# 人口 ----------------------------------------------------------------------
data=read.csv("E:/UKB/UKB94885/Delirium/Score.csv")
id1=which(data$status==2) # id1 发展为delirium的人数：8717
data2=data[id1,]
data3=data[-id1,]
time=mean(data1$time)
sdtime=sd(data1$time)
mean(data2$time)
sd(data2$time)
mean(data3$time)
sd(data3$time)
## sex
id1=which(data3$Sex==0) #female
id2=which(data3$Sex==1) # male
length(id1)
length(id2)
length(id1)/(length(id1)+length(id2))
# age
mean(data3$Age)
sd(data3$Age)
