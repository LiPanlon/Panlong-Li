## RF实现生多分类预测
library(randomForest)
library(pROC)
data2=read.csv("G:/Data/ZhouYun/results/individual network/RFmodel/Features.csv")
data2=na.omit(data2)
View(data2)
data2$classes <-factor(data2$classes, levels=c(1,2,3),labels=c("HC","MCI","AD"))
train_sub=sample(nrow(data2),9/10*nrow(data2))
train_data=data2[train_sub,]
test_data=data2[-train_sub,]
set.seed(71)
classes.rf<- randomForest(classes~ ., data=train_data,importance=TRUE,proximity=TRUE)
print(classes.rf)
plot(classes.rf)
nt=which.min(classes.rf$err.rate[,1]) #确定树的棵树ntree 
classes.rf<- randomForest(classes~ ., data=train_data,ntree=50000,mtry=30,importance=TRUE,proximity=TRUE)
importance(classes.rf)# 计算自变量对因变量预测的重要性
varImpPlot(classes.rf)
print(classes.rf)
# 多分类的ROC曲线
pre2=predict(classes.rf,test_data,type="prob")
roc2 <-multiclass.roc(test_data$classes, pre2[,1])
roc2
plot(roc2$rocs[[1]],col='green')
plot(roc2$rocs[[2]],add=TRUE,col='blue')
plot(roc2$rocs[[3]],add=TRUE, col='red',lty=2)
round(auc(roc2$rocs[[1]]),3)
round(auc(roc2$rocs[[2]]),3)
round(auc(roc2$rocs[[3]]),3)
##
data3=read.csv("G:/Data/ZhouYun/results/individual network/RFmodel/Features3.csv")
data3=na.omit(data3)
View(data3)
data3$classes <-factor(data3$classes, levels=c(1,2,3),labels=c("HC","MCI","AD"))
train_data1=data3[train_sub,]
test_data1=data3[-train_sub,]
classes.rf1<- randomForest(classes~ ., data=train_data1,ntree=50000,mtry=6,importance=TRUE,proximity=TRUE)
importance(classes.rf1)# 计算自变量对因变量预测的重要性
varImpPlot(classes.rf1,n.var=10)
print(classes.rf1)
# 多分类的ROC曲线
pre3=predict(classes.rf1,test_data1,type="prob")
roc3 <-multiclass.roc(test_data1$classes, pre3[,1])
roc3
# plot(1-ROC1$specificities,ROC1$sensitivities,type="l",
# col="red",lty=1,xlab="FP",ylab="TP",lwd=2,xlim=c(0,1),ylim=c(0,1),auc.polygon=TRUE,auc.polygon.col="blue",
# max.auc.polygon=TRUE)

# plot(roc3$rocs[[2]],add=TRUE,col='blue')
# plot(roc3$rocs[[3]],add=TRUE, col='red',lty=2)
# 最终绘图
par(mfrow=c(2,2))
# 
plot(1-roc3$rocs[[1]]$specificities, roc3$rocs[[1]]$sensitivities, col='green',
     type="l",lty=1,xlab="FP",ylab="TP",lwd=2,xlim=c(0,1),ylim=c(0,1),auc.polygon=TRUE,auc.polygon.col="blue",
     main="ROC of SUVR-RF Model",font.main=2,cex.main=1)
lines(1-roc3$rocs[[2]]$specificities, roc3$rocs[[2]]$sensitivities, col='blue',
     type="l",lty=1,xlab="FP",ylab="TP",lwd=2,xlim=c(0,1),ylim=c(0,1),auc.polygon=TRUE,auc.polygon.col="blue",
)
lines(1-roc3$rocs[[3]]$specificities, roc3$rocs[[3]]$sensitivities, col='red',
      type="l",lty=1,xlab="FP",ylab="TP",lwd=2,xlim=c(0,1),ylim=c(0,1),auc.polygon=TRUE,auc.polygon.col="blue",
)
abline(0,1,col="black",lty=2,lwd=2)#绘制参考线
round(auc(roc3$rocs[[1]]),3)
round(auc(roc3$rocs[[2]]),3)
round(auc(roc3$rocs[[3]]),3)
legend("bottomright",cex=1,inset=0.1,title="Classes",c("HC-MCI,AUC=0.72","HC-AD,AUC=0.95","MCI-AD,AUC=0.86"),
       lty=c(1,1,1),col=c("green","blue","red"))
#
plot(1-roc2$rocs[[1]]$specificities, roc2$rocs[[1]]$sensitivities, col='green',
     type="l",lty=1,xlab="FP",ylab="TP",lwd=2,xlim=c(0,1),ylim=c(0,1),auc.polygon=TRUE,auc.polygon.col="blue",
main="ROC of NT-RF Model",font.main=2,cex.main=1)
lines(1-roc2$rocs[[2]]$specificities, roc2$rocs[[2]]$sensitivities, col='blue',
      type="l",lty=1,xlab="FP",ylab="TP",lwd=2,xlim=c(0,1),ylim=c(0,1),auc.polygon=TRUE,auc.polygon.col="blue",
)
lines(1-roc2$rocs[[3]]$specificities, roc2$rocs[[3]]$sensitivities, col='red',
      type="l",lty=1,xlab="FP",ylab="TP",lwd=2,xlim=c(0,1),ylim=c(0,1),auc.polygon=TRUE,auc.polygon.col="blue",
)
abline(0,1,col="black",lty=2,lwd=2)#绘制参考线
round(auc(roc2$rocs[[1]]),3)
round(auc(roc2$rocs[[2]]),3)
round(auc(roc2$rocs[[3]]),3)
legend("bottomright",cex=1,inset=0.1,title="Classes",c("HC-MCI,AUC=0.78","HC-AD,AUC=0.98","MCI-AD,AUC=0.93"),
       lty=c(1,1,1),col=c("green","blue","red"))
varImpPlot(classes.rf,n.var=10,typ=1,main="feature importance measured by SUVR-RF model",font.main=2,cex.main=1)
varImpPlot(classes.rf1,n.var=10,type=1, main="feature importance measured by NT-RF model",font.main=2,cex.main=1)

