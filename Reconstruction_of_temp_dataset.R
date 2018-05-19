# Cleaned code for
# Reconstruction of the indoor temperature dataset of a house using data driven models for performance evaluation
# Luis M Candanedo, Veronique Feldheim, Dominique Deramaix
# Building and Environment 138, 2018
#


library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)
library(gridExtra)


# Set your working directory where you will have the csv file.
#setwd("C:/Users/luism/Dropbox/Room_clustering")
 
energy_data2  <- read.table("energy_weather_temp.csv",header = TRUE,sep=',',
                           colClasses=c("character",rep("numeric",29)))

energy_data2$date <- as.POSIXct(energy_data2$date,tz="UTC") 


dim(energy_data2)
# 57168 x 30



interval_tcleaned2 <- energy_data2$date >= as.POSIXct("2016-01-1 00:00:00",tz="UTC") & 
  energy_data2$date < as.POSIXct("2017-01-01 00:00:00",tz="UTC")

energy_data2_c2 <- energy_data2[interval_tcleaned2,] 
View(energy_data2_c2)
dim(energy_data2_c2)
# 52704 x30
energy_data2_c2$date[1]-energy_data2_c2$date[52704]
# Time difference of  -365.9931 days days


energy_data2_c2$dayyear <- yday(energy_data2_c2$date)
#View(energy_data2_c2)

summary(energy_data2_c2)
View(energy_data2_c2)


energy_data3_removed_NAN <- na.omit(energy_data2_c2)
summary(energy_data3_removed_NAN)

energy_data2_c2$TOTAL_ELEC <- energy_data2_c2$ElectricHeater+energy_data2_c2$ECS+
  energy_data2_c2$Appliances+energy_data2_c2$lights+
  energy_data2_c2$Ventilation

summary(energy_data2_c2$TOTAL_ELEC)

# now only selected the columns we are interested in.



TempDFb <- data.frame(T1=energy_data2_c2$T1,
                     T2=energy_data2_c2$T2,
                     T3=energy_data2_c2$T3,
                     T4=energy_data2_c2$T4,
                     T5=energy_data2_c2$T5,
                     T7=energy_data2_c2$T7,
                     T8=energy_data2_c2$T8,
                     T9=energy_data2_c2$T9,
                     date=energy_data2_c2$date,
                     Tout=energy_data2_c2$T_out,
                     RH_out=energy_data2_c2$RH_out,
                     WS=energy_data2_c2$Windspeed,
                     VISI=energy_data2_c2$Visibility,
                     Tdew=energy_data2_c2$Tdewpoint,
                     Pressure=energy_data2_c2$Press_mm_hg,
                     TOTAL_ELEC=energy_data2_c2$TOTAL_ELEC )
summary(TempDFb)
dim(TempDFb)
summary(TempDF)
dim(TempDF)

names(TempDFb)
TempDFb <- dplyr::mutate(TempDFb,max = pmax(T1,T2,T3,T4,T5,T7,T8,T9,na.rm = TRUE))
summary(TempDFb)
#View(TempDF)

TempDFb <-dplyr::mutate(dplyr::rowwise(TempDFb), AVG=mean(c(T1,T2,T3,T4,T5,T7,T8,T9),na.rm = TRUE))
summary(TempDFb)
#View(TempDF )

TempDFb <- dplyr::mutate(TempDFb,min = pmin(T1,T2,T3,T4,T5,T7,T8,T9,na.rm = TRUE))


View(TempDFb)
names(TempDFb)

TAVGVolweight <- function(T1,T2,T3,T4,T5,T7,T8,T9){
  VolTotal = 332.64
  
  if(sum(is.na(c(T1,T2,T3,T4,T5,T7,T8,T9)))>=2)
  {
    TAvg <- NA
    return(TAvg)
  }
  
  
  if(is.na(T1)){
    VolTotal = VolTotal -58.21
    
  }
  if(is.na(T2)){
    VolTotal = VolTotal -103.77
  }
  if(is.na(T3)){
    VolTotal = VolTotal -43.12
  }
  if(is.na(T4)){
    VolTotal = VolTotal -41.93
  }
  if(is.na(T5)){
    VolTotal = VolTotal -10.15
  }
  if(is.na(T7)){
    VolTotal = VolTotal -10.8
  }
  if(is.na(T8)){
    VolTotal = VolTotal -25.13
  }
  if(is.na(T9)){
    VolTotal = VolTotal -39.53
  }
  
  
  Weightsumval <- sum(T1*58.21,T2*103.77,T3*43.12,T4*41.93,T5*10.15,
                      T7*10.8,T8*25.13,T9*39.53,na.rm=TRUE)
  
  TAvg <- Weightsumval/(VolTotal)
  return(TAvg)
}

TAVGVolweight(1,NA,2,2,2,2,NA,NA)
TAVGVolweight(1,1,1,1,NA,NA,NA,1)
TAVGVolweight(1,1,1,10,1,1,1,1)
TAVGVolweight(1,2,2,2,2,2,NA,3)



TempDFb$TAVG_weighed <- mapply(TAVGVolweight,TempDFb$T1,TempDFb$T2,
                              TempDFb$T3,
                              TempDFb$T4,
                              TempDFb$T5,
                              TempDFb$T7,
                              TempDFb$T8,
                              TempDFb$T9)

# TempDF <-dplyr::mutate(dplyr::rowwise(TempDF), AVG=mean(c(T1,T2,T3,T4,T5,T7,T8,T9),na.rm = TRUE))
# summary(TempDF)
plot(TempDFb$date,TempDFb$TAVG_weighed,col="red",type='l')
lines(TempDFb$date,TempDFb$AVG,col="black",type='l')

summary(TempDFb)

names(TempDFb)

library(timeSeries)
result <- timeSeries::interpNA(TempDFb[,10],method="linear")
dim(result)
result[,1]

TempDFb$Tout <- timeSeries::interpNA(TempDFb[,10],method="linear")[,1]

names(TempDFb)
summary(TempDFb)
library(zoo)
summary(TempDFb)
TempDFb[,10] <- na.spline(TempDFb[,10])[,1]
summary(TempDFb[,10])

TempDFb[,11] <- interpNA(TempDFb[,11],method="linear")[,1]
TempDFb[,11] <- na.spline(TempDFb[,11])[,1]

TempDFb[,12] <- interpNA(TempDFb[,12],method="linear")[,1]
TempDFb[,12] <- na.spline(TempDFb[,12])[,1]

TempDFb[,13] <- interpNA(TempDFb[,13],method="linear")[,1]
TempDFb[,13] <- na.spline(TempDFb[,13])[,1]

TempDFb[,14] <- interpNA(TempDFb[,14],method="linear")[,1]
TempDFb[,14] <- na.spline(TempDFb[,14])[,1]

TempDFb[,15] <- interpNA(TempDFb[,15],method="linear")[,1]
TempDFb[,15] <- na.spline(TempDFb[,15])[,1]

names(TempDFb)
TempDFb[,16] <- interpNA(TempDFb[,16],method="linear")[,1]
TempDFb[,16] <- na.spline(TempDFb[,16])[,1]


summary(TempDFb)
dim(TempDFb)


hist(TempDFb$max-TempDFb$min)
qplot(TempDFb$max-TempDFb$min,geom="histogram",fill=I("blue"))+
  xlab("Max-Min, C")+ylab("Count")


TempDF <- TempDFb
# Nice plot showing the mean, max and min temperatures inside the house
dt_ggplot <- data.frame(date = TempDF$date,
                        MaxT = TempDF$max,
                        MinT = TempDF$min,
                        AvgT = TempDF$TAVG_weighed,
                        Tout = TempDF$Tout)

class(dt_ggplot$date)

dt_ggplot$date <-  as.POSIXct(dt_ggplot$date ,tz = "UTC")

# Following code reproduces the FIgure 7 in the paper
# Maximum, average, and minimum indoor temperature and outdoor air temperature for the passive house in 2016.

Temperature_plot  <- ggplot(dt_ggplot,aes(x=date))+
  geom_line(aes(y=MaxT,colour="MaxT"))+
  geom_line(aes(y=MinT,colour="MinT"))+
  geom_line(aes(y=AvgT,colour="AvgT"))+
  geom_line(aes(y=Tout,colour="Tout"))+
  scale_colour_manual("", 
                      breaks = c("MaxT",  "AvgT","MinT","Tout"),
                      values = c(MaxT="red", AvgT="black",MinT= "blue",
                                 Tout="green"))+
  
  scale_x_datetime(date_breaks="1 month",labels = date_format("%d/%b")) +
  #scale_x_datetime(date_breaks="1 month",labels = date_format("%d/%b"))
  xlab("Time")+ylab("Temperature, C")+
  theme(legend.position=c(1,1),legend.justification=c(1,1))
Temperature_plot 

# Uncomment the line below if you want to save the image as a png file.
#ggsave("Temperature_year.png", units="in",width =3.07, height = 2.11,dpi=300,scale=3.5)

#Same plot using base R instead of ggplot2

plot(TempDF$date,TempDF$max,col='red',type='l',ylim=c(-10,35),
     xlab="Time",ylab="Temperature, C")
lines(TempDF$date,TempDF$TAVG_weighed,col='black')
lines(TempDF$date,TempDF$min,col='blue')
lines(TempDF$date,TempDF$Tout,col="green")

names(TempDF)

dim(TempDF)
#52704 x20
TempDF2_na_removed <-  na.omit(TempDF)
dim(TempDF2_na_removed )
#37025 x 20

cor(TempDF2_na_removed$T1,TempDF2_na_removed$Tout)
names(TempDF2_na_removed)



Tem_subset <-TempDF2_na_removed[,-c(9,17,19)] # Removing some features
names(Tem_subset)
Tem_subset2 <- TempDF2_na_removed[,c(10,11,12,13,14,15,16,17,18,19,20)]
names(Tem_subset2)
#[1] "Tout"       "RH_out"     "WS"         "VISI"      
#[5] "Tdew"       "Pressure"   "TOTAL_ELEC" "AVG"
summary(Tem_subset2)
str(Tem_subset2)

library(corrplot)
M <- cor(Tem_subset)
corrplot(M, method="number")

names(energy_data2_c2)
energy_data2_c2b <- energy_data2_c2[c(-12,-13,-25,-34,-43)]

names(Tem_subset2)
#Tem_subset2 <- names(Tem_subset2[c(-8,-10)])
#[1] "Tout"     "RH_out"   "WS"       "VISI"     "Tdew"    
#[6] "Pressure" "AVG" 



library(caret)
set.seed(43251)
train_index <- createDataPartition(Tem_subset2$AVG,p=0.75,list=FALSE)

train_data <- Tem_subset2[train_index,]
dim(train_data)
names(train_data)


test_data <- Tem_subset2[-train_index,]
dim(test_data)
fitControl <- trainControl(method = "repeatedcv", # cv
                           number=10,repeats=3,  # 10  
                           verboseIter = TRUE,returnResamp = "all")

names(train_data[,c(1,2,3,4,5,6,7,8)])
#[1] "Tout"       "RH_out"     "WS"         "VISI"       "Tdew"       "Pressure"  
#[7] "TOTAL_ELEC" "max"        "AVG"        "min" 

names(train_data[,c(1,2,3,4,5,6,7,11)])
#"Tout"         "RH_out"       "WS"           "VISI"         "Tdew"         "Pressure"     "TOTAL_ELEC"   "TAVG_weighed"


lmcvFit <- train(TAVG_weighed~., data=train_data[,c(1,2,3,4,5,6,7,11)],  method="lm",trControl = fitControl,
                 metric='RMSE')

lmcvFit



library(doParallel)


ctrl<-trainControl(method="cv", number=5)
grid_rf<-expand.grid(.mtry=c( 2))


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 3)


names(train_data[,c(1,2,3,4,5,6,7,11)])

set.seed(123)
registerDoParallel(4)
getDoParWorkers()
ptm <- proc.time()

set.seed(seed)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "RMSE"
mtry <- ncol(train_data[,c(1,2,3,4,5,6,7,11)])/3

tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(TAVG_weighed~., 
                    data=train_data[,c(1,2,3,4,5,6,7,11)],
                    method="rf", metric=metric,
                    tuneGrid=tunegrid, trControl=control,
                    importance=TRUE)
print(rf_default)

rf_time <- proc.time() - ptm
rf_time

mtry


names(train_data[,c(1,2,3,4,5,6,7,11)])

#[1] "Tout"         "RH_out"       "WS"           "VISI"         "Tdew"        
#[6] "Pressure"     "TOTAL_ELEC"   "TAVG_weighed"

testY <- test_data[,c(1,2,3,4,5,6,7,11)]$TAVG_weighed



### insert code for sample size here!

### FOR LINEAR MODEL TRAINING # SAMPLE SIZE
dim(train_data[,c(1,2,3,4,5,6,7,11)])
# 27771     8
dim(test_data[,c(1,2,3,4,5,6,7,11)])
# 9254    8

#learnCurve <- data.frame(m = integer(27771 ),
#                         trainRMSE = integer(27771 ),
#                         cvRMSE = integer(27771 ))

learnCurve_lm <- data.frame(m =   integer(27771 ),
                            trainRMSE =  integer(27771 ),
                            cvRMSE =   integer(27771 ),
                            time_ela =  integer(27771 ))
trainControl <- trainControl(method = "none",
                             allowParallel=TRUE)

metric <- "RMSE"


#names(train_data[1:i,c(1,2,3,4,5,6,7,11)])
ptm_lm <- proc.time()
for (i in  seq(500,27700,400)  ) {
  ptm <- proc.time()
  print(i)
  learnCurve_lm$m[i] <- i
  
  # train learning algorithm with size i
  fit.lm <- train(TAVG_weighed~., data=train_data[1:i,c(1,2,3,4,5,6,7,11)], method="lm", metric=metric,
                  trainControl=trainControl)        
  elapsed_time <- proc.time() - ptm
  learnCurve_lm$time_ela[i] <- elapsed_time['elapsed']
  learnCurve_lm$trainRMSE[i] <- fit.lm$results$RMSE
  
  # use trained parameters to predict on test data
  prediction <- predict(fit.lm, newdata =test_data[,c(1,2,3,4,5,6,7,11)])
  rmse <- postResample(prediction, testY)
  learnCurve_lm$cvRMSE[i] <- rmse[1]
  
  
}
learnCurve_time_lm <- proc.time() - ptm_lm
learnCurve_time_lm

(18500-18000) *57.39/(100-30)/60

# sample size and random forest model

mtry <- ncol(train_data[,c(1,2,3,4,5,6,7,11)])/3
mtry
tunegrid <- expand.grid(.mtry=mtry)

learnCurve_rf <- data.frame(m =   integer(27771 ),
                            trainRMSE =  integer(27771 ),
                            cvRMSE =   integer(27771 ),
                            time_ela =  integer(27771 ))

###

ptm1 <- proc.time()
for (i in seq(500,27700,400) ) {
  ptm <- proc.time()
  print(i)
  learnCurve_rf$m[i] <- i
  # train learning algorithm with size i
  fit.rf <- train(TAVG_weighed~., data=train_data[1:i,c(1,2,3,4,5,6,7,11)], 
                  method="rf", metric=metric,tuneGrid=tunegrid,
                  trainControl=trainControl)  
  #print( fit.rf$results$RMSE)
  learnCurve_rf$trainRMSE[i] <- fit.rf$results$RMSE
  
  # use trained parameters to predict on test data
  prediction <- predict(fit.rf, newdata =test_data[,c(1,2,3,4,5,6,7,11)])
  rmse <- postResample(prediction, testY)
  learnCurve_rf$cvRMSE[i] <- rmse[1]
  elapsed_time <- proc.time() - ptm
  learnCurve_rf$time_ela[i] <- elapsed_time['elapsed']
  
  
}
learnCurve_time_rf <- proc.time() - ptm1
learnCurve_time_rf



















# plot learning curves of training set size vs. error measure
# for training set and test set

par(mfrow=c(1,2))
plot(learnCurve_lm$m[learnCurve_lm$trainRMSE>0] ,
     learnCurve_lm$trainRMSE[learnCurve_lm$trainRMSE>0],
     ylim=c(0,7),col="black",pch=16,
     ylab="RMSE",xlab="Sample size")
points(learnCurve_lm$m[learnCurve_lm$trainRMSE>0],
       learnCurve_lm$cvRMSE[learnCurve_lm$trainRMSE>0],
       col="blue",pch=16)
legend("topright",	
       c("LM_test","LM_train"),pch=c(16,16),
       col=c("blue","black"), horiz=FALSE)

plot(learnCurve_rf$m[learnCurve_rf$trainRMSE>0] ,
     learnCurve_rf$trainRMSE[learnCurve_rf$trainRMSE>0],ylim=c(0,7),
     ylab="RMSE",xlab="Sample size",pch=16)
points(learnCurve_rf$m[learnCurve_rf$trainRMSE>0],
       learnCurve_rf$cvRMSE[learnCurve_rf$trainRMSE>0],col="blue",
       pch=16)
legend("topright",	
       c("RF_test","RF_train"),pch=c(16,16),
       col=c("blue","black"), horiz=FALSE)

# Plotting learning curves using ggplot2


data_ggplot_RMSE_lm <- data.frame("Samplesize"=learnCurve_lm$m[learnCurve_lm$trainRMSE>0],
                                  "RMSE_train_LM"= learnCurve_lm$trainRMSE[learnCurve_lm$trainRMSE>0],
                                  "RMSE_test_LM"=learnCurve_lm$cvRMSE[learnCurve_lm$trainRMSE>0])
names(data_ggplot_RMSE_lm)

data_ggplot_RMSE_RF <- data.frame("Samplesize"=learnCurve_rf$m[learnCurve_rf$trainRMSE>0],
                                  "RMSE_train_RF"= learnCurve_rf$trainRMSE[learnCurve_rf$trainRMSE>0],
                                  "RMSE_test_RF"=learnCurve_rf$cvRMSE[learnCurve_rf$trainRMSE>0])
names(data_ggplot_RMSE_RF)






plot1 <- ggplot(data_ggplot_RMSE_lm, aes(Samplesize))+  
  geom_point(aes(y=RMSE_train_LM,colour="Train_LM",shape=c(3)))+
  geom_point(aes(y=RMSE_test_LM,colour="Test_LM",shape=c(1)))+
  scale_shape_identity()+
  scale_colour_manual("", 
                      breaks = c("Train_LM",  "Test_LM"),
                      values = c(Train_LM="black", Test_LM="blue"))+
  xlab("Sample size")+ylab("RMSE")+
  theme(legend.position=c(1,1),legend.justification=c(1,1))+
  guides(colour = guide_legend(override.aes = list(shape=c(3,1))))+
  #scale_x_datetime(date_breaks="1 month",labels = date_format("%d/%m")) +
  #xlab("Time")+ylab("Temperature, C")+
  #theme(legend.position=c(1,1),legend.justification=c(1,1))
  scale_x_continuous(breaks=seq(0,30000,5000))+
  scale_y_continuous(limits = c(0,8))


plot1  

plot2 <- ggplot(data_ggplot_RMSE_RF, aes(Samplesize))+  
  geom_point(aes(y=RMSE_train_RF,colour="Train_RF",shape=c(3)))+
  geom_point(aes(y=RMSE_test_RF,colour="Test_RF",shape=c(1)))+
  scale_shape_identity()+
  scale_colour_manual("", 
                      breaks = c("Train_RF",  "Test_RF"),
                      values = c(Train_RF="black", Test_RF="blue"))+
  xlab("Sample size")+ylab("RMSE")+ 
  theme(legend.position=c(1,1),legend.justification=c(1,1))+
  guides(colour = guide_legend(override.aes = list(shape=c(3,1))))+
  scale_x_continuous(breaks=seq(0,30000,5000))+
  scale_y_continuous(limits = c(0,8))

plot2


library(gridExtra)  
grid.arrange(plot1, plot2, ncol=2)



View(learnCurve_rf)

new_data_learnCurve_rf <- data.frame(ssize = learnCurve_rf$m[learnCurve_rf$trainRMSE>0],
                                     rf_time_elap = learnCurve_rf$time_ela[learnCurve_rf$trainRMSE>0],
                                     lm_time_elap = learnCurve_lm$time_ela[learnCurve_lm$trainRMSE>0])

View(new_data_learnCurve_rf)



model_qua_poly_time_rf <- lm(new_data_learnCurve_rf$rf_time_elap  ~ 
                               new_data_learnCurve_rf$ssize + 
                               I(new_data_learnCurve_rf$ssize^2) )
summary(model_qua_poly_time_rf)

model_lm_time_lm <- lm(new_data_learnCurve_rf$lm_time_elap ~ 
                         new_data_learnCurve_rf$ssize)
summary(model_lm_time_lm)




View(new_data_learnCurve_rf)
# ELAPSED Time plots
par(mfrow=c(1,2))
plot(new_data_learnCurve_lm$ssize,
     new_data_learnCurve_lm$lm_time_elap,col="black",ylab="LM Elapsed time, s",
     xlab="Sample size",pch=16)
points(new_data_learnCurve_rf$ssize,model_lm_time_lm$fitted.values,col="green",pch=16)
legend("topleft",	
       c("Timed","Linear_fit"),pch=c(16,16),
       col=c("black","green"), horiz=FALSE)
text(18500,1.2,expression(6.551*'E-05'~SS+ 0.740169 ),cex=1)
text(18000,1.1,expression( 'R'^2~ '= 0.9905'),cex=1)


plot(new_data_learnCurve_rf$ssize,new_data_learnCurve_rf$rf_time_elap,
     ylab="RF Elapsed time, s",
     xlab="Sample size",pch=16)
points(new_data_learnCurve_rf$ssize,model_qua_poly_time_rf$fitted.values,col="green",pch=16)
legend("topleft",	
       c("Timed","Quadratic_fit"),pch=c(16,16),
       col=c("black","green"), horiz=FALSE)
text(19500,3000,expression(2.665*'E-05'~SS^2+ 0.06195~SS-169 ),cex=1)
text(19000,1500,expression( 'R'^2~ '= 0.9998'),cex=1)
dev.off()
#points(new_data_learnCurve_rf$ssize,
#      new_data_learnCurve_rf$lm_time_elap,col="red")
#lines(new_data_learnCurve_rf$ssize,model_lm_time_lm$fitted.values,col="black")



# plotting the same with ggplot2

LM_time_plot  <- ggplot(data=new_data_learnCurve_rf,aes(x=ssize))+
  geom_point(aes(y=lm_time_elap,colour="Timed",shape=c(16)))+
  geom_point(aes(y=model_lm_time_lm$fitted.values,colour="Linear_Fit",
                 shape=c(17)))+
  scale_shape_identity()+
  xlab("Sample size")+ylab("LM Elapsed time, s")+
  annotate("text",  x = 18500, y = 1.2,label= "6.551E-05*SS+ 0.740169")+
  annotate("text",x=18000,y=1.0,label='Rsquared = 0.9905')+
  scale_colour_manual("",values=c(Timed="black", Linear_Fit="green"))+
  theme(legend.position=c(0,1),legend.justification=c(0,1))+
  guides(colour = guide_legend(override.aes = list(shape=c(17,16))))+
  scale_x_continuous(breaks=seq(0,30000,5000))


lb1 <- paste("2.665*'E'*-05*SS^{2}", "+0.06195*'SS' -169")

RF_time_plot  <-ggplot(data=new_data_learnCurve_rf,aes(x=ssize))+
  geom_point(aes(y=rf_time_elap,colour="Timed",shape=c(16)))+
  geom_point(aes(y=model_qua_poly_time_rf$fitted.values,colour="Quadratic_Fit",shape=c(17)))+
  scale_shape_identity()+
  xlab("Sample size")+ylab("RF Elapsed time, s")+
  annotate("text",  x = 19500, y = 3000,label= lb1,parse=TRUE)+
  annotate("text",x=19000,y=1500,label='Rsquared = 0.9998')+
  scale_colour_manual("",values=c(Timed="black", Quadratic_Fit="green"))+
  theme(legend.position=c(0,1),legend.justification=c(0,1))+
  guides(colour = guide_legend(override.aes = list(shape=c(17,16))))+
  scale_x_continuous(breaks=seq(0,30000,5000))

grid.arrange(LM_time_plot , RF_time_plot, ncol=2)

# code for sample size ends here.


## Training RF model with all the training data now

set.seed(123)
registerDoParallel(4)
getDoParWorkers()
ptm <- proc.time()

set.seed(seed)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "RMSE"
mtry <- ncol(train_data[,c(1,2,3,4,5,6,7,11)])/3

tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(TAVG_weighed~., 
                    data=train_data[,c(1,2,3,4,5,6,7,11)],
                    method="rf", metric=metric,
                    tuneGrid=tunegrid, trControl=control,
                    importance=TRUE)
print(rf_default)

rf_time <- proc.time() - ptm
rf_time

plot(varImp(rf_default))

#

library(Metrics)
rmse(train_data$TAVG_weighed,predict(rf_default ,train_data))
#0.1494419
rmse(test_data$TAVG_weighed,predict(rf_default,test_data))
#0.3500492

MAPE <- function(y, yhat) {
  mean(abs((y - yhat)/y))
}
MAE <- function(y, yhat) {
  mean(abs((y - yhat)))
}
Rsqu <- function(y, yhat) {
  
  mdata <- mean(y)
  
  1 - sum((y-yhat)^2)/(sum((y-mdata)^2))
  
}


rf_default
rf_default$call

# Evaluating the RF model performance in training and testing data sets (Table 3 of paper)

MAPE(train_data$TAVG_weighed,predict(rf_default ,train_data))*100
#0.45

MAPE(test_data$TAVG_weighed,predict(rf_default ,test_data))*100
#1.058

Rsqu(train_data$TAVG_weighed,predict(rf_default ,train_data))
#0.996 
Rsqu(test_data$TAVG_weighed,predict(rf_default ,test_data))
# 0.9781

MAE(train_data$TAVG_weighed,predict(rf_default ,train_data))
#0.096
MAE(test_data$TAVG_weighed,predict(rf_default ,test_data))
#0.2277

###

# errors for lmcvFit with all the samples 
rmse(test_data$TAVG_weighed,predict(lmcvFit ,test_data))
#1.05

rmse(train_data$TAVG_weighed,predict(lmcvFit ,train_data))
#1..0647



max(abs(test_data$TAVG_weighed-predict(rf_default,test_data)))
# 2.547
max(abs(train_data$TAVG_weighed-predict(rf_default,train_data)))
# 1.18935

mean(abs(test_data$TAVG_weighed-predict(rf_default,test_data)))

max(test_data$TAVG_weighed-predict(rf_default,test_data))
#2.029
min(test_data$TAVG_weighed-predict(rf_default,test_data))
#-2.547

#hist(abs(train_data$AVG-predict(gbm_model,train_data)))

#hist(abs(test_data$TAVG_weighed-predict(rf_random,test_data)))

par(mfrow=c(1,2))
hist(train_data$TAVG_weighed-predict(rf_default ,train_data),
     xlab="Train$TAVG-prediction, C",col='lightblue',main="",
     breaks=10)

hist(test_data$TAVG_weighed-predict(rf_default ,test_data),
     xlab="Test$TAVG-prediction, C",col='lightblue',main="",
     breaks=10)
dev.off()

# Here is the code to reproduce Figure 11 of the paper
#Histogram plots of error in training and testing sets (ggplot2)
#

hist1 <-qplot(train_data$TAVG_weighed-predict(rf_default ,train_data),
              geom="histogram",fill=I("blue"))+
  xlab("Error in Training set")+ylab("Count")
hist2 <-qplot(test_data$TAVG_weighed-predict(rf_default ,test_data),
              geom="histogram",fill=I("blue"))+
  xlab("Error in Testing set")+ylab("Count")

grid.arrange(hist1 , hist2, ncol=2)

# Or using base r 
hist(train_data$TAVG_weighed-predict(rf_default ,train_data),
     xlab="Train$TAVG-prediction",col='lightblue',main="")



max(abs(test_data$AVG-predict(rf_model,test_data)))


# visualizing it as a empirical cumulative distribution function
plot(ecdf(test_data$AVG-predict(rf_default,test_data)),
     col='red', main=NA)


fun.ecdf <- ecdf(test_data$AVG-predict(rf_default,test_data))

fun.ecdf

my.ecdf <- fun.ecdf(sort(c(-2,-1,-0.5,0,0.5,1,2,4)))

my.ecdf

1-my.ecdf[6]
1-my.ecdf[5]


plot(TempDF$date,TempDF$AVG,col='red',type='l')
lines(TempDF$date,TempDF$AVG,col='black')

names(TempDF)
whole_data <-as.data.frame(TempDF[,c(10,11,12,13,14,15,16)])
summary(whole_data)
#names(whole_data) <- c("Tout" ,"RH_out","WS",
#"VISI","Tdew","Pressure")
class(whole_data)
str(as.data.frame(whole_data))
dim(whole_data)
#52704    7
dim(TempDF)
#52704   20



View(whole_data)

whole_data2 <-whole_data
whole_data2$date <- TempDF$date
View(whole_data2)
View(TempDF)

## PLOTS THE PREDICTIONS AGAINST THE REAL DATA
plot(TempDF$date,predict(rf_default,whole_data),col='red',type='l')
#lines(TempDF$date,TempDF$AVG,col='black')
lines(TempDF$date,TempDF$TAVG_weighed,col='black')

###



TempDFcopy <- TempDF
names(TempDFcopy)
#TempDFcopy$AVG[is.na(TempDF$AVG)]<-predict(rf_default,whole_data)[is.na(TempDF$AVG)]




sum(is.na(TempDF$TAVG_weighed))
#8182
# now completing the missing "NA" TAVG with predictions
TempDFcopy$TAVG_weighed[is.na(TempDF$TAVG_weighed)]<-predict(rf_default,whole_data)[is.na(TempDF$TAVG_weighed)]


summary(TempDFcopy)

summary(TempDFcopy$TAVG_weighed)

# Plots of missing TAVG profile and the completed profile with the predictions side by side
par(mfrow=c(1,2))
plot( TempDF$date, TempDF$TAVG_weighed,col='black',type='l')

plot(TempDFcopy$date,TempDFcopy$TAVG_weighed,col='black',type='l')


# Now predicting the other missing columns with linear fits and avg temperature

# first get linear model, then predict and fill NAs
names(train_data)

Tem_subset2_for_linear <- TempDF2_na_removed
names(Tem_subset2_for_linear)

train_data_forlinear <- Tem_subset2_for_linear[train_index,]

test_data_forlinear <- Tem_subset2_for_linear[-train_index,]


names(train_data_forlinear)



lmcvFit_T1 <- train(T1~TAVG_weighed, data=train_data_forlinear,  method="lm",trControl = fitControl,
                    metric='RMSE')

lmcvFit_T1
lmcvFit_T1$finalModel

print(lmcvFit_T1$coefnames)

summary(TempDFcopy)

# Linear model performance.. for table 4 of the paper... 

rmse(train_data_forlinear$T1,predict(lmcvFit_T1,train_data_forlinear))
#0.43827
rmse(test_data_forlinear$T1,predict(lmcvFit_T1,test_data_forlinear))
#0.4382

Rsqu(train_data_forlinear$T1,predict(lmcvFit_T1 ,train_data_forlinear))
#0.9557
Rsqu(test_data_forlinear$T1,predict(lmcvFit_T1,test_data_forlinear))
# 0.9557

MAE(train_data_forlinear$T1,predict(lmcvFit_T1  ,train_data_forlinear))
#0.333
MAE(test_data_forlinear$T1,predict(lmcvFit_T1 ,test_data_forlinear))
#0.333


MAPE(train_data_forlinear$T1,predict(lmcvFit_T1 ,train_data_forlinear))*100
#1.491

MAPE(test_data_forlinear$T1,predict(lmcvFit_T1,test_data_forlinear))*100
#1.4944



# Code to produce Figure 12 Linear regression prediction for T1 and training data
# base R plot
plot(train_data_forlinear$T1,predict(lmcvFit_T1 ,train_data_forlinear),
     col='black',pch=1,
     xlab="T1 (train data set), deg. C",ylab="LM model T1 predictions, deg. C")
abline(coef=c(0,1),col="red")


# Same plot in ggplot2
qplot(train_data_forlinear$T1,predict(lmcvFit_T1 ,train_data_forlinear),alpha=I(0.4))+
  geom_abline(intercept=0,slope=1,colour="red")+ xlab("T1 (train data set), deg. C")+
  ylab("LM model T1 predictions, deg. C")+
  
  scale_x_continuous(breaks=seq(16,29,1),limits=c(16,29))+
  scale_y_continuous(breaks=seq(16,29,1),limits=c(16,29))







length(predict(lmcvFit_T1,TempDFcopy))
#52704
length(TempDFcopy$date)
#52704
sum(is.na(TempDF$T1))
#8134

length(predict(lmcvFit_T1,TempDFcopy)[is.na(TempDF$T1)])
#8134

# filling the empty spaces of T1 in the dataframe copy
TempDFcopy$T1[is.na(TempDF$T1)]<-predict(lmcvFit_T1,TempDFcopy)[is.na(TempDF$T1)]

summary(TempDFcopy)

plot(TempDFcopy$date,TempDFcopy$T1, type='l',xlab="Time",ylab="T1, deg.C",lty=1)
lines(TempDF$date,TempDF$T1,col='red',lty=4)

# Plot of the Measured and reconstructed for T1

plot(TempDF$date,TempDF$T1,col='black',xlab="Time",ylab="T1, deg.C",pch=16,cex=0.6)
points(TempDFcopy$date[is.na(TempDF$T1)],TempDFcopy$T1[is.na(TempDF$T1)],
       xlab="Time",ylab="T1, deg.C",col="blue",pch=16,cex=0.6)
legend('topleft', c("Measured", "Prediction"), pch = 16, cex = 0.9,
       col = c("black", "blue"),bty = "n")




sum(is.na(TempDF$T1))


data_ggplot <- data.frame(date = TempDF$date,
                          T1_measured = TempDF$T1,
                          T2_predicted =TempDFcopy$T1[is.na(TempDF$T1)])



data_ggplot <-merge(TempDF, TempDFcopy,
                    by = "date")


data_ggplot <- left_join(TempDF,TempDFcopy,by="date")


View(data_ggplot)

# This code repoduces Figure 13 in the paper
# T1 profile for the whole year completed with predictions for missing data
ggplot(data=data_ggplot,aes(x=date))+
  
  geom_line(linetype = 2,aes(y=T1.y,colour="Predicted")) +
  geom_line(aes(y=T1.x,colour="Measured"))+
  scale_colour_manual("", 
                      breaks = c("Predicted", "Measured"),
                      values = c(Predicted="blue",Measured="black"))+
  
  scale_x_datetime(date_breaks="1 month",labels = date_format("%d/%b")) +
  xlab("Time")+ylab("T1 Temperature, C")+
  theme(legend.position=c(1,1),legend.justification=c(1,1))+
  scale_y_continuous(breaks = seq(10, 30, by = 2))



ggplot(dt_ggplot,aes(x=date))+
  geom_line(aes(y=MaxT,colour="MaxT"))+
  geom_line(aes(y=MinT,colour="MinT"))+
  geom_line(aes(y=AvgT,colour="AvgT"))+
  geom_line(aes(y=Tout,colour="Tout"))+
  scale_colour_manual("", 
                      breaks = c("MaxT",  "AvgT","MinT","Tout"),
                      values = c(MaxT="red", AvgT="black",MinT= "blue",
                                 Tout="green"))+
  
  scale_x_datetime(date_breaks="1 month",labels = date_format("%d/%m")) +
  xlab("Time")+ylab("Temperature, C")+
  theme(legend.position=c(1,1),legend.justification=c(1,1))





TempDFcopy$date[is.na(TempDF$T1)]

dim(TempDF)
#52704 20
dim(TempDFcopy)
#52704 20

View(TempDFcopy)

summary(TempDFcopy)

# doing the same for T2



lmcvFit_T2 <- train(T2~TAVG_weighed, data=train_data_forlinear,  method="lm",trControl = fitControl,
                    metric='RMSE')

lmcvFit_T2
lmcvFit_T2$finalModel

#error metrics for T2

rmse(train_data_forlinear$T2,predict(lmcvFit_T2 ,train_data_forlinear))
#0.870
rmse(test_data_forlinear$T2,predict(lmcvFit_T2,test_data_forlinear))
#0.874

Rsqu(train_data_forlinear$T2,predict(lmcvFit_T2 ,train_data_forlinear))
#0.887
Rsqu(test_data_forlinear$T2,predict(lmcvFit_T2,test_data_forlinear))
# 0.886

MAE(train_data_forlinear$T2,predict(lmcvFit_T2  ,train_data_forlinear))
#0.707
MAE(test_data_forlinear$T2,predict(lmcvFit_T2 ,test_data_forlinear))
#0.707


MAPE(train_data_forlinear$T2,predict(lmcvFit_T2 ,train_data_forlinear))*100
#3.347

MAPE(test_data_forlinear$T2,predict(lmcvFit_T2,test_data_forlinear))*100
#3.334
#







TempDFcopy$T2[is.na(TempDF$T2)]<-predict(lmcvFit_T2,TempDFcopy)[is.na(TempDF$T2)]

plot(TempDFcopy$date,TempDFcopy$T2, type='l')

# doing the same for T3



lmcvFit_T3 <- train(T3~TAVG_weighed, data=train_data_forlinear,  method="lm",trControl = fitControl,
                    metric='RMSE')

lmcvFit_T3
lmcvFit_T3$finalModel


TempDFcopy$T3[is.na(TempDF$T3)]<-predict(lmcvFit_T3,TempDFcopy)[is.na(TempDF$T3)]

summary(TempDFcopy$T3)

plot(TempDFcopy$date,TempDFcopy$T3, type='l')
# Error metrics for T3

rmse(train_data_forlinear$T3,predict(lmcvFit_T3 ,train_data_forlinear))
#1.03
rmse(test_data_forlinear$T3,predict(lmcvFit_T3,test_data_forlinear))
#1.02

Rsqu(train_data_forlinear$T3,predict(lmcvFit_T3 ,train_data_forlinear))
#0.8478
Rsqu(test_data_forlinear$T3,predict(lmcvFit_T3,test_data_forlinear))
# 0.851

MAE(train_data_forlinear$T3,predict(lmcvFit_T3  ,train_data_forlinear))
#0.771
MAE(test_data_forlinear$T3,predict(lmcvFit_T3 ,test_data_forlinear))
#0.7643


MAPE(train_data_forlinear$T3,predict(lmcvFit_T3 ,train_data_forlinear))*100
#3.414

MAPE(test_data_forlinear$T3,predict(lmcvFit_T3,test_data_forlinear))*100
#3.37



# Error metrics for T3 ends here



# doing the same for T4


lmcvFit_T4 <- train(T4~TAVG_weighed, data=train_data_forlinear,  method="lm",trControl = fitControl,
                    metric='RMSE')

lmcvFit_T4
lmcvFit_T4$finalModel


TempDFcopy$T4[is.na(TempDF$T4)]<-predict(lmcvFit_T4,TempDFcopy)[is.na(TempDF$T4)]

plot(TempDFcopy$date,TempDFcopy$T4, type='l')

# error metrics for T4

rmse(train_data_forlinear$T4,predict(lmcvFit_T4 ,train_data_forlinear))
#0.765
rmse(test_data_forlinear$T4,predict(lmcvFit_T4,test_data_forlinear))
#0.762

Rsqu(train_data_forlinear$T4,predict(lmcvFit_T4 ,train_data_forlinear))
# 0.9046

Rsqu(test_data_forlinear$T4,predict(lmcvFit_T4,test_data_forlinear))
# 0.906

MAE(train_data_forlinear$T4,predict(lmcvFit_T4  ,train_data_forlinear))
#0.573
MAE(test_data_forlinear$T4,predict(lmcvFit_T4 ,test_data_forlinear))
#0.574


MAPE(train_data_forlinear$T4,predict(lmcvFit_T4 ,train_data_forlinear))*100
#2.70

MAPE(test_data_forlinear$T3,predict(lmcvFit_T3,test_data_forlinear))*100
#3.373


#error metrics for T4 ends here






# doing the same for T5



lmcvFit_T5 <- train(T5~TAVG_weighed, data=train_data_forlinear,  method="lm",trControl = fitControl,
                    metric='RMSE')



lmcvFit_T5
lmcvFit_T5$finalModel


TempDFcopy$T5[is.na(TempDF$T5)]<-predict(lmcvFit_T5,TempDFcopy)[is.na(TempDF$T5)]

plot(TempDFcopy$date,TempDFcopy$T5, type='l')
# error metrics for T5

rmse(train_data_forlinear$T5,predict(lmcvFit_T5 ,train_data_forlinear))
#0.814
rmse(test_data_forlinear$T5,predict(lmcvFit_T5,test_data_forlinear))
#0.817

Rsqu(train_data_forlinear$T5,predict(lmcvFit_T5 ,train_data_forlinear))
# 0.9068

Rsqu(test_data_forlinear$T5,predict(lmcvFit_T5,test_data_forlinear))
# 0.906

MAE(train_data_forlinear$T5,predict(lmcvFit_T5  ,train_data_forlinear))
#0.643
MAE(test_data_forlinear$T5,predict(lmcvFit_T5 ,test_data_forlinear))
#0.644


MAPE(train_data_forlinear$T5,predict(lmcvFit_T5 ,train_data_forlinear))*100
#3.120

MAPE(test_data_forlinear$T5,predict(lmcvFit_T5,test_data_forlinear))*100
#3.127


# error metrics for T5 ends here






# doing the same for T7

lmcvFit_T7 <- train(T7~TAVG_weighed, data=train_data_forlinear,  method="lm",trControl = fitControl,
                    metric='RMSE')

lmcvFit_T7
lmcvFit_T7$finalModel


TempDFcopy$T7[is.na(TempDF$T7)]<-predict(lmcvFit_T7,TempDFcopy)[is.na(TempDF$T7)]

plot(TempDFcopy$date,TempDFcopy$T7, type='l')

# error metrics T7

rmse(train_data_forlinear$T7,predict(lmcvFit_T7 ,train_data_forlinear))
#0.969
rmse(test_data_forlinear$T7,predict(lmcvFit_T7,test_data_forlinear))
#0.966

Rsqu(train_data_forlinear$T7,predict(lmcvFit_T7 ,train_data_forlinear))
# 0.866

Rsqu(test_data_forlinear$T7,predict(lmcvFit_T7,test_data_forlinear))
# 0.866

MAE(train_data_forlinear$T7,predict(lmcvFit_T7  ,train_data_forlinear))
#0.747
MAE(test_data_forlinear$T7,predict(lmcvFit_T7 ,test_data_forlinear))
#0.744


MAPE(train_data_forlinear$T7,predict(lmcvFit_T7 ,train_data_forlinear))*100
#3.603

MAPE(test_data_forlinear$T7,predict(lmcvFit_T7,test_data_forlinear))*100
#3.589


# error metrics T7 ends here






#
# doing the same for T8

lmcvFit_T8 <- train(T8~TAVG_weighed, data=train_data_forlinear,  method="lm",trControl = fitControl,
                    metric='RMSE')

lmcvFit_T8
lmcvFit_T8$finalModel


TempDFcopy$T8[is.na(TempDF$T8)]<-predict(lmcvFit_T8,TempDFcopy)[is.na(TempDF$T8)]

plot(TempDFcopy$date,TempDFcopy$T8, type='l')

#error metrics for T8

rmse(train_data_forlinear$T8,predict(lmcvFit_T8 ,train_data_forlinear))
#0.995
rmse(test_data_forlinear$T8,predict(lmcvFit_T8,test_data_forlinear))
#0.9997

Rsqu(train_data_forlinear$T8,predict(lmcvFit_T8 ,train_data_forlinear))
# 0.830

Rsqu(test_data_forlinear$T8,predict(lmcvFit_T8,test_data_forlinear))
# 0.8271

MAE(train_data_forlinear$T8,predict(lmcvFit_T8  ,train_data_forlinear))
#0.789
MAE(test_data_forlinear$T8,predict(lmcvFit_T8 ,test_data_forlinear))
#0.789


MAPE(train_data_forlinear$T8,predict(lmcvFit_T8 ,train_data_forlinear))*100
#3.535

MAPE(test_data_forlinear$T8,predict(lmcvFit_T8,test_data_forlinear))*100
#3.537



#error metrics for T8 ENDs here


# doing the same for T9

names(train_data_forlinear)
lmcvFit_T9 <- train(T9~TAVG_weighed, data=train_data_forlinear,  method="lm",trControl = fitControl,
                    metric='RMSE')

lmcvFit_T9
lmcvFit_T9$finalModel


TempDFcopy$T9[is.na(TempDF$T9)]<-predict(lmcvFit_T9,TempDFcopy)[is.na(TempDF$T9)]

plot(TempDFcopy$date,TempDFcopy$T9, type='l')




summary(TempDFcopy)

summary(TempDF)
names(TempDF)

# Error metrics for T9

rmse(train_data_forlinear$T9,predict(lmcvFit_T9 ,train_data_forlinear))
#0.8103
rmse(test_data_forlinear$T9,predict(lmcvFit_T9,test_data_forlinear))
#0.8134

Rsqu(train_data_forlinear$T9,predict(lmcvFit_T9 ,train_data_forlinear))
# 0.914

Rsqu(test_data_forlinear$T9,predict(lmcvFit_T9,test_data_forlinear))
# 0.9134

MAE(train_data_forlinear$T9,predict(lmcvFit_T9  ,train_data_forlinear))
#0.639
MAE(test_data_forlinear$T9,predict(lmcvFit_T9 ,test_data_forlinear))
#0.640


MAPE(train_data_forlinear$T9,predict(lmcvFit_T9 ,train_data_forlinear))*100
#3.167

MAPE(test_data_forlinear$T9,predict(lmcvFit_T9,test_data_forlinear))*100
#3.174

# Ends metrics for T9


# now generating the ecd curves, before and after


ggdata3 <- data.frame(BathRoom=TempDF$T5,
                      Kitchen= TempDF$T1,
                      Parents=TempDF$T9,
                      Office=TempDF$T4,
                      Laundry=TempDF$T3,
                      Boys=TempDF$T8,
                      LivingRoom=TempDF$T2,
                      IroningRoom=TempDF$T7 )
#View(ggdata3)
ggdata3 <- na.omit(ggdata3)
dim(ggdata3)
#37025     8

# now with completed set with fittings and Random forest 

ggdata3b <- data.frame(BathRoom=TempDFcopy$T5,
                       Kitchen= TempDFcopy$T1,
                       Parents=TempDFcopy$T9,
                       Office=TempDFcopy$T4,
                       Laundry=TempDFcopy$T3,
                       Boys=TempDFcopy$T8,
                       LivingRoom=TempDFcopy$T2,
                       IroningRoom=TempDFcopy$T7 )
#View(ggdata3)

dim(ggdata3b)
#52704x8


#Summary of descriptive statistics (Table 5)

# incomplete data set
summary(ggdata3)

#completed dataset
summary(ggdata3b)




#  Box plot comparison below


library(reshape2)
ggdata3melted <- melt(ggdata3)
dim(ggdata3melted)
#View(ggdata3melted)



names(ggdata3melted) <- c("Room","Temperature")
ggdata3melted$Dataset <- c("INCMPLT")


ggdata3bmelted <- melt(ggdata3b)
dim(ggdata3bmelted)
#View(ggdata3bmelted)
names(ggdata3bmelted) <- c("Room","Temperature")

ggdata3bmelted$Dataset <- c("CMPLT")

# making one single dataset for ease of work in ggplot2

ggdata3_complet_incomplete   <- rbind(ggdata3melted,ggdata3bmelted)
names(ggdata3_complet_incomplete)


#  This code here generates the Figure 14
qplot(Dataset, Temperature, data = ggdata3_complet_incomplete,fill=Room)+
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot() + facet_grid(~Room) +
  scale_fill_brewer()+
  scale_y_continuous(breaks=seq(14,35,1))+
  theme(legend.position="none")






