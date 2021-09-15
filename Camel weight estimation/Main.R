#===============================================================================
rm(list = ls())
gc()

#===============================================================================
# load packages
library(DMwR)
library(OutlierDetection)
library(caret)
library(tcltk)
library(kernlab)

#===============================================================================
# Choice directory
MainDir <- tk_choose.dir(default = "", caption = "Select main directory")
source(paste(MainDir,"/Evaluation.R",sep = ""))

#===============================================================================
# Read data
Data <- xlsx::read.xlsx(paste(MainDir, "/Data.xlsx", sep = ""), 1, header = T)
Data <- Data[,-c(11,12)]
NOfSamples <- dim(Data)[1]
NOfFeatures <- dim(Data)[2]-1

#===============================================================================
# Impute null value
Data <- knnImputation(data = Data, k = 5, scale = T)

#===============================================================================
# Remove outlier
Outlier <-numeric(0)
K = 10
for (i in 1:K) {
    temp <- nnk(Data, k=i, cutoff = 0.95)
    Outlier <- c(Outlier, temp$`Location of Outlier`)
}
Outlier <- table(Outlier)
Outlier <- as.numeric(names(Outlier))[which(Outlier > (K * 0.75))]

Data <- Data[-Outlier,]

#===============================================================================
set.seed(1)
fitControl <- trainControl(method = "repeatedcv",   
                           number = 10, 
                           repeats = 10,
                           summaryFunction = NewSummary,
                           timingSamps = 0)

#===============================================================================
# Linear Regression with caret PKG
LM.Model <- train(BW ~ .,
                  data = Data,
                  method = "lm",  # now we're using the lasso method
                  trControl = fitControl,
                  metric = "R2")

#===============================================================================
# linear support vector machine
L.SVM.Model <- train(BW ~., 
                     data = Data, 
                     method = "svmLinear",
                     trControl = fitControl,
                     tuneGrid = expand.grid(C = seq(0.05,1,0.05)),
                     verbose = TRUE,
                     metric = "R2")

#===============================================================================
# non linear svm with radial basis kernel
grid <- expand.grid(sigma = c(0.001, 0.01, 0.02, 0.05, 0.1, 0.25, 0.5, 1, 10, 25),
                    C     = seq(0.05,1,0.05))
NL.R.SVM.Model <- train(BW ~., 
                   data = Data, 
                   method = "svmRadial",
                   trControl = fitControl,
                   tuneGrid = grid,
                   metric = "R2")

#===============================================================================
# non linear svm with polynomial kernel
grid <- expand.grid(degree = 1:5,
                    scale = c(0.01, 0.05, 0.1, 0.2, 0.5),
                    C = seq(0.05,1,0.05))
NL.P.SVM.Model <- train(BW ~., 
                   data = Data, 
                   method = "svmPoly",
                   trControl = fitControl,
                   tuneGrid = grid,
                   metric = "R2")

#===============================================================================
# random forest
tunegrid <- expand.grid(.mtry = 1:10)
RF.Model <- train(BW~., 
                 data = Data, 
                 method = 'rf', 
                 tuneGrid = tunegrid, 
                 trControl = fitControl,
                 ntree = 250, importance = T,
                 metric = "R2")

#===============================================================================
# Extreme learning machine
tunegrid <- expand.grid(actfun = c("radbas", "sin", "purelin", "tansig"),
                        nhid =2^(0:8))
ELM.Model <- train(BW ~ ., 
                 data = Data, 
                 method = 'elm',
                 tuneGrid = tunegrid,
                 trControl = fitControl,
                 metric = "R2")

#===============================================================================
# Neural network
tunegrid <- expand.grid(
    neurons =2^(0:5))
    # size = 2^(seq(0,5,1)),
                         # decay = 10 ^ seq(-6,-1,1))
BRNN.Model <- train(BW ~ . - BW, 
                  data = Data, 
                  method = 'brnn',
                  tuneGrid = tunegrid,
                  trControl = fitControl,
                  metric = "R2")

#===============================================================================
# Compare models and plot
models_compare <- resamples(
    list(LM     = LM.Model, 
         RF     = RF.Model,
         ELM    = ELM.Model, 
         RNLSVM = NL.R.SVM.Model,
         LSVM   = L.SVM.Model,
         PNLSVM = NL.P.SVM.Model,
         BRNN   = BRNN.Model))
summary(models_compare)

png(paste(MainDir,"/Result/Results.png",sep = ""),width = 2000,height = 1000,res = 210)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales, fill = "steelblue",
       cex = 1.5, pch = 20, col = "black", box.width = 0.5, ylab = 1:6)
dev.off()

#===============================================================================
# Training me complexity + visualization 
models_time <- c(LM.Model$times$final[3], 
                 RF.Model$times$final[3], 
                 ELM.Model$times$final[3], 
                 NL.R.SVM.Model$times$final[3], 
                 L.SVM.Model$times$final[3],
                 NL.P.SVM.Model$times$final[3],
                 BRNN.Model$times$final[3])
df <- data.frame(y = models_time,
                     x = c("Linear","Random Forest","ELM","Radial-SVM",
                           "Linear-SVM","Poly-SVM", "BRNN"))

png(paste(MainDir,"/Result/TrainTC.png",sep = ""),width = 800,height = 500,res = 150)
ggplot(data=df, aes(x=x, y=y)) +
    geom_bar(stat="identity", fill="steelblue",color="black",width=0.75)+
    theme(axis.text.x = element_text(angle = 90), text = element_text(size = 14))+
    xlab("Regression Methods") + ylab("Time (Sec)") +
    ggtitle("Training phase time complexity") +
    theme(axis.text.x = element_text(angle = 0))+ coord_flip()
dev.off()

#===============================================================================
# Testing time complexity + visualization 
source(paste(MainDir, "/TC.R", sep = ""))
models_time <- matrix(ncol = 7, nrow = 0)
for (i in 1:100) {
    models_time <- rbind(models_time, c(TC(LM.Model, Data[1:100,-1]), 
                     TC(RF.Model, Data[1:100,-1]), 
                     TC(ELM.Model, Data[1:100,-1]), 
                     TC(NL.R.SVM.Model, Data[1:100,-1]), 
                     TC(L.SVM.Model, Data[1:100,-1]),
                     TC(NL.P.SVM.Model, Data[1:100,-1]),
                     TC(BRNN.Model, Data[1:100,-1])) * 1000)
}
models_time <- colMeans(models_time)

df <- data.frame(y = models_time,
                 x = c("Linear","Random Forest","ELM","Radial-SVM",
                       "Linear-SVM","Poly-SVM", "BRNN"))

png(paste(MainDir,"/Result/TestTC.png",sep = ""),width = 800,height = 500,res = 150)
ggplot(data=df, aes(x=x, y=y)) +
    geom_bar(stat="identity", fill="steelblue",color="black",width=0.75)+
    theme(axis.text.x = element_text(angle = 90), text = element_text(size = 14))+
    xlab("Regression Methods") + ylab("Time (ms)")+ coord_flip() +
    ggtitle("Prediction time complexity") +
    theme(axis.text.x = element_text(angle = 0))
dev.off()
#===============================================================================
# Variable importance via random forest
temp <- data.frame(Actual = Data$BW,
                   Predicted = predict(NL.P.SVM.Model, Data))
g1 <- ggplot(temp, aes(x = Actual, y = Predicted)) + 
    geom_point(alpha = 0.75, shape = 21, colour = "black", 
               fill = "steelblue", size = 2, stroke = 1)+ 
    xlim(20,250) + ylim(20,250) +
    geom_abline(intercept = 0, slope = 1, color = "black", 
                size = 1, alpha = 0.75)+ 
    theme(text = element_text(size = 14))+ 
    # ggtitle("Feature importance") +
    theme(plot.title = element_text(hjust = 0.5))

temp <- varImp(NL.P.SVM.Model)
temp <- as.data.frame(temp$importance)
temp <- cbind(temp, as.character(rownames(temp)))
colnames(temp) <- c("value","feature")
temp <- temp[order(temp$value, decreasing = T),]

g2 <- ggplot(data=temp, aes(x=reorder(feature,-value), y=value)) +
    geom_bar(stat="identity", fill="steelblue",color="black",width=0.75)+
    theme(axis.text.x = element_text(angle = 0), text = element_text(size = 14))+
    ylab("Importance feature") + xlab("Feature name")
    # ggtitle("Feature importance") +
    # theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
png(paste(MainDir,"/Result/BestModel.png",sep = ""),width = 1800,height = 900,res = 200)
grid.arrange(g1, g2, nrow = 1)
dev.off()

#==========================================================================
temp <- data.frame(Actual = Data$BW,
                   LM = predict(L.SVM.Model, Data),
                   RF = predict(RF.Model, Data),
                   ELM = predict(ELM.Model, Data),
                   RNLSVM = predict(NL.R.SVM.Model, Data),
                   LSVM = predict(L.SVM.Model,Data),
                   BRNN = predict(BRNN.Model,Data),
                   PNLSVM = predict(NL.P.SVM.Model, Data))
xlsx::write.xlsx(temp,paste(MainDir,"/Result/Result.xlsx",sep = ""), row.names = F, col.names = T)



