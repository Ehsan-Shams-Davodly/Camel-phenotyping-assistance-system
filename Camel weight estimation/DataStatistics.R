#===============================================================================
rm(list = ls())
gc()

library(tcltk)
MainDir <- tk_choose.dir(default = "", caption = "Select main directory")

#===============================================================================
Data <- xlsx::read.xlsx(paste(MainDir, "/Data.xlsx", sep = ""), 1, header = T)
Data <- Data[,-c(11,12)]
NOfSamples <- dim(Data)[1]
NOfFeatures <- dim(Data)[2]-1

#===============================================================================
Mean <- apply(Data, 2, mean, na.rm = T)
SD <- apply(Data, 2, sd, na.rm = T)
CV <- apply(Data, 2, function(x) sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE))
SE <- apply(Data, 2, function(x) sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])))

Res <- data.frame(Mean = Mean, SE = SE, CV = CV, SD = SD)
xlsx::write.xlsx(Res, paste(MainDir,"/Result/SummaryData.xlsx", sep = ""), col.names = T)
