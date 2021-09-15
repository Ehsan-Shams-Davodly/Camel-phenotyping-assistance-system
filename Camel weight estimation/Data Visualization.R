#===============================================================================
rm(list = ls())
gc()

library(tcltk)
MainDir <- tk_choose.dir(default = "", caption = "Select main directory")

#===============================================================================
Data <- xlsx::read.xlsx(paste(MainDir, "/Data.xlsx", sep = ""),sheetIndex = 1,
                        header = T)
Data <- Data[,-c(11,12)]
NOfSamples <- dim(Data)[1]
NOfFeatures <- dim(Data)[2]-1

#===============================================================================
SumNaNF <- data.frame(colnames(Data),
                      apply(Data, 2, function(x) sum(is.na(x))))
colnames(SumNaNF) <- c("FName","NOfNaN")
Weight <- Data[,1]

#===============================================================================
library(ggplot2)
P1 <- ggplot(Data, aes(x = BW)) +
    geom_histogram(aes(y = ..count..), bins = 50,
                   color = "black", fill = "steelblue", size = 0.7) +
    scale_x_continuous(breaks = seq(20,300,50), name = "Body weight (kg)") +
    scale_y_continuous(name = "Number") +
    # ggtitle("Frequency histogram of weights") +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16))

png(paste(MainDir,"/Result/Histogram of Weights.png",sep = ""),width = 2000, height = 1000,res = 200)
print(P1)
dev.off()

#===============================================================================
P2 <- ggplot(SumNaNF, aes(x = FName, y =NOfNaN)) +
    geom_bar(stat="identity") +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 14),
          axis.text.x = element_text(angle = -90))+
    xlab("Features") + ylab("Number of NaN values") +
    scale_x_discrete(limits = c("BW","HL","ML","NEL","BG","WH","HH","WPL","BL","TL","PW","AW","ABH"))

png(paste(MainDir,"/Result/Number Of NAN.png",sep = ""),width = 2000, height = 1000,res = 200)
print(P2)
dev.off()

#===============================================================================
library(forcats)
t <- Data[,-1]
t <- reshape2::melt(t)
P3 <- ggplot(t, aes(x = variable,  y = value)) + 
    geom_jitter(position=position_jitter(0.3), alpha = 0.25) +
    geom_violin(aes(fill = variable), alpha = 0.4, size = 0.5, colour = "black") + 
    theme_bw(base_size = 14) +
    xlab("Independent variable") +
    ylab("Length (cm)") + theme(legend.position = "none",
                                axis.text.x = element_text(angle = -90))

png(paste(MainDir,"/Result/Boxplot.png", sep = ""),width = 2000, height = 1000,res = 250)
print(P3)
dev.off()

#===============================================================================
library(corrplot)
t <- cor(Data, use =  "complete.obs")

png(paste(MainDir,"/Result/corrplot.png",sep = ""),width = 1800, height = 1800,res = 250)
corrplot(t, method = "color", type = "lower", tl.col = "#009999", 
         addCoef.col = "gray25", diag = F)
dev.off()
