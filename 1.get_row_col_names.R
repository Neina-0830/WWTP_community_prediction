#Read library
library(miscTools)
setwd("E:/Desktop/WWTP-机器学习/All_ANN/result/4-fold/ASV/results/raw/all_select/sigmoid-output/")
##Select samples
##Alpha results
Mic <- read.csv("../../../../rawdata/Microbiom-ASV-alpha_select-Summary.csv",head=T,row.names=1)
Mic <- t(Mic)
Mic_list <- rownames(Mic)
CV <- Mic[,3]
SD <- Mic[,2]
Mean <- Mic[,1]

data <- read.csv("Wd0.01_drop0-alpha_select-sigmoid.csv", header = F) 
Header <- c('Col','Seed','Wd','Drop','Train_MSE','Train_R2','Valid_MSE','Valid_R2')
#Rename colnames
names(data) <- Header
#Added rownames
Rows1 <- character()
for(i in 1:length(Mic_list)){
  Rows1 <- append(Rows1, rep(Mic_list[[i]], times=20))
}
data1 <- as.data.frame(insertCol(as.matrix(data), 1, Rows1, cName = 'Micro'))
Rows2 <- numeric()
for(i in 1:length(CV)){
  Rows2 <- append(Rows2, rep(CV[[i]], times=20))
}
data2 <- as.data.frame(insertCol(as.matrix(data1), 2, Rows2, cName = 'CV'))
Rows3 <- numeric()
for(i in 1:length(SD)){
  Rows3 <- append(Rows3, rep(SD[[i]], times=20))
}
data3 <- as.data.frame(insertCol(as.matrix(data2), 3, Rows3, cName = 'SD'))
Rows4 <- numeric()
for(i in 1:length(Mean)){
  Rows4 <- append(Rows4, rep(Mean[[i]], times=20))
}
data4 <- as.data.frame(insertCol(as.matrix(data3), 3, Rows4, cName = 'Mean'))
write.csv(data4, 'Train_Valid-alpha_select-sigmoid.csv', quote = FALSE, row.names = FALSE)

data1 <- read.csv("Test_and_parameters-alpha_select-sigmoid.csv", header =F)
Header <- c('Col','Seed','Wd','Drop','test_MSE','test_R2','AMMaxT','AMMinT','AMT','AP','AtHRT','AtInfBOD','AtInfCOD',
            'AtInfNH4N','AtInfTN','AtInfTP','BY','Con','DC','Denitri','DO','EffBOD','EffCOD',
            'EffNH4N','EffTN','EffTP','F.M','GDP','HRT','IndConInf','IndPer','InfBOD','InfCOD',
            'InfNH4N','InfR','InfTN','InfTP','Lat','Lon','MIT','MLSS','MMT','NH4N','Nitri','NO3N',
            'Ph','Pop','ReInfBOD','ReInfCOD','SMP','SMT','SRT','SVI','Vol','bias')
#Rename colnames
names(data1) <- Header
#Added rownames
Rows <- character()
for(i in 1:length(Mic_list)){
  Rows <- append(Rows, rep(Mic_list[[i]], times=20))
}
data1 <- as.data.frame(insertCol(as.matrix(data1), 1, Rows, cName = 'Micro'))
write.csv(data1, 'Test_and_parameters-alpha_select-sigmoid.csv', quote = FALSE, row.names = FALSE)

data1 <- read.table("Test_log-alpha_select-sigmoid.txt", header = F, sep = "\t") 
Header <- c('Col','Seed','Wd','Drop','AMMaxT','AMMinT','AMT','AP','AtHRT','AtInfBOD','AtInfCOD',
            'AtInfNH4N','AtInfTN','AtInfTP','BY','Con','DC','Denitri','DO','EffBOD','EffCOD',
            'EffNH4N','EffTN','EffTP','F.M','GDP','HRT','IndConInf','IndPer','InfBOD','InfCOD',
            'InfNH4N','InfR','InfTN','InfTP','Lat','Lon','MIT','MLSS','MMT','NH4N','Nitri','NO3N',
            'Ph','Pop','ReInfBOD','ReInfCOD','SMP','SMT','SRT','SVI','Vol','Test_observed','Test_predicted')
#Rename colnames
names(data1) <- Header
#Added rownames
Rows <- character()
#3100=155*20
for(i in 1:length(Mic_list)){
  Rows <- append(Rows, rep(Mic_list[[i]], times=3100))
}
data1 <- as.data.frame(insertCol(as.matrix(data1), 1, Rows, cName = 'Micro'))
write.table(data1, 'Test_log-alpha_select-sigmoid.txt', sep ="\t", quote=FALSE, row.names = FALSE, col.names=TRUE)

#Get Microbiom list
Mic <- read.csv("../../../../rawdata/Microbiom-ASV-10_select-Summary.csv",head=T,row.names=1)
Mic <- t(Mic)
Mic_list <- rownames(Mic)
CV <- Mic[,3]
SD <- Mic[,2]
Mean <- Mic[,1]

data <- read.csv("Wd0.01_drop0-10_select-sigmoid.csv", header = F) 
Header <- c('Col','Seed','Wd','Drop','Train_MSE','Train_R2','Valid_MSE','Valid_R2')
#Rename colnames
names(data) <- Header
#Added rownames
Rows1 <- character()
for(i in 1:length(Mic_list)){
  Rows1 <- append(Rows1, rep(Mic_list[[i]], times=20))
}
data1 <- as.data.frame(insertCol(as.matrix(data), 1, Rows1, cName = 'Micro'))
Rows2 <- numeric()
for(i in 1:length(CV)){
  Rows2 <- append(Rows2, rep(CV[[i]], times=20))
}
data2 <- as.data.frame(insertCol(as.matrix(data1), 2, Rows2, cName = 'CV'))
Rows3 <- numeric()
for(i in 1:length(SD)){
  Rows3 <- append(Rows3, rep(SD[[i]], times=20))
}
data3 <- as.data.frame(insertCol(as.matrix(data2), 3, Rows3, cName = 'SD'))
Rows4 <- numeric()
for(i in 1:length(Mean)){
  Rows4 <- append(Rows4, rep(Mean[[i]], times=20))
}
data4 <- as.data.frame(insertCol(as.matrix(data3), 3, Rows4, cName = 'Mean'))
write.csv(data4, 'Train_Valid-10_select-sigmoid.csv', quote = FALSE, row.names = FALSE)

data1 <- read.csv("Test_and_parameters-10_select-sigmoid.csv", header = F) 
Header <- c('Col','Seed','Wd','Drop','test_MSE','test_R2','AMMaxT','AMMinT','AMT','AP','AtHRT','AtInfBOD','AtInfCOD',
            'AtInfNH4N','AtInfTN','AtInfTP','BY','Con','DC','Denitri','DO','EffBOD','EffCOD',
            'EffNH4N','EffTN','EffTP','F.M','GDP','HRT','IndConInf','IndPer','InfBOD','InfCOD',
            'InfNH4N','InfR','InfTN','InfTP','Lat','Lon','MIT','MLSS','MMT','NH4N','Nitri','NO3N',
            'Ph','Pop','ReInfBOD','ReInfCOD','SMP','SMT','SRT','SVI','Vol','bias')
#Rename colnames
names(data1) <- Header
#Added rownames
Rows <- character()
for(i in 1:length(Mic_list)){
  Rows <- append(Rows, rep(Mic_list[[i]], times=20))
}
data1 <- as.data.frame(insertCol(as.matrix(data1), 1, Rows, cName = 'Micro'))
write.csv(data1, 'Test_and_parameters-10_select-sigmoid.csv', quote = FALSE, row.names = FALSE)

data1 <- read.table("Test_log-10_select-sigmoid.txt", header=FALSE,sep="\t") 
Header <- c('Col','Seed','Wd','Drop','AMMaxT','AMMinT','AMT','AP','AtHRT','AtInfBOD','AtInfCOD',
            'AtInfNH4N','AtInfTN','AtInfTP','BY','Con','DC','Denitri','DO','EffBOD','EffCOD',
            'EffNH4N','EffTN','EffTP','F.M','GDP','HRT','IndConInf','IndPer','InfBOD','InfCOD',
            'InfNH4N','InfR','InfTN','InfTP','Lat','Lon','MIT','MLSS','MMT','NH4N','Nitri','NO3N',
            'Ph','Pop','ReInfBOD','ReInfCOD','SMP','SMT','SRT','SVI','Vol','Test_observed','Test_predicted')
#Rename colnames
names(data1) <- Header
#Added rownames
Rows <- character()
for(i in 1:length(Mic_list)){
  Rows <- append(Rows, rep(Mic_list[[i]], times=3100))
}
data1 <- as.data.frame(insertCol(as.matrix(data1), 1, Rows, cName = 'Micro'))
write.table(data1, 'Test_log-10_select-sigmoid.txt', sep ="\t", quote=FALSE, row.names = FALSE, col.names=TRUE)


##Functioner
Mic <- read.csv("E:/Desktop/数据库数据下载/SRA数据库/WWTP/WWTP_16S/qiime分析/ASV-new/OTU_analysis/ASV_select-MiDAS-functioner-Summary.csv",header=T,row.names=1)
Mic <- t(Mic)
Mic_list <- rownames(Mic)
CV <- Mic[,3]
SD <- Mic[,2]
Mean <- Mic[,1]

data <- read.csv("Wd0.01_drop0-functioner-sigmoid.csv", header = F) 
Header <- c('Col','Seed','Wd','Drop','Train_MSE','Train_R2','Valid_MSE','Valid_R2')
#Rename colnames
names(data) <- Header
#Added rownames
Rows1 <- character()
for(i in 1:length(Mic_list)){
  Rows1 <- append(Rows1, rep(Mic_list[[i]], times=20))
}
data1 <- as.data.frame(insertCol(as.matrix(data), 1, Rows1, cName = 'Micro'))
Rows2 <- numeric()
for(i in 1:length(CV)){
  Rows2 <- append(Rows2, rep(CV[[i]], times=20))
}
data2 <- as.data.frame(insertCol(as.matrix(data1), 2, Rows2, cName = 'CV'))
Rows3 <- numeric()
for(i in 1:length(SD)){
  Rows3 <- append(Rows3, rep(SD[[i]], times=20))
}
data3 <- as.data.frame(insertCol(as.matrix(data2), 3, Rows3, cName = 'SD'))
Rows4 <- numeric()
for(i in 1:length(Mean)){
  Rows4 <- append(Rows4, rep(Mean[[i]], times=20))
}
data4 <- as.data.frame(insertCol(as.matrix(data3), 3, Rows4, cName = 'Mean'))
write.csv(data4, 'Train_Valid-functioner-sigmoid.csv', quote = FALSE, row.names = FALSE)

data1 <- read.csv("Test_and_parameters-functioner-sigmoid.csv", header =F)
Header <- c('Col','Seed','Wd','Drop','test_MSE','test_R2','AMMaxT','AMMinT','AMT','AP','AtHRT','AtInfBOD','AtInfCOD',
            'AtInfNH4N','AtInfTN','AtInfTP','BY','Con','DC','Denitri','DO','EffBOD','EffCOD',
            'EffNH4N','EffTN','EffTP','F.M','GDP','HRT','IndConInf','IndPer','InfBOD','InfCOD',
            'InfNH4N','InfR','InfTN','InfTP','Lat','Lon','MIT','MLSS','MMT','NH4N','Nitri','NO3N',
            'Ph','Pop','ReInfBOD','ReInfCOD','SMP','SMT','SRT','SVI','Vol','bias')
#Rename colnames
names(data1) <- Header
#Added rownames
Rows <- character()
for(i in 1:length(Mic_list)){
  Rows <- append(Rows, rep(Mic_list[[i]], times=20))
}
data1 <- as.data.frame(insertCol(as.matrix(data1), 1, Rows, cName = 'Micro'))
write.csv(data1, 'Test_and_parameters-functioner-sigmoid.csv', quote = FALSE, row.names = FALSE)

data1 <- read.table("Test_log-functioner-sigmoid.txt", header = F, sep = "\t") 
Header <- c('Col','Seed','Wd','Drop','AMMaxT','AMMinT','AMT','AP','AtHRT','AtInfBOD','AtInfCOD',
            'AtInfNH4N','AtInfTN','AtInfTP','BY','Con','DC','Denitri','DO','EffBOD','EffCOD',
            'EffNH4N','EffTN','EffTP','F.M','GDP','HRT','IndConInf','IndPer','InfBOD','InfCOD',
            'InfNH4N','InfR','InfTN','InfTP','Lat','Lon','MIT','MLSS','MMT','NH4N','Nitri','NO3N',
            'Ph','Pop','ReInfBOD','ReInfCOD','SMP','SMT','SRT','SVI','Vol','Test_observed','Test_predicted')
#Rename colnames
names(data1) <- Header
#Added rownames
Rows <- character()
#3100=155*20
for(i in 1:length(Mic_list)){
  Rows <- append(Rows, rep(Mic_list[[i]], times=3100))
}
data1 <- as.data.frame(insertCol(as.matrix(data1), 1, Rows, cName = 'Micro'))
write.table(data1, 'Test_log-functioner-sigmoid.txt', sep ="\t", quote=FALSE, row.names = FALSE, col.names=TRUE)
