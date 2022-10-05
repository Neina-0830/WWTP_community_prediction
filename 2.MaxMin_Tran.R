library(miscTools)
setwd("E:/Desktop/WWTP-机器学习/All_ANN/result/4-fold/ASV/results/raw/all_select/sigmoid-output/")

##Alpha data
data <- read.table("Test_log-alpha_select-sigmoid.txt",header=TRUE,sep="\t")
Samples <- numeric()
##4*20=80
Samples <- append(Samples, rep(seq(1:155), times=80))
data <- as.data.frame(insertCol(as.matrix(data), 2, Samples, cName = 'Sample'))
data_info <- data[,c(1:2,4:6)]
test_data <- data[,55:56]
Summary <- read.csv("../../../../rawdata/Microbiom-ASV-alpha_select-Summary.csv",header=TRUE,row.names = 1)
Max_list <- Summary[4,]
Min_list <- Summary[5,]
Maxs <- numeric()
Mins <- numeric()
#3100=155*20
for(i in 1:length(Max_list)){
  Maxs <- append(Maxs, rep(Max_list[[i]], times=3100))
  Mins <- append(Mins, rep(Min_list[[i]], times=3100))
}
data <- as.data.frame(insertCol(as.matrix(data), 1, Maxs, cName = 'Max'))
data <- as.data.frame(insertCol(as.matrix(data), 1, Mins, cName = 'Min'))
##Calculate rawdata
Diff <- as.matrix(as.numeric(data[,2])-as.numeric(data[,1])+1e-12)
Min <- as.matrix(as.numeric(data[,1])-1e-12)
test_data <- apply(as.matrix(test_data),2,as.numeric)
Test_raw <- apply(test_data, 2, function(x) x*Diff[,1] + Min[,1])
Test <- cbind(data_info, Test_raw)
error <- Test$Test_predicted - Test$Test_observed
Test <- as.data.frame(insertCol(as.matrix(Test), 8, error, cName = 'prediction_error'))
write.table(Test, "Test_observed_predicted-alpha_select-sigmoid-raw.txt",sep ="\t", quote=FALSE, row.names = FALSE, col.names=TRUE)


##ASV>10%
data <- read.table("Test_log-10_select-sigmoid.txt",header=TRUE,sep="\t")
Samples <- numeric()
##1493*20=29860
Samples <- append(Samples, rep(seq(1:155), times=29860))
data <- as.data.frame(insertCol(as.matrix(data), 2, Samples, cName = 'Sample'))
data_info <- data[,c(1:2,4:6)]
test_data <- data[,55:56]
MaxMin <- read.csv("../../../../rawdata/Microbiom-ASV-10_select-Summary.csv",header=TRUE,row.names = 1)
Max_list <- MaxMin[4,]
Min_list <- MaxMin[5,]
Maxs <- numeric()
Mins <- numeric()
#3100=155*20
for(i in 1:length(Max_list)){
  Maxs <- append(Maxs, rep(Max_list[[i]], times=3100))
  Mins <- append(Mins, rep(Min_list[[i]], times=3100))
}
data <- as.data.frame(insertCol(as.matrix(data), 1, Maxs, cName = 'Max'))
data <- as.data.frame(insertCol(as.matrix(data), 1, Mins, cName = 'Min'))
##Calculate rawdata
Diff <- as.matrix(as.numeric(data[,2])-as.numeric(data[,1])+1e-12)
Min <- as.matrix(as.numeric(data[,1])-1e-12)
test_data <- apply(as.matrix(test_data),2,as.numeric)
Test_raw <- apply(test_data, 2, function(x) x*Diff[,1] + Min[,1])
Test_raw[Test_raw<1e-12] <- 0
Test <- cbind(data_info, Test_raw)
error <- Test$Test_predicted - Test$Test_observed
Test <- as.data.frame(insertCol(as.matrix(Test), 20, error, cName = 'prediction_error'))
write.table(Test, "Test_observed_predicted-10_select-sigmoid-raw.txt", sep ="\t", quote=FALSE, row.names = FALSE, col.names=TRUE)


##Functioner
data <- read.table("Test_log-functioner-sigmoid.txt",header=TRUE,sep="\t")
Samples <- numeric()
##5*20=100
Samples <- append(Samples, rep(seq(1:155), times=100))
data <- as.data.frame(insertCol(as.matrix(data), 2, Samples, cName = 'Sample'))
data_info <- data[,c(1:2,4:6)]
test_data <- data[,55:56]
Summary <- read.csv("E:/Desktop/数据库数据下载/SRA数据库/WWTP/WWTP_16S/qiime分析/ASV-new/OTU_analysis/ASV_select-MiDAS-functioner-Summary.csv",header=TRUE,row.names = 1)
Max_list <- Summary[4,]
Min_list <- Summary[5,]
Maxs <- numeric()
Mins <- numeric()
#3100=155*20
for(i in 1:length(Max_list)){
  Maxs <- append(Maxs, rep(Max_list[[i]], times=3100))
  Mins <- append(Mins, rep(Min_list[[i]], times=3100))
}
data <- as.data.frame(insertCol(as.matrix(data), 1, Maxs, cName = 'Max'))
data <- as.data.frame(insertCol(as.matrix(data), 1, Mins, cName = 'Min'))
##Calculate rawdata
Diff <- as.matrix(as.numeric(data[,2])-as.numeric(data[,1])+1e-12)
Min <- as.matrix(as.numeric(data[,1])-1e-12)
test_data <- apply(as.matrix(test_data),2,as.numeric)
Test_raw <- apply(test_data, 2, function(x) x*Diff[,1] + Min[,1])
Test_raw[Test_raw<0] <- 0
Test <- cbind(data_info, Test_raw)
error <- Test$Test_predicted - Test$Test_observed
Test <- as.data.frame(insertCol(as.matrix(Test), 8, error, cName = 'prediction_error'))
write.table(Test, "Test_observed_predicted-functioner-sigmoid-raw.txt",sep ="\t", quote=FALSE, row.names = FALSE, col.names=TRUE)
