setwd("E:/Desktop/WWTP-机器学习/All_ANN/result/4-fold/ASV/results/raw/all_select/sigmoid-output/")
# Library
library(ggplot2)
library(hrbrthemes)

data_summary <-  function(data=NULL, varname, groupnames=NULL, na.rm=FALSE,conf.interval=.95, .drop=TRUE) {
  library(plyr)
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  #group_by + summarise
  datac <- ddply(data, groupnames, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 varname
  )
  datac <- plyr::rename(datac, c("mean" = varname))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}

rsq_1.1 <- function(observed, predicted){
  if(sum(is.na(observed )) > 0){stop('Error: NAs in observed vector.' )}
  if(sum(is.na(predicted)) > 0){stop('Error: NAs in predicted vector.')}
  #calculate residual sums of squares relative to actual prediction (i.e. "1:1 line")
  SS.res <- sum((observed - predicted)^2)
  #calculate total sum of squares.
  SS.tot <- sum((observed - mean(observed))^2)
  #calculate rsq and return.
  rsq.1 <- 1 - SS.res/SS.tot
  return(rsq.1)
}
##alpha多样性
data <- read.table("Test_observed_predicted-alpha_select-sigmoid-raw.txt",header=TRUE,sep="\t")
Summary <- read.csv("../../../../rawdata/Microbiom-ASV-alpha_select-new-Summary.csv",header=TRUE,row.names = 1)
Mic_list <- colnames(Summary)
R2 <- numeric()
R2_1.1 <- numeric()
for(i in 1:length(Mic_list)){
  data1 <- data[which(data$Micro == Mic_list[[i]]),]
  for(j in 0:19){
    data2 <- data1[which(data1$Seed == j),]
    model.lm <- lm(Test_predicted ~ Test_observed, data = data2)
    R2 <- append(R2, summary(model.lm)$r.squared)
    R2_1.1 <- append(R2_1.1, rsq_1.1(data2$Test_observed,data2$Test_predicted))
  }
}
R2[R2 < 0] <- 0
R2_1.1[R2_1.1 < 0] <- 0
Micro <- character()
Seed <- character()
for(i in 1:length(Mic_list)){
  Micro <- append(Micro, rep(Mic_list[[i]], times=20))
  Seed <- append(Seed, seq(from=0,to=19,by=1))
}
R2_summary <- as.data.frame(cbind(Micro, Seed, R2, R2_1.1))
names(R2_summary) <- c("Micro","Seed","R2","R2_1.1")
write.csv(R2_summary, 'Test-alpha_select-sigmoid-raw-R2-R2_1.1.csv', quote = FALSE)

R2_summary$R2 <- as.numeric(R2_summary$R2)
R2_summary$R2_1.1 <- as.numeric(R2_summary$R2_1.1)
R2_1.1_data <- data_summary(R2_summary, varname = "R2_1.1", groupnames = "Micro")
##plot R2~alpha
R2_1.1_data$Micro <- factor(R2_1.1_data$Micro, levels = c("shannon", "evenness", "richness", "PD"))

par(mar=c(3,5,2,2))
p<- ggplot(R2_1.1_data, aes(x=Micro, y=R2_1.1)) + 
  geom_point(size=2) +
  geom_errorbar(aes(ymin=R2_1.1-ci, ymax=R2_1.1+ci), width = 0) +
  ylim(0,1)
p + labs(x="Alpha diversity", y = "Test R2_1:1") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Predictability of different alpha diversity_select-sigmoid.pdf", width = 4, height = 3)

##分组画图
Test_all_shannon <- data[which(data$Micro=="shannon"),]
Test_all_evenness <- data[which(data$Micro=="evenness"),]
Test_all_richness <- data[which(data$Micro=="richness"),]
Test_all_PD <- data[which(data$Micro=="PD"),]

par(mar=c(3,5,2,2))
p1 <- ggplot(Test_all_shannon, aes(x = Test_observed, y = Test_predicted)) +
  geom_point(size=1) +
  geom_smooth(method="lm" , formula = y~x, color="#009E73", fill="#56B4E9", se=TRUE,linetype = "dashed",size=1) +
  geom_abline(slope = 1, intercept = 0, color="black", size=1)
model.lm <- lm(Test_predicted ~ Test_observed, data = Test_all_shannon)
l <- list(r2 = format(summary(model.lm)$r.squared, digits = 4),
          r2_1.1 = format(rsq_1.1(Test_all_shannon$Test_observed,Test_all_shannon$Test_predicted), digits = 4))
eq1 <- substitute(italic(R)^2~"="~r2, l)
eq2 <- substitute(italic(R1.1)^2~"="~r2_1.1, l)
p1 + geom_text(aes(x = 3.5, y = 7.2, label = as.character(as.expression(eq1))), parse = TRUE, size = 3) +
  geom_text(aes(x = 3.5, y = 6.8, label = as.character(as.expression(eq2))), parse = TRUE, size = 3) +
  labs(x="Shannon-Wiener index", y = "Predicted value") + 
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Test_shannon-observed-predicted_select-sigmoid.pdf", width = 4, height = 3)

par(mar=c(3,5,2,2))
p1 <- ggplot(Test_all_evenness, aes(x = Test_observed, y = Test_predicted)) +
  geom_point(size=1) +
  geom_smooth(method="lm" , formula = y~x, color="#009E73", fill="#56B4E9", se=TRUE,linetype = "dashed",size=1) +
  geom_abline(slope = 1, intercept = 0, color="black", size=1)
model.lm <- lm(Test_predicted ~ Test_observed, data = Test_all_evenness)
l <- list(r2 = format(summary(model.lm)$r.squared, digits = 4),
          r2_1.1 = format(rsq_1.1(Test_all_evenness$Test_observed,Test_all_evenness$Test_predicted), digits = 4))
eq1 <- substitute(italic(R)^2~"="~r2, l)
eq2 <- substitute(italic(R1.1)^2~"="~r2_1.1, l)
p1 + geom_text(aes(x = 0.58, y = 1.00, label = as.character(as.expression(eq1))), parse = TRUE, size = 3) +
  geom_text(aes(x = 0.58, y = 0.95, label = as.character(as.expression(eq2))), parse = TRUE, size = 3) +
  labs(x="Pielou's evenness index", y = "Predicted value") + 
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Test_evenness-observed-predicted_select-sigmoid.pdf", width = 4, height = 3)

par(mar=c(3,5,2,2))
p1 <- ggplot(Test_all_richness, aes(x = Test_observed, y = Test_predicted)) +
  geom_point(size=1) +
  geom_smooth(method="lm" , formula = y~x, color="#009E73", fill="#56B4E9", se=TRUE,linetype = "dashed",size=1) +
  geom_abline(slope = 1, intercept = 0, color="black", size=1)
model.lm <- lm(Test_predicted ~ Test_observed, data = Test_all_richness)
l <- list(r2 = format(summary(model.lm)$r.squared, digits = 4),
          r2_1.1 = format(rsq_1.1(Test_all_richness$Test_observed,Test_all_richness$Test_predicted), digits = 4))
eq1 <- substitute(italic(R)^2~"="~r2, l)
eq2 <- substitute(italic(R1.1)^2~"="~r2_1.1, l)
p1 + geom_text(aes(x = 500, y = 2200, label = as.character(as.expression(eq1))), parse = TRUE, size = 3) +
  geom_text(aes(x = 500, y = 2100, label = as.character(as.expression(eq2))), parse = TRUE, size = 3) +
  labs(x="Species richness", y = "Predicted value") + 
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Test_richness-observed-predicted_select-sigmoid.pdf", width = 4, height = 3)

par(mar=c(3,5,2,2))
p1 <- ggplot(Test_all_PD, aes(x = Test_observed, y = Test_predicted)) +
  geom_point(size=1) +
  geom_smooth(method="lm" , formula = y~x, color="#009E73", fill="#56B4E9", se=TRUE,linetype = "dashed",size=1) +
  geom_abline(slope = 1, intercept = 0, color="black", size=1)
model.lm <- lm(Test_predicted ~ Test_observed, data = Test_all_PD)
l <- list(r2 = format(summary(model.lm)$r.squared, digits = 4),
          r2_1.1 = format(rsq_1.1(Test_all_PD$Test_observed,Test_all_PD$Test_predicted), digits = 4))
eq1 <- substitute(italic(R)^2~"="~r2, l)
eq2 <- substitute(italic(R1.1)^2~"="~r2_1.1, l)
p1 + geom_text(aes(x = 45, y = 160, label = as.character(as.expression(eq1))), parse = TRUE, size = 3) +
  geom_text(aes(x = 45, y = 150, label = as.character(as.expression(eq2))), parse = TRUE, size = 3) +
  labs(x="Phylogenetic diversity", y = "Predicted value") + 
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Test_pd-observed-predicted_select-sigmoid.pdf", width = 4, height = 3)


##ASV>10%
data <- read.table("Test_observed_predicted-10_select-sigmoid-raw.txt",header=TRUE,sep="\t")
Summary <- read.csv("../../../../rawdata/Microbiom-ASV-10_select-Summary.csv",header=TRUE,row.names = 1)
Mic_list <- colnames(Summary)
R2 <- numeric()
R2_1.1 <- numeric()
for(i in 1:length(Mic_list)){
  data1 <- data[which(data$Micro == Mic_list[[i]]),]
  for(j in 0:19){
    data2 <- data1[which(data1$Seed == j),]
    model.lm <- lm(Test_predicted ~ Test_observed, data = data2)
    R2 <- append(R2, summary(model.lm)$r.squared)
    R2_1.1 <- append(R2_1.1, rsq_1.1(data2$Test_observed,data2$Test_predicted))
  }
}
R2[R2 < 0] <- 0
R2_1.1[R2_1.1 < 0] <- 0
Micro <- character()
Seed <- character()
for(i in 1:length(Mic_list)){
  Micro <- append(Micro, rep(Mic_list[[i]], times=20))
  Seed <- append(Seed, seq(from=0,to=19,by=1))
}
R2_summary <- as.data.frame(cbind(Micro, Seed, R2, R2_1.1))
names(R2_summary) <- c("Micro","Seed","R2","R2_1.1")
write.csv(R2_summary, 'Test-10freq_select-sigmoid-raw-R2-R2_1.1.csv', quote = FALSE)


Test_observed <- data_summary(data, varname = "Test_observed", groupnames = "Micro")
Test_predicted <- data_summary(data, varname = "Test_predicted", groupnames ="Micro")
Test_summary <- cbind(Test_observed[,c(1:3)], Test_predicted[,3:6])
write.table(Test_summary, "Test_observed_predicted-10freq_select-sigmoid-raw-summary.txt",sep ="\t", quote=FALSE, row.names = FALSE, col.names=TRUE)

par(mar=c(3,5,2,2))
p1 <- ggplot(Test_summary, aes(x = Test_observed, y = Test_predicted)) +
  geom_point(size=1) +
  geom_errorbar(aes(ymin=Test_predicted-ci, ymax=Test_predicted+ci)) +
  geom_smooth(method="lm" , formula = y~x, color="#009E73", fill="#56B4E9", se=TRUE,linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color="black")
model.lm <- lm(Test_predicted ~ Test_observed, data = Test_summary)
l <- list(r2 = format(summary(model.lm)$r.squared, digits = 4),
          r2_1.1 = format(rsq_1.1(Test_summary$Test_observed,Test_summary$Test_predicted), digits = 4))
eq1 <- substitute(italic(R)^2~"="~r2, l)
eq2 <- substitute(italic(R1.1)^2~"="~r2_1.1, l)
p1 + geom_text(aes(x = 0.002, y = 0.015, label = as.character(as.expression(eq1))), parse = TRUE, size = 3) +
  geom_text(aes(x = 0.002, y = 0.014, label = as.character(as.expression(eq2))), parse = TRUE, size = 3) +
  labs(x="Observed relative abundance", y = "Predicted relative abundance")  + 
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Test_10freq-observed-predicted_select-sigmoid.pdf", width = 4, height = 3)


##Functioner
data <- read.table("Test_observed_predicted-functioner-sigmoid-raw.txt",header=TRUE,sep="\t")
Summary <- read.csv("E:/Desktop/数据库数据下载/SRA数据库/WWTP/WWTP_16S/qiime分析/ASV-new/OTU_analysis/ASV_select-MiDAS-functioner-Summary.csv",header=TRUE,row.names = 1)
Mic_list <- colnames(Summary)
R2 <- numeric()
R2_1.1 <- numeric()
for(i in 1:length(Mic_list)){
  data1 <- data[which(data$Micro == Mic_list[[i]]),]
  for(j in 0:19){
    data2 <- data1[which(data1$Seed == j),]
    model.lm <- lm(Test_predicted ~ Test_observed, data = data2)
    R2 <- append(R2, summary(model.lm)$r.squared)
    R2_1.1 <- append(R2_1.1, rsq_1.1(data2$Test_observed,data2$Test_predicted))
  }
}
R2[R2 < 0] <- 0
R2_1.1[R2_1.1 < 0] <- 0
Micro <- character()
Seed <- character()
for(i in 1:length(Mic_list)){
  Micro <- append(Micro, rep(Mic_list[[i]], times=20))
  Seed <- append(Seed, seq(from=0,to=19,by=1))
}
R2_summary <- as.data.frame(cbind(Micro, Seed, R2, R2_1.1))
names(R2_summary) <- c("Micro","Seed","R2","R2_1.1")
write.csv(R2_summary, 'Test-functioner-sigmoid-raw-R2-R2_1.1.csv', quote = FALSE)

R2_summary <- read.csv("Test-functioner-sigmoid-raw-R2-R2_1.1.csv",header=TRUE,row.names = 1)
R2_summary$R2 <- as.numeric(R2_summary$R2)
R2_summary$R2_1.1 <- as.numeric(R2_summary$R2_1.1)
R2_1.1_data <- data_summary(R2_summary, varname = "R2_1.1", groupnames = "Micro")
R2_1.1_data$Micro <- factor(R2_1.1_data$Micro, levels = c("Nitrifiers", "Dentrifiers", "PAOs", "GAOs", "Filamentous"))

par(mar=c(3,5,2,2))
p<- ggplot(R2_1.1_data, aes(x=Micro, y=R2_1.1)) + 
  geom_point(size=2) +
  geom_errorbar(aes(ymin=R2_1.1-ci, ymax=R2_1.1+ci), width = 0) +
  ylim(0,1)
p + labs(x="Functional groups", y = "Test R2_1:1") +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Predictability of different functioner_select-sigmoid.pdf", width = 4, height = 3.5)
