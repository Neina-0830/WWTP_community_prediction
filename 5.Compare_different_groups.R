setwd("E:/Desktop/WWTP-机器学习/All_ANN/result/4-fold/ASV/results/raw/all_select/sigmoid-output/")
library(ggplot2)
library(dplyr)
library(reshape2) 
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

##全部R21:1信息
R2_all <- read.csv("Test-10freq_select-sigmoid-R2_1.1-MiDAS-taxo-niche-summary.csv",header=TRUE)

##Above and below groups
##全部ASVs抽平2000后拟合划分的Above and below groups
Above <- read.csv("E:/Desktop/WWTP-机器学习/中性模型分析/result/ASV level/select/ASV_select-rarefied-2000-NCM-above.csv", head = T, row.names = 1)
Above_all <- R2_all[R2_all$Micro %in% rownames(Above),]
Above_all$group <- "Above"
Above_phylum <- as.data.frame(table(Above_all$Phylum))
colnames(Above_phylum) <- c("Phylum","Above")
Above_phylum <- arrange(Above_phylum, desc(`Above`)) #按数量降序排列
Above_phylum$Phylum<-factor(Above_phylum$Phylum,levels = unique(Above_phylum$Phylum),ordered = T)
Neutral <- read.csv("E:/Desktop/WWTP-机器学习/中性模型分析/result/ASV level/select/ASV_select-rarefied-2000-NCM-neutral.csv", head = T, row.names = 1)
Neutral_all <- R2_all[R2_all$Micro %in% rownames(Neutral),]
Neutral_all$group <- "Neutral"
Neutral_phylum <- as.data.frame(table(Neutral_all$Phylum))
colnames(Neutral_phylum) <- c("Phylum","Neutral")
Neutral_phylum <- arrange(Neutral_phylum, desc(`Neutral`)) #按数量降序排列
Neutral_phylum$Phylum<-factor(Neutral_phylum$Phylum,levels = unique(Neutral_phylum$Phylum),ordered = T)
Below <- read.csv("E:/Desktop/WWTP-机器学习/中性模型分析/result/ASV level/select/ASV_select-rarefied-2000-NCM-below.csv", head = T, row.names = 1)
Below_all <- R2_all[R2_all$Micro %in% rownames(Below),]
Below_all$group <- "Below"
Below_phylum <- as.data.frame(table(Below_all$Phylum))
colnames(Below_phylum) <- c("Phylum","Below")
Below_phylum <- arrange(Below_phylum, desc(`Below`)) #按数量降序排列
Below_phylum$Phylum<-factor(Below_phylum$Phylum,levels = unique(Below_phylum$Phylum),ordered = T)

group_all_phylum <- merge(Above_phylum, Neutral_phylum, by="Phylum", all = TRUE)
group_all_phylum <- merge(group_all_phylum, Below_phylum, by="Phylum", all = TRUE)
group_all_phylum [is.na(group_all_phylum)] <- 0
rownames(group_all_phylum) <- group_all_phylum$Phylum
group_all_phylum <- group_all_phylum[,c(2:4)]
group_all_phylum$Sum <- rowSums(group_all_phylum)
group_all_phylum$Phylum <- rownames(group_all_phylum)
group_all_phylum <- arrange(group_all_phylum, desc(Sum)) #按数量降序排列
group_all_phylum$Phylum<-factor(group_all_phylum$Phylum,levels = unique(group_all_phylum$Phylum),ordered = T)
group_all_phylum$`Above` <- group_all_phylum$`Above`/group_all_phylum$Sum
group_all_phylum$`Neutral` <- group_all_phylum$`Neutral`/group_all_phylum$Sum
group_all_phylum$`Below` <- group_all_phylum$`Below`/group_all_phylum$Sum
group_all_phylum <- group_all_phylum[,c(1:3,5)]
write.csv(group_all_phylum, "Rarefied-2000-Above-Below-phylum-proportion.csv", quote=FALSE)

group_all_phylum_long <- melt(group_all_phylum, id = 'Phylum')
group_all_phylum_long$Phylum<-factor(group_all_phylum_long$Phylum,levels = unique(group_all_phylum_long$Phylum),ordered = T)

par(mar=c(3,5,2,2))
ggplot(group_all_phylum_long, aes(Phylum, value*100,fill= variable)) +
  geom_bar(stat = "identity")+
  labs(y = 'Proportion(%)') +
  ylim(0,101) +
  scale_fill_manual(values = c("#29A6A6","#000000","#A52A2A"),limits = c('Above','Neutral','Below'))+
  theme(axis.text = element_text(size = 10,face = "plain",color ='black'), axis.title = element_text(size = 10,color ='black')) +
  theme(legend.text = element_text(size = 6,color ='black'))+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  geom_col(position = 'stack', width = 0.6)+
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +
  theme(legend.title = element_blank())
dev.off()
ggsave("Rarefied-2000-Proportion-Above-Below.pdf",height=6,width=8)

##R2_1.1 plot
group_all <- rbind(Above_all, Neutral_all, Below_all)
group_all_R2 <- data_summary(group_all, varname = "R2_1.1", groupnames = "group")
write.csv (group_all_R2, file ="Rarefied-2000-Above-Below-select-sigmoid-R2_1.1-summary.csv", quote = FALSE)

par(mar=c(3,5,2,2))
p<- ggplot(group_all_R2, aes(x=group, y=R2_1.1)) + 
  geom_point(size = 2) +
  #geom_bar(stat="identity", color="#000000", position=position_dodge()) +
  geom_errorbar(aes(ymin=R2_1.1-ci, ymax=R2_1.1+ci), width = 0, size = 0.5) +
  ylim(0,0.6)
p + labs(y = "Test R2_1:1") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Rarefied-2000-Above_Below-sigmoid-R2_1.1-all.pdf", width = 4, height = 3)
t.test(group_all[group_all$group=="Above",]$R2_1.1, group_all[group_all$group=="Neutral",]$R2_1.1) ##p-value = 4.967e-08(0.3334049/0.4466043)
t.test(group_all[group_all$group=="Neutral",]$R2_1.1, group_all[group_all$group=="Below",]$R2_1.1) ##p-value = 0.6104（0.4466043/0.4596026）

##frequency
group_freq <- data_summary(group_all, varname = "frequency", groupnames = "group")

par(mar=c(3,5,2,2))
p<- ggplot(group_freq, aes(x=group, y=frequency)) + 
  geom_point(size = 2) +
  #geom_bar(stat="identity", color="#000000", position=position_dodge()) +
  geom_errorbar(aes(ymin=frequency-ci, ymax=frequency+ci), width = 0, size = 0.5) +
  ylim(0,0.4)
p + labs(y = "Occurrence frequency") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Rarefied-2000-Above_below-frequency-all.pdf", width = 4, height = 3)

t.test(group_all[group_all$group=="Above",]$frequency, group_all[group_all$group=="Neutral",]$frequency) ##p-value = 0.8128
t.test(group_all[group_all$group=="Neutral",]$frequency, group_all[group_all$group=="Below",]$frequency) ##p-value = 0.00118

##Mean
group_mean <- data_summary(group_all, varname = "Mean", groupnames = "group")

par(mar=c(3,5,2,2))
p<- ggplot(group_mean, aes(x=group, y=Mean)) + 
  geom_point(size = 2) +
  #geom_bar(stat="identity", color="#000000", position=position_dodge()) +
  geom_errorbar(aes(ymin=Mean-ci, ymax=Mean+ci), width = 0, size = 0.5) +
  ylim(0,0.0025)
p + labs(y = "Average relative abundance") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Rarefied-2000-Above_below-MRA.pdf", width = 4, height = 3)

t.test(group_all[group_all$group=="Above",]$Mean, group_all[group_all$group=="Neutral",]$Mean) ##p-value = 1.551e-10
t.test(group_all[group_all$group=="Neutral",]$Mean, group_all[group_all$group=="Below",]$Mean) ##p-value = 5.369e-13

##CV
group_CV <- data_summary(group_all, varname = "CV", groupnames = "group")

par(mar=c(3,5,2,2))
p<- ggplot(group_CV, aes(x=group, y=CV)) + 
  geom_point(size = 2) +
  #geom_bar(stat="identity", color="#000000", position=position_dodge()) +
  geom_errorbar(aes(ymin=CV-ci, ymax=CV+ci), width = 0, size = 0.5) +
  ylim(0,5)
p + labs(y = "Coefficient of variation in relative abundance") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Rarefied-2000-Above_below-CV.pdf", width = 4, height = 3)

t.test(group_all[group_all$group=="Above",]$CV, group_all[group_all$group=="Neutral",]$CV) ##p-value = 7.157e-06
t.test(group_all[group_all$group=="Neutral",]$CV, group_all[group_all$group=="Below",]$CV) ##p-value = 0.4776

##niche_width_levins
group_levins <- data_summary(group_all, varname = "niche_width_levins", groupnames = "group")

par(mar=c(3,5,2,2))
p<- ggplot(group_levins, aes(x=group, y=niche_width_levins)) + 
  geom_point(size = 2) +
  #geom_bar(stat="identity", color="#000000", position=position_dodge()) +
  geom_errorbar(aes(ymin=niche_width_levins-ci, ymax=niche_width_levins+ci), width = 0, size = 0.5) +
  ylim(0,100)
p + labs(y = "niche_width_levins") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Rarefied-2000-Above_below-niche_width_levins.pdf", width = 4, height = 3)

t.test(group_all[group_all$group=="Above",]$niche_width_levins, group_all[group_all$group=="Neutral",]$niche_width_levins) ##p-value = 5.926e-08
t.test(group_all[group_all$group=="Neutral",]$niche_width_levins, group_all[group_all$group=="Below",]$niche_width_levins) ##p-value = 0.1485

##Core & Non-core
Core <- read.csv("E:/Desktop/数据库数据下载/SRA数据库/WWTP/WWTP_16S/qiime分析/ASV-new/OTU_analysis/ASV_select_10_core_abundance.csv", header = T, row.names = 1)
Core <- as.data.frame(t(Core))
Core_all <- R2_all[R2_all$Micro %in% rownames(Core),]
Core_all$group <- "Core"
Core_phylum <- as.data.frame(table(Core_all$Phylum))
colnames(Core_phylum) <- c("Phylum","Core")
Core_phylum <- arrange(Core_phylum, desc(Core)) #按数量降序排列
Core_phylum$Phylum<-factor(Core_phylum$Phylum,levels = unique(Core_phylum$Phylum),ordered = T)


Non-core <- read.csv("E:/Desktop/数据库数据下载/SRA数据库/WWTP/WWTP_16S/qiime分析/ASV-new/OTU_analysis/ASV_select_10_Non-core_abundance.csv", head = T, row.names = 1)
Non-core <- as.data.frame(t(Non-core))
Non-core_all <- R2_all[R2_all$Micro %in% rownames(Non-core),]
Non-core_all$group <- "Non-core"
Non-core_phylum <- as.data.frame(table(Non-core_all$Phylum))
colnames(Non-core_phylum) <- c("Phylum","Non-core")
Non-core_phylum <- arrange(Non-core_phylum, desc(Non-core)) #按数量降序排列
Non-core_phylum$Phylum<-factor(Non-core_phylum$Phylum,levels = unique(Non-core_phylum$Phylum),ordered = T)

Core_Non-core_phylum <- merge(Core_phylum, Non-core_phylum, by="Phylum", all = TRUE)
Core_Non-core_phylum[is.na(Core_Non-core_phylum)] <- 0
Core_Non-core_phylum_long <- melt(Core_Non-core_phylum, id = 'Phylum')
Core_Non-core_phylum_long$Phylum<-factor(Core_Non-core_phylum_long$Phylum,levels = unique(Core_Non-core_phylum_long$Phylum),ordered = T)
write.csv(Core_Non-core_phylum,"ASV_select-10freq-phylum-Core-Non-core.csv", row.names = FALSE, quote=FALSE)

par(mar=c(3,5,2,2))
ggplot(Core_Non-core_phylum_long, aes(variable, value,fill= Phylum)) +
  geom_bar(stat = "identity")+
  labs(y = 'Number of ASVs') +
  scale_fill_manual(values = c('#008B8B','#4682B4','#B0C4DE','#778899','#808080','#483D8B','#6495ED','#556B2F','#228B22','#20B2AA','#3CB371','#8FBC8F','#9ACD32','#808000','#BDB76B',
                               '#B8860B','#FFD700','#EEE8AA','#BC8F8F','#CD5C5C','#A0522D','#FFA07A','#DEB887','#4B0082','#8A2BE2','#CD853F','#9370DB','#6A5ACD','#8B008B','#BA55D3',
                               '#DDA0DD','#D87093','#800000','#FF1493','#FF69B4','#0066ff'),
                    limits = c('Proteobacteria','Bacteroidota','Planctomycetota','Myxococcota','Firmicutes','Actinobacteriota','Chloroflexi','Acidobacteriota','Verrucomicrobiota',
                               'Unknown','Bdellovibrionota','Desulfobacterota','Patescibacteria','Armatimonadota','Campylobacterota','Cyanobacteria','Dependentiae','Latescibacterota',
                               'Spirochaetota','Synergistota','Elusimicrobiota','Euryarchaeota','Fibrobacterota','Fusobacteriota','Hydrogenedentes','midas_p_13758','midas_p_28883',
                               'SAR324_cladeMarine_group_B','WPS-2','Gemmatimonadota','Nitrospirota','midas_p_49974','Sumerlaeota','Calditrichota','Halobacterota','Margulisbacteria'))+
  theme(axis.text = element_text(size = 10,face = "plain",color ='black'), axis.title = element_text(size = 10,color ='black')) +
  theme(legend.text = element_text(size = 6,color ='black'))+
  theme(axis.text.x = element_text(hjust=1, vjust=1)) +
  geom_col(position = 'stack', width = 0.6)+
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +
  theme(legend.title = element_blank())
dev.off()
ggsave("Numbers_ASVs-Core-Non-core.pdf",height=6,width=8)

##R2_1.1 plot
Core_Non-core_all <- rbind(Core_all, Non-core_all)
write.csv (Core_Non-core_all, file ="Core-Non-core-select-sigmoid-R2_1.1-all.csv", quote = FALSE)
Core_Non-core_all_R2 <- data_summary(Core_Non-core_all, varname = "R2_1.1", groupnames = "group")
write.csv (Core_Non-core_all_R2, file ="Core-Non-core-select-sigmoid-R2_1.1-summary.csv", quote = FALSE)

par(mar=c(3,5,2,2))
p<- ggplot(Core_Non-core_all_R2, aes(x=group, y=R2_1.1)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin=R2_1.1-ci, ymax=R2_1.1+ci), width = 0, size = 0.5) +
  ylim(0,0.6)
p + labs(y = "Test R2_1:1") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Core_Non-core-sigmoid-R2_1.1-all.pdf", width = 4, height = 3)
t.test(Core_Non-core_all[Core_Non-core_all$group=="Core",]$R2_1.1, Core_Non-core_all[Core_Non-core_all$group=="Non-core",]$R2_1.1) ##p-value < 2.2e-16(0.4299219/0.3318756 )

##frequency
Core_Non-core_all_freq <- data_summary(Core_Non-core_all, varname = "frequency", groupnames = "group")

par(mar=c(3,5,2,2))
p<- ggplot(Core_Non-core_all_freq, aes(x=group, y=frequency)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin=frequency-ci, ymax=frequency+ci), width = 0, size = 0.5) +
  ylim(0,0.6)
p + labs(y = "Occurrence frequency") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Core_Non-core-frequency.pdf", width = 4, height = 3)

##MRA
Core_Non-core_all_abun <- data_summary(Core_Non-core_all, varname = "Mean", groupnames = "group")

par(mar=c(3,5,2,2))
p<- ggplot(Core_Non-core_all_abun, aes(x=group, y=Mean)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin=Mean-ci, ymax=Mean+ci), width = 0, size = 0.5) +
  ylim(0,0.002)
p + labs(y = "Average relative abundance") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Core_Non-core-MRA.pdf", width = 4, height = 3)

##CV
Core_Non-core_all_cv <- data_summary(Core_Non-core_all, varname = "CV", groupnames = "group")

par(mar=c(3,5,2,2))
p<- ggplot(Core_Non-core_all_cv, aes(x=group, y=CV)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin=CV-ci, ymax=CV+ci), width = 0, size = 0.5) +
  ylim(0,5)
p + labs(y = "Coefficient of variation") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Core_Non-core-CV.pdf", width = 4, height = 3)

##niche_width_levins
Core_Non-core_all_levins <- data_summary(Core_Non-core_all, varname = "niche_width_levins", groupnames = "group")

par(mar=c(3,5,2,2))
p<- ggplot(Core_Non-core_all_levins, aes(x=group, y=niche_width_levins)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin=niche_width_levins-ci, ymax=niche_width_levins+ci), width = 0, size = 0.5) +
  ylim(0,150)
p + labs(y = "niche width_levins") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Core_Non-core-niche_width_levins.pdf", width = 4, height = 3)

t.test(Core_Non-core_all[Core_Non-core_all$group=="Core",]$Mean, Core_Non-core_all[Core_Non-core_all$group=="Non-core",]$Mean) ##p-value < 2.2e-16(0.0014145843 0.0001990679)
t.test(Core_Non-core_all[Core_Non-core_all$group=="Core",]$frequency, Core_Non-core_all[Core_Non-core_all$group=="Non-core",]$frequency) ##p-value < 2.2e-16(0.4081347 0.1780758 )
t.test(Core_Non-core_all[Core_Non-core_all$group=="Core",]$CV, Core_Non-core_all[Core_Non-core_all$group=="Non-core",]$CV) ##p-value < 2.2e-16(2.571626  3.634164)
t.test(Core_Non-core_all[Core_Non-core_all$group=="Core",]$niche_width_levins, Core_Non-core_all[Core_Non-core_all$group=="Non-core",]$niche_width_levins) ##p-value < 2.2e-16(126.5054   68.5148)
t.test(Core_Non-core_all[Core_Non-core_all$group=="Core",]$niche_width_shannon, Core_Non-core_all[Core_Non-core_all$group=="Non-core",]$niche_width_shannon) ##p-value < 2.2e-16(5.131123  4.423400)

##different occurrence frequency
occur_10_20 <- R2_all[R2_all$frequency<0.2,] #882个ASVs
occur_10_20['group'] <- "10-20"
occur_20_50 <- R2_all[R2_all$frequency>=0.2&R2_all$frequency<0.5,] #523个ASVs
occur_20_50['group'] <- "20-50"
occur_50_100 <- R2_all[R2_all$frequency>=0.5,] #88个ASVs
occur_50_100['group'] <- "50-100"

##Phylum
frequency_all <- rbind(occur_10_20, occur_20_50, occur_50_100)
frequency_all_R2 <- data_summary(frequency_all, varname = "R2_1.1", groupnames = "group")
write.csv (frequency_all_R2, file ="Occurrence_frequency-select-R2_1.1-summary.csv", quote = FALSE)

par(mar=c(3,5,2,2))
p<- ggplot(frequency_all_R2, aes(x=group, y=R2_1.1)) + 
  geom_point(size = 2) +
  #geom_bar(stat="identity", color="#000000", position=position_dodge()) +
  geom_errorbar(aes(ymin=R2_1.1-ci, ymax=R2_1.1+ci), width = 0, size = 0.5) +
  ylim(0,0.6)
p + labs(y = "Test R2_1:1") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Occurrence_frequency-new-R2_1.1-all.pdf", width = 4, height = 3)
t.test(frequency_all[frequency_all$group=="10-20",]$R2_1.1, frequency_all[frequency_all$group=="20-50",]$R2_1.1) ##p-value = 4.638e-06(0.330317  0.373202)
t.test(frequency_all[frequency_all$group=="20-50",]$R2_1.1, frequency_all[frequency_all$group=="50-100",]$R2_1.1) ##p-value = 0.006912(0.3732020 0.4249926)

##frequency
frequency_all_freq <- data_summary(frequency_all, varname = "frequency", groupnames = "group")

par(mar=c(3,5,2,2))
p<- ggplot(frequency_all_freq, aes(x=group, y=frequency)) + 
  geom_point(size = 2) +
  #geom_bar(stat="identity", color="#000000", position=position_dodge()) +
  geom_errorbar(aes(ymin=frequency-ci, ymax=frequency+ci), width = 0, size = 0.5) +
  ylim(0,1.0)
p + labs(y = "Occurrence frequency") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Occurrency_frequency-new-frequency.pdf", width = 4, height = 3)

t.test(frequency_all[frequency_all$group=="10-20",]$frequency, frequency_all[frequency_all$group=="20-50",]$frequency) ##p-value < 2.2e-16(0.1397228 0.2959783)
t.test(frequency_all[frequency_all$group=="20-50",]$frequency, frequency_all[frequency_all$group=="50-100",]$frequency) ##p-value < 2.2e-16(0.2959783 0.6199105)

##Abun
frequency_all_abun <- data_summary(frequency_all, varname = "Mean", groupnames = "group")

par(mar=c(3,5,2,2))
p<- ggplot(frequency_all_abun, aes(x=group, y=Mean)) + 
  geom_point(size = 2) +
  #geom_bar(stat="identity", color="#000000", position=position_dodge()) +
  geom_errorbar(aes(ymin=Mean-ci, ymax=Mean+ci), width = 0, size = 0.5) +
  ylim(0,0.003)
p + labs(y = "Average relative abundance") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Occurrency_frequency-new-MRA.pdf", width = 4, height = 3)

t.test(frequency_all[frequency_all$group=="10-20",]$Mean, frequency_all[frequency_all$group=="20-50",]$Mean) ##p-value < 2.2e-16(0.0001965760 0.0005661328)
t.test(frequency_all[frequency_all$group=="20-50",]$Mean, frequency_all[frequency_all$group=="50-100",]$Mean) ##p-value = 1.417e-09(0.0005661328 0.0020481888)

##CV
frequency_all_cv <- data_summary(frequency_all, varname = "CV", groupnames = "group")

par(mar=c(3,5,2,2))
p<- ggplot(frequency_all_cv, aes(x=group, y=CV)) + 
  geom_point(size = 2) +
  #geom_bar(stat="identity", color="#000000", position=position_dodge()) +
  geom_errorbar(aes(ymin=CV-ci, ymax=CV+ci), width = 0, size = 0.5) +
  ylim(0,5)
p + labs(y = "Coefficient of variation") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Occurrency_frequency-new-CV.pdf", width = 4, height = 3)

t.test(frequency_all[frequency_all$group=="10-20",]$CV, frequency_all[frequency_all$group=="20-50",]$CV) ##p-value < 2.2e-16(3.951444  2.808301)
t.test(frequency_all[frequency_all$group=="20-50",]$CV, frequency_all[frequency_all$group=="50-100",]$CV) ##p-value < 2.2e-16(2.808301  1.860856)

##niche_width_levins
frequency_all_levins <- data_summary(frequency_all, varname = "niche_width_levins", groupnames = "group")

par(mar=c(3,5,2,2))
p<- ggplot(frequency_all_levins, aes(x=group, y=niche_width_levins)) + 
  geom_point(size = 2) +
  #geom_bar(stat="identity", color="#000000", position=position_dodge()) +
  geom_errorbar(aes(ymin=niche_width_levins-ci, ymax=niche_width_levins+ci), width = 0, size = 0.5) +
  ylim(0,250)
p + labs(y = "niche width_levins") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("Occurrency_frequency-new-niche_width_levins.pdf", width = 4, height = 3)

t.test(frequency_all[frequency_all$group=="10-20",]$niche_width_levins, frequency_all[frequency_all$group=="20-50",]$niche_width_levins) ##p-value < 2.2e-16(53.94503 103.95789)
t.test(frequency_all[frequency_all$group=="20-50",]$niche_width_levins, frequency_all[frequency_all$group=="50-100",]$niche_width_levins) ##p-value < 2.2e-16(103.9579  195.0041)
