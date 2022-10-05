##对Test结果按random平均化处理后，再按预测对象平均处理，接着对预测对象分组统计和比较
setwd("E:/Desktop/WWTP-机器学习/All_ANN/result/4-fold/ASV/results/raw/all_select/sigmoid-output/")
# Library
library("ggplot2")

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

all_data <- read.csv("Test-10freq_select-sigmoid-raw-R2-R2_1.1.csv", header=TRUE, row.names = 1)
R2_1.1_data <- data_summary(all_data, varname = "R2_1.1", groupnames = "Micro")
Mic <- read.csv("../../../../rawdata/Microbiom-ASV-10_select-Summary.csv",head=T,row.names=1)
Mic <- as.data.frame(t(Mic))
Mic["Micro"] <- rownames(Mic)
newdata <- merge(R2_1.1_data, Mic, by = "Micro")
write.csv(newdata,"Test-10freq_select-sigmoid-R2_1.1-summary-mean-sd-cv.csv",quote = FALSE, row.names = FALSE)
#Plot R2_1.1 ~*
par(mar=c(3,5,2,2))
p1 <- ggplot(newdata, aes(x = Mean, y = R2_1.1)) +
  geom_point(size =1 ) +
  #geom_errorbar(aes(ymin=R2_1.1-ci, ymax=R2_1.1+ci), width = 0) +
  geom_smooth(method="lm" , formula = y~x, color="#009E73", fill="#56B4E9", se=TRUE,linetype = "dashed",size=1)
model.lm <- lm(R2_1.1 ~ Mean, data = newdata)
l <- list(a = format(coef(model.lm)[1], digits = 4),
          b = format(abs(coef(model.lm)[2]), digits = 4),
          r2 = format(summary(model.lm)$r.squared, digits = 4),
          p = format(summary(model.lm)$coefficients[2,4], digits = 4))
eq <- substitute(italic(R)^2~"="~r2~","~italic(P)~"="~p, l)
p11 <- p1 + geom_text(aes(x = 0.003, y = 1.0, label = as.character(as.expression(eq))), parse = TRUE, size = 3) +
  labs(x="Average relative abundance", y = "Test R2_1.1") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
p11 
dev.off()
ggsave("Sigmoid-mean-R2_1.1-Mean-linear.pdf", width = 4, height = 3)

par(mar=c(3,5,2,2))
p1 <- ggplot(newdata, aes(x = SD, y = R2_1.1)) +
  geom_point(size =1 ) +
  #geom_errorbar(aes(ymin=R2_1.1-ci, ymax=R2_1.1+ci), width = 0) +
  geom_smooth(method="lm" , formula = y~x, color="#009E73", fill="#56B4E9", se=TRUE,linetype = "dashed",size=1)
model.lm <- lm(R2_1.1 ~ SD, data = newdata)
l <- list(a = format(coef(model.lm)[1], digits = 4),
          b = format(abs(coef(model.lm)[2]), digits = 4),
          r2 = format(summary(model.lm)$r.squared, digits = 4),
          p = format(summary(model.lm)$coefficients[2,4], digits = 4))
eq <- substitute(italic(R)^2~"="~r2~","~italic(P)~"="~p, l)
p11 <- p1 + geom_text(aes(x = 0.008, y = 1.0, label = as.character(as.expression(eq))), parse = TRUE, size = 3) +
  labs(x="Standard deviation", y = "Test R2_1.1") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
p11
dev.off()
ggsave("Sigmoid-mean-R2_1.1-SD-linear.pdf", width = 4, height = 3)

par(mar=c(3,5,2,2))
p1 <- ggplot(newdata, aes(x = CV, y = R2_1.1)) +
  geom_point(size =1 ) +
  #geom_errorbar(aes(ymin=R2_1.1-ci, ymax=R2_1.1+ci), width = 0) +
  geom_smooth(method="lm" , formula = y~x, color="#009E73", fill="#56B4E9", se=TRUE,linetype = "dashed",size=1)
model.lm <- lm(R2_1.1 ~ CV, data = newdata)
l <- list(a = format(coef(model.lm)[1], digits = 4),
          b = format(abs(coef(model.lm)[2]), digits = 4),
          r2 = format(summary(model.lm)$r.squared, digits = 4),
          p = format(summary(model.lm)$coefficients[2,4], digits = 4))
eq <- substitute(italic(R)^2~"="~r2~","~italic(P)~"="~p, l)
p11 <- p1 + geom_text(aes(x = 4.6, y = 1.0, label = as.character(as.expression(eq))), parse = TRUE, size = 3) +
  labs(x="Coefficient of variation", y = "Test R2_1.1") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
p11
dev.off()
ggsave("Sigmoid-mean-R2_1.1-CV-linear.pdf", width = 4, height = 3)


##taxo_frequency_nichewidth
Taxo <- read.csv("E:/Desktop/数据库数据下载/SRA数据库/WWTP/WWTP_16S/qiime分析/ASV-new/ASV_MiDAS_taxonomy_frequency_nichewidth_groups_select.csv", head = T, row.names = 1)
all_taxo <- Taxo[rownames(Taxo) %in% newdata$Micro,]
all_taxo$Micro <- rownames(all_taxo)
all_taxo_data <- merge(all_taxo, newdata, by="Micro")
write.csv(all_taxo_data,"Test-10freq_select-sigmoid-R2_1.1-MiDAS-taxo-niche-summary.csv",quote = FALSE, row.names = FALSE)

par(mar=c(3,5,2,2))
p1 <- ggplot(all_taxo_data, aes(x = frequency, y = R2_1.1)) +
  geom_point(size =1 ) +
  #geom_errorbar(aes(ymin=R2_1.1-ci, ymax=R2_1.1+ci), width = 0) +
  geom_smooth(method="lm" , formula = y~x, color="#009E73", fill="#56B4E9", se=TRUE,linetype = "dashed",size=1)
model.lm <- lm(R2_1.1 ~ frequency, data = all_taxo_data)
l <- list(a = format(coef(model.lm)[1], digits = 4),
          b = format(abs(coef(model.lm)[2]), digits = 4),
          r2 = format(summary(model.lm)$r.squared, digits = 4),
          p = format(summary(model.lm)$coefficients[2,4], digits = 4))
eq <- substitute(italic(R)^2~"="~r2~","~italic(P)~"="~p, l)
p11 <- p1 + geom_text(aes(x = 0.35, y = 1.0, label = as.character(as.expression(eq))), parse = TRUE, size = 3) +
  labs(x="Occurrence frequency", y = "Test R2_1.1") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
p11
dev.off()
ggsave("Sigmoid-mean-R2_1.1-frequency-linear.pdf", width = 4, height = 3)

par(mar=c(3,5,2,2))
p1 <- ggplot(all_taxo_data, aes(x = niche_width_levins, y = R2_1.1)) +
  geom_point(size =1 ) +
  #geom_errorbar(aes(ymin=R2_1.1-ci, ymax=R2_1.1+ci), width = 0) +
  geom_smooth(method="lm" , formula = y~x, color="#009E73", fill="#56B4E9", se=TRUE,linetype = "dashed",size=1)
model.lm <- lm(R2_1.1 ~ niche_width_levins, data = all_taxo_data)
l <- list(a = format(coef(model.lm)[1], digits = 4),
          b = format(abs(coef(model.lm)[2]), digits = 4),
          r2 = format(summary(model.lm)$r.squared, digits = 4),
          p = format(summary(model.lm)$coefficients[2,4], digits = 4))
eq <- substitute(italic(R)^2~"="~r2~","~italic(P)~"="~p, l)
p11 <- p1 + geom_text(aes(x = 100, y = 1.0, label = as.character(as.expression(eq))), parse = TRUE, size = 3) +
  labs(x="ASVs's niche width(levins)", y = "Test R2_1.1") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
p11
dev.off()
ggsave("Sigmoid-mean-R2_1.1-niche_width_levins-linear.pdf", width = 4, height = 3)

##frequency and abundance
par(mar=c(3,5,2,2))
p1 <- ggplot(all_taxo_data, aes(x = frequency, y = Mean)) +
  geom_point(size =1 ) +
  #geom_errorbar(aes(ymin=R2_1.1-ci, ymax=R2_1.1+ci), width = 0) +
  geom_smooth(method="lm" , formula = y~x, color="#009E73", fill="#56B4E9", se=TRUE,linetype = "dashed",size=1)
model.lm <- lm(Mean ~ frequency, data = all_taxo_data)
l <- list(a = format(coef(model.lm)[1], digits = 4),
          b = format(abs(coef(model.lm)[2]), digits = 4),
          r2 = format(summary(model.lm)$r.squared, digits = 4),
          p = format(summary(model.lm)$coefficients[2,4], digits = 4))
eq <- substitute(italic(R)^2~"="~r2~","~italic(P)~"="~p, l)
p11 <- p1 + geom_text(aes(x = 0.35, y = 0.012, label = as.character(as.expression(eq))), parse = TRUE, size = 3) +
  labs(x="Occurrence frequency", y = "Relative abundance") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
p11
dev.off()
ggsave("Abundance-frequency-linear.pdf", width = 4, height = 3)
