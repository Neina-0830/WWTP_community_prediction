setwd("E:/Desktop/WWTP-机器学习/All_ANN/result/4-fold/ASV/results/raw/all_select/sigmoid-output/")

library(ggplot2)

parameter_mean <- read.csv("ASV-10_select-sigmoid-para-mean.csv", header = T, row.names = 1)
environment_summary <- read.csv("../../../../rawdata/Environment_select-Summary.csv", header = T, row.names = 1)
environment_correlation <- read.csv("../../../../rawdata/ASV-10_select-env-mantel-test-all.csv", header = T)
ASV_list <- rownames(parameter_mean)
rmean <- vector()
rsd <- vector()
for(i in 1:ncol(parameter_mean)){
  rmean = c(rmean, mean(parameter_mean[,i]))
  rsd = c(rsd, sd(parameter_mean[,i]))
}
parameter_mean <- rbind(parameter_mean,rmean,rsd)
rownames(parameter_mean) <- c(ASV_list,"para_mean","para_sd")

all_data <- rbind(parameter_mean, environment_summary) 
all_data <- as.data.frame(t(all_data))
write.csv(all_data, '10_select_weights_summary.csv', quote = FALSE)


par(mar=c(3,5,2,2))
p<- ggplot(all_data, aes(x=CV, y=para_mean)) + 
  geom_point(size=1) +
  geom_smooth(method="lm" , formula = y~x, color="#009E73", fill="#56B4E9", se=TRUE,linetype = "dashed",size=1) +
  geom_errorbar(aes(ymin=para_mean-para_sd, ymax=para_mean+para_sd), width = 0) 
model.lm <- lm(para_mean ~ CV, data = all_data)
l <- list(a = format(coef(model.lm)[1], digits = 4),
          b = format(abs(coef(model.lm)[2]), digits = 4),
          r2 = format(summary(model.lm)$r.squared, digits = 4),
          p = format(summary(model.lm)$coefficients[2,4], digits = 4))
eq <- substitute(italic(R)^2~"="~r2~","~italic(P)~"="~p, l)
p + geom_text(aes(x = 0.7, y = 0.040, label = as.character(as.expression(eq))), parse = TRUE, size = 3) +
  labs(x="Coefficient of variation", y = "Garson’s connection weights") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("env_cv-env_weight-sigmoid-all.pdf", width = 4, height = 3)

par(mar=c(3,5,2,2))
p<- ggplot(all_data, aes(x=Mean, y=para_mean)) + 
  geom_point(size=1)  +
  geom_smooth(method="lm" , formula = y~x, color="#009E73", fill="#56B4E9", se=TRUE,linetype = "dashed",size=1) +
  geom_errorbar(aes(ymin=para_mean-para_sd, ymax=para_mean+para_sd), width = 0) 
model.lm <- lm(para_mean ~ Mean, data = all_data)
l <- list(a = format(coef(model.lm)[1], digits = 4),
          b = format(abs(coef(model.lm)[2]), digits = 4),
          r2 = format(summary(model.lm)$r.squared, digits = 4),
          p = format(summary(model.lm)$coefficients[2,4], digits = 4))
eq <- substitute(italic(R)^2~"="~r2~","~italic(P)~"="~p, l)
p + geom_text(aes(x = 25, y = 0.040, label = as.character(as.expression(eq))), parse = TRUE, size = 3) +
  labs(x="Mean value", y = "Garson’s connection weights") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("env_mean-env_weight-sigmoid-all.pdf", width = 4, height = 3)

par(mar=c(3,5,2,2))
p<- ggplot(all_data, aes(x=Skew, y=para_mean)) + 
  geom_point(size=1)  +
  geom_smooth(method="lm" , formula = y~x, color="#009E73", fill="#56B4E9", se=TRUE,linetype = "dashed",size=1) +
  geom_errorbar(aes(ymin=para_mean-para_sd, ymax=para_mean+para_sd), width = 0) 
model.lm <- lm(para_mean ~ Skew, data = all_data)
l <- list(a = format(coef(model.lm)[1], digits = 4),
          b = format(abs(coef(model.lm)[2]), digits = 4),
          r2 = format(summary(model.lm)$r.squared, digits = 4),
          p = format(summary(model.lm)$coefficients[2,4], digits = 4))
eq <- substitute(italic(R)^2~"="~r2~","~italic(P)~"="~p, l)
p + xlim(-2,8) +
  geom_text(aes(x = 0.8, y = 0.040, label = as.character(as.expression(eq))), parse = TRUE, size = 3) +
  labs(x="Skewness", y = "Garson’s connection weights") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("env_Skew-env_weight-sigmoid-all.pdf", width = 4, height = 3)

par(mar=c(3,5,2,2))
p<- ggplot(all_data, aes(x=Kurtosis, y=para_mean)) + 
  geom_point(size=1)  +
  geom_smooth(method="lm" , formula = y~x, color="#009E73", fill="#56B4E9", se=TRUE,linetype = "dashed",size=1) +
  geom_errorbar(aes(ymin=para_mean-para_sd, ymax=para_mean+para_sd), width = 0) 
model.lm <- lm(para_mean ~ Kurtosis, data = all_data)
l <- list(a = format(coef(model.lm)[1], digits = 4),
          b = format(abs(coef(model.lm)[2]), digits = 4),
          r2 = format(summary(model.lm)$r.squared, digits = 4),
          p = format(summary(model.lm)$coefficients[2,4], digits = 4))
eq <- substitute(italic(R)^2~"="~r2~","~italic(P)~"="~p, l)
p + xlim(-2,60) +
  geom_text(aes(x = 15, y = 0.040, label = as.character(as.expression(eq))), parse = TRUE, size = 3) +
  labs(x="Kurtosis", y = "Garson’s connection weights") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("env_Kurtosis-env_weight-sigmoid-all.pdf", width = 4, height = 3)

environment_correlation <- as.data.frame(t(environment_correlation))
colnames(environment_correlation) <- environment_correlation[2,]
corr_data <- rbind(parameter_mean, environment_correlation) 
corr_data <- as.data.frame(t(corr_data))
corr_data$para_mean <- as.numeric(corr_data$para_mean)
corr_data$para_sd <- as.numeric(corr_data$para_sd)
corr_data$r <- as.numeric(corr_data$r)
write.csv(corr_data, '10_select_weights_correlation.csv', quote = FALSE)

##plot env-spe correlation~para_mean
par(mar=c(3,5,2,2))
p<- ggplot(corr_data, aes(x=r, y=para_mean)) + 
  geom_point(size=1) +
  geom_smooth(method="lm" , formula = y~x, color="#009E73", fill="#56B4E9", se=TRUE,linetype = "dashed",size=1) +
  geom_errorbar(aes(ymin=para_mean-para_sd, ymax=para_mean+para_sd), width = 0) 
model.lm <- lm(para_mean ~ r, data = corr_data)
l <- list(a = format(coef(model.lm)[1], digits = 4),
          b = format(abs(coef(model.lm)[2]), digits = 4),
          r2 = format(summary(model.lm)$r.squared, digits = 4),
          p = format(summary(model.lm)$coefficients[2,4], digits = 4))
eq <- substitute(italic(R)^2~"="~r2~","~italic(P)~"="~p, l)
p + geom_text(aes(x = 0.09, y = 0.040, label = as.character(as.expression(eq))), parse = TRUE, size = 3) +
  labs(x="Correlation coefficients", y = "Garson’s connection weights") +
  theme(axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title = element_text(face = "bold", size = 12, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"))
dev.off()
ggsave("env_spe_corr-env_weight-sigmoid-all.pdf", width = 4, height = 3)
