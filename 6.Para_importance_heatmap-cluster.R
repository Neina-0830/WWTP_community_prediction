setwd("E:/Desktop/WWTP-机器学习/All_ANN/result/4-fold/ASV/results/raw/all_select/sigmoid-output/")
library(reshape2) #用于排列数据 
library(tidyr)
library(dplyr)
library(pheatmap)
library(factoextra)
library(cluster)
library(useful) #加载该包,用于下面作图
#library(devtools)
#install_github("jokergoo/ComplexHeatmap")
#library(ComplexHeatmap)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm = T),
      sd = sd(x[[col]], na.rm = T))
  }
  data_sum <- ddply(data, groupnames, .fun = summary_func, varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

##alpha
data <- read.csv("Test_and_parameters-alpha_select-sigmoid.csv", header=T)
data <- data[,c(1:5,8:56)]
data_mean <- aggregate(data[2:54], by=list(Micro=data$Micro), mean)
data_para <- data_mean[,6:53]
rownames(data_para) <- data_mean$Micro
write.csv(data_para,"ASV-alpha_select-sigmoid-para-mean.csv", quote=FALSE)
data_para <- as.matrix(t(data_para))
bk <- c(seq(0,0.04,by=0.001))
p <- pheatmap::pheatmap(data_para,cluster_row = TRUE,cluster_col = TRUE, angle_col = 0, fontsize=5,
              clustering_method = "ward.D2",color = c(colorRampPalette(colors = c("white","darksalmon","darkred"))(length(bk))),
              legend_breaks=seq(0,0.04,by=0.01),breaks=bk,filename="ASV-alpha_select-sigmoid-para-mean-cluster.pdf",width = 4,height = 4)

##ASV-10
data <- read.csv("Test_and_parameters-10_select-sigmoid.csv", header=T)
data <- data[,c(1:5,8:56)]
data_mean <- aggregate(data[2:54], by=list(Micro=data$Micro), mean)
data_para <- data_mean[,6:53]
rownames(data_para) <- data_mean$Micro
write.csv(data_para,"ASV-10_select-sigmoid-para-mean.csv", quote=FALSE)
data_para <- as.matrix(t(data_para))
##k-means聚类（使用不同ASVs的不同环境因子权重系数）
####环境因子聚类
##1.聚类数量 vs. 总体平方和
fviz_nbclust(data_para, kmeans, method = "wss", k.max = 20)
##注：通常我们创建这类图形寻找某个K类对应的平方和值开始弯曲或趋于平缓的肘形。这通常是最理想的聚类数量。上图中显然在k = 4个时出现肘形
##2.聚类数量 vs. 差距统计
gap_stat <- clusGap(data_para,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 20,
                    B = 50)
fviz_gap_stat(gap_stat)
##从上图可以看到k=3时，差距统计最大，这与前面图的结果一致。
##使用上述两种方法确定的最优k执行kmeans聚类
# 设置随机种子，让结果可以重现
set.seed(1)
km <- kmeans(data_para, centers = 3, nstart = 25)
write.csv(km$cluster, file ="All-environment-factors-kmeans-clusters.csv", quote = FALSE)

fviz_cluster(object=km,data=data_para,
             star.plot=F, repel=F, show.clust.cent=F,
             geom = c("point", "text"), pointsize = 1.2, labelsize = 3,
             ellipse = F, 
             palette='jco',main="",
             ggtheme=theme_bw())+
  theme(axis.title = element_blank())
ggsave("Env-kmeans-clusters.pdf",height=3,width=4)

##小提琴图
data_para_median <- as.data.frame(apply(data_para, 2, median))
colnames(data_para_median) <- "weight_median"
write.csv(data_para_median,"Env_select-sigmoid-all-para-median.csv", quote=FALSE)

data_order <- as.data.frame(t(data_para))
##获得按权重平均值从高到低的环境因子顺序
data_order <- arrange(data_order, desc(apply(data_order,1,median)))
env_list <- rownames(data_order)

data_para_long <- gather(data=data_para,key=Env,value=Weight)
data_para_long$Env <- factor(data_para_long$Env, levels = env_list)
# sample size
sample_size = data_para_long %>% group_by(Env) %>% summarize(num=n())
data_para_long %>%
  left_join(sample_size) %>%
  ggplot(aes(x=Env, y=Weight, fill=Env)) +
  geom_violin(width=1.0) +
  #geom_boxplot(width=0.2, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  theme(legend.position="none", axis.text.x = element_text(size=12,color="black"), axis.text.y= element_text(size=12,color="black")) +
  theme(axis.title.x = element_text(size=12,color="black"), axis.title.y = element_text(size=12,color="black"))
dev.off()
ggsave("Environment-weight-violin-plot.pdf", width = 8, height = 6)


##Functioner
data<- read.csv("Test_and_parameters-functioner-sigmoid.csv", header=T)
data <- data[,c(1:5,8:56)]
data_mean <- aggregate(data[2:54], by=list(Micro=data$Micro), mean)
data_para <- data_mean[,6:53]
rownames(data_para) <- data_mean$Micro
data_para <- as.matrix(t(data_para))

annotation_col = data.frame(Function = factor(colnames(data_para)))
row.names(annotation_col) = colnames(data_para)
ann_colors = list(Function = c(Nitrifiers='#008B8B',Dentrifiers='#3CB371',PAOs='#FFA07A',GAOs='#CD5C5C',Filamentous='#6495ED'))
##k-means
#fviz_nbclust(data_para, kmeans, method = "wss",k.max = 100)
data_para <- as.matrix(data_para)
bk <- c(seq(0,0.07,by=0.001))
p <- pheatmap::pheatmap(data_para,cluster_row = TRUE,cluster_col = TRUE, show_colnames= FALSE,annotation_col = annotation_col, annotation_colors = ann_colors,
              fontsize=5,clustering_method = "ward.D2",color = c(colorRampPalette(colors = c("white","darksalmon","darkred"))(length(bk))),
              legend_breaks=seq(0,0.07,by=0.01),breaks=bk,filename="ASV-functioner-sigmoid-para-mean-cluster.pdf",width = 4.5,height = 3)
