rm(list = ls())
setwd("/xywhb/16sfinal20241119/R_p&g")
getwd()
library(ggplot2)
library(reshape2)
library(randomcoloR)
data<-read.table("sum_p.txt",sep="\t",header = T,check.names = FALSE,row.names = 1)
data$Taxonomy <- factor(rownames(data),levels = rev(rownames(data)))
data1<-melt(data,id = 'Taxonomy')
#设置分面数据#R语言中给数据框增加分组/根据样本名称添加分组的代码
x <- ifelse(str_detect(data1$variable,"HC"),"HC","PA")
data1$group <- x
#data1$Group <- rep(c("Treament", "Control"), each = 96) #添加新列
#data1$Group <- factor(data1$Group, levels = c("Treament", "Control"), ordered = TRUE)
# 将Taxonomy变量设置为重复的颜色向量
#设置堆叠柱形图的颜色
#colors<-rev(c( "#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF","#BB0021FF",
               "#5F559BFF","#A20056FF","#42B540FF","#0099B4FF","#1B1919FF","#808180FF")) #最后一个色是最上面的颜色，一般灰色#CCCCCC浅灰

#随机生成60种颜色，其实里面有重复的
colors <- randomColor(count = 18) 
#差异明显的60种
#colors <- distinctColorPalette(18) 

ggplot(data1, aes(x = variable, y = value, fill = Taxonomy)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = colors, labels = levels(data1$Taxonomy)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 105))+
  guides(fill = guide_legend(reverse = TRUE, title = NULL)) +
  facet_wrap(~group, scales = "free_x") +#分面操作
  theme_test()+
  labs(x= NULL,y="Relative Abundance (%)",fill="Phylum",title=NULL)+
  theme(legend.position = "right",
        axis.text.x = element_text(size = 12,angle = 90, hjust = 1,color = "black"),
        axis.text.y=element_text(size=12,color = "black"),#X轴刻度字体大小和颜色设置
        legend.text=element_text(size=12), #图例字体设置
        panel.background = element_blank()
  )

library(ggplot2)
library(reshape2)
library(randomcoloR)
data<-read.table("sum_p_sum.txt",sep="\t",header = T,check.names = FALSE,row.names = 1)
data$Taxonomy <- factor(rownames(data),levels = rev(rownames(data)))
data1<-melt(data,id = 'Taxonomy')
#设置分面数据#R语言中给数据框增加分组/根据样本名称添加分组的代码
x <- ifelse(str_detect(data1$variable,"HC"),"HC","PA")
data1$group <- x
#data1$Group <- rep(c("Treament", "Control"), each = 96) #添加新列
#data1$Group <- factor(data1$Group, levels = c("Treament", "Control"), ordered = TRUE)
# 将Taxonomy变量设置为重复的颜色向量
#设置堆叠柱形图的颜色
#colors<-rev(c( "#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF","#BB0021FF",
"#5F559BFF","#A20056FF","#42B540FF","#0099B4FF","#1B1919FF","#808180FF")) #最后一个色是最上面的颜色，一般灰色#CCCCCC浅灰

#随机生成60种颜色，其实里面有重复的
colors <- randomColor(count = 18) 
#差异明显的60种
#colors <- distinctColorPalette(18) 

ggplot(data1, aes(x = variable, y = value, fill = Taxonomy)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = colors, labels = levels(data1$Taxonomy)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 105))+
  guides(fill = guide_legend(reverse = TRUE, title = NULL)) +
  facet_wrap(~group, scales = "free_x") +#分面操作
  theme_test()+
  labs(x= NULL,y="Relative Abundance (%)",fill="Phylum",title=NULL)+
  theme(legend.position = "right",
        axis.text.x = element_text(size = 12,angle = 90, hjust = 1,color = "black"),
        axis.text.y=element_text(size=12,color = "black"),#X轴刻度字体大小和颜色设置
        legend.text=element_text(size=12), #图例字体设置
        panel.background = element_blank()
  )

setwd("/xywhb/16sfinal20241119/R_p&g")
getwd()
library(ggplot2)
library(reshape2)
library(randomcoloR)
data<-read.table("sum_p_top.txt",sep="\t",header = T,check.names = FALSE,row.names = 1)
data$Taxonomy <- factor(rownames(data),levels = rev(rownames(data)))
data1<-melt(data,id = 'Taxonomy')
#设置分面数据#R语言中给数据框增加分组/根据样本名称添加分组的代码
x <- ifelse(str_detect(data1$variable,"HC"),"HC","PA")
data1$group <- x
#data1$Group <- rep(c("Treament", "Control"), each = 96) #添加新列
#data1$Group <- factor(data1$Group, levels = c("Treament", "Control"), ordered = TRUE)
# 将Taxonomy变量设置为重复的颜色向量
#设置堆叠柱形图的颜色
colors<-rev(c( "#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF",
"#0099B4FF","#1B1919FF","#808180FF")) #最后一个色是最上面的颜色，一般灰色#CCCCCC浅灰

#随机生成60种颜色，其实里面有重复的
#colors <- randomColor(count = 18) 
#差异明显的60种
#colors <- distinctColorPalette(18) 

ggplot(data1, aes(x = variable, y = value, fill = Taxonomy)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = colors, labels = levels(data1$Taxonomy)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 105))+
  guides(fill = guide_legend(reverse = TRUE, title = NULL)) +
  facet_wrap(~group, scales = "free_x") +#分面操作
  theme_test()+
  labs(x= NULL,y="Relative Abundance (%)",fill="Phylum",title=NULL)+
  theme(legend.position = "right",
        axis.text.x = element_text(size = 12,angle = 90, hjust = 1,color = "black"),
        axis.text.y=element_text(size=12,color = "black"),#X轴刻度字体大小和颜色设置
        legend.text=element_text(size=12), #图例字体设置
        panel.background = element_blank()
  )

setwd("/xywhb/16sfinal20241119/R_p&g")
getwd()
library(ggplot2)
library(reshape2)
library(randomcoloR)
data<-read.table("sum_p_sum_top.txt",sep="\t",header = T,check.names = FALSE,row.names = 1)
data$Taxonomy <- factor(rownames(data),levels = rev(rownames(data)))
data1<-melt(data,id = 'Taxonomy')
#设置分面数据#R语言中给数据框增加分组/根据样本名称添加分组的代码
x <- ifelse(str_detect(data1$variable,"HC"),"HC","PA")
data1$group <- x
#data1$Group <- rep(c("Treament", "Control"), each = 96) #添加新列
#data1$Group <- factor(data1$Group, levels = c("Treament", "Control"), ordered = TRUE)
# 将Taxonomy变量设置为重复的颜色向量
#设置堆叠柱形图的颜色
colors<-rev(c( "#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF",
               "#0099B4FF","#1B1919FF","#808180FF")) #最后一个色是最上面的颜色，一般灰色#CCCCCC浅灰

#随机生成60种颜色，其实里面有重复的
#colors <- randomColor(count = 18) 
#差异明显的60种
#colors <- distinctColorPalette(18) 

ggplot(data1, aes(x = variable, y = value, fill = Taxonomy)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = colors, labels = levels(data1$Taxonomy)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 105))+
  guides(fill = guide_legend(reverse = TRUE, title = NULL)) +
  facet_wrap(~group, scales = "free_x") +#分面操作
  theme_test()+
  labs(x= NULL,y="Relative Abundance (%)",fill="Phylum",title=NULL)+
  theme(legend.position = "right",
        axis.text.x = element_text(size = 12,angle = 90, hjust = 1,color = "black"),
        axis.text.y=element_text(size=12,color = "black"),#X轴刻度字体大小和颜色设置
        legend.text=element_text(size=12), #图例字体设置
        panel.background = element_blank()
  )

setwd("/xywhb/16sfinal20241119/R_p&g")
getwd()
library(ggplot2)
library(reshape2)
library(randomcoloR)
data<-read.table("sum_g_top2.txt",sep="\t",header = T,check.names = FALSE,row.names = 1)
data$Genus <- factor(rownames(data),levels = rev(rownames(data)))
data1<-melt(data,id = 'Genus')
#设置分面数据#R语言中给数据框增加分组/根据样本名称添加分组的代码
x <- ifelse(str_detect(data1$variable,"HC"),"HC","PA")
data1$group <- x
#data1$Group <- rep(c("Treament", "Control"), each = 96) #添加新列
#data1$Group <- factor(data1$Group, levels = c("Treament", "Control"), ordered = TRUE)
# 将Taxonomy变量设置为重复的颜色向量
#设置堆叠柱形图的颜色
colors<-rev(c( "#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF","#BB0021FF",
"#5F559BFF","#0099B4FF","#42B540FF","#808180FF")) #最后一个色是最上面的颜色，一般灰色#CCCCCC浅灰

#随机生成60种颜色，其实里面有重复的
#colors <- randomColor(count = 18) 
#差异明显的60种
#colors <- distinctColorPalette(18) 

ggplot(data1, aes(x = variable, y = value, fill = Genus)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = colors, labels = levels(data1$Genus)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 105))+
  guides(fill = guide_legend(reverse = TRUE, title = NULL)) +
  facet_wrap(~group, scales = "free_x") +#分面操作
  theme_test()+
  labs(x= NULL,y="Relative Abundance (%)",fill="Phylum",title=NULL)+
  theme(legend.position = "right",
        axis.text.x = element_text(size = 12,angle = 90, hjust = 1,color = "black"),
        axis.text.y=element_text(size=12,color = "black"),#X轴刻度字体大小和颜色设置
        legend.text=element_text(size=12), #图例字体设置
        panel.background = element_blank()
  )

setwd("/xywhb/16sfinal20241119/R_p&g")
getwd()
library(ggplot2)
library(reshape2)
library(randomcoloR)
data<-read.table("sum_g_sum_top2.txt",sep="\t",header = T,check.names = FALSE,row.names = 1)
data$Genus <- factor(rownames(data),levels = rev(rownames(data)))
data1<-melt(data,id = 'Genus')
#设置分面数据#R语言中给数据框增加分组/根据样本名称添加分组的代码
x <- ifelse(str_detect(data1$variable,"HC"),"HC","PA")
data1$group <- x
#data1$Group <- rep(c("Treament", "Control"), each = 96) #添加新列
#data1$Group <- factor(data1$Group, levels = c("Treament", "Control"), ordered = TRUE)
# 将Taxonomy变量设置为重复的颜色向量
#设置堆叠柱形图的颜色
colors<-rev(c( "#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF","#BB0021FF",
               "#5F559BFF","#0099B4FF","#42B540FF","#808180FF")) #最后一个色是最上面的颜色，一般灰色#CCCCCC浅灰

#随机生成60种颜色，其实里面有重复的
#colors <- randomColor(count = 18) 
#差异明显的60种
#colors <- distinctColorPalette(18) 

ggplot(data1, aes(x = variable, y = value, fill = Genus)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = colors, labels = levels(data1$Genus)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 105))+
  guides(fill = guide_legend(reverse = TRUE, title = NULL)) +
  facet_wrap(~group, scales = "free_x") +#分面操作
  theme_test()+
  labs(x= NULL,y="Relative Abundance (%)",fill="Phylum",title=NULL)+
  theme(legend.position = "right",
        axis.text.x = element_text(size = 12,angle = 90, hjust = 1,color = "black"),
        axis.text.y=element_text(size=12,color = "black"),#X轴刻度字体大小和颜色设置
        legend.text=element_text(size=12), #图例字体设置
        panel.background = element_blank()
  )