##加载R包
#installed.packages("microeco")
#install.packages("ggplot2")
#install.packages("magrittr")
#install.packages("tidytree")
#install.packages("BiocManager")
#library("devtools")
#devtools::install_version("microeco", version = "0.20.0")
#install.packages("igraph")
#BiocManager::install("ggtree")
library(microeco) # Microbial Community Ecology Data Analysis
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(magrittr) # A Forward-Pipe Operator for R
packageVersion("microeco")
####加载数据，包括OTU特征表、样本信息以及物种分类表
#OTU特征表
otu <- read.table(file="otu.txt",sep="\t", header=T, check.names=FALSE, row.names=1)
#样本信息
sample <- read.table("sample.txt", sep='\t', header=T, check.names=FALSE, row.names=1)
sample$sample <- rownames(sample)
#物种分类表
tax <- read.table("tax.txt", sep='\t', header=T, check.names=FALSE, row.names=1)


#%<>% 是magrittr包中的一个管道操作符，它的作用是将左边的对象传递给右边的函数执行，
#然后将结果再赋值回左边的对象，即tax。简单来说，就是对tax执行tidy_taxonomy函数处理后，再把结果保存回tax。
#tidy_taxonomy很可能是一个用户定义的函数或者来自某个特定的包，用于整理或清理分类信息（taxonomy）。
#微生物组数据中的分类信息通常包括种、属、科等生物分类级别，这一步可能是为了格式化这些信息，使之更规整或满足后续分析的需要。
#统一分类信息
tax %<>% tidy_taxonomy

## 构造microtable
df <- microtable$new(sample_table = sample,
                     otu_table = otu,
                     tax_table = tax,
                     auto_tidy = F)
df
# ####数据处理步骤，根据个人数据进行操作
# ##去除不属于非古菌和细菌的OTU
# # df$tax_table %<>% subset(Kingdom == "k__Archaea" | Kingdom == "k__Bacteria")
# df$tax_table %<>% .[grepl("Bacteria|Archaea", .$Kingdom), ]
# df
# 
# ##去除“线粒体”和“叶绿体”污染
# df$filter_pollution(taxa = c("mitochondria", "chloroplast"))
# df
# 
# ##统一各数据的样本和OTU信息
# df$tidy_dataset()
# df
# 
# ##检查序列号
# df$sample_sums() %>% range
# 
# ##重采样以减少测序深度对多样性测量的影响，使每个样本的序列号相等。
# df$rarefy_samples(sample.size = 10000)
# df$sample_sums() %>% range

lefse <- trans_diff$new(dataset = df, #数据
                        method = "lefse", #方法
                        group = "group"#分组
)

lefse$plot_diff_bar(threshold = 4,#设定LDA SCORE显示阈值
                    width = 0.8,#柱子宽度
                    group_order = c("A", "B", "C"))#分组顺序
lefse$plot_diff_cladogram(use_taxa_num = 60,#显示丰度最高的40个分类群
                          use_feature_num = 40, #显示30个差异特征
                          clade_label_level = 5, #用字母标记标签的分类层级，5表示目，4表示科。
                          group_order = c("A", "B", "C"),#组间排序
                          color = RColorBrewer::brewer.pal(8, "Dark2"),#颜色
                          filter_taxa = 0.0001,#过滤掉相对丰度低于0.0001的分类单元。
                          select_show_labels = NULL,
                          only_select_show = FALSE,# 设置为TRUE，可以只展示select_show_labels选择的分类单元标签
                          sep = "|",#识别的辨识字符间隔
                          branch_size = 0.5,
                          alpha = 0.2,
                          clade_label_size = 1.5)




