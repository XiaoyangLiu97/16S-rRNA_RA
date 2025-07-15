rm(list = ls())
setwd("result/RF_R")
#读取数据
env <- read.delim('envfinal.txt', row.names = 1)


#使用函数 rfPermut() 重新对上述数据执行随机森林分析，详情 ?rfPermut
#rfPermut() 封装了 randomForest() 的方法，因此在给定数据和运行参数一致的情况下，两个函数结果也是一致的
#并在这里额外通过 nrep 参数执行 1000 次的随机置换以评估变量显著性的 p 值
#若数据量较大，可通过 num.cores 参数设置多线程运算
library(rfPermute)
set.seed(123)
env_rfP <- rfPermute(group~., data = env, importance = TRUE, ntree = 500,
                     nrep = 1000, num.cores = 20)
#提取预测变量（env）的重要性得分（标准化后的得分）
importance_env.scale <- data.frame(importance(env_rfP, scale = TRUE), check.names = FALSE)
importance_env.scale
#提取预测变量（env）的重要性得分的显著性（以标准化后的得分为例）
# summary(otu_rfP)
importance_env.scale.pval <- (env_rfP$pval)[ , , 2]
importance_env.scale.pval
#对预测变量（env）按重要性得分排个序，例如根据“%IncMSE”
importance_env.scale <- importance_env.scale[order(importance_env.scale$'%IncMSE',
                                                   decreasing =  FALSE), ]
#简单地作图展示预测变量（env）的 %IncMSE 值
library(ggplot2)
importance_env.scale$env_name <- rownames(importance_env.scale)
importance_env.scale$env_name <- factor(importance_env.scale$env_name, levels = importance_env.scale$env_name)
p <- ggplot() +
  geom_col(data = importance_env.scale, aes(x = env_name, y = `%IncMSE`), width = 0.5, fill = "skyblue", color = NA) +
  labs(title = NULL, x = NULL, y = 'Increase in MSE (%)', fill = NULL) +
  theme(panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = 'black')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(expand = c(0, 0), limit = c(0, 15))
p
# 添加显著性标记
importance_env.scale$'%IncMSE.sig' <- '' # 初始化显著性列
# 使用 p 值确定显著性标记
for (env in rownames(importance_env.scale)) {
  if (importance_env.scale[env, '%IncMSE.pval'] >= 0.1) {
    importance_env.scale[env, '%IncMSE.sig'] <- ''
  } else if (importance_env.scale[env, '%IncMSE.pval'] >= 0.01 & importance_env.scale[env, '%IncMSE.pval'] < 0.1) {
    importance_env.scale[env, '%IncMSE.sig'] <- '*'
  } else if (importance_env.scale[env, '%IncMSE.pval'] >= 0.001 & importance_env.scale[env, '%IncMSE.pval'] < 0.01) {
    importance_env.scale[env, '%IncMSE.sig'] <- '**'
  } else if (importance_env.scale[env, '%IncMSE.pval'] < 0.001) {
    importance_env.scale[env, '%IncMSE.sig'] <- '***'
  }
}
p <- p +
  geom_text(data = importance_env.scale, aes(x = env_name, y = `%IncMSE`, label = `%IncMSE.sig`), nudge_y = 1)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  coord_flip()
p