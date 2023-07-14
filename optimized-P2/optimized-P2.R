# 变量解释说明：####
# gender <- 性别  1男 2女
# grade <- 年级 1初一，2初二，3初三，4高一，5高二，6高三
# Q1~Q63 <- 为中学生积极心理品质问卷（孟万金）63题  修订后
# 
# 总分:
# D1 <- 思维与洞察力维度
# D2 <- 爱学习维度
# D3 <- 创造性维度
# A <- 认知分量表
# D4 <- 社交智力维度
# D5 <- 友善维度
# D6 <- 爱维度
# B <- 情感分量表
# D7 <- 执着维度
# D8 <- 真诚维度
# D9 <- 勇敢维度
# C <- 意志力分量表
# D10 <- 持重维度
# D11 <- 宽容维度
# D12 <- 谦虚维度
# D <- 律己分量表
# D13 <- 合作力维度
# D14 <- 领导力维度
# E <- 利群分量表
# D15 <- 幽默维度
# D16 <- 信念与希望维度
# D17 <- 心灵触动维度
# F <- 超越分量表
# TOTAL <- 总分
# junior <- 是否为高三
# 
# 总均分：
# AD1 <- 思维与洞察力维度
# AD2 <- 爱学习维度
# AD3 <- 创造性维度
#                         AA <- 认知分量表
# AD4 <- 社交智力维度
# AD5 <- 友善维度
# AD6 <- 爱维度
#                         AB <- 情感分量表
# AD7 <- 执着维度
# AD8 <- 真诚维度
# AD9 <- 勇敢维度
#                         AC <- 意志力分量表
# AD10 <- 持重维度
# AD11 <- 宽容维度
# AD12 <- 谦虚维度
#                         AD <- 律己分量表
# AD13 <- 合作力维度
# AD14 <- 领导力维度
#                         AE <- 利群分量表
# AD15 <- 幽默维度
# AD16 <- 信念与希望维度
# AD17 <- 心灵触动维度
#                         AF <- 超越分量表
#                     ATOTAL <- 总分

# 1.导入包####
library(vcd)
library(gmodels)
library(psych)
library(grid)
library(MASS)
library(multcomp)
library(gplots)
library(ggpubr)
library(ggsci)
library(ggsignif)
library(gplots)

# 2.读取数据####
data1 <- read.csv("数据1.csv")

# 3.人口统计学变量####
vars <- c("grade", "gender")
mytable1.1 <- table(data1$gender)
mytable1.2 <- table(data1$grade)
print(mytable1.1)
print(mytable1.2)

# 4.中学生积极心理品质现状分析####
states <- data1[, 109:115]
    # 4.1描述各个维度总均分的均值、标准差
  describe(states)
  
    # 4.2计算皮尔逊积差相关系数
  mycor <- corr.test(states)
    # 输出关系矩阵的下三角部分，保留两位小数
  lowerMat(mycor$r, digits = 2)
    # 输出关系系数的显著性检验p值
  lowerMat(mycor$p, digits = 4)
  
    # 4.3中学生总体积极心理品质性别差异分析（独立样本T检验）
  t.test(AA ~ gender, data = data1)
  t.test(AB ~ gender, data = data1)
  t.test(AC ~ gender, data = data1)
  t.test(AD ~ gender, data = data1)
  t.test(AE ~ gender, data = data1)
  t.test(AF ~ gender, data = data1)
  t.test(ATOTAL ~ gender, data = data1)
    # 进行T检验的事后分析
  pairwise.t.test(data1$AA, data1$gender, p.adjust.method = "bonferroni")
  pairwise.t.test(data1$AB, data1$gender, p.adjust.method = "bonferroni")
  pairwise.t.test(data1$AC, data1$gender, p.adjust.method = "bonferroni")
  pairwise.t.test(data1$AD, data1$gender, p.adjust.method = "bonferroni")
  pairwise.t.test(data1$AE, data1$gender, p.adjust.method = "bonferroni")
  pairwise.t.test(data1$AF, data1$gender, p.adjust.method = "bonferroni")
  pairwise.t.test(data1$ATOTAL, data1$gender, p.adjust.method = "bonferroni")
    # 4.4中学生总体积极心理品质年级差异分析（方差分析）
  aov.AA <- aov(AA ~ grade, data = data1)
  aov.AB <- aov(AB ~ grade, data = data1)
  aov.AC <- aov(AC ~ grade, data = data1)
  aov.AD <- aov(AD ~ grade, data = data1)
  aov.AE <- aov(AE ~ grade, data = data1)
  aov.AF <- aov(AF ~ grade, data = data1)
  aov.ATOTAL <- aov(ATOTAL ~ grade,data = data1)
  summary(aov.AA)
  summary(aov.AB)
  summary(aov.AC)
  summary(aov.AD)
  summary(aov.AE)
  summary(aov.AF)
  summary(aov.ATOTAL)
    # 事后分析
  bartlett.test(AA ~ grade, data = data1)
  plotmeans(AA ~ grade, xlab = "年级", ylab = "认知分量表", main = "Mean Plot\nwith 95% CI")
  bartlett.test(AB ~ grade, data = data1)
  plotmeans(AB ~ grade, xlab = "年级", ylab = "情感分量表", main = "Mean Plot\nwith 95% CI")
  bartlett.test(AC ~ grade, data = data1)
  plotmeans(AC ~ grade, xlab = "年级", ylab = "意志力分量表", main = "Mean Plot\nwith 95% CI")
  bartlett.test(AD ~ grade, data = data1)
  plotmeans(AD ~ grade, xlab = "年级", ylab = "律己分量表", main = "Mean Plot\nwith 95% CI")
  bartlett.test(AE ~ grade, data = data1)
  plotmeans(AE ~ grade, xlab = "年级", ylab = "利群分量表", main = "Mean Plot\nwith 95% CI")
  bartlett.test(AF ~ grade, data = data1)
  plotmeans(AF ~ grade, xlab = "年级", ylab = "超越分量表", main = "Mean Plot\nwith 95% CI")
  
  #无法执行：
  #原因：未找到ATOTAL变量 ？ 
  # bartlett.test(ATOTAL ~ grade, data = data1)
  # plotmeans(ATOTAL ~ grade, xlab = "年级", ylab = "总分", main = "Mean Plot\nwith 95% CI")
  # head(data1)
  
    # 4.5中学生积极心理品质高三与非高三差异分析
  t.test(AA ~ junior, data = data1)
  t.test(AB ~ junior, data = data1)
  t.test(AC ~ junior, data = data1)
  t.test(AD ~ junior, data = data1)
  t.test(AE ~ junior, data = data1)
  t.test(AF ~ junior, data = data1)
    #事后分析
  pairwise.t.test(data1$AA, data1$junior, p.adjust.method = "bonferroni")#不显著
  pairwise.t.test(data1$AB, data1$junior, p.adjust.method = "bonferroni")#**
  pairwise.t.test(data1$AC, data1$junior, p.adjust.method = "bonferroni")#边缘显著
  pairwise.t.test(data1$AD, data1$junior, p.adjust.method = "bonferroni")#*
  pairwise.t.test(data1$AE, data1$junior, p.adjust.method = "bonferroni")#***
  pairwise.t.test(data1$AF, data1$junior, p.adjust.method = "bonferroni")#*
  pairwise.t.test(data1$ATOTAL, data1$gender, p.adjust.method = "bonferroni")#***
