#变量说明####
#grade <- 1，2021级；2,2020级；3，2019级；4，2018级
#gender <- 1，男；2，女
#evaluate <- 对周围内卷程度的评价（1-5级评分）
#I1-I16 <- 自编内卷量表
#S1-S10 <- 一般自我效能感量表
#P1-P52 <- 职业价值观量表
#T1 <- 内卷量表总分
#T2 <- 一般自我效能感量表总分
#T3 <- 职业价值观量表总分
#N1-N4 <- 内卷 四个维度得分（不良影响、环境、家庭、个人）
#Z1-Z3 <- 职业价值观 三个维度得分（内在价值，外在价值，外在报酬）
#A1-A13 <- 职业价值观 因素
#1.导入库#####
library(table1)
library(boot)
library(grid)
library(gmodels)
library(psych)
library(vcd)
library(lavaan)
library(semPlot)
#2.读取数据   （内卷对职业价值观的影响：自我效能感的中介作用）####
  data1 <- read.csv("筛选1.csv", header=TRUE)
  #各维度总分
  states <- data1[,87:89]
#3.人口统计学变量#####
  #对数据进行转化，将变量grade、gender换成字符
  data1$grade <-  factor(data1$grade,levels=c(1,2,3,4,5),labels=c("2018级", "2019级", "2020级","2021级","2022级"))
  data1$gender <- factor(data1$gender,levels = c(1,2),labels = c("男","女"))
  #设置标签
  label(data1$grade) <- "年级"
  label(data1$gender) <- "性别"
  #绘制三线表
  table1(~ grade+gender ,data= data1,overall="总人数")
#4.内卷、自我效能感、职业价值观的相关分析#####
  #三个问卷的总分的平均数、标准差
  describe(states[,c(1:3)])
  #4.1计算皮尔逊积差相关系数
  mycor = corr.test(states[,c(1:3)])
  #4.2输出下关系矩阵的下三角部分，保留两位小数
  lowerMat(mycor$r,digits = 2)
  #4.3输出关系系数的显著性检验P值
  lowerMat(mycor$p,digits = 4)

#5.自我效能感在大学生内卷与职业价值观间的中介作用####
  #第一步：建立模型
  model <- ' 
         Involution =~ N1+N2+N3
         Professional Values =~ Z1+Z2+Z3
         SelfEfficacy =~ S
         
         SelfEfficacy ~Involution
         Professional Values ~Involution+SelfEfficacy
         
         N2 ~~ N3'
  #第二步：拟合SEM
  fit <- sem(model,data = data1)
  #第三步：提取结果
  summary(fit,standardized = TRUE)
  #第四步：检验结果的准确性  (因为是自编量表，所以拟合效果差强人意)
  fitMeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea"))
  #第五步：作图
  semPaths(fit,
           what = "std",
           layout = "tree",
           fade=F,
           residuals= F,
           nCharNodes = 0
  )