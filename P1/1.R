#变量说明
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
library(vcd)
library(gmodels)
library(psych)
library(grid)
#2.读取数据   （内卷对职业价值观的影响：自我效能感的中介作用）####
data1 <- read.csv("筛选1.csv")
#3.人口统计学变量#####
vars <- c("grade","gender","evaluate")
# mystats <- function(x, na.omit = FALSE){
#   if (na.omit)
#     x <- x[!is.na(x)]
#   m <- mean(x)
#   n <- length(x)
#   s <- sd(x)
#   skew <- sum((x-m)^3/s^3)
#   kurt <- sum((x-m)^4/s^4)-3
#   return(c(n=n, mean=m, stdev=s, skew=skew,kurtosis=kurt))
# }
# sapply(data1[vars],mystats)
mytable1.1 <- with(data1, table(gender))
mytable1.2 <- with(data1, table(grade))
mytable1.3 <- with(data1, table(evaluate))
mytable1.1
mytable1.2
mytable1.3
#4.内卷、自我效能感、职业价值观的相关分析#####
states <- data1[,87:89]
#描述变量的均值、标准差
describe(states[,c(1:3)])
#4.1计算皮尔逊积差相关系数
mycor = corr.test(states[,c(1:3)])
#4.2输出下关系矩阵的下三角部分，保留两位小数
lowerMat(mycor$r,digits = 2)
#4.3输出关系系数的显著性检验P值
lowerMat(mycor$p,digits = 4)


# cov(states)
# cor(states)
# cor(states,method = "pearson")
# x <- data1[,c(1)]
# y <- data1[,c(2)]
# z <- data1[,c(3)]
# cor.test(y,z,alternative= "two.sided")

# cor_matrix = cor(states)
# cor_matrix
# df = data.frame(x, y, z)
#5.三步回归检验中介模型####

#Modle 1 自变量对中介变量的影响(内卷 -> 自我效能感)
med.ml <- lm(T2 ~ T1,data = states)
summary(med.ml)

#Modle 2 自变量对因变量的影响
med.m2 <- lm(T3 ~ T1,data = states)
summary(med.m2)

#Modle 3 自变量、中介变量对因变量的影响
med.m3 <- lm(T3 ~ T1 + T2,data = states)
summary(med.m3)
#中介成立，且为完全中介

