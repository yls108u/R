#1.從外部匯入資料
setwd("/Users/yls108u/Desktop/R語言/期末報告")    #設定路徑
data = read.csv("avocado.csv", sep=",") #匯入資料

#2.基本敘述統計
head(data, 5)   #前五筆資料
#視覺畫列出欄位資訊
oldpar <- par(mfcol = c(2, 2))
titles <- names(data)
for(i in c(2, 3, 4, 5)){
  hist(x = data[, i], main = paste(titles[i]), xlab = titles[i])
}
par(oldpar)
summary(data)   #資料統計
library(Hmisc)  #使用 Hmisc library
describe(data)  #使用 Hmisc 的內建函數來查看 data summary
#了解欄位間關聯性
cor.target = cor(data)
cor.target 

#3.迴歸分析
glm <- glm(formula = organic ~ AveragePrice, family = "binomial", data = data)
summary(glm)
#模型預測結果
df <- data.frame(AveragePrice = 2.38) #當價格為2.38美元時
result <- predict(glm, newdata = df) 
result

#4.決策樹
require(rpart) #讀取rpart套件
#將資料區分成train:test=8:2
set.seed(22)
train.index <- sample(x=1:nrow(data), size=ceiling(0.8*nrow(data))) 
train <- data[train.index, ]
test <- data[-train.index, ]
#建立cart模型，並把organic欄位當成y，剩下全部欄位當成x
cart.model<- rpart(data$organic ~., data=data, method = "class")
#畫出決策樹
library(rpart.plot)
require(rpart.plot) 
prp(cart.model, faclen=0, fallen.leaves=TRUE, shadow.col="gray", extra=2)
#預測準確度
pred <- predict(cart.model, newdata=test, type="class")
#用table看預測的情況
table(real=test$organic, predict=pred)
#計算預測準確率 = 對角線的數量/總數量
confus.matrix <- table(real=test$organic, predict=pred)
sum(diag(confus.matrix))/sum(confus.matrix)

#5.群集
#dev.new()
# 歐式距離
E.dist <- dist(data[,-6], method="euclidean") 
h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="Euclidean distance")
#曼哈頓距離
M.dist <- dist(data[,-6], method="manhattan")
h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="Manhattan distance")
#歐式距離與華德法
h.cluster <- hclust(E.dist, method="ward.D2") 
plot(h.cluster)
abline(h=2100000, col="red")
#分群結果
cut.h.cluster <- cutree(h.cluster, k=2)  #分2群
cut.h.cluster
table(cut.h.cluster, data$organic) #分群結果和實際結果比較
#切割式分群
kmeans.cluster <- kmeans(data[,-6], centers=2) #分2群 
kmeans.cluster$withinss #群內的變異數
table(kmeans.cluster$cluster, data$organic) #分群結果

#6.迴歸分析average price & large size avocado
library(ggplot2)
price <- data$AveragePrice
large <- data$Large
LM <- lm(price ~ large, data = data)
ggplot(data, aes(x = price, y = large)) + 
  geom_point(shape = 10, size = 5) + 
  geom_smooth(method = lm) + 
  labs(x = "price", y = "large size")
summary(LM) #取得方程式參數



