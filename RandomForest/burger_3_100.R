library("party")
library(randomForest)

data=read.csv("C:/Users/Milosz/Desktop/studia/3_rok/SI/burger-king-menu.csv", header=TRUE, stringsAsFactors=TRUE)
data = data[,-1]

myList <- list()
for (x in 1:100) {
  idx=sample(2, length(data), replace = TRUE, prob = c(0.7, 0.3))
  idx
  train = data[idx == 1,]
  test = data[idx == 2,]
  
  model = randomForest(Category~.,data=train)
  
  randomForest::varImpPlot(model,
                           sort=FALSE,
                           main="Variable Importance Plot")
  
  pred=predict(model,test)
  cm=table(pred,test$Category)
  cm
  q = sum(diag(cm))/sum(cm)
  q
  
  myList <- append(myList,q)
  
  
  cat("x", x, ": ", q)
  print("")
}
srednia = mean(unlist(myList))
srednia
odchylenie = sd(unlist(myList))
odchylenie