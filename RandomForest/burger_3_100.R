library("party")
library(randomForest)

data=read.csv("C:/Users/Milosz/Desktop/studia/3_rok/SI/burger-king-menu.csv", header=TRUE, stringsAsFactors=TRUE)
data = data[,-1]
x <- 1

myList <- list()
while (x <= 100) {
  idx=sample(2, length(data), replace = TRUE, prob = c(0.9, 0.1))
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
  
  if (q != 'NaN'){
    myList <- append(myList,q)
    
  } else {
    x = x-1
  }
  
  cat("x", x, ": ", q, q=='NaN')
  print("")
  
  x = x+1

}
srednia = mean(unlist(myList))
srednia
odchylenie = sd(unlist(myList))
odchylenie