library(naivebayes)

data=read.csv("C:/Users/Milosz/Desktop/studia/3_rok/SI/burger-king-menu.csv", header=TRUE, stringsAsFactors=TRUE)
data = data[,-1]

xtabs(~ Category, data=data)

myList <- list()
for (x in 1:100) {
  # generwoanie indexow do samplowania, metoda losowania
  idx=sample(2,77,replace=T,prob=c(0.8,0.2))
  idx
  
  # wyluskiwanie ze zioru z pewnym prawdopodobienstwem
  train=data[idx==1,]
  test=data[idx==2,]
  
  # budowa modelu polegajaca na wzieciu klas w zestawieniu do wszytskich cech
  # model uczymy na danych treningowych
  model=naive_bayes(Category ~ .,data=train)
  plot(model)
  
  # budowanie modelu
  p=predict(model,train[-1])
  tab=table(p,train$Category) #confusion matrix
  
  tab
  
  sum(diag(tab))/sum(tab) #jakość/dobroć klasyfikacji
  
  #-----------------------
  
  # model sprawdzamy na danych testowych
  p1=predict(model,test[-1])
  tab1=table(p1,test$Category) #confusion matrix
  
  tab1
  
  s = sum(diag(tab1))/sum(tab1) #jakość/dobroć klasyfikacji
  
  myList <- append(myList,s)
  

  cat("x", x, ": ", s)
  print("")
}

odchylenie = max(unlist(myList)) - min(unlist(myList))
odchylenie

