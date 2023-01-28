library(ggplot2)

data=read.csv("C:/Users/Milosz/Desktop/studia/3_rok/SI/burger-king-menu.csv", header=TRUE, stringsAsFactors=TRUE)
data = data[,-1]

nor=function(x)
{(x-min(x))/(max(x)-min(x))}

idx=sample(1:nrow(data),0.8*nrow(data))

cl = data$Category

clTrain = cl[idx]
clTest = cl[-idx]

data = data[,-1]
data_norm=as.data.frame(lapply(data, nor))

train = data_norm[idx,]
test = data_norm[-idx,] # jak minus to bierze tablice za wyjatkiem tych elementow

#knn(train, test, kolumna_z_klasami, liczba_sasiadow_do_rozwazenia)
model = knn(train, test,cl=clTrain,k=5) 

#tworzenie confusionMatrix
tab=table(model,clTest)
tab

# funkcja wyliczajaca jakość klasyfikatora
acc=function(x)
{sum(diag(x))/sum(x)}

acc(tab)