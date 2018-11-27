
#nastavi wd s mnou zvolenou cestou
setwd("C:/Users/katerinas/Desktop/KUBA_NEMAZAT/rko_2411")


#nacte data&ponecha jen pozorovani, kde nic nechybi&vynecha prvni sloupec (s cislem pozorovani)
data<-read.csv(file="data_bezNA.csv")[,2:42]


vazbaCR<-read.csv(file="data_summary.csv")


#nahodne permutuje radky a nasledne rozdeli zakladni soubor na holdout, train, test a imputation sety
#vytvorene datasety pak rovnez ulozi ve formatu csv

set.seed(123)
data_shuffled <- data[sample(nrow(data)),]

data_shuffled$oca <- NULL   #odstrani nadbytecne promenne (pro LNZ promennych) 
data_shuffled$oca9 <- NULL
data_shuffled$dmy_vzd_vys <- NULL

write.csv(data_holdout<-data_shuffled[1:9866,],file="data_holdout.csv")
write.csv(data_train<-data_shuffled[9867:45386, ],file="data_train.csv")
write.csv(data_test<-data_shuffled[45387:63146, ],file="data_test.csv")
write.csv(data_imputation<-data_shuffled[63147:98666,],file="data_imputation.csv")


#zkouska baliku xgboost (boosted gradient trees), jenz je treba stahnout

library(xgboost)

sparse_matrix = model.matrix(vazba~.-1, data = data_train)
vazba_vector <- data_train[,"vazba"] == 1
vazba_vector2<-as.matrix(as.numeric(vazba_vector))



dtrain <- xgb.DMatrix(data = sparse_matrix,label=vazba_vector2)
dlabel<-xgb.DMatrix(data=vazba_vector2)


bst <- xgboost(data = dtrain, label = vazba_vector2, max.depth = 2,
               eta = 1, nthread = 2, nrounds = 5,objective = "binary:logistic")

importance <- xgb.importance( model = bst)   #relat.dulezitost parametru
head(importance)

print(bst)


#zkousky predikce
sparse_matrix2 = model.matrix(vazba~.-1, data = data_test)

vazba_test<- data_test[,"vazba"] == 1
vazba_test2<-as.matrix(as.numeric(vazba_test))

dtest <- xgb.DMatrix(data = sparse_matrix2,label=vazba_test2)



pred <- predict(bst, dtest)
err <- mean(as.numeric(pred > 0.5) != vazba_test2)
print(paste("test-error=", err))


