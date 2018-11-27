
#nacte a dale vyuziva balicek glmnet, jenz je treba stahnout  

library(glmnet)                   

#setwd("C:/Users/katerinas/Desktop/KUBA_NEMAZAT/rko_2411")

data<-read.csv(file="data_bezNA.csv")[,2:42]

data_shuffled <- data[sample(nrow(data)),]

data_shuffled$oca <- NULL   #odstrani nadbytecne promenne (pro LNZ promennych) 
data_shuffled$oca9 <- NULL
data_shuffled$dmy_vzd_vys <- NULL

data_holdout<-data_shuffled[1:9866,]
data_train<-data_shuffled[9867:45386, ]
data_test<-data_shuffled[45387:63146, ]
data_imputation<-data_shuffled[63147:98666,]




sparse_matrix = model.matrix(vazba~.-1, data = data_train)
vazba_vector <- data_train[,"vazba"] == 1
vazba_vector2<-as.matrix(as.numeric(vazba_vector))


fit = glmnet(sparse_matrix, vazba_vector2, family = "binomial",alpha=0.5)

plot(fit)
print(fit)



cvfit = cv.glmnet(sparse_matrix, vazba_vector2, family = "binomial", type.measure = "class")

plot(cvfit)
