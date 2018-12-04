
#nastavi wd s mnou zvolenou cestou
setwd("C:/Users/katerinas/Desktop/KUBA_NEMAZAT/rko_412")


#nacte data
data_train_ml1<-rbind(read.csv(file="data_train_vse.csv"), read.csv(file="data_imputation_vse.csv"))
data_test_ml1<-read.csv(file="data_test_vse.csv")
data_holdout_vse<-read.csv(file="data_holdout_vse.csv")



#vytvori mci pro trenink a ulozi

data_train_ml1<-data_train_ml1[,-c(1,3,4,5,6,7,8,15,18,27,35,36,
                                   37,38,39,40,42)]

vektor_okresy<-data_train_ml1[,25]


data_s_kraji<-read.csv(file="data_workout_nofactor2.csv")

levels(data_train_ml1$soid) <- c("JIC", "JIM", "SCE", "SEM", "STC","VYC", "ZPC", "PHA",
                               "JIC","JIC","JIC","JIC","JIC","JIC","JIC","JIC","JIM",
                               "JIM","JIM","JIM","JIM","JIM","JIM","JIM","JIM","JIM",
                               "JIM","JIM","PHA","PHA","PHA","PHA","PHA","PHA","PHA",
                               "PHA","PHA","PHA","SCE","SCE","SCE","SCE","SCE","SCE",
                               "SCE","SCE","SCE","SCE","SEM","SEM","SEM","SEM","SEM",
                               "SEM","SEM","SEM","SEM","SEM","SEM","STC","STC","STC",
                               "STC","STC","STC","STC","STC","STC","STC","STC","STC",
                               "VYC","VYC","VYC","VYC","VYC","VYC","VYC","VYC","VYC",
                               "VYC","ZPC","ZPC","ZPC","ZPC","ZPC","ZPC","ZPC","ZPC",
                               "ZPC","ZPC")

data_train_ml1<-cbind(data_train_ml1, vektor_okresy)

write.csv(data_train_ml1,file="data_train_ml1.csv")

#totez pro test. mnozinu
data_test_ml1<-data_test_ml1[,-c(1,3,4,5,6,7,8,15,18,27,35,36,
                                   37,38,39,40,42)]

vektor_okresy2<-data_test_ml1[,25]


levels(data_test_ml1$soid) <- c("JIC", "JIM", "SCE", "SEM", "STC","VYC", "ZPC", "PHA",
                                 "JIC","JIC","JIC","JIC","JIC","JIC","JIC","JIC","JIM",
                                 "JIM","JIM","JIM","JIM","JIM","JIM","JIM","JIM","JIM",
                                 "JIM","JIM","PHA","PHA","PHA","PHA","PHA","PHA","PHA",
                                 "PHA","PHA","PHA","SCE","SCE","SCE","SCE","SCE","SCE",
                                 "SCE","SCE","SCE","SCE","SEM","SEM","SEM","SEM","SEM",
                                 "SEM","SEM","SEM","SEM","SEM","SEM","STC","STC","STC",
                                 "STC","STC","STC","STC","STC","STC","STC","STC","STC",
                                 "VYC","VYC","VYC","VYC","VYC","VYC","VYC","VYC","VYC",
                                 "VYC","ZPC","ZPC","ZPC","ZPC","ZPC","ZPC","ZPC","ZPC",
                                 "ZPC","ZPC")

data_test_ml1<-cbind(data_test_ml1, vektor_okresy2)


data_train_ml1<-cbind(data_train_ml1,c("JIC", "JIM", "SCE", "SEM", "STC","VYC", "ZPC", "PHA",
  "JIC","JIC","JIC","JIC","JIC","JIC","JIC","JIC","JIM",
  "JIM","JIM","JIM","JIM","JIM","JIM","JIM","JIM","JIM",
  "JIM","JIM","PHA","PHA","PHA","PHA","PHA","PHA","PHA",
  "PHA","PHA","PHA","SCE","SCE","SCE","SCE","SCE","SCE",
  "SCE","SCE","SCE","SCE","SEM","SEM","SEM","SEM","SEM",
  "SEM","SEM","SEM","SEM","SEM","SEM","STC","STC","STC",
  "STC","STC","STC","STC","STC","STC","STC","STC","STC",
  "VYC","VYC","VYC","VYC","VYC","VYC","VYC","VYC","VYC",
  "VYC","ZPC","ZPC","ZPC","ZPC","ZPC","ZPC","ZPC","ZPC",
  "ZPC","ZPC"))

write.csv(data_test_ml1,file="data_test_ml1.csv")

#xgboost balik

library(xgboost)

sparse_matrix_zkouska = model.matrix(vazba~.-1, data = data_train_ml1[,1:25])
vazba_vector_zkouska <- data_train_ml1[,"vazba"] == 1
vazba_vector2_zkouska<-as.matrix(as.numeric(vazba_vector_zkouska))


dtrain <- xgb.DMatrix(data = sparse_matrix_zkouska,label=vazba_vector2_zkouska)
dlabel<-xgb.DMatrix(data=vazba_vector2_zkouska)

#bstcv <- xgb.cv(data = dtrain, label = vazba_vector2_zkouska, nfold = 5,
#                nrounds = 100, objective = "binary:logistic",
#                early_stopping_rounds = 5, maximize = FALSE)

#posud to funguje, nize je kod pomoci nehoz prohledam mozne hodnoty parametru
#toto cele nize opakuji pro jine hodnoty nekterych parametru, konkretne
#max_depth z mnoziny 4,6,8; eta 0.01 nebo 0.1; gamma z mnoziny 0,1,10;
#subsample, colsample_bytree a colsample_bylevel muzu nechat takto (jsou to
#rozumny hodnoty); konecne min_child_weight z mnoziny 1,5,10......
#
#Celkem tedy 3*2*3*3=6*9=54 moznych kombinaci parametru
##########################################
#pak vyhodnotim metriku AUC a vemu parametry davajici nejvetsi AUC
#stejne to budu delat i pro ML model 2, tj. pro model zachycujici crime jako
#vysvetlovanou promennou



best_param = list()
best_seednumber = 1234
best_auc = 0
best_auc_index = 0


param <- list(objective = "binary:logistic",
                eval_metric = "auc",
                max_depth = c(3),
                eta = c(.01),
                gamma = c(1), 
                subsample = 0.8,
                colsample_bytree = 0.8, 
                min_child_weight = c(1),
                colsample_bylevel=0.8
                )
  cv.nround = 100
  cv.nfold = 4
  set.seed(1234)
  mdcv <- xgb.cv(data=dtrain, params = param,  
                 nfold=cv.nfold, nrounds=cv.nround,
                 verbose = T, early_stopping_rounds=5, maximize=TRUE)
  
  max_auc = max(mdcv$evaluation_log[, test_auc_mean])
  max_auc_index = which.max(mdcv$evaluation_log[, test_auc_mean])
  
  if (max_auc > best_auc) {
    best_auc = max_auc
    best_auc_index = max_auc_index
    best_seednumber = seed.number
    best_param = param
  }

nround = best_auc_index






#toto komentovane je jen pokus...
#############################################################
#provede crossvalidaci (nfold=5) pro ziskani optimalniho nrounds,
#ostatni parametry lze nastavit rucne


#bstcv <- xgb.cv(data = train$data, label = train$label, nfold = 5,
#              nrounds = 20, objective = "binary:logistic",
#              early.stop.round = 3, maximize = FALSE)
#################################################################




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


