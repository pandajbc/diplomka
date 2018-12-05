
#nastavi wd s mnou zvolenou cestou
setwd("C:/Users/katerinas/Desktop/KUBA_NEMAZAT/rko_412")


#nacte data
data_train_ml1<-rbind(read.csv(file="data_train_vse.csv"), read.csv(file="data_imputation_vse.csv"))
data_test_ml1<-read.csv(file="data_test_vse.csv")



#vytvori mci pro trenink a ulozi

names(data_train_ml1)<-c("x","custody","reoffense","violence",
                         "nonenter","sentenced","sentenced_uncond",
                         "sentenced_cond","age","foreigner","female",
                         "unschooled","elementary","highschool","university",
                         "recidivist","firstcase","oca","homicide","robbery",
                         "theft/property","fraud/embezzlement",
                         "driving/administrative","otherviolent",
                         "sexoffenses","againstfamily","publicsafetyorder",
                         "circum_traffic","circum_alcohol","circum_drugs",
                         "circum_finance","circum_femvictim",
                         "circum_childvictim","circum_malevictim",
                         "par_prim","law_prim","article_prim","maxsent_prim",
                         "szid","districtp","soid","year")


data_train_ml1<-data_train_ml1[,-c(1,3,4,5,6,7,8,15,18,27,35,36,
                                   37,38,39,40,42)]

vektor_okresy_train<-data_train_ml1[,25]


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

data_train_ml1<-cbind(data_train_ml1, vektor_okresy_train)

write.csv(data_train_ml1,file="data_train_ml1.csv")



#totez pro test. mnozinu
names(data_test_ml1)<-c("x","custody","reoffense","violence",
                         "nonenter","sentenced","sentenced_uncond",
                         "sentenced_cond","age","foreigner","female",
                         "unschooled","elementary","highschool","university",
                         "recidivist","firstcase","oca","homicide","robbery",
                         "theft/property","fraud/embezzlement",
                         "driving/administrative","otherviolent",
                         "sexoffenses","againstfamily","publicsafetyorder",
                         "circum_traffic","circum_alcohol","circum_drugs",
                         "circum_finance","circum_femvictim",
                         "circum_childvictim","circum_malevictim",
                         "par_prim","law_prim","article_prim","maxsent_prim",
                         "szid","districtp","soid","year")




data_test_ml1<-data_test_ml1[,-c(1,3,4,5,6,7,8,15,18,27,35,36,
                                   37,38,39,40,42)]

vektor_okresy_test<-data_test_ml1[,25]


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

data_test_ml1<-cbind(data_test_ml1, vektor_okresy_test)


write.csv(data_test_ml1,file="data_test_ml1.csv")

#xgboost balik

library(xgboost)

sparse_matrix_ml1 = model.matrix(custody~.-1, data = data_train_ml1[,1:25])
vazba_vektor_ml1 <- data_train_ml1[,"custody"] == 1
vazba_vektor_ml1<-as.matrix(as.numeric(vazba_vektor_ml1))


dtrain_ml1 <- xgb.DMatrix(data = sparse_matrix_ml1,label=vazba_vektor_ml1)
dlabel_ml1<-xgb.DMatrix(data=vazba_vektor_ml1)


#posud to funguje, nize je kod pomoci nehoz prohledam mozne hodnoty parametru
#toto cele nize opakuji pro jine hodnoty nekterych parametru, konkretne
#max_depth z mnoziny 4,6,8; eta 0.01 nebo 0.1; gamma z mnoziny 0,1,10;
#subsample, colsample_bytree a colsample_bylevel muzu nechat takto (jsou to
#rozumny hodnoty); konecne min_child_weight z mnoziny 1,5,10......
#
#Celkem tedy 4*2*3*3=8*9=72 moznych kombinaci parametru
##########################################
#pak vyhodnotim metriku AUC a vemu parametry davajici nejvetsi AUC
#stejne to budu delat i pro ML model 2, tj. pro model zachycujici crime jako
#vysvetlovanou promennou


param_ml1 <- list(objective = "binary:logistic",
                eval_metric = "auc",
                eta = c(.1),
                gamma = c(1), 
                min_child_weight = c(1),
                max_depth = c(8),
                subsample = 0.8,
                colsample_bytree = 0.8, 
                colsample_bylevel=0.8
                )
  cv.nround_ml1 = 100
  cv.nfold_ml1 = 4
  set.seed(1234)
  mdcv_ml1 <- xgb.cv(data=dtrain_ml1, params = param_ml1,  
                 nfold=cv.nfold_ml1, nrounds=cv.nround_ml1,
                 verbose = T, early_stopping_rounds=5, maximize=TRUE)
  
  max_auc_ml1 = max(mdcv_ml1$evaluation_log[, test_auc_mean])
  max_auc_index_ml1 = which.max(mdcv_ml1$evaluation_log[, test_auc_mean])

###########################################################

  
  bst_ml1 <- xgboost(data = dtrain_ml1, label = vazba_vektor_ml1, max.depth = 8,
                 eta = 0.1, gamma = 1, min_child_weight=1,nthread = 4, nrounds = 84
                 ,objective = "binary:logistic")  
  
  
  
importance_ml1 <- xgb.importance( model = bst_ml1)   #relat.dulezitost parametru
head(importance_ml1)

print(bst_ml1)


#zkousky predikce
sparse_matrix2_ml1 = model.matrix(custody~.-1, data = data_test_ml1[,1:25])

vazba_vektor2_ml1<- data_test_ml1[,"custody"] == 1
vazba_vektor2_ml1<-as.matrix(as.numeric(vazba_vektor2_ml1))

dtest_ml1 <- xgb.DMatrix(data = sparse_matrix2_ml1,label=vazba_vektor2_ml1)



pred_ml1 <- predict(bst_ml1, dtest_ml1)
err_ml1 <- mean(as.numeric(pred_ml1 > 0.5) != vazba_vektor2_ml1)
print(paste("test-error=", err_ml1))

####posud to funguje cele, nyni vystupy do latexu a obrazky

library(xtable)

xtable(importance_ml1[,-c(5)],digits=c(0,0,4,4,4))


xgb.plot.tree(model = bst_ml1,tree=83,plot_width = 5000,plot_height = 5000)

xgb.plot.multi.trees(model = bst_ml1,plot_width = 9000,plot_height = 6000)

xgb.ggplot.deepness(model = bst_ml1, which="med.depth")

xgb.plot.importance(importance_matrix = importance_ml1,top_n=6)













