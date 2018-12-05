
#nastavi wd s mnou zvolenou cestou
setwd("C:/Users/katerinas/Desktop/KUBA_NEMAZAT/rko_412")


#nacte data
data_train_ml2<-rbind(read.csv(file="data_train_vse.csv"), read.csv(file="data_imputation_vse.csv"))

data_test_ml2<-read.csv(file="data_test_vse.csv")




#vytvori mci pro trenink a ulozi

names(data_train_ml2)<-c("x","custody","reoffense","violence",
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


vektor_okresy_train_ml2<-data_train_ml2[,41]

vektor_custody_train_ml2<-data_train_ml2[,2]


data_train_ml2<-data_train_ml2[,-c(1,2,3,5,6,7,8,15,18,27,35,36,
                                   37,38,39,40,42)]





levels(data_train_ml2$soid) <- c("JIC", "JIM", "SCE", "SEM", "STC","VYC", "ZPC", "PHA",
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

data_train_ml2<-cbind(data_train_ml2, vektor_okresy_train_ml2)

write.csv(data_train_ml2,file="data_train_ml2.csv")



#totez pro test. mnozinu
names(data_test_ml2)<-c("x","custody","reoffense","violence",
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


vektor_okresy_test_ml2<-data_test_ml2[,41]

vektor_custody_test_ml2<-data_test_ml2[,2]



data_test_ml2<-data_test_ml2[,-c(1,2,3,5,6,7,8,15,18,27,35,36,
                                 37,38,39,40,42)]




levels(data_test_ml2$soid) <- c("JIC", "JIM", "SCE", "SEM", "STC","VYC", "ZPC", "PHA",
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

data_test_ml2<-cbind(data_test_ml2, vektor_okresy_test_ml2)


write.csv(data_test_ml2,file="data_test_ml2.csv")

#xgboost balik

library(xgboost)

sparse_matrix_ml2 = model.matrix(violence~.-1, data = data_train_ml2[,1:25])
crime_vektor_ml2 <- data_train_ml2[,"violence"] == 1
crime_vektor_ml2<-as.matrix(as.numeric(crime_vektor_ml2))


dtrain_ml2 <- xgb.DMatrix(data = sparse_matrix_ml2,label=crime_vektor_ml2)
dlabel_ml2<-xgb.DMatrix(data=crime_vektor_ml2)


#viz xgboost_ML2

param_ml2 <- list(objective = "binary:logistic",
                  eval_metric = "auc",
                  eta = c(.01),
                  gamma = c(1), 
                  min_child_weight = c(1),
                  max_depth = c(8),
                  subsample = 0.8,
                  colsample_bytree = 0.8, 
                  colsample_bylevel=0.8
)
cv.nround_ml2 = 100
cv.nfold_ml2 = 4
set.seed(1234)
mdcv_ml2 <- xgb.cv(data=dtrain_ml2, params = param_ml2,  
                   nfold=cv.nfold_ml2, nrounds=cv.nround_ml2,
                   verbose = T, early_stopping_rounds=5, maximize=TRUE)

max_auc_ml2 = max(mdcv_ml2$evaluation_log[, test_auc_mean])
max_auc_index_ml2 = which.max(mdcv_ml2$evaluation_log[, test_auc_mean])

###########################################################


bst_ml2 <- xgboost(data = dtrain_ml2, label = crime_vektor_ml2, max.depth = 8,
                   eta = 0.1, gamma = 1, min_child_weight=1,nthread = 4, nrounds = 84
                   ,objective = "binary:logistic")  



importance_ml2 <- xgb.importance( model = bst_ml2)   #relat.dulezitost parametru
head(importance_ml2)

print(bst_ml2)


#zkousky predikce
sparse_matrix2_ml2 = model.matrix(violence~.-1, data = data_test_ml2[,1:25])

crime_vektor2_ml2<- data_test_ml2[,"violence"] == 1
crime_vektor2_ml2<-as.matrix(as.numeric(crime_vektor2_ml2))

dtest_ml2 <- xgb.DMatrix(data = sparse_matrix2_ml2,label=crime_vektor2_ml2)

pred_ml2 <- predict(bst_ml2, dtest_ml2)
err_ml2 <- mean(as.numeric(pred_ml2 > 0.5) != crime_vektor2_ml2)
print(paste("test-error=", err_ml2))

####posud to funguje cele, nyni vystupy do latexu a obrazky

library(xtable)

xtable(importance_ml2,digits=c(0,0,4,4,4))


xgb.plot.tree(model = bst_ml2,tree=83,plot_width = 5000,plot_height = 5000)

xgb.plot.multi.trees(model = bst_ml2,plot_width = 9000,plot_height = 6000)

xgb.ggplot.deepness(model = bst_ml2, which="med.depth")

xgb.plot.importance(importance_matrix = importance_ml2,top_n=6)
