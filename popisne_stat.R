#nastavi wd s mnou zvolenou cestou
setwd("C:/Users/katerinas/Desktop/KUBA_NEMAZAT/rko_2711")


#######################################################
#Summary stats of custody in CR########################
#######################################################


data_vazba_celkem<-read.csv(file="data_summary.csv")


#nacte balik pro tvorbu latex tabulek primo v Rku

library(xtable)

summary(data_vazba_celkem)

xtable(summary(data_vazba_celkem))


xtable(data_vazba_celkem)

plot(data_vazba_celkem[,1:2])


#nacte baliky pro kresleni a spravu fontu

library(ggplot2)
library(extrafont)
font_import()
loadfonts(device = "win")


plot1<-ggplot(data_vazba_celkem, aes(year)) + 
  geom_line(aes(y = vazba, colour = "Imposed custody")) + 
  geom_line(aes(y = uvazovano, colour = "Considered custody")) + 
  xlab("Year") + ylab("Number of cases") +
  theme(text=element_text(size=11,  family="serif")) + 
  scale_color_manual(values = c("blue", "red", "red"),name=element_blank()) 

print(plot1)

#######################################################
#Summary stats of used dataset#########################
#######################################################

data<-read.csv(file="data_bezNA.csv")[,2:42]

set.seed(123)
data_shuffled <- data[sample(nrow(data)),]

write.csv(data_shuffled,file="data_shuffled.csv")

#ulozi vytvorene sety jako csv, zustavaji vsechny promenne (neodstranuji oca9 atd)


write.csv(data_shuffled[1:9866,],file="data_holdout_vse.csv")
write.csv(data_shuffled[9867:45386, ],file="data_train_vse.csv")
write.csv(data_shuffled[45387:63146, ],file="data_test_vse.csv")
write.csv(data_shuffled[63147:98666,],file="data_imputation_vse.csv")




data_train_vse<-read.csv(file="data_train_vse.csv")
data_test_vse<-read.csv(file="data_test_vse.csv")
data_imputation_vse<-read.csv(file="data_imputation_vse.csv")
data_holdout_vse<-read.csv(file="data_holdout_vse.csv")
data_workout<-rbind(data_train_vse, data_test_vse, data_imputation_vse)


#xtable(data_workout)
#xtable(summary(data_workout))


#rozdeli dataset na propustene a nepropustene, chce to rozmyslet promenne


released<- subset(data_workout, vazba==0)[,2:42]
nonreleased<- subset(data_workout, vazba==1)[,2:42]


write.csv(released,file="data_released.csv")
write.csv(nonreleased,file="data_nonreleased.csv")



released2<-released[,1:16]
released3<-released[,18:33]
released4<-released[,41]
released_nofactor<-cbind(released2,released3,released4)

write.csv(released_nofactor,file="data_released_nofactor.csv")


nonreleased2<-nonreleased[,1:16]
nonreleased3<-nonreleased[,18:33]
nonreleased4<-nonreleased[,41]
nonreleased_nofactor<-cbind(nonreleased2,nonreleased3,nonreleased4)

write.csv(nonreleased_nofactor,file="data_nonreleased_nofactor.csv")

data_workout2<-data_workout[,2:17]
data_workout3<-data_workout[,19:34]
data_workout4<-data_workout[,42]
data_workout_nofactor=cbind(data_workout2,data_workout3,data_workout4)

write.csv(data_workout_nofactor,file="data_workout_nofactor.csv")




prumery_celkem<-colMeans(data_workout_nofactor)
prumery_released<-colMeans(released_nofactor)
prumery_noreleased<-colMeans(nonreleased_nofactor)

pr_c<-t(t(prumery_celkem))
pr_r<-t(t(prumery_released))
pr_n<-t(t(prumery_noreleased))

tabulka<-cbind(pr_c, pr_r, pr_n)

xtable(tabulka)

#zde udelam ttesty, nejde mi to loopnout, takze asi vidlacky jeden po druhym,
# to je zalezitost 15minut...

ttest1<-t.test(released_nofactor[,5],nonreleased_nofactor[,5]) 
phod1<-ttest1$p.value

ttest2<-t.test(released_nofactor[,6],nonreleased_nofactor[,6]) 
phod2<-ttest2$p.value



#staci dodelat phodnoty, bindnout k objektu "tabulka" (ten jeste prejmenuju)
#a hodit do latexu








