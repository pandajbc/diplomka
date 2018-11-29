#nastavi wd s mnou zvolenou cestou
setwd("C:/Users/katerinas/Desktop/KUBA_NEMAZAT/rko_2911")


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


#rozdeli dataset na propustene a nepropustene, prekoduje soid do 8 oblasti (jakobykraju)


levels(released$soid)
levels(data_workout$soid) <- c("JIC", "JIM", "SCE", "SEM", "STC","VYC", "ZPC", "PHA",
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



released<- subset(data_workout, vazba==0)[,2:42]
nonreleased<- subset(data_workout, vazba==1)[,2:42]


write.csv(released,file="data_released_recod.csv")
write.csv(nonreleased,file="data_nonreleased_recod.csv")







released2<-released[,1:16]
released3<-released[,18:33]
released4<-released[,40:41]
released_nofactor2<-cbind(released2,released3,released4)

write.csv(released_nofactor2,file="data_released_nofactor2.csv")


nonreleased2<-nonreleased[,1:16]
nonreleased3<-nonreleased[,18:33]
nonreleased4<-nonreleased[,40:41]
nonreleased_nofactor2<-cbind(nonreleased2,nonreleased3,nonreleased4)

write.csv(nonreleased_nofactor2,file="data_nonreleased_nofactor2.csv")

data_workout2<-data_workout[,2:17]
data_workout3<-data_workout[,19:34]
data_workout4<-data_workout[,41:42]
data_workout_nofactor2=cbind(data_workout2,data_workout3,data_workout4)

write.csv(data_workout_nofactor2,file="data_workout_nofactor2.csv")




#zjisti relativni cetnosti kategorii soudu
poctysoudu<-count(data_workout_nofactor2$soid)
poctysoudu2<-poctysoudu[,2]
relat_freq_soid<-lapply(poctysoudu2, function(x) x/88000)

poctysoudur<-count(released_nofactor2$soid)
poctysoudu2r<-poctysoudur[,2]
relat_freq_soidr<-lapply(poctysoudu2r, function(x) x/88000)

poctysoudun<-count(nonreleased_nofactor2$soid)
poctysoudu2n<-poctysoudun[,2]
relat_freq_soidn<-lapply(poctysoudu2n, function(x) x/88000)




#nasledujici prikaz svaze s vektorem s hodnotami 1az8, phodnoty sedaji vypocitat 
#rucne, cili techto 8 cislic pak nahradim 8 phodnotami (nulami)


rel_freq_all<-cbind(relat_freq_soid, relat_freq_soidr, relat_freq_soidn,
                    rbind(1,2,3,4,5,6,7,8))



prumery_celkem2<-colMeans(data_workout_nofactor2 [,1:32])
prumery_released2<-colMeans(released_nofactor2 [,1:32])
prumery_noreleased2<-colMeans(nonreleased_nofactor2 [,1:32])

pr_c<-t(t(prumery_celkem2))
pr_r<-t(t(prumery_released2))
pr_n<-t(t(prumery_noreleased2))

tabulka<-cbind(pr_c,pr_r,pr_n)


#toto jen zkouska
#pr_c2<-t(t(prumery_celkem2))
#pr_r2<-t(t(prumery_released2))
#pr_n2<-t(t(prumery_noreleased2))

#tabulka2<-cbind(pr_c2, pr_r2, pr_n2)

#xtable(tabulka2)



#ttesty (nejde mi pres apply, nedejbuh smyckou, takze takle vidlacky)

ttest1<-t.test(released_nofactor[,5],nonreleased_nofactor[,5]) 
phod1<-ttest1$p.value

ttest2<-t.test(released_nofactor[,6],nonreleased_nofactor[,6]) 
phod2<-ttest2$p.value

ttest3<-t.test(released_nofactor[,7],nonreleased_nofactor[,7]) 
phod3<-ttest3$p.value

ttest4<-t.test(released_nofactor[,7],nonreleased_nofactor[,7]) 
phod4<-ttest4$p.value

ttest5<-t.test(released_nofactor[,8],nonreleased_nofactor[,8]) 
phod5<-ttest5$p.value

ttest6<-t.test(released_nofactor[,9],nonreleased_nofactor[,9]) 
phod6<-ttest6$p.value

ttest7<-t.test(released_nofactor[,10],nonreleased_nofactor[,10]) 
phod7<-ttest7$p.value

ttest8<-t.test(released_nofactor[,11],nonreleased_nofactor[,11]) 
phod8<-ttest8$p.value

ttest9<-t.test(released_nofactor[,12],nonreleased_nofactor[,12]) 
phod9<-ttest9$p.value

ttest10<-t.test(released_nofactor[,13],nonreleased_nofactor[,13]) 
phod10<-ttest10$p.value

ttest11<-t.test(released_nofactor[,14],nonreleased_nofactor[,14]) 
phod11<-ttest11$p.value

ttest12<-t.test(released_nofactor[,15],nonreleased_nofactor[,15]) 
phod12<-ttest12$p.value

ttest13<-t.test(released_nofactor[,16],nonreleased_nofactor[,16]) 
phod13<-ttest13$p.value

ttest14<-t.test(released_nofactor[,17],nonreleased_nofactor[,17]) 
phod14<-ttest14$p.value

ttest15<-t.test(released_nofactor[,18],nonreleased_nofactor[,18]) 
phod15<-ttest15$p.value

ttest16<-t.test(released_nofactor[,19],nonreleased_nofactor[,19]) 
phod16<-ttest16$p.value

ttest17<-t.test(released_nofactor[,20],nonreleased_nofactor[,20]) 
phod17<-ttest17$p.value

ttest18<-t.test(released_nofactor[,21],nonreleased_nofactor[,21]) 
phod18<-ttest18$p.value

ttest19<-t.test(released_nofactor[,22],nonreleased_nofactor[,22]) 
phod19<-ttest19$p.value

ttest20<-t.test(released_nofactor[,23],nonreleased_nofactor[,23]) 
phod20<-ttest20$p.value

ttest21<-t.test(released_nofactor[,24],nonreleased_nofactor[,24]) 
phod21<-ttest21$p.value

ttest22<-t.test(released_nofactor[,25],nonreleased_nofactor[,25]) 
phod22<-ttest22$p.value

ttest23<-t.test(released_nofactor[,26],nonreleased_nofactor[,26]) 
phod23<-ttest23$p.value

ttest24<-t.test(released_nofactor[,27],nonreleased_nofactor[,27]) 
phod24<-ttest24$p.value

ttest25<-t.test(released_nofactor[,28],nonreleased_nofactor[,28]) 
phod25<-ttest25$p.value

ttest26<-t.test(released_nofactor[,29],nonreleased_nofactor[,29]) 
phod26<-ttest26$p.value

ttest27<-t.test(released_nofactor[,30],nonreleased_nofactor[,30]) 
phod27<-ttest27$p.value

ttest28<-t.test(released_nofactor[,31],nonreleased_nofactor[,31]) 
phod28<-ttest28$p.value

ttest29<-t.test(released_nofactor[,32],nonreleased_nofactor[,32]) 
phod29<-ttest29$p.value

ttest30<-t.test(released_nofactor[,33],nonreleased_nofactor[,33]) 
phod30<-ttest30$p.value

vektor_phod<-rbind(0, 0,0,phod1, phod2,phod3,phod4,phod5,phod6,phod7,phod8,phod9,phod10,
                   phod11,phod12,phod13,phod14,phod15,phod16,phod17,phod18,phod19,phod20,
                   phod21,phod22,phod23,phod24,phod24,phod26,phod27,phod28,phod29,phod30)

tab_popis<-cbind(tabulka,vektor_phod)[1:32,]
tab_popis2<-rbind(tab_popis, rel_freq_all)


tab_popisxxx<-cbind(tabulka2,vektor_phod)[1:32,]
tab_popis2xxx<-rbind(tab_popisxxx, rel_freq_all)



#toto nize vytvori prozatim finalni tabulku...

write.csv(tab_popis2,file="tab_popis_workout2.csv")

xtable(tab_popis2)        

#tabulka k rozdeleni datasetu


table_rozdeleni_dataset<-cbind(rbind("Full dataset","Hold-out sample",
                                     "Workout set", "Training set", "Test set",
                                     "Imputation set" ),rbind(98666,9866,88800,
                               35520,35520,17760), rbind(1,0.1,0.9,0.36,0.36,
                                                         0.18))
xtable(table_rozdeleni_dataset)






