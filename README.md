# diplomka
Repository for a master thesis conducted at VSE;      

ZDE JIZ FUNKCNI XGBOOST (i ml1 - s vazbou jako vysvetlovanou i ml2 - se zlocinem jako vysvetlovanou)..soubory   
xgboost_ml1_renaming a xgboost_ml2 obsahuji Rkovej skript, pricemz ono renamed znamena, ze jsem tam prejmenoval promenne na   
anglicke zkratky (napr. aby v tech grafickych vystupech nebylo "dmy_cizinec" ale "foreigner" apod.)...jeste doladim parametry pro model ml2 (hledam to nakonec pres mnoziny rozumnejch hodnot - viz uvedene R soubory) ale heuristicky to vypada docela ok...jinak jsem zkousel i 10fold crossvalidaci, ale vysledky (vzajemne relace (<,>,"o kolik") auc mezi urcitymi volbami parametru) jsou obdobny, takze dle me staci 4fold nebo 5 fold bohate


UZIVAM HLAVNE DATASETY: data_train_vse; data_imputation_vse (nadbytecnej, ten binduju pro ucely treninku s data_train_vse, je to   pozustatek toho, kdy jsem chtel delat i imputaci pro reranking, ale na to neni cas); data_test_vse....viz Rkovy skripty, kde z techto jsem schopen vyextrahovat matici pro trenink xgboostu, udelat kraje (z 91 levelu promenne soid muzu udelat 8 				novych dle regionu (cca jihocesky, severomoravsky a tak podobne - "analogicky" ke krajim v CR, jen jich je mene, cili 				jim budu rikat "kraje")) atd.....   


Potrebuju od tebe prosim ZDROJ DATA_micro i DATA_summary  


INFO O DANYCH SEKCICH: je v TeXu..., mrkni, zda to souhlasi s tvou predstavou (aktualne v ML model1 nebudu asi delat komparaci s   
logitem, ale je to asi zbytecny, spis me napadlo (coz i udelam, melo by to bejt trivialni) ukazat, zda-li by nas ML model 1 (jakysi judge_predicted) dokazal byt lepsi nez realni soudci, a to tak, ze seradim dle custody_predicted a opet zacnu uveznovat lidi od tech s nejvetsi ppsti uvezneni...zde muzu dodat ze bez kontrakce neznam counterfactual a udelat opet onu kontrakci...  
pozn: doslo mi, ze to dela Kleinberg v sekci VI.C, nicmene prijde mi to jako dobre...navic nemam soudce oznacene na uroven jednotlivcu, cili v mem pripade by dobry vsledek mohl byt vysledek "crowd wisdom efektu", coz ale neva, aspon je to jedna z veci, na ktere mohu upozornit jako na mozne problemy a zaroven mozna rozsireni do budoucna...  

  
data_bezNA jsou tva data_micro, jen bez NA pozorovani a data_summary jsou tva data_summary; zbyvajici datasety jsou vytvoreny v Rku vzdy pro dany ucel (napr. pro popis. stat. datasetu odstraneny factor. promenne jako szid (moc kategorii) atd.)    
Pozn: (i)  vytvoril jsem z 91 levelu promenne soid 8 novych - "kraje", jejich rel. frekvenci jsem pak zahrnul do popis. statistik...do modelu bychom myslim nemeli promenne, ktere ty jsi nazval jako "oznaceni oblasti a roku" davat (ani do blackboxu), mozna vyjma soid (takto prekodovanych, musi se to predelat na dummy a mit tam 90 dummy navic je pomerne moc,   
navic myslim, ze by to ani mezi vysvetlovanymi byt nemelo, jeste se rozhodnu
      (ii) ad (i): Uvazuju i, ze bych jako vysvetlujici nepouzil ani ony "kraje", zkousel jsem to a auc (metrika kt. pouzivam)
      vychazi hur (zatim) pro model bez kraju (to je intuitivni - xgboost ma mene dat ze kt. delat model), ale zkusim to behem dneska vyladit a nejak se rozhodnu (vlastne resim, zda nevadi mit kraje (shrnujici v podstate soid) jako vysvetlujici promennou modeu, a pak radit vysledky takto zhotoveneho modelu dle mean(crime_predicted) a dle kategorii soid...pokud myslis, ze ne, tak to tak necham, pokud myslis ze ano (a napises mi to nejak rychle), tak je vynecham...ta auc vxchazi treba 0,75 resp 0,738 - cili to o tolik horsi bez kraju neni)



VECI NA DODELANI (mysleno spis formalni, taky dle toho jak se bude stihat): spravna katedra, zadani, nazvy (sub-)kapitol, popisy grafu a tabulek (velikost pisma, cisla, font atd., pozor napr. na tabulku summary stats, ktera je udelana ze dvou a ta druha ma mnou dodany cislovani (aby ji to nezapocitalo), cili na konci prace toto zkontrolovat, zda je spravne), biblio (i sjednotit styl - napr. odkazy v textu, ci u obrazku, kde chci mit jen napr. (Crowley, 1966) a v bibliu pak kompletni url/zdroj), zdroj vazebnich dat (jak tech data_summary tak data_micro) - rocenka?, opravit pomlcky a spojovniky, zkontrolovat anglicke uvozovky (dvojite se pisou alt+0147 resp. alt+0148, jednoduche 0145 a 0146), smazat cesky kecy v textu :D, latinu (prip. jine jazyky) dat do kurzivy, do priloh dat mimojine seznam promennych s vysvetlenim co dana promenna znamena (at to nemusim vysvetlovat v textu, ve kterem se jen odkazu na tuto prilohu), dodat pevne mezery (kde budou potreba) a zarovnat popisy tabulek a grafu do bloku (staci vnorit prostredi quotation), tabulka summary doplnit pocty released&detained a obecne dodelat,dat si pozor na preklepy jako: at first misto as first, heigth misto height, stringer (stringest) misto stricter (strictest) apod.
