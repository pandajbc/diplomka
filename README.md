# diplomka
Repository for a master thesis conducted at VSE;      

ZDE JIZ FUNKCNI XGBOOST (i ml1 - s vazbou jako vysvetlovanou i ml2 - se zlocinem jako vysvetlovanou)..soubory   
xgboost_ml1_renamed a xgboost_ml2 obs. Rkovej skript



V texovym dokumentu jsou informace o danych sekcich, jak si to predstavuji, mrkni, zda to souhlasi s tvou predstavou. 
data_bezNA jsou tva data_micro, jen bez NA pozorovani a data_summary jsou tva data_summary; zbyvajici datasety jsou vytvoreny v Rku vzdy pro dany ucel (napr. pro popis. stat. datasetu odstraneny factor. promenne jako szid (moc kategorii) atd.)    
Pozn: vytvoril jsem z 91 levelu promenne soid 8 novych, jejich rel. frekvenci jsem pak zahrnul do popis. statistik...do modelu bychom myslim nemeli promenne, ktere ty jsi nazval jako "oznaceni oblasti a roku" davat (ani do blackboxu), mozna vyjma soid, se kterym budeme muset pracovat v ramci onech leniency trid...jinak by nedavalo smysl dle me aby soudce bral v uvahu napriklad szid (a je pravdou, ze treba  Kleinberg to tam taky nema) ci rok...navic ty stromy potrebuji prekodovat kategoricky promenny na dummy, cili cim mene kategorickych promennych (resp. aspon co nejmene kategorii nejake kat. promenne, kt. je nemozne vynechat (soid)) tim "lepe"..
  

Veci na dodelani (mysleno spis formalni, taky dle toho jak se bude stihat): spravna katedra, zadani, nazvy (sub-)kapitol, popisy grafu a tabulek (velikost pisma, cisla, font atd., pozor napr. na tabulku summary stats, ktera je udelana ze dvou a ta druha ma mnou dodany cislovani (aby ji to nezapocitalo), cili na konci prace toto zkontrolovat, zda je spravne), biblio (i sjednotit styl - napr. odkazy v textu, ci u obrazku, kde je otazkou, zda je idealni uvadet cely url odkaz uz u obrazku/tabulky nebo zda je lepsi uvest jen napr. "brownlee 2018" a v biblio mit pak cely odkaz i s datem citace), zdroj vazebnich dat (jak tech data_summary tak data_micro) - rocenka?, opravit pomlcky a spojovniky, zkontrolovat anglicke uvozovky (dvojite se pisou alt+0147 resp. alt+0148, jednoduche 0145 a 0146), smazat cesky kecy v textu :D, latinu (prip. jine jazyky) dat do kurzivy, do priloh dat mimojine seznam promennych s vysvetlenim co dana promenna znamena (at to nemusim vysvetlovat v textu, ve kterem se jen odkazu na tuto prilohu), dodat pevne mezery (kde budou potreba) a zarovnat popisy tabulek a grafu do bloku (staci vnorit prostredi quotation), tabulka summary doplnit pocty released&detained a obecne dodelat,...
