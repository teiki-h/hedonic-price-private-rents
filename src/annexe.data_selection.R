#prend en entrée la table de données récupéré par le questionnaire selectionne uniquement les nouvelles observations et pas leurs copies, anonymise le tout  
load("../stage-teiki/src/projet 2/input/all_tables_avant_traitement.RData")
library(data.table)
data_loyer[,indicateur:=FALSE]
data_loyer[month(Periode)==CVague,indicateur:=TRUE]
data_loyer[month(Periode)-6==CVague,indicateur:=TRUE]
data_loyer_av_tr <- data_loyer[indicateur==TRUE]
data_loyer_av_tr <- data_loyer[,.SD,.SDcols = !c()]
data_loyer_av_tr <- data_loyer_av_tr[,.SD,.SDcols = !c("CLoge_Old","CEnqueteurTER","CEnqueteurTEL","CCons","CHab","CVague","CTirage","CTirage","DEnquete","Nom","Prenom","Identite","Tel1","Tel2","Tel","Adresse","CLogeRemp")]
data_loyer <- data_loyer_av_tr

#anonymise l'id des logements
table <- data.table(CLoge=unique(data_loyer[,CLoge]),newID=0)
table[,newID:=.I]
data_loyer <- merge(data_loyer,table)
data_loyer[,CLoge:=newID]
data_loyer <- data_loyer[,.SD,.SDcols = !c("newID","indicateur")]
save(list = c("data_loyer"),file = "src/projet 2/input/data_loyer_rectif.RData")