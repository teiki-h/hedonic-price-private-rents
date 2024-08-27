# correction loyer avec charge actif
DATA[,LoyerAct_Mont:=pmax(LoyerAct_Mont,LoyerAct_Base,TotalAct,na.rm = TRUE)] 
#fusion de frais d'entretien et charges locatif question 9
DATA[,IsChargeCopro := IsFraisEntretien & IsChargeloc]
DATA <- DATA %>% relocate(IsChargeCopro, .before = IsFraisEntretien)
#suppression desvar non utilisés
DATA <- DATA[,.SD,.SDcols = !c("CDist",
                               "ConstructionPrec",
                               "OccupationPrec",
                               "EmlogPrec",
                               "Ischangement","IsDepart","IsConstruit","IsLoyer","IsDistance","IsInsecure","IsVetuste","IsAutre","MotifPrec",
                               "NomAgence","DestinatairePrec",
                               "Loyer_Dec",
                               "LoyerAct_Mois","LoyerAct_Annee",
                               "LoyerAct_Base","IsFraisEntretien","FraisEntretienAct","EntretienJardinAct","GardienAct","MenageAct","IsChargeloc","ChargelocAct","ProvisionEauAct","OrdureMenageAct","AutrefraisAct","AutrefraisPrec","TotalAct",
                               "DSaisie","TS")]
#correction charge locatif question 8 
DATA[,IsChargesloc:=IsChargesloc|IsChargeCopro|IsEntretienJardin|IsGardien|IsProvisionEau|IsOrdureMenage|IsAutrefrais] 

#correction surface
DATA[SurfaceT==0,SurfaceT:=NA]
DATA[SurfaceH==0,SurfaceH:=NA]
#correction nb de pièce
DATA[CNBPieces>100,CNBPieces:=NA]
DATA[CNBPieces==0,CNBPieces:=NA]
#correction destinataire du loyer
DATA[CDestloyer==0,CDestloyer:=NA]
#correction emmenagement du loyer
DATA[CEmlog==0,CEmlog:=NA]

#groupe chambre et salle de bain
DATA[NBChambre>=5,NBChambre:=5]
DATA[NBSDB>=4,NBSDB:=4]

#conversion en chaine de caractère pour pouvoir utiliser bibliothèque fast_dummies
DATA[,CConstruction:=as.character(CConstruction)]
DATA[,CNBPieces:=as.character(CNBPieces)]
DATA[,NBChambre:=as.character(NBChambre)]
DATA[,NBSDB:=as.character(NBSDB)]
DATA[,COccupation:=as.character(COccupation)]
DATA[,CEmlog:=as.character(CEmlog)]
DATA[,CDestloyer:=as.character(CDestloyer)]
DATA[NBChambre=="5",NBChambre:="5plus"]
DATA[NBSDB=="4",NBSDB:="4plus"]
DATA[,CCom:=substr(CCom,1,2)]

#list des nom des variables explicative (besoin car il y a des dummy)

VarNameList <- list()

for (nom in c("CCom","CConstruction","CNBPieces","NBChambre","NBSDB","COccupation","CDestloyer")){
  Temp <- setDT(dummy_cols(DATA[,.SD,.SDcols=c("ID", nom)],remove_selected_columns = TRUE,ignore_na = TRUE,remove_most_frequent_dummy =TRUE))
  TempName<-list(as.list(colnames(Temp[,.SD,.SDcols = !c("ID")])))
  names(TempName)<-nom
  VarNameList <- c(VarNameList,TempName)
}
rm(Temp)
for (nom in c("IsBureau","IsPiscine","IsChargesloc","IsTaxeseauxord","IsChargeCopro","IsEntretienJardin","IsGardien","IsMenage","IsProvisionEau","IsOrdureMenage","IsAutrefrais","SurfaceT","SurfaceH")){
  TempName<-list(list(nom))
  names(TempName)<-nom
  VarNameList <- c(VarNameList,TempName)
}
rm(TempName,nom)
#dummy
dummies <- setDT(dummy_cols(DATA[,.(ID,CCom,CConstruction,CNBPieces,NBChambre,NBSDB,COccupation,CDestloyer)],remove_selected_columns = TRUE,ignore_na = TRUE,remove_most_frequent_dummy =TRUE))
DATA <- merge(DATA,dummies,by="ID",all = TRUE)
DATA <- DATA %>% mutate_if(is.logical,as.integer)
rm(dummies)