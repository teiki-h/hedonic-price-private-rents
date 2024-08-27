ref_models <- data.table()
hedonic_models <- list()

row_empty <- data.table( CCom=0 , CConstruction=0 , CNBPieces=0 , NBChambre=0  , NBSDB=0 , COccupation=0 , CDestloyer=0 , IsBureau=0 , IsPiscine=0 , IsChargesloc=0 , IsTaxeseauxord=0 , IsChargeCopro=0 , IsEntretienJardin=0 , IsGardien=0 , IsMenage=0 , IsProvisionEau=0 , IsOrdureMenage=0 , IsAutrefrais=0 ,SurfaceT=0,SurfaceH=0,rien=0)

names(VarNameList)


LOCALISATION <- list("rien","CCom")
SURFACE <- list(c("SurfaceH","NBChambre"),c("SurfaceT","NBChambre"),c("SurfaceT","SurfaceH"),c("CNBPieces","NBChambre","NBSDB"), c("CNBPieces","NBChambre","NBSDB","SurfaceH","SurfaceT"))
BAIL <- list("rien","COccupation",c("COccupation","CDestloyer"))
MISC <- list("rien","CConstruction",c("CConstruction","IsPiscine","IsBureau"))
CHARGE <- list("rien",c("IsChargesloc","IsTaxeseauxord"),c("IsChargeCopro","IsEntretienJardin","IsGardien","IsMenage","IsProvisionEau","IsOrdureMenage","IsAutrefrais","IsTaxeseauxord"))


for (l in LOCALISATION){
  for (s in SURFACE){
    for (b in BAIL){
      for (m in MISC){
        for (ch in CHARGE){
          temp <- copy(row_empty)
          cols <- c(l,s,b,m,ch)
          temp[,unique(cols) := lapply(.SD, "+",1), .SDcols=unique(cols)]
          ref_models <- rbind(ref_models,temp[,.SD,.SDcols = !c("rien")])
        }
      }
    } 
  }
}

ref_models[,ID:=as.character(.I)]
ref_models <- ref_models %>% relocate(ID, .before = CCom)
ref_models <- ref_models %>% mutate_if(is.numeric,as.logical)

stat_models <- data.table(ID=copy(ref_models[,ID]),rsquared=NULL,rsquared_adj=NULL,pValue_overallF=NULL,testStatistic_JB=NULL,pValue_JB=NULL,testStatistic_BP=NULL,pValue_BP=NULL,testStatistic_RESET=NULL,pValue_RESET=NULL, AIC=NULL, BIC= NULL,MAE=NULL,RMSE=NULL)

for (i in 1:nrow(ref_models)){
  parameter <- ref_models[,.SD[i]]
  lm_temp <- getLogLinModel(DATA[year(Periode)>=2021],"LoyerAct_Mont",unlist(names(ref_models))[which(ref_models[,.SD[i]]==TRUE)],VarNameList)
  lm_temp_sum <- summary(lm_temp)
  Fstat_temp <- lm_temp_sum$fstatistic
  JB_temp <- jarque.bera.test(lm_temp$residuals)
  BP_temp <- bptest(lm_temp,studentize = FALSE)
  RESET_temp <- resettest(lm_temp)
  
  stat_models[ ID == ref_models[,.SD[i]]$ID , rsquared := lm_temp_sum$r.squared ]
  stat_models[ ID == ref_models[,.SD[i]]$ID , rsquared_adj := lm_temp_sum$adj.r.squared ]
  stat_models[ ID == ref_models[,.SD[i]]$ID , pValue_overallF := unname(pf(Fstat_temp[1], Fstat_temp[2], Fstat_temp[3], lower.tail=FALSE)) ]
  stat_models[ ID == ref_models[,.SD[i]]$ID , AIC := AIC(lm_temp) ]
  stat_models[ ID == ref_models[,.SD[i]]$ID , BIC := BIC(lm_temp) ]
  stat_models[ ID == ref_models[,.SD[i]]$ID , MAE := mean(abs(lm_temp$residuals)) ]
  stat_models[ ID == ref_models[,.SD[i]]$ID , RMSE := sqrt(mean((lm_temp$residuals)**2)) ]
  stat_models[ ID == ref_models[,.SD[i]]$ID , testStatistic_JB := unname(JB_temp[[1]]) ]
  stat_models[ ID == ref_models[,.SD[i]]$ID , pValue_JB := unname(JB_temp[[3]]) ]
  stat_models[ ID == ref_models[,.SD[i]]$ID , testStatistic_BP := unname(BP_temp[[1]]) ]
  stat_models[ ID == ref_models[,.SD[i]]$ID , pValue_BP := unname(BP_temp[[4]]) ]
  stat_models[ ID == ref_models[,.SD[i]]$ID , testStatistic_RESET := unname(RESET_temp[[1]]) ]
  stat_models[ ID == ref_models[,.SD[i]]$ID , pValue_RESET := unname(RESET_temp[[4]]) ]
  
  lm_temp <- list(lm_temp)
  names(lm_temp) <- parameter[["ID"]]
  hedonic_models <- c(hedonic_models,lm_temp)
}

save(list = c("hedonic_models"),file = "src/projet 2/output/all_models.RData")
save(list = c("ref_models","stat_models"),file = "src/projet 2/output/models_analysis.RData")
