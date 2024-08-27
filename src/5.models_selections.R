#stat_models <- merge(stat_models,ref_models[(LoyerAct_Annee==FALSE) & (CDist==FALSE) & (CNBPieces==FALSE),.(ID)],by="ID")

setorder(stat_models,BIC)
stat_selectBIC_models <- head(stat_models,4)
parameter <- stat_selectBIC_models[,.SD[1]]

DATA_Work <- getDATAPresent(DATA,ref_models,parameter[,ID])
selected_model <- getLogLinModel(DATA_Work,"LoyerAct_Mont",unlist(names(ref_models))[which(ref_models[ID==parameter[,ID]]==TRUE)],VarNameList)
rm(DATA_Work)

selected_model_iter <- copy(selected_model)
iterative <- data.table()
iterative <- rbind(iterative, ref_models[ID==parameter[,ID]])
iterative[,ID:=0]
ind_iter <- 0

# boucle retire de manière itérative
while (TRUE){
  
  CovarTest <- names(iterative)[unlist(c(FALSE,iterative[ID==ind_iter,-1]))==TRUE]
  
  for (var in CovarTest){
    new_iter <- data.table(tail(iterative,1))
    new_iter[,(var):=FALSE]
    
    form <- " ~ . "
    form <- sprintf("%s - %s",form, paste(VarNameList[[var]],collapse = " - "))
    form <- update(selected_model$terms,as.formula(form))
    DATA_Work <- getDATAPresent(DATA,new_iter,new_iter[,ID])
    selected_model_iter <- lm(form,DATA_Work)
    rm(DATA_Work)
    
    if(BIC(selected_model_iter)<BIC(selected_model)){
      
      
      new_iter[,ID:=ID+1]
      iterative <- rbind(iterative,new_iter)
      selected_model <- copy(selected_model_iter)
      break
    }
  }
  if(BIC(selected_model_iter)==BIC(selected_model)){
    ind_iter <- ind_iter+1
    next
  }
  break
}

save(list = c("iterative"),file = "src/projet 2/output/model_iteration.RData")