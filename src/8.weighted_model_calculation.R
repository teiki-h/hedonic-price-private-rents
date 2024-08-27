DATA_Work <- getDATAPresent(DATA,model_parameter,model_parameter[,ID])
DATA_Work[,poids:=.N,by=CLoge]
DATA_Work[,poids:=1/poids]
selected_model <- getLogLinModelPond(DATA_Work,"LoyerAct_Mont",unlist(names(model_parameter))[which(model_parameter[ID==model_parameter[,ID]]==TRUE)],VarNameList)