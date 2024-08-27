#calcul du modèle
source("src/1.functions.R")
source("src/2.load.R")
DATA <- data_loyer[year(Periode)>=2021]
source("src/3.data_preparation.R")
model_parameter <- iterative[,.SD[length(iterative[,ID])]]
model_parameter[,CDestloyer:=FALSE]
source("src/6.model_calculation.R") #source("src/9.weighted_model_calculation.R")
source("src/7.model_evaluation.R")

#évaluation du modèle sur d'autre années
DATA <- data_loyer[year(Periode)>=2019]
source("src/3.data_preparation.R")
DATA <- DATA[(year(Periode)<=2020)]
DATA_Work <- getDATAPresent(DATA,model_parameter,model_parameter[,ID])
source("src/7.model_evaluation.R")
