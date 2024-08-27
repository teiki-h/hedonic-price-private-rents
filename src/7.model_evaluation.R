VIF <- data.table(variable=names(vif(selected_model)),vif=vif(selected_model)) #propre au model pas de DATA_Work

selected_model_summary <- summary(selected_model)

#attention les r2 ne marche que pour les lm non pondérés
y <- log(DATA_Work[,LoyerAct_Mont])
fittedval <- getFittedValue(DATA_Work,selected_model)
rsquared_log <- getRsqr(log(DATA_Work[,LoyerAct_Mont]),fittedval)
adj.rsquared_log <-  getAdjRsqr(log(DATA_Work[,LoyerAct_Mont]),fittedval,selected_model)

mae <- mean(abs(y-fittedval))
rmse <- sqrt(mean((y-fittedval)^2))

selected_model.residuals_xpf <- exp(fittedval)*(exp(y - fittedval)-1)
names(selected_model.residuals_xpf) <- NULL

mae.xpf <- mean(abs(selected_model.residuals_xpf))
rmse.xpf <- sqrt(mean((selected_model.residuals_xpf)^2))