library(caret)

N <- 10
rf_coefs_path <- c()
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
for (i in 1:N) {
  set.seed(i+10)
  if(i %% 1==0){
    print(i/1)
  }
  rf_keep <- sample(1:49,39)
  P_rf <- P_train[rf_keep]
  pathway_surv_rf <- pathway_surv_train[rf_keep,]
  pathway.scores_rf <- pathway.scores_train[rf_keep,]
  
  set.seed(07272023)
  #Random Forests
  # run the RFE algorithm
  results_pathway_rf <- rfe(pathway.scores_rf, P_rf, sizes=c(1:30), rfeControl=control)
  # list the chosen features
  
  rf_coefs_path <- c(rf_coefs_path,predictors(results_pathway_rf))
}
coefs_rf_path=as.data.frame(rf_coefs_path)
ggplot(coefs_rf_path,aes(x = rf_coefs_path)) +
  geom_bar()
coefs_rf_path <- coefs_rf_path %>% count(rf_coefs_path)
cutoff <- as.numeric(quantile(coefs_rf_path$n,probs = 0.8))
coefs_rf_path <- coefs_rf_path %>% filter(n>cutoff,rf_coefs_path!="(Intercept)")

formula_pathway_rf <- as.formula(paste("P ~", 
                                          paste(coefs_rf_path$rf_coefs_path, 
                                                collapse = "+")))
path_rf<-train(formula_pathway_rf, data = pathway_surv_train,
                  method = 'glmnet', 
                  tuneGrid = expand.grid(alpha = 0, lambda = parameters),
                  trControl = ctrl,
                  metric = "RMSE"
)
predictions_path_rf <- path_rf %>% predict(pathway_surv_test)
train_predictions_path_rf <- path_rf %>% predict(pathway_surv_train)

path_rf_sum = postResample(predictions_path_rf, P_test)

N <- 10
rf_coefs_pc <- c()
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
for (i in 1:N) {
  set.seed(i)
  if(i %% 1==0){
    print(i/1)
  }
  rf_keep <- sample(1:49,39)
  P_rf <- P_train[rf_keep]
  pc_surv_rf <- pc_surv_train[rf_keep,]
  pc_scores_rf <- pc_scores_train[rf_keep,]
  
  set.seed(07272023)
  #Random Forests
  # run the RFE algorithm
  results_pc_rf <- rfe(pc_scores_rf, P_rf, sizes=c(1:30), rfeControl=control)
  # list the chosen features
  
  rf_coefs_pc <- c(rf_coefs_pc,predictors(results_pc_rf))
}
coefs_rf_pc=as.data.frame(rf_coefs_pc)
ggplot(coefs_rf_pc,aes(x = rf_coefs_pc)) +
  geom_bar()
coefs_rf_pc <- coefs_rf_pc %>% count(rf_coefs_pc)
cutoff <- as.numeric(quantile(coefs_rf_pc$n,probs = 0.8)) #fraction wanted
coefs_rf_pc <- coefs_rf_pc %>% filter(n>(N*frac),rf_coefs_pc!="(Intercept)")

formula_pc_rf <- as.formula(paste("P ~", 
                                          paste(coefs_rf_pc$rf_coefs_pc, 
                                                collapse = "+")))
pc_rf<-train(formula_pc_rf, data = pc_surv_train,
                  method = 'glmnet', 
                  tuneGrid = expand.grid(alpha = 0, lambda = parameters),
                  trControl = ctrl,
                  metric = "RMSE"
)
predictions_pc_rf <- pc_rf %>% predict(pc_surv_test)
train_predictions_pc_rf <- pc_rf %>% predict(pc_surv_train)

pc_rf_sum = postResample(predictions_pc_rf, P_test)
