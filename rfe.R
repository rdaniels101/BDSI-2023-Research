set.seed(7)
# load the library
library(caret)
# load the data
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(pathway.scores, Y, sizes=c(1:30), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"),xlim=c(0,30))

features_pathway <- results[["optVariables"]][1:10]

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results_pc <- rfe(pc_scores, Y, sizes=c(1:30), rfeControl=control)

features_pc <- results_pc[["optVariables"]][1:4]

# summarize the results
print(results_pc)
# list the chosen features
predictors(results_pc)
# plot the results
plot(results_pc, type=c("g", "o"),xlim=c(0,30))

features_pc <- results[["optVariables"]][1:4]

pc_surv2=pc_surv %>% 
  select(FLAIR_NCR.NET.17, T2_NCR.NET.4, T2_ED.1, FLAIR_NCR.NET.5,Y)

intercept_only <- lm(g(Y) ~ 1, data=pc_surv2)

#define model with all predictors
all <- lm(g(Y) ~ ., data=pc_surv2)

#perform forward stepwise regression
forward_pc <- step(intercept_only, direction='forward', scope=formula(all), trace=0)