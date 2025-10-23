#1. Install Package
devtools::install_github("giopogg/webSDM")
library(webSDM)
library(ggplot2)
library(rstanarm)

#2. Going through the example
#' Data: 
#' - Site by species
#' - site by covariates
#' - iGraph object describing interaction network (links Predator -> Prey)
#' 
#'  Quadratic relationship wrt environment. Assuming bottom-up control. Bayesian 
#' framework.

data(X, Y, G) # Fake data provided by the package

m <- trophicSDM(Y = Y, X = X, G = G, env.formula = "~ (X_1 + X_2)^2",
               family = binomial(link = "logit"),
               mode = "prey", method = "stan_glm")

m$form.all
plot(m)
coef(m, standardise = T, level = 0.9)
plotG_inferred(m)
VarImpo = computeVariableImportance(m, 
                                    groups = list("Abiotic" = c("X_1","X_2"),
                                                  "Biotic" = c("Y1","Y2", "Y3", "Y4", "Y5", "Y6")))
VarImpo = apply(VarImpo, 2, function(x) x/(x[1]+x[2]))
tab = reshape2::melt(VarImpo)
tab$Var2 = factor(tab$Var2, levels = colnames(Y))
ggplot(tab, aes(x = Var2, y = value, fill = Var1)) + geom_bar(stat="identity") +
  theme_classic()

m$model$Y5

Ypred = predict(m, fullPost = FALSE, pred_samples = 50, prob.cov = FALSE)
# predict returns a list contaning for each species the predictive samples at each site
# But since we specified fullPost = FALSE it only give back the predictive mean and quantiles 

Ypred = do.call(cbind,
                lapply(Ypred, function(x) x$predictions.mean))

Ypred = Ypred[,colnames(Y)]
evaluateModelFit(m, Ynew = Y, Ypredicted = Ypred)
##         auc        tss species
## 1 0.6733574 0.28064516      Y1
## 2 0.7048959 0.31015078      Y2
## 3 0.6690035 0.27931242      Y3
## 4 0.5988342 0.17092061      Y4
## 5 0.6236528 0.20786045      Y5
## 6 0.5581631 0.09940342      Y6

m$log.lik
## [1] -3638.686
m$AIC
## [1] 7337.373
loo(m)
## [1] -3650.931

# Cross Validation
CV = trophicSDM_CV(m, K = 3, prob.cov = T, run.parallel = FALSE)
# Transfom in a table
Ypred = CV$meanPred
# Re order columns
Ypred = Ypred[,colnames(Y)]
evaluateModelFit(m, Ynew = Y, Ypredicted = Ypred)

#pPres for X_1 = 0.5, X_2 = 0.5
Pred = predict(m, Xnew = data.frame(X_1 = 0.5, X_2 = 0.5), fullPost = F)
t(do.call(cbind, Pred))

# Probability assuming prey is present given environmental conditions
Ypred = predictPotential(m, fullPost = FALSE, pred_samples = 100, Xnew = data.frame(X_1 = 0.5, X_2 = 0.5))