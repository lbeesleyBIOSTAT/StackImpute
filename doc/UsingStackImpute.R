## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = TRUE, eval = FALSE,  fig.width = 7, fig.height= 4----------------
#  library(dplyr)
#  ### Simulate Data
#  Nobs = 2000
#  DAT = MASS::mvrnorm(n = Nobs, mu = c(0,0,0), Sigma = rbind(c(1, 0.18, 0.42), c(0.18, 0.09, 0.12),c(0.42, 0.12, 0.49 )))
#  Y = DAT[,1]
#  B = DAT[,2]
#  X = DAT[,3]
#  S = sample(x=c(0,1), size = Nobs, prob = c(0.5,0.5), replace = TRUE)
#  complete_cases = data.frame(Y, X, B, S)[S == 1,] #complete case subjects only
#  observed_data = data.frame(Y, X, B, S) #data with missingness in B
#  observed_data[S==0,'B'] = NA
#  
#  ### Step 1: Impute B|X,Y
#  imputes = mice::mice(observed_data, m=50, method="norm", printFlag=F, maxit = 1)
#  pred = imputes$predictorMatrix
#  pred[pred != 0] = 0
#  pred["B","X"] = 1
#  pred["B","Y"] = 1
#  imputes = mice::mice(observed_data, m=50, predictorMatrix=pred, method="norm", printFlag=F)
#  
#  ### Step 2: Stack imputed datasets	
#  stack = mice::complete(imputes, action="long", include = FALSE)
#  
#  ### Step 3: Obtain weights
#  stack$wt = 1
#  stack = as.data.frame(stack %>% group_by(.id) %>% mutate(wt = wt / sum(wt)))
#  
#  ### Step 4: Point estimation
#  fit = glm(Y ~X + B, data=stack, family=gaussian(), weights = stack$wt)
#  
#  ### Step 5a: Variance estimation option 1 (for glm and coxph models only)
#  Info = StackImpute::Louis_Information(fit, stack, M = 50)
#  VARIANCE = diag(solve(Info))
#  
#  ### Step 5b: Variance estimation using custom score and covariance matrices (any model with corresponding likelihood)
#  covariates = as.matrix(cbind(1,stack$X, stack$B))
#  score = sweep(covariates,1,stack$Y - covariates %*% matrix(coef(fit)), '*')/StackImpute::glm.weighted.dispersion(fit)
#  covariance_weighted = summary(fit)$cov.unscaled*StackImpute::glm.weighted.dispersion(fit)
#  Info = StackImpute::Louis_Information_Custom(score, covariance_weighted, stack, M = 50)
#  VARIANCE_custom = diag(solve(Info))
#  
#  ### Step 5c: Variance estimation using bootstrap (any model with vcov method)
#  bootcovar = StackImpute::Bootstrap_Variance(fit, stack, M = 50, n_boot = 100)
#  VARIANCE_boot = diag(bootcovar)

## ---- echo = TRUE, eval = FALSE,  fig.width = 7, fig.height= 4----------------
#  ### Step 1: Impute B|X
#  imputes = mice::mice(observed_data, m=50, method="norm", printFlag=F, maxit = 1)
#  pred = imputes$predictorMatrix
#  pred[pred != 0] = 0
#  pred["B","X"] = 1
#  imputes = mice::mice(observed_data, m=50, predictorMatrix=pred, method="norm", printFlag=F)
#  
#  ### Step 2: Stack imputed datasets	
#  stack = mice::complete(imputes, action="long", include = FALSE)
#  
#  ### Step 3: Obtain weights
#  fit_cc = glm(Y ~ X + B, family='gaussian', data= complete_cases)
#  stack$wt = dnorm(stack$Y,mean = predict(fit_cc, newdata = stack), sd = sqrt(summary(fit_cc)$dispersion))
#  stack = as.data.frame(stack %>% group_by(.id) %>% mutate(wt = wt / sum(wt)))
#  
#  ### Step 4: Point estimation
#  fit = glm(Y ~X + B, data=stack, family=gaussian(), weights = stack$wt)
#  
#  ### Step 5a: Variance estimation option 1 (for glm and coxph models only)
#  Info = StackImpute::Louis_Information(fit, stack, M = 50)
#  VARIANCE = diag(solve(Info))
#  
#  ### Step 5b: Variance estimation using custom score and covariance matrices (any model with corresponding likelihood)
#  covariates = as.matrix(cbind(1,stack$X, stack$B))
#  score = sweep(covariates,1,stack$Y - covariates %*% matrix(coef(fit)), '*')/StackImpute::glm.weighted.dispersion(fit)
#  covariance_weighted = summary(fit)$cov.unscaled*StackImpute::glm.weighted.dispersion(fit)
#  Info = StackImpute::Louis_Information_Custom(score, covariance_weighted, stack, M = 50)
#  VARIANCE_custom = diag(solve(Info))
#  
#  ### Step 5c: Variance estimation using bootstrap (any model with vcov method)
#  bootcovar = StackImpute::Bootstrap_Variance(fit, stack, M = 50, n_boot = 100)
#  VARIANCE_boot = diag(bootcovar)

## ---- echo = TRUE, eval = FALSE,  fig.width = 7, fig.height= 4----------------
#  ### Step 2: Stack imputed datasets	
#  stack = mice::complete(imputes, action="long", include = FALSE)
#  cc = unique(stack$.id[stack$S == 1])
#  stack_short = rbind(stack[stack$S==0,], stack[stack$S==1 & !duplicated(stack$.id),])

