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
#  
#  ### Step 5d: Variance estimation using jackknife (any model with vcov method)
#  jackcovar = StackImpute::Jackknife_Variance(fit, stack, M = 50)
#  VARIANCE_jack = diag(jackcovar)
#  

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
#  ### Any one of the above variance estimation strategies can then be applied.

## ---- echo = TRUE, eval = FALSE,  fig.width = 7, fig.height= 4----------------
#  ### Step 2: Stack imputed datasets	
#  stack = mice::complete(imputes, action="long", include = FALSE)
#  cc = unique(stack$.id[stack$S == 1])
#  stack_short = rbind(stack[stack$S==0,], stack[stack$S==1 & !duplicated(stack$.id),])

## ---- echo = TRUE, eval = FALSE,  fig.width = 7, fig.height= 4----------------
#  ### Simulate Data
#  prob_obs = exp(2*B + 1*Y)/(1+exp(2*B + 1*Y))
#  S_mnar = as.numeric(prob_obs > runif(Nobs,0,1))
#  complete_cases = data.frame(Y, X, B, S=S_mnar)[S_mnar == 1,] #complete case subjects only
#  observed_data_mnar = data.frame(Y, X, B, S=S_mnar) #data with missingness in B
#  observed_data_mnar[S_mnar==0,'B'] = NA
#  
#  ### Step 1: Impute B|X,Y
#  imputes_mnar = mice::mice(observed_data_mnar, m=50, method="norm", printFlag=F, maxit = 1)
#  pred = imputes_mnar$predictorMatrix
#  pred[pred != 0] = 0
#  pred["B","X"] = 1
#  pred["B","Y"] = 1
#  imputes_mnar = mice::mice(observed_data_mnar, m=50, predictorMatrix=pred, method="norm", printFlag=F)
#  
#  ### Step 2: Stack imputed datasets
#  stack = mice::complete(imputes_mnar, action="long", include = FALSE)
#  
#  ### Step 3: Obtain weights
#  phi1_assumed = 2
#  stack$wt = exp(-phi1_assumed*stack$B)
#  stack = as.data.frame(stack %>% group_by(.id) %>% mutate(wt = wt / sum(wt)))
#  
#  ### Step 4: Point estimation
#  fit = glm(Y ~X + B, data=stack, family=gaussian(), weights = stack$wt)
#  
#  ### Any one of the above variance estimation strategies can then be applied.

## ---- echo = TRUE, eval = FALSE,  fig.width = 7, fig.height= 4----------------
#  ### Imputation Function (modified version of mice::mice.impute.mnar.norm())
#  mice.impute.mnar.norm2 = function (y, ry, x, wy = NULL, ums = NULL, umx = NULL, ...){
#    u <- mice:::parse.ums(x, ums = ums, umx = umx, ...)
#    if (is.null(wy))
#      wy <- !ry
#    x <- cbind(1, as.matrix(x))
#    parm <- mice:::.norm.draw(y, ry, x, ...)
#    return(x[wy, ] %*% parm$beta + as.matrix(u$x[wy, ]) %*% as.matrix(u$delta) + rnorm(sum(wy)) *parm$sigma)
#  }
#  
#  ### *Ideal* pattern mixture model offset parameter for these simulated data:
#  delta1_assumed = -0.087
#  
#  ### Step 1: Impute B|X,Y,S
#  mnar.blot <- list(B = list(ums =paste0('-',abs(delta1_assumed))))
#  imputes_pmm = mice::mice(observed_data_mnar, m=50, method="mnar.norm2", printFlag=F, maxit = 1, blots = mnar.blot)
#  pred = imputes_pmm$predictorMatrix
#  pred[pred != 0] = 0
#  pred["B","X"] = 1
#  pred["B","Y"] = 1
#  imputes_pmm = mice::mice(observed_data_mnar, m=50, predictorMatrix=pred, method="mnar.norm2", printFlag=F, blots = mnar.blot)
#  
#  ### Step 2: Apply Rubin's Rules to obtain point estimates and standard errors
#  fit = summary(pool(with(imputes_pmm,glm(Y ~ X + B, family=gaussian()))))
#  param = fit$estimate
#  VARIANCE = (fit$std.error)^2

