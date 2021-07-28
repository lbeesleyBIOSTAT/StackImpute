


#' Jackknife_Variance
#' @description This function takes a dataset with stacked multiple imputation and a model fit and applies jackknife to estimate the covariance matrix accounting for imputation uncertainty.
#'
#' @param fit object with corresponding vcov method (e.g. glm, coxph, survreg, etc.) from fitting to the (weighted) stacked dataset
#' @param stack data frame containing stacked dataset across multiple imputations. Could have 1 or M rows for each subject with complete data. Should have M rows for each subject with imputed data. Must contain the following named columns: (1) stack$.id, which correspond to a unique identifier for each subject. This column can be easily output from MICE. (2) stack$wt, which corresponds to weights assigned to each row. Standard analysis of stacked multiple imputations should set these weights to 1 over the number of times the subject appears in the stack. (3) stack$.imp, which indicates the multiply imputed dataset (from 1 to M). This column can be easily output from MICE.
#' @param M number of multiple imputations
#'
#' @return Variance, estimated covariance matrix accounting for within and between imputation variation
#' @details This function implements the jackknife-based estimation method for stacked multiple imputations proposed by Beesley and Taylor (2021).
#' @export


Jackknife_Variance = function(fit, stack, M){
  if('glm' %in% class(fit)){
    if(substr(fit$family$family, 1, 17) %in% c("poisson", "binomial", "Negative Binomial")) {
      dispersion = 1
    }else{
      dispersion = StackImpute::glm.weighted.dispersion(fit)
    }
    covariance_weighted = summary(fit)$cov.unscaled*dispersion
  }else{
    covariance_weighted = vcov(fit)
  }
  results <- apply(cbind(c(1:M)), 1,FUN=StackImpute::func.jack, stack)
  #Nobs = length(stack[,1])
  #results_corrected = matrix(rep(as.vector(coef(fit)),M), ncol = M, byrow=F) - ((Nobs-M)/Nobs)*results
  theta_var = var(t(results))*(M-1)*((M-1)/M)
  Variance =covariance_weighted + (1+M)*theta_var
  return(Variance)
}




#' Title
#'
#' Description
#'
#' @param leaveout desc
#' @param stack desc
#'
#' @export

func.jack <- function(leaveout, stack){
  stack_temp = stack[stack$.imp != leaveout, ]
  stack_temp <- stack_temp %>% dplyr::group_by(.id) %>% dplyr::mutate(wt = wt / sum(wt))
  stack_temp <- as.data.frame(stack_temp)
  fit_jack <- StackImpute::my_update(fit, . ~ ., data = stack_temp, weights = stack_temp$wt)
  param = coef(fit_jack)
  return(param)
}


