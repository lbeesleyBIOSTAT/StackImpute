


#' Bootstrap_Variance
#' @description This function takes a dataset with stacked multiple imputation and a model fit and applies bootstrap to estimate the covariance matrix accounting for imputation uncertainty.
#'
#' @param fit object with corresponding vcov method (e.g. glm, coxph, survreg, etc.) from fitting to the (weighted) stacked dataset
#' @param stack data frame containing stacked dataset across multiple imputations. Could have 1 or M rows for each subject with complete data. Should have M rows for each subject with imputed data. Must contain the following named columns: (1) stack$.id, which correspond to a unique identifier for each subject. This column can be easily output from MICE. (2) stack$wt, which corresponds to weights assigned to each row. Standard analysis of stacked multiple imputations should set these weights to 1 over the number of times the subject appears in the stack. (3) stack$.imp, which indicates the multiply imputed dataset (from 1 to M). This column can be easily output from MICE.
#' @param M number of multiple imputations
#' @param n_boot number of bootstrap samples
#'
#' @return Variance, estimated covariance matrix accounting for within and between imputation variation
#' @details This function implements the bootstrap-based estimation method for stacked multiple imputations proposed by Dr. Paul Bernhardt in ``A Comparison of Stacked and Pooled Multiple Imputation" at the Joint Statistical Meetings, 2019.
#' @export


Bootstrap_Variance = function(fit, stack, M, n_boot = 100){
  results <- boot::boot(data=cbind(c(1:M)), statistic=StackImpute::func.boot, R=n_boot)
  theta_var = var(results$t)
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
  Variance =covariance_weighted + (1+M)*theta_var
  return(Variance)
}

#' func.boot
#'
#' @description This function is called internal to Bootstrap_Variance and re-estimates glm model parameters
#'
#' @param data matrix with indices of possible imputed datasets to sample
#' @param indices sampled indices 
#'
#' @export

func.boot <- function(data, indices){
  stack_temp = stack[stack$.imp %in% indices, ]
  stack_temp = merge(stack_temp, data.frame(.imp = sort(unique(indices)), multiples = as.numeric(table(indices)
)), by = '.imp', all.x = TRUE)
  stack_temp$wt =  stack_temp$wt*stack_temp$multiples
  stack_temp <- stack_temp %>% dplyr::group_by(.id) %>% dplyr::mutate(wt = wt / sum(wt))
  stack_temp <- as.data.frame(stack_temp)
  fit_boot <- StackImpute::my_update(fit, . ~ ., data = stack_temp, weights = stack_temp$wt)
  param = coef(fit_boot)
  return(param)
}


#' my_update
#'
#' @description Function for updating a model fit using either new data or a new model structure
#'
#' @param mod object of class 'glm' or 'coxph'
#' @param formula formula for updated model fit, default = no change
#' @param data data used for updated model fit, default = no change
#' @param weights weights used for updated model fit, default = no change
#' @export

my_update <- function(mod, formula = NULL, data = NULL, weights = NULL) {
  ### Author's note: This is a modification of a function provided by Hadley Wickham
  ### on Stack Overflow to handle environment issues with the update function. Thanks, Hadley!!!
  call <- getCall(mod)
  if (is.null(call)) {
    stop("Model object does not support updating (no call)", call. = FALSE)
  }
  term <- terms(mod)
  if (is.null(term)) {
    stop("Model object does not support updating (no terms)", call. = FALSE)
  }
  if (!is.null(weights)) call$weights <- weights
  if (!is.null(data)) call$data <- data
  if (!is.null(formula)) call$formula <- update.formula(call$formula, formula)
  env <- attr(term, ".Environment")
  eval(call, env, parent.frame())
}
