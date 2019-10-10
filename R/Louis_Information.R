

#' Louis_Information
#' @description This function takes a dataset with stacked multiple imputations and a glm or coxph fit and estimates corresponding standard errors.
#'
#' @param fit object of class glm or coxph from fitting to the (weighted) stacked dataset
#' @param stack data frame containing stacked dataset across multiple imputations. Could have 1 or M rows for each subject with complete data. Should have M rows for each subject with imputed data. Must contain the following named columns: (1) stack$.id, which correspond to a unique identifier for each subject. This column can be easily output from MICE. (2) stack$wt, which corresponds to weights assigned to each row. Standard analysis of stacked multiple imputations should set these weights to 1 over the number of times the subject appears in the stack.
#' @param M number of multiple imputations
#' @param IMPUTED vector of subject ids (matching stack$.id) corresponding to subjects with imputed data. Could specify this vector to include all patient ids, but this will result in slower estimation.
#'
#' @return Info, estimated information matrix accounting for within and between imputation variation
#' @details  This function uses the observed information matrix principle proposed in Louis (1982) and applied to imputations in Wei and Tanner (1990). This estimator is a further extension specifically designed for analyzing stacks of multiply imputed data.
#'
#' @export

Louis_Information = function(fit, stack, M, IMPUTED){
 # cov = model.matrix(fit)
  p = length(as.matrix(coef(fit))[,1])
  if('summary.glm' %in% class(fit) | 'summary.coxph' %in% class(fit)){
    stop('Error: fit object should be of class glm or coxph, not a summary object')
  }
  if('glm' %in% class(fit)  ){
    SCORE = estfun_mod.glm(fit)
    if(substr(fit$family$family, 1, 17) %in% c("poisson", "binomial", "Negative Binomial")) {
      dispersion = 1
    }else{
      dispersion = sum(as.vector(residuals(fit, "response"))^2 * weights(fit, "prior"), na.rm = TRUE)/sum(weights(fit, "prior"), na.rm = TRUE)
    }
  }
  if('coxph' %in% class(fit) ){
    A= sandwich::estfun(fit)
    SCORE = sandwich::estfun(fit)/stack$wt
  }
  SS_mis = matrix(0,ncol = p, nrow = p) 
  for(m in IMPUTED){
    A = matrix(rep(apply(as.matrix(sweep(SCORE[stack$.id==m,], MARGIN = 1, stack[stack$.id==m,'wt'], '*')),2,sum),M), ncol = p, nrow = M, byrow = T)
    SS_mis = SS_mis + t(as.matrix(sweep(as.matrix(SCORE[stack$.id==m,] - A), MARGIN = 1, stack[stack$.id==m,'wt'], '*')))%*%as.matrix(SCORE[stack$.id == m,] - A)
  }
  if('glm' %in% class(fit)){
    J = solve(summary(fit)$cov.unscaled)/dispersion
  }else{
    J = solve(vcov(fit))
  }
  Info = J - SS_mis
  return(Info)
}



#' @export

estfun_mod.glm = function (x, ...)
{
  xmat <- model.matrix(x)
  xmat <- naresid(x$na.action, xmat)
  if(any(alias <- is.na(coef(x)))){
    xmat <- xmat[, !alias, drop = FALSE]
  }
  if(substr(x$family$family, 1, 17) %in% c("poisson", "binomial", "Negative Binomial")){
    dispersion = 1
  }else{
    sum(as.vector(residuals(x, "response"))^2 * weights(x, "prior"), na.rm = TRUE)/sum(weights(x, "prior"), na.rm = TRUE)
  }
  #rval <- wres * xmat/dispersion
  rval = residuals(x,  "response")  * xmat/dispersion
  attr(rval, "assign") <- NULL
  attr(rval, "contrasts") <- NULL
  res <- residuals(x, type = "pearson")
  if (is.ts(res))
    rval <- ts(rval, start = start(res), frequency = frequency(res))
  if (zoo::is.zoo(res))
    rval <- zoo::zoo(rval, index(res), attr(res, "frequency"))
  return(rval)
}



