

#' Louis_Information
#' @description This function takes a dataset with stacked multiple imputations and a glm or coxph fit and estimates the corresponding information matrix accounting for the imputation uncertainty.
#'
#' @param fit object of class glm or coxph from fitting to the (weighted) stacked dataset
#' @param stack data frame containing stacked dataset across multiple imputations. Could have 1 or M rows for each subject with complete data. Should have M rows for each subject with imputed data. Must contain the following named columns: (1) stack$.id, which correspond to a unique identifier for each subject. This column can be easily output from MICE. (2) stack$wt, which corresponds to weights assigned to each row. Standard analysis of stacked multiple imputations should set these weights to 1 over the number of times the subject appears in the stack.
#' @param M number of multiple imputations
#'
#' @return Info, estimated information matrix accounting for within and between imputation variation
#' @details  This function uses the observed information matrix principle proposed in Louis (1982) and applied to imputations in Wei and Tanner (1990). This estimator is a further extension specifically designed for analyzing stacks of multiply imputed data as proposed in Beesley and Taylor (2019) https://arxiv.org/abs/1910.04625.
#'
#' @export


Louis_Information = function(fit, stack, M){
  p = length(as.matrix(coef(fit))[,1])
  if('summary.glm' %in% class(fit) | 'summary.coxph' %in% class(fit)){
    stop('Error: fit object should be of class glm or coxph, not a summary object')
  }
  if('coxph' %in% class(fit)){
    print('Note: coxph models should be fit using +cluster(.id) in the model formula')
  }
  if('glm' %in% class(fit)  ){
    if(substr(fit$family$family, 1, 17) %in% c("poisson", "binomial", "Negative Binomial")) {
      dispersion = 1
    }else{
      dispersion = glm.weighted.dispersion(fit)
    }
    SCORE = residuals(fit,  "response")*model.matrix(fit)/dispersion
  }
  if('coxph' %in% class(fit) ){
    SCORE = residuals(fit, type = "score")
  }
  MEANS = aggregate(sweep(SCORE, MARGIN = 1, stack$wt, '*')~stack$.id,FUN = sum)
  names(MEANS)[1] = c('.id')
  MEANS_LONG = merge(data.frame(.id = stack$.id, ROWKEY = c(1:length(stack$.id))) , MEANS, by = '.id', all.x = TRUE) #need to check that this does not reorder rows
  MEANS_LONG = MEANS_LONG[order(MEANS_LONG$ROWKEY),]
  SCORE = SCORE - subset(MEANS_LONG, select = -c(.id, ROWKEY))
  SS_mis = t(as.matrix(sweep(SCORE, MARGIN = 1, stack$wt, '*'))) %*% as.matrix(SCORE)
  if('glm' %in% class(fit)){
    J = solve(summary(fit)$cov.unscaled)/dispersion
  }else{
    J = solve(vcov(fit))
  }
  Info = J - SS_mis
  return(Info)
}



#' Louis_Information_Custom
#' @description This function takes a dataset with stacked multiple imputations and a score matrix and covariance matrix from stacked and weighted analysis as inputs to estimates the corresponding information matrix accounting for the imputation uncertainty.
#'
#' @param score n x p matrix containing the contribution to the outcome model score matrix for each subject (n rows) and each model parameter (p columns).
#' @param covariance_weighted p x p matrix containing the estimated covariance matrix from fitting the desired model to the stacked and weighted multiple imputations. Note: For GLM models, use summary(fit)$cov.unscaled*StackImpute::glm.weighted.dispersion(fit) as the default dispersion parameter will be incorrect.
#' @param stack data frame containing stacked dataset across multiple imputations. Could have 1 or M rows for each subject with complete data. Should have M rows for each subject with imputed data. Must contain the following named columns: (1) stack$.id, which correspond to a unique identifier for each subject. This column can be easily output from MICE. (2) stack$wt, which corresponds to weights assigned to each row. Standard analysis of stacked multiple imputations should set these weights to 1 over the number of times the subject appears in the stack.
#' @param M number of multiple imputations
#'
#' @return Info, estimated information matrix accounting for within and between imputation variation
#' @details This function uses the observed information matrix principle proposed in Louis (1982) and applied to imputations in Wei and Tanner (1990). This estimator is a further extension specifically designed for analyzing stacks of multiply imputed data as proposed in Beesley and Taylor (2019) https://arxiv.org/abs/1910.04625.
#'
#' @export


Louis_Information_Custom = function(score, covariance_weighted,stack, M){
  p = length(score[1,])
  if(!(substr(fit$family$family, 1, 17) %in% c("poisson", "binomial", "Negative Binomial"))) {
  print('Note: For glm fits with dispersion not equal to 1, the default output dispersion parameter does not correctly account for weights. Use summary(fit)$cov.unscaled*StackImpute::glm.weighted.dispersion(fit) to estimate covariance matrix.')
  }
  if(!is.matrix(covariance_weighted) | dim(covariance_weighted)[1] != p){
    stop('Covariance matrix from weighted regression must be provided. Dimension must match number of columns in score')
  }
  J = solve(covariance_weighted)
  MEANS = aggregate(sweep(score, MARGIN = 1, stack$wt, '*')~stack$.id,FUN = sum)
  names(MEANS)[1] = c('.id')
  MEANS_LONG = merge(data.frame(.id = stack$.id, ROWKEY = c(1:length(stack$.id))) , MEANS, by = '.id', all.x = TRUE) #need to check that this does not reorder rows
  MEANS_LONG = MEANS_LONG[order(MEANS_LONG$ROWKEY),]
  score = score - subset(MEANS_LONG, select = -c(.id, ROWKEY))
  SS_mis = t(as.matrix(sweep(score, MARGIN = 1, stack$wt, '*'))) %*% as.matrix(score)
  Info = J - SS_mis
  return(Info)
}


#' @export

glm.weighted.dispersion = function(fit){
  return( sum(as.vector(residuals(fit, "response"))^2 * weights(fit, "prior"), na.rm = TRUE)/sum(weights(fit, "prior"), na.rm = TRUE))
}


