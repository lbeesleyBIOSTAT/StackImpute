% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Louis_Information.R
\name{Louis_Information_Custom}
\alias{Louis_Information_Custom}
\title{Louis_Information_Custom}
\usage{
Louis_Information_Custom(score, covariance_weighted, stack, M)
}
\arguments{
\item{score}{n x p matrix containing the contribution to the outcome model score matrix for each subject (n rows) and each model parameter (p columns).}

\item{covariance_weighted}{p x p matrix containing the estimated covariance matrix from fitting the desired model to the stacked and weighted multiple imputations. Note: For GLM models, use summary(fit)$cov.unscaled*StackImpute::glm.weighted.dispersion(fit) as the default dispersion parameter will be incorrect.}

\item{stack}{data frame containing stacked dataset across multiple imputations. Could have 1 or M rows for each subject with complete data. Should have M rows for each subject with imputed data. Must contain the following named columns: (1) stack$.id, which correspond to a unique identifier for each subject. This column can be easily output from MICE. (2) stack$wt, which corresponds to weights assigned to each row. Standard analysis of stacked multiple imputations should set these weights to 1 over the number of times the subject appears in the stack.}

\item{M}{number of multiple imputations}
}
\value{
Info, estimated information matrix accounting for within and between imputation variation
}
\description{
This function takes a dataset with stacked multiple imputations and a score matrix and covariance matrix from stacked and weighted analysis as inputs to estimates the corresponding information matrix accounting for the imputation uncertainty.
}
\details{
This function uses the observed information matrix principle proposed in Louis (1982) and applied to imputations in Wei and Tanner (1990). This estimator is a further extension specifically designed for analyzing stacks of multiply imputed data as proposed in Beesley and Taylor (2019) https://arxiv.org/abs/1910.04625.
}
\examples{
data(stackExample)

fit = stackExample$fit
stack = stackExample$stack

covariates = as.matrix(cbind(1, stack$X, stack$B))
score = sweep(covariates, 1, stack$Y - covariates \%*\%
         matrix(coef(fit)), '*') / glm.weighted.dispersion(fit)
covariance_weighted = summary(fit)$cov.unscaled * glm.weighted.dispersion(fit)
Info = Louis_Information_Custom(score, covariance_weighted, stack, M = 50)
VARIANCE_custom = diag(solve(Info))

}
