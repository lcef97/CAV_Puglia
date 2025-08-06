##' Simulate BYM model ---------------------------------------------------------

#' Neighbourhood matrix  ==> binary
load(url("https://github.com/lcef97/CAV_Puglia/raw/main/input/Replication/W.RData"))
#' Source functions to implement models; 
#' Link to R code: 'https://github.com/lcef97/CAV_Puglia/blob/main/Auxiliary/Functions.R'
devtools::source_url("https://raw.githubusercontent.com/lcef97/CAV_Puglia/refs/heads/main/Auxiliary/Functions.R")

n <- nrow(W)

#' Hyperparameters ------------------------------------------------------------#
#' Mixing
phi.true <- c(0.6, 0.4, 0.9,  0.7)
#' Standard deviations
sigma.true <- c(0.7, 1.1, 0.8, 0.6)
# Correlations
corr.true <- c(0.9, 0.7, 0.5, 0.6, 0.3, 0.7)
#' Variance-covariance matrix
Scale.true <- varcov(c(sigma.true, corr.true), tri=F)
#' M-matrix
M.true <- diag(sqrt(eigen(Scale.true)$values)) %*% t(eigen(Scale.true)$vectors)

#' Add covariate --------------------------------------------------------------#
set.seed(11)
X <- runif(n=4*n, min = -sqrt(3), max = sqrt(3))
beta0 <- 2
beta1 <- -0.6

#' Simulate BYM field ---------------------------------------------------------#
L.unscaled <- Matrix::Diagonal(rowSums(W), n=n) - W
L <- INLA:::inla.scale.model.bym.internal(L.unscaled)$Q
constr <- INLA:::inla.bym.constr.internal(L)
constr.BYM <- list(A = cbind(Matrix::Matrix(0, nrow=4, ncol = 4*n), 
                             kronecker(diag(4), constr$constr$A)),
                   e = c(0, 0, 0, 0))

set.seed(11)
#' IID component
V.true <- matrix(rnorm(n=4*n, mean=0, sd=1), nrow = n, ncol = 4, byrow=F)
#' ICAR component
U.lst <- lapply(c(1:4), function(i) {
  MASS::mvrnorm(n=1, mu=rep(0,n), Sigma=INLA:::inla.ginv(L)+Matrix::Diagonal(n=n, x=1e-7))
  })
U.true <- scale(do.call(cbind, U.lst))
#' BYM2 process - weighted sum of IID and ICAR, then scaled by M  
Z.true <- as.vector(U.true %*% diag(sqrt(phi.true)) %*% M.true + 
  V.true %*% diag(sqrt(1 - phi.true)) %*% M.true)

#' Simulation of the observed variable ----------------------------------------#
y.sim <- numeric(length(Z.true))
for(i in seq(length(y.sim))) {
  eta <- beta0 + beta1*X[i] + Z.true[i]
  y.sim[i] <- rpois(n=1, lambda = exp(eta) )
}

dd <- data.frame(Y = y.sim, ID = c(1:(4*n)), X=X, group=rep(c(1:4), each=n))

##' Toy examples: simpler models -----------------------------------------------

#' Easiest: IMCAR -------------------------------------------------------------#
#' 
#' Not the true model, but very easy to fit.
#' Covariances matrix has Wishart(2k, I_k) prior.
#' Can be still changed in the function call.
#' Now, if Bartlett factorisation is used, both IMCAR, PMCAR and LMCAR work well.

mod.IMCAR <- inla(
  Y ~ 1 + X +
    f(ID, model = inla.IMCAR.Bartlett(k = 4, W = W, df=8),
      extraconstr = list(A=constr.BYM$A[,-c(1:(4*n))], e = c(0,0,0,0)) ),
  family = "poisson", data = dd, num.threads = 1, #control.inla = list(cmin=0.001),
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

#' Hyperparameters posterior.
#' Variance ==> Overestimated, like 3x  (not true DGP...)
vcov_summary(mod.IMCAR)$var
#' Correlations: perfect
vcov_summary(mod.IMCAR)$cor
 

#' BYM toy example: independent fields ----------------------------------------#
#'
#' Of course it is not the true DGP either. 
#' Uniform prior on the mixing parameter (PC prior would be too restrictive), 
#' sum-to-zero constraint on the ICAR component and NOT on the convolution component,
#' as it appears to be in the univariate version implemented with `model='bym2'` 
#' 
mod.INDMMBYM  <- inla(
  Y ~ 1+ X+
    f(ID, model = inla.INDMMBYM.model(k = 4, W = W, df=8, PC = F ),
      extraconstr = constr.BYM),
  family = "poisson", data = dd, num.threads = 1,
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)
# Mixing parameter ==> ok
Mmodel_compute_mixing(mod.INDMMBYM) 
#' Variances ==> perfect
data.frame( do.call(rbind, lapply(
  lapply(mod.INDMMBYM$marginals.hyperpar[c(5:8)], function(f){
    inla.tmarginal(fun = function(X) exp(-X), marginal = f)
  }), function(x) unlist(inla.zmarginal(x, silent = TRUE)))))[,c(1,2,3,5,7)]


##' multivariate BYM - where trouble begins ------------------------------------
#' Default configuration of the BYM;

#' A priori, again, var-cov ~ Wishart(2k, I_k), while the mixing parameter
#' follows a multivariate Uniform distribution (worse with PC)
mod.MMBYM <- inla(
  Y ~ 1+ X+
    f(ID, model = inla.MMBYM.model(k = 4, W = W, df=8, PC = F),
      extraconstr = constr.BYM),
  family = "poisson", data = dd, num.threads = 1,
  #control.inla = list(tolerance = 1e-7),
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)
  
#' Variances: messy
vcov_summary(mod.MMBYM)$var
#' Correlations: still messy
vcov_summary(mod.MMBYM)$cor
#' mixing 
Mmodel_compute_mixing(mod.MMBYM)


 
#' What would've been with initial values based on ICAR output ----------------#
mod.MMBYM.guided <- inla(
  Y ~ 1+ X+ f(ID, model = inla.MMBYM.model(
    k = 4, W = W, df=6, PC = T, Wishart.on.scale=F, 
    initial.values = c(rep(-3, 4),  mod.IMCAR$summary.hyperpar$mode)),
      extraconstr = constr.BYM),
  family = "poisson", data = dd, num.threads = 1,
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

 
 
#' Correlations: perfect (it's the true DGP)
#' Variances: a bit overestimated
vcov_summary(mod.MMBYM.guided)
#' Mixing: ok
Mmodel_compute_mixing(mod.MMBYM.guided)

#' Mode comparison ------------------------------------------------------------#
#' 
#' Core issue: same priors, same data yet the mode seems different:

#' -2 and 0 as starting log-standard deviation and correlation:
mode.idx <- which.max(unlist(lapply(mod.MMBYM$misc$configs$config, function(x) x$log.posterior)))
mod.MMBYM$misc$configs$config[[mode.idx]]$theta
#' Joint posterior = -5.64
mod.MMBYM$misc$configs$config[[mode.idx]]$log.posterior

#' starting values from ICAR:
mode.idx.guided <- which.max(unlist(lapply(mod.MMBYM.guided$misc$configs$config, function(x) x$log.posterior)))
mod.MMBYM.guided$misc$configs$config[[mode.idx.guided]]$theta
#' Joint posterior = -13.99 ==> Definitively not the mode!
mod.MMBYM.guided$misc$configs$config[[mode.idx.guided]]$log.posterior




#' this happens at least with zero-inflated:
#' *** Warning *** Skewness correction for transf.hyperpar is to high/low: gmean =  , corr= .
#' This IS corrected for, but is usually a sign of a ill-defined model and/or issues with the fit.
 