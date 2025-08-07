##' Simulate BYM model ---------------------------------------------------------

#' Neighbourhood matrix  ==> binary
load(url("https://github.com/lcef97/CAV_Puglia/raw/main/input/Replication/W.RData"))
#' Source functions to implement models; 
#' Link to R code: 'https://github.com/lcef97/CAV_Puglia/blob/main/Auxiliary/Functions.R'
devtools::source_url("https://raw.githubusercontent.com/lcef97/CAV_Puglia/refs/heads/main/Auxiliary/Functions.R")

n <- nrow(W)
#' number of variables --------------------------------------------------------#
k <- 3
#' Hyperparameters ------------------------------------------------------------#
phi.true <- c(0.6, 0.4, 0.9)
sigma.true <- c(0.7, 1.1, 0.6)
corr.true <- c(0.9, 0.7, 0.5)
Scale.true <- varcov(c(sigma.true, corr.true), tri=F)
M.true <- diag(sqrt(eigen(Scale.true)$values)) %*% t(eigen(Scale.true)$vectors)

#' Add covariate --------------------------------------------------------------#
set.seed(11)
X <- runif(n=k*n, min = -sqrt(3), max = sqrt(3))
beta0 <- 2
beta1 <- -0.6

#' Simulate BYM field ---------------------------------------------------------#
L.unscaled <- Matrix::Diagonal(rowSums(W), n=n) - W
L <- INLA:::inla.scale.model.bym.internal(L.unscaled)$Q
constr <- INLA:::inla.bym.constr.internal(L)
constr.BYM <- list(A = cbind(Matrix::Matrix(0, nrow=k, ncol = k*n), 
                             kronecker(diag(k), constr$constr$A)),
                   e = rep(0, k))
#' Change seed!
set.seed(11)
#' IID component
V.true <- matrix(rnorm(n=k*n, mean=0, sd=1), nrow = n, ncol = k, byrow=F)
#' ICAR component
U.lst <- lapply(c(1:k), function(i) {
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

dd <- data.frame(Y = y.sim, ID = c(1:(k*n)), X=X, group=rep(c(1:k), each=n))

##' Toy examples: simpler models -----------------------------------------------

#' Easiest: IMCAR -------------------------------------------------------------#
#' 
#' Not the true model, but very easy to fit.
#' Covariances matrix has Wishart(2k, I_k) prior. 
#' If Bartlett factorisation is used, all IMCAR, PMCAR and LMCAR work well.

mod.IMCAR <- inla(
  Y ~ 1 + X +
    f(ID, model = inla.IMCAR.Bartlett(k = 3, W = W, df=6, scale.model = T ),
      extraconstr = list(A=constr.BYM$A[,-c(1:(k*n))], e = rep(0, k)) ),
  family = "poisson", data = dd, num.threads = 1, 
  control.inla = list(tolerance=1e-7, h=1e-5),
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

#' Hyperparameters posterior.
#' Variance ==> Overestimated, like 2x  (not true DGP...)
vcov_summary(mod.IMCAR)$var
#' Correlations: good, maybe a little underestimated
vcov_summary(mod.IMCAR)$cor
 

#' BYM TOY example: independent fields ----------------------------------------#
#'
#' Of course it is not the true DGP either. 
#' sum-to-zero constraint on the ICAR component and NOT on the convolution component,
#' as it appears to be in the univariate version implemented with `model='bym2'` 
#' 
mod.INDMMBYM  <- inla(
  Y ~ 1+ X+
    f(ID, model = inla.INDMMBYM.model(k = 3, W = W, df=6, PC = T),
      extraconstr = constr.BYM),
  family = "poisson", data = dd, num.threads = 1,
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)
# Mixing parameter ==> perfect
Mmodel_compute_mixing(mod.INDMMBYM) 
#' Variances ==> literally perfect
data.frame( do.call(rbind, lapply(
  lapply(mod.INDMMBYM$marginals.hyperpar[-c(1:k)], function(f){
    inla.tmarginal(fun = function(X) exp(-X), marginal = f)
  }), function(x) unlist(inla.zmarginal(x, silent = TRUE)))))[,c(1,2,3,5,7)]


##' multivariate BYM - where trouble begins ------------------------------------
 
#' A priori, again, var-cov ~ Wishart(2k, I_k), while the mixing parameter
#' follows a multivariate Uniform distribution 
#' WARNING: slow - falls into `Enable early_stop`` 
mod.MMBYM <- inla(
  Y ~ 1+ X+ f(ID, model = inla.MMBYM.model(
    k = 3, W = W, df=6 ),
      extraconstr = constr.BYM),
  family = "poisson", data = dd, num.threads = 1,
  control.inla = list(tolerance = 1e-7),
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)
  

#' Correlations: definitively out of the way; variance: messy.
vcov_summary(mod.MMBYM)
#' mixing: also quite messy
Mmodel_compute_mixing(mod.MMBYM)
 


#' What would've been with initial values based on ICAR output ----------------#

mod.MMBYM.guided <- inla(
  Y ~ 1+ X+  f(ID, model = inla.MMBYM.model(
    k = 3, W = W, df=6,    
    initial.values = c(rep(-3, 4), mod.IMCAR$summary.hyperpar$mode)),
      extraconstr = constr.BYM),
  family = "poisson", data = dd, num.threads = 1,
  #control.inla = list(tolerance = 1e-7),
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

#' Correlations: perfect (it's the true DGP)
#' Variances: perfect
vcov_summary(mod.MMBYM.guided)
#' Mixing: ok
Mmodel_compute_mixing(mod.MMBYM.guided)

#' Mode comparison ------------------------------------------------------------#
#' 
#' Core issue: same priors, same data yet the mode seems different:

#' -2 and 0 as starting log-standard deviation and correlation:
mod.MMBYM$mode$log.posterior.mode
mod.MMBYM$misc$configs$max.log.posterior

#' starting values from ICAR:
mod.MMBYM.guided$mode$log.posterior.mode
mod.MMBYM.guided$misc$configs$max.log.posterior

