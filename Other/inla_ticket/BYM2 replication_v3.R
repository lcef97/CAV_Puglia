##' Simulate BYM model - limit case --------------------------------------------

#' Neighbourhood matrix  ==> binary
load(url("https://github.com/lcef97/CAV_Puglia/raw/main/input/Replication/W.RData"))
n <- nrow(W)

#' Hyperparameters ------------------------------------------------------------#
#' Mixing
phi.true <- c(0.998, 0.998, 0.998, 0.998)
#' Standard deviations
sigma.true <- c(0.9, 1.1, 0.7, 0.6)
# Correlations
corr.true <- c(0.9, 0.7, 0.6, 0.6, 0.8, 0.6)
#' Variance-covariance matrix
R.true <- array(0, dim=c(4,4))
R.true[lower.tri(R.true)] <- corr.true
R.true <- R.true + t(R.true) + diag(x=1, nrow=nrow(R.true))
Scale.true <- diag(sigma.true) %*% R.true %*% diag(sigma.true)
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

dd <- data.frame(Y = y.sim, ID = c(1:(4*n)), X=X,
                 group=rep(c(1:4), each=n))

##' Models implementation with INLA --------------------------------------------

library(INLA)
#' Source functions to implement models 
devtools::source_url("https://raw.githubusercontent.com/lcef97/CAV_Puglia/refs/heads/main/Auxiliary/Functions.R")

##' Toy example: simpler models ------------------------------------------------
##' 
#' Easiest: IMCAR -------------------------------------------------------------#
#' Since the DGP is actually a limit case, this should be the best fit.
mod.IMCAR <- inla(
  Y ~ 1 + X +
    f(ID, model = inla.IMCAR.Bartlett(k = 4, W = W, df=8,  
                                      #scale.fac = scale.fac.prior,
                                      initial.values = c(rep(-2, 4), rep(0,6))),
      extraconstr = list(A=constr.BYM$A[,-c(1:(4*n))], e = c(0,0,0,0)) ),
  family = "poisson", data = dd,
  num.threads = 1,
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)
#' Hyperparameters posterior.
#' Variance ==> Overestimated 
vcov_summary(mod.IMCAR, k=4)$var
#' Correlations: literally, perfect
vcov_summary(mod.IMCAR, k=4)$cor
#' WAIC: lowest to be seen around here, though model is miss-specified
mod.IMCAR$waic$waic

 
#' Other toy example: PCAR ----------------------------------------------------#

mod.PMCAR <- inla(
  Y ~ 1+ X+
    f(ID, model = inla.PMCAR.Bartlett(k = 4, W = W, df=8, PC = F )),
  family = "poisson", data = dd, num.threads = 1,
  control.inla = list(stupid.search=F, h=1e-5), safe = F,
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)
vcov_summary(mod.PMCAR, k=4)

 
#' BYM toy example: independent fields ----------------------------------------#
mod.INDMMBYM.zero <- inla(
  Y ~ 1+ X+
    f(ID, model = inla.INDMMBYM.model(k = 4, W = W, df=6, PC = T ),
      extraconstr = constr.BYM),
  family = "poisson", data = dd, num.threads = 1,
  #control.inla = list(verbose = T),
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)
# Mixing parameter ==> Underestimated but close
data.frame( do.call(rbind, lapply(
  lapply(mod.INDMMBYM.zero$marginals.hyperpar[c(1:4)], function(f){
      inla.tmarginal(fun = function(X) 1/(1 + exp(-X)), marginal = f)
    }), function(x) unlist(inla.zmarginal(x, silent = TRUE))))) %>% 
  dplyr::select(1,2,3,5,7)
#' Variances ==> perfect
data.frame( do.call(rbind, lapply(
  lapply(mod.INDMMBYM.zero$marginals.hyperpar[c(5:8)], function(f){
    inla.tmarginal(fun = function(X) exp(-X), marginal = f)
  }), function(x) unlist(inla.zmarginal(x, silent = TRUE))))) %>% 
  dplyr::select(1,2,3,5,7)


#' Second toy example: unique mixing parameter (which in this limit case is actually true)
mod.MBYM.zero <- inla(
  Y ~ 1+ X+
    f(ID, model = inla.MBYM.Bartlett(k = 4, W = W, df=6, PC = F, sparse=T ),
      extraconstr = constr.BYM),
  family = "poisson", data = dd, num.threads = 1,
  control.inla = list(h=1e-4), safe = FALSE,
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)
 



#' Default configuration of the BYM
mod.MMBYM <- inla(
  Y ~ 1+ X+
    f(ID, model = inla.MMBYM.model(k = 4, W = W, df=8, PC = F ),
      extraconstr = constr.BYM),
  family = "poisson", data = dd, num.threads = 1,
  #control.inla = list(h=1e-4),
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

vcov_summary(mod.MMBYM.zero, k=4)$var
#' Correlations: reasonable
vcov_summary(mod.MMBYM.zero, k=4)$cor
#' WAIC: lowest to be seen around here, though model is miss-specified
mod.MMBYM.zero$waic$waic
Mmodel_compute_mixing(mod.MMBYM.zero, k=4)


#' Posteriors: all concentrated around zero. 
mod.MMBYM.bis <- inla.rerun(mod.MMBYM.zero)
vcov_summary(mod.MMBYM.bis, k=4)$var
#' Correlations: reasonable
vcov_summary(mod.MMBYM.bis, k=4)$cor
#' WAIC: lowest to be seen around here, though model is miss-specified
mod.MMBYM.bis$waic$waic
Mmodel_compute_mixing(mod.MMBYM.bis, k=4)

tmp <- file("modMMBYMinit_logfile.txt")
writeLines(mod.MMBYM.zero$logfile, con=tmp)
close(tmp)



#' 
mod.MMBYM.guided <- inla(
  Y ~ 1+ X+
    f(ID, model = inla.MMBYM.model(k = 4, W = W, df=6, PC = T,
                                   initial.values = c(rep(-3, 4),  mod.IMCAR$summary.hyperpar$mode)),
      extraconstr = constr.BYM),
  family = "poisson", data = dd, num.threads = 1,
  control.inla = list(stupid.search=F, h=1e-5), safe = F,
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

vcov_summary(mod.MMBYM.guided, k=4)$var
#' Correlations: perfect (it's the true DGP)
vcov_summary(mod.MMBYM.guided, k=4)$cor
#' WAIC: lowest to be seen around here, though model is miss-specified
mod.MMBYM.guided$waic$waic
Mmodel_compute_mixing(mod.MMBYM.guided, k=4)




#' Variance ==> 2nd and 4th are plausible, 1st and 3rd definitively underestimated
vcov_summary(mod.MMBYM.init, k=4)$var
#' Correlations: messed up
vcov_summary(mod.MMBYM.init, k=4)$cor
#' WAIC: definitively worse than ICAR
mod.MMBYM.init$waic$waic


mod.MMBYM.init.bis <- inla.rerun(mod.MMBYM.init)

#' Variance ==> utter mess 
vcov_summary(mod.MMBYM.init.bis, k=4)$var
#' Correlations: messy, but at least CI are all in the positive axis
vcov_summary(mod.MMBYM.init.bis, k=4)$cor
#' WAIC: improved, but higher than ICAR
mod.MMBYM.init.bis$waic$waic

##' Alternative models: PCAR and LCAR ------------------------------------------
 
#' PMCAR ----------------------------------------------------------------------#
mod.PMMCAR <- inla(
  Y ~ 1+ X+
    f(ID, model = inla.PMMCAR.model(k = 4, W = W, df=6, PC = F )),
  family = "poisson", data = dd, num.threads = 1,
  control.inla = list(stupid.search=F, h=1e-5), safe = F,
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)
#mod.PMMCAR.init.bis <- inla.rerun(mod.PMMCAR.init)

vcov_summary(mod.PMMCAR, k=4)

#' LMCAR ----------------------------------------------------------------------#
mod.LMMCAR <- inla(
  Y ~ 1+ X+
    f(ID, model = inla.LMMCAR.model(k = 4, W = W, df=6, PC = T )),
  family = "poisson", data = dd, num.threads = 1,
  control.inla = list(stupid.search=F, h=1e-5), safe = F,
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)
 

