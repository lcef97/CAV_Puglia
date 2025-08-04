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
#' 
mod.IMCAR <- inla(
  Y ~ 1 + X +
    f(ID, model = inla.IMCAR(k = 4, W = W, df=8, Bartlett = F),
      extraconstr = list(A=constr.BYM$A[,-c(1:(4*n))], e = c(0,0,0,0)) ),
  family = "poisson", data = dd, num.threads = 1,control.inla=list(cmin=0),
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

