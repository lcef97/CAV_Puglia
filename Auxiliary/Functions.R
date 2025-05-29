#'  ---------------------------------------------------------------------------#
#'                          Auxiliary functions                                #
#'  ---------------------------------------------------------------------------#
# #'  PC-prior for multivariate BYM model --------------------------------------
#'
#' Multivariate extension of the PC-prior on the
#' BYM mixing parameter for the case of M-models.
#' Here we follow a sequential approach with as many steps as the number of
#' mixing parameters. At each j-th step, the flexible model employing a total of
#' j nonzero mixing parameters is tested against the base model with only the
#' previous nonzero j-1 mixing parameters.
#' 
#' Unfortunately, this prior seems to depend on the correlation hyperparameter.

inla.pc.mbym.phi <- function(phi, invL, M, alpha = 2/3, U = 1/2){
  In <- Matrix::Diagonal(n = nrow(W), x = 1)
  variances <- list()
  gammas <- list()
  variances[[1]] <- kronecker(t(M) %*% M, In)
  log.p <- numeric(length(phi))
  KLD <- function(phi, eigenvalues){
    res <- -1/2 * sum(log(1 + phi*eigenvalues)) +
      1/2 * phi*sum(eigenvalues)
    return(res)
  }
  deriv.KLD <- function(phi, eigenvalues){
    res <- 1/2 * sum(-eigenvalues/(1+phi*eigenvalues) + eigenvalues)
    return(res)
  }
  for(j in c(1:length(phi))){
    phi.diag <- phi
    phi.diag[-c(1:j)] <- 0
    Phi_j <- Matrix::Diagonal(x=phi.diag, n=length(phi))
    variances[[j+1]] <- variances[[j]] + phi[j] * 
      kronecker(M[j,]%*%t(M[j,]), (invL - In)) %*% solve(variances[[j]])
    tmp <- kronecker(M[j,]%*%t(M[j,]), (invL - In)) %*% solve(variances[[j]])
    gammas[[j]] <- eigen(tmp)$values
    KLD_j <- KLD(phi = phi[j], eigenvalues = gammas[[j]])
    if(KLD_j < 0){
      message("PROBLEM: NEGATIVE KLD - MUST FIX MODEL")
      return(NULL)
    }
    derivative <- 1/2 * sum(-gammas[[j]]/(1+phi[j]*gammas[[j]]) + gammas[[j]])
    rate <- -1/KLD(U, eigenvalues = gammas[[j]]) * log(1 - alpha)
    log.p[j] <- dexp(x=sqrt(2*KLD), rate = rate, log = T) -log(2) +
      log(1/2*sqrt(KLD)) + log(abs(derivative))
  }
  return(sum(log.p))
}

# #' INLA code for M-model extension of the BYM --------------------------------
#'
#' General function to implement the M-model extension of the BYM.
#' Allows for either uniform or PC-prior on the mixing parameter.
#' Warning: the PC-prior has an extremely high computational cost.
#' The user can also choose between dense or sparse parametrisation.

inla.MMBYM.model <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.Mmodel.BYM, ...)

inla.rgeneric.Mmodel.BYM <- 
  function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                    "log.prior", "quit"), theta = NULL) {
    envir <- parent.env(environment())
    if(!exists("cache.done", envir=envir)){
      starttime.scale <- Sys.time()
      #' Unscaled Laplacian matrix (marginal precision of u_1, u_2 ... u_k)
      L_unscaled <- Matrix::Diagonal(nrow(W), rowSums(W)) -  W
      constr <- INLA:::inla.bym.constr.internal(L_unscaled, adjust.for.con.comp = TRUE)
      scaleQ <- INLA:::inla.scale.model.internal(
        L_unscaled,  constr = 
          list(A = constr$constr$A, e = constr$constr$e))
      n <- nrow(W)
      L <- scaleQ$Q
      invL <- MASS::ginv(as.matrix(L))
      endtime.scale <- Sys.time()
      cat("Time needed for scaling Laplacian matrix: ",
          round(difftime(endtime.scale, starttime.scale), 3), " seconds \n")
      assign("invL", invL, envir = envir)
      assign("cache.done", TRUE, envir = envir)
    }
    interpret.theta <- function() {
      phi <- 1/(1 + exp(-theta[as.integer(1:k)]))
      #' Bartlett decomposition ==> First define Sigma, 
      #' then use its eigendecomposition to define M ==> 
      #' ==> the function employs k(k+1)/2 parameters, 
      #' i.e. lower-triangular factor in the Bartlett decomposition indeed.
      diag.N <- sapply(theta[as.integer(k + 1:k)], function(x) {
        exp(x)
      })
      no.diag.N <- theta[as.integer(2 * k + 1:(k * (k - 1)/2))]
      N <- diag(diag.N, k)
      N[lower.tri(N, diag = FALSE)] <- no.diag.N
      Sigma <- N %*% t(N)
      e <- eigen(Sigma)
      M <- t(e$vectors %*% diag(sqrt(e$values)))
      return(list(phi = phi, M = M))
    }
    graph <- function() {
      MI <- kronecker(Matrix::Matrix(1, ncol = k, nrow = k), 
                      Matrix::Diagonal(nrow(W), 1))
      IW <- Matrix::Diagonal(nrow(W), 1) + W
      BlockIW <- Matrix::bdiag(replicate(k, IW, simplify = FALSE))
      G <- (MI %*% BlockIW) %*% MI
      if(sparse){
        G <- kronecker(matrix(1, nrow=2, ncol=2), G)
      }
      return(G)
    }
    Q <- function() {
      param <- interpret.theta()
      M.inv <- solve(param$M)
      In <- Matrix::Diagonal(nrow(W), 1)
      if(!sparse){
        MI <- kronecker(M.inv, In)
        #D <- rowSums(W)
        BlockIW <- Matrix::bdiag(lapply(1:k, function(i) {
          solve(param$phi[i]*invL + (1-param$phi[i])*In)
        }))
        Q <- (MI %*% BlockIW) %*% kronecker(t(M.inv),In)
      } else {
        #' Matrix-valued mixing parameter, diagonal = phi_1 ... phi_k
        Phi <- Matrix::Diagonal(param$phi, n=k)
        #' Matrix of square roots of \phi:
        Phi.sqrt <- Matrix::Diagonal(sqrt(param$phi), n=k)
        #' Diagonal of: 1/(1-phi_1), ... 1/(1-phi_k)
        invPhihat <- Matrix::Diagonal(1/(1-param$phi), n=k)
        #' Precision blocks, defined as in equation 3
        Q.11 <- kronecker( M.inv %*% invPhihat %*% t(M.inv), In)
        Q.12 <- kronecker(-M.inv %*% invPhihat %*% Phi.sqrt, In)
        Q.22 <- kronecker(Phi %*% invPhihat, In) +
          kronecker(Matrix::Diagonal(n=k, x=1), L)
        Q <- cbind(rbind(Q.11, Matrix::t(Q.12)), 
                   rbind(Q.12, Q.22))
      }
      
      return(Q)
    }
    mu <- function() {
      return(numeric(0))
    }
    log.norm.const <- function() {
      val <- numeric(0)
      return(val)
    }
    log.prior <- function() {
      param <- interpret.theta()
      if(!PC){
        #' Uniform prior
        val <- sum(-theta[as.integer(1:k)] - 2 * log(1 + exp(-theta[as.integer(1:k)])))
      } else{
        #' PC-prior
        val <- inla.pc.mbym.phi(invL = invL, M = param$M, phi = param$phi, alpha = 2/3, U = 1/2)
      }
      #' Diagonal entries of the lower-triangular
      #' factor of Sigma: Chi-squared prior
      val <- val + k * log(2) + 2 * sum(theta[k + 1:k]) + 
        sum(dchisq(exp(2 * theta[k + 1:k]), df = (k + 2) - 
                     1:k + 1, log = TRUE))
      #' Off-diagonal entries of the factor:
      #' Normal prior
      val <- val + sum(dnorm(theta[as.integer((2 * k) + 1:(k *  (k - 1)/2))],
                               mean = 0, sd = 1, log = TRUE))
      return(val)
    }
    initial <- function() {
       return(c(rep(0, k), as.vector(diag(rep(1, k)))))
    }
    quit <- function() {
      return(invisible())
    }
    if (as.integer(R.version$major) > 3) {
      if (!length(theta)) 
        theta = initial()
    }
    else {
      if (is.null(theta)) {
        theta <- initial()
      }
    }
    val <- do.call(match.arg(cmd), args = list())
    return(val)
  }
