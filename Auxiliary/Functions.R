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
    log.p[j] <- dexp(x=sqrt(2*KLD_j), rate = rate, log = T) -log(2) +
      log(1/sqrt(2*KLD_j)) + log(abs(derivative))
  }
  return(sum(log.p))
}
#' for benchmarking, e.g.
#' theta <- c(1.32, 2.01, -0.74, -0.46, -0.16, -0.52, -0.06, -0.52, -0.08)

# #'  INLA code for M-model extension of the BYM --------------------------------
#'
#' General function to implement the M-model extension of the BYM.
#' Allows for either uniform or PC-prior on the mixing parameter.
#' Warning: the PC-prior has an extremely high computational cost.
#' The user can also choose between dense or sparse parametrisation.

inla.MMBYM.model <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.Mmodel.BYM, ...)


inla.rgeneric.Mmodel.BYM <- 
  function(cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                   "log.prior", "quit"), theta = NULL){
  
  envir <- parent.env(environment())
  if(!exists("cache.done", envir=envir)){
    starttime.scale <- Sys.time()
    #' Laplacian matrix scaling: only needs being done once
    L_unscaled <- Matrix::Diagonal(n=nrow(W), x=rowSums(as.matrix(W))) -  W
    constr <- INLA:::inla.bym.constr.internal(L_unscaled, adjust.for.con.comp = T)
    scaleQ <- INLA:::inla.scale.model.internal(
      L_unscaled, constr = list(A = constr$constr$A, e = constr$constr$e))
    L <- scaleQ$Q
    invL <- INLA:::inla.ginv(L)
    endtime.scale <- Sys.time()
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
        gammas.nonzero <- RSpectra::eigs_sym(tmp, k = nrow(W), which = "LM")$values
        if(k==1) {
          gammas[[j]] <- gammas.nonzero 
        } else {
            gammas[[j]] <- c(gammas.nonzero, rep(0, (k-1)*nrow(W)))
          }
        gammas[[j]] <- unlist(sapply(gammas[[j]], function(x) {
          if(abs(Im(x)) < 1e-15) x <- round(Re(x), 12)
        }))
        if(any(!is.numeric(gammas[[j]]))){
          message("!!! PROBLEM !!! \n!!! messed up eigenvalues here !!! \n")
          xx <- which(!is.numeric(gammas[[j]]))
          print(gammas[[j]][xx])
          print(str(gammas[[j]][xx]))
        }
        if(any(sapply(gammas, function(x) is.complex(x)))){
          message("!!! PROBLEM !!! \n!!! complex eigenvalues here !!! \n")
          print(gammas)
          cat(which(sapply(gammas, function(x) is.complex(x))))
          return(NULL)
        }
        KLD_j <- KLD(phi = phi[j], eigenvalues = gammas[[j]])
        if(KLD_j < 0){
          message("!!! PROBLEM !!!! \n!!! NEGATIVE KLD - MUST FIX MODEL !!!")
          return(NULL)
        }
        derivative <- 1/2 * sum(-gammas[[j]]/(1+phi[j]*gammas[[j]]) + gammas[[j]])
        rate <- -1/KLD(U, eigenvalues = gammas[[j]]) * log(1 - alpha)
        log.p[j] <- dexp(x=sqrt(2*KLD_j), rate = rate, log = T) -log(2) +
          log(1/sqrt(2*KLD_j)) + log(abs(derivative))
      }
      return(sum(log.p))
    }
    
    cat("Time needed for scaling Laplacian matrix: ",
        round(difftime(endtime.scale, starttime.scale), 3), " seconds \n")
    assign("L", L, envir = envir)
    assign("invL", invL, envir = envir)
    assign("inla.pc.mbym.phi", inla.pc.mbym.phi, envir = envir)
    assign("cache.done", TRUE, envir = envir)
  }
  interpret.theta <- function() {
    #' Same as the dense version, plus the M-factorisation of the scale parameter
    #' like in the M-models
    phi <-  1/(1+exp(-theta[as.integer(1:k)]))
    
    diag.N <- sapply(theta[k+1:k], function(x) {exp(x)})
    no.diag.N <- theta[2*k+1:(k*(k-1)/2)]
    
    N <- diag(diag.N) 
    N[lower.tri(N)] <- no.diag.N
    
    Sigma <- N %*% t(N)    
    e <- eigen(Sigma)
    M <- t(e$vectors %*% diag(sqrt(e$values)))
    invM.t <- diag(1/sqrt(e$values)) %*% t(e$vectors)
    Lambda <- t(invM.t) %*% invM.t
    return(list(phi = phi, M = M, Lambda = Lambda, invM.t = invM.t))
  }
  graph <- function() {
    QQ <- Q()
    G <- (QQ != 0) * 1
    return(G)
  }
  Q <- function() {
    param <- interpret.theta()
    In <- Matrix::Diagonal(nrow(W), 1)
    if(!sparse){
      MI <- kronecker(t(invM.t), In)
      BlockIW <- Matrix::bdiag(lapply(1:k, function(i) {
        solve(param$phi[i]*invL + (1-param$phi[i])*In)
      }))
      Q <- (MI %*% BlockIW) %*% kronecker(t(M.inv),In)
    } else {
      q11 <- t(param$invM.t) %*% Matrix::Diagonal(x = 1/(1 - param$phi), n = k) %*% param$invM.t
      q12 <- t(param$invM.t) %*% Matrix::Diagonal(x = sqrt(param$phi)/(1 - param$phi), n = k)
      q22 <- Matrix::Diagonal(x = param$phi/(1 - param$phi), n = k)
      
      Q11 <- kronecker(q11, In)
      Q12 <- - kronecker(q12, In)
      Q21 <- Matrix::t(Q12)
      Q22 <- kronecker(q22, In) + kronecker(Matrix::Diagonal(x=1, n=k), L)
    }
    

    #' Structure matrix. Problem: it is of size 2n * 2n
    Q <- rbind(cbind(Q11, Q12), cbind(Q21, Q22))
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
      if(is.null(val)){
        stop("!!! SOMETHING BAD HAPPENED HERE !!! \n!!! ABORTED !!! \n!!! ABORTED !!! \n!!! ABORTED !!!\n")
      }
    }
    # n^2_jj ~ chisq(k-j+1) (k degrees of freedom)
    val <- val + k*log(2) + 2*sum(theta[k+1:k]) + sum(dchisq(exp(2*theta[k+1:k]), df=k-(1:k)+1, log=TRUE))
    # df = (k+2):3
    # df=k-1:k+1
    # n_ki ~ N(0,1)
    val <- val + sum(dnorm(theta[(2*k)+1:(k*(k-1)/2)], mean=0, sd=1, log=TRUE))
    return(val)
  }
  initial <- function(){
    if(!exists("initial.values", envir= envir )){
      return(c(rep(0, k*(k+3)/2)))
    } else {
      return(initial.values)
    }
  }
  quit <- function() {
    return(invisible())
  }
  if (as.integer(R.version$major) > 3) {
    if (!length(theta))  theta <- initial()
  }
  else {
    if (is.null(theta))  theta <- initial()
  }
  val <- do.call(match.arg(cmd), args = list())
  return(val)
}



# #'  OLD: INLA code for general M-models - not includes sparse BYM/PC-prior----
inla.rgeneric.Mmodel <- 
  function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                    "log.prior", "quit"), theta = NULL) {
    envir <- parent.env(environment())
    if(!exists("cache.done", envir=envir)){
      starttime.scale <- Sys.time()
      if(Qmod == "BYM"){
        #' Unscaled Laplacian matrix (marginal precision of u_1, u_2 ... u_k)
        L_unscaled <- Matrix::Diagonal(nrow(W), rowSums(W)) -  W
        L_unscaled_block <- kronecker(diag(1,k), L_unscaled)
        A_constr <- t(pracma::nullspace(as.matrix(L_unscaled_block)))
        scaleQ <- INLA:::inla.scale.model.internal(
          L_unscaled_block, constr = list(A = A_constr, e = rep(0, nrow(A_constr))))
        #' Block Laplacian, i.e. precision of U = I_k \otimes L
        n <- nrow(W)
        L <- scaleQ$Q[c(1:n), c(1:n)]
        Sigma.u <- MASS::ginv(as.matrix(L))
        endtime.scale <- Sys.time()
        cat("Time needed for scaling Laplacian matrix: ",
            round(difftime(endtime.scale, starttime.scale), 3), " seconds \n")
        assign("Sigma.u", Sigma.u, envir = envir)
      }
      D <- Matrix::Diagonal(n=nrow(W), x=rowSums(W))
      assign("D", D, envir = envir)
      assign("cache.done", TRUE, envir = envir)
    }
    if(!exists("Bartlett", envir = envir)){
      assign("Bartlett", FALSE, envir = envir)
    }
    interpret.theta <- function() {
      alpha <- 1/(1 + exp(-theta[as.integer(1:k)]))
      if(!Bartlett){
        #' No Bartlett decomposition ==> M is modelled directly 
        #' AND the function employs k^2 parameters, i.e. the 
        #' entries of M
        M <- matrix(theta[-as.integer(1:k)], ncol = k)
      } else{
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
      }
      return(list(alpha = alpha, M = M))
    }
    graph <- function() { return(Q())}
    Q <- function() {
      param <- interpret.theta()
      M.inv <- solve(param$M)
      MI <- kronecker(M.inv, Matrix::Diagonal(nrow(W), 1))
      if(Qmod == "LCAR"){
        BlockIW <- Matrix::bdiag(lapply(1:k, function(i) {
          param$alpha[i]*(D - W) + 
            (1 - param$alpha[i]) * Matrix::Diagonal(nrow(W), 1)
        }))
      } else if (Qmod == "BYM"){
        BlockIW <- Matrix::bdiag(lapply(1:k, function(i) {
          solve(param$alpha[i]*Sigma.u + 
                  (1-param$alpha[i])*Matrix::Diagonal(nrow(W), 1))
        }))
      } else if(Qmod == "PCAR"){
        BlockIW <- Matrix::bdiag(lapply(1:k, function(i) {
          D - param$alpha[i] * W
        }))
      }
      Q <- (MI %*% BlockIW) %*% Matrix::t(MI)
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
      val <- sum(-theta[as.integer(1:k)] - 2 * log(1 + exp(-theta[as.integer(1:k)])))
      if(!Bartlett){
        #' Direct parametrisation ==> Wishart prior
        #' directly assigned to Sigma
        sigma2 <- 1 
        val = val + log(MCMCpack::dwish(W = crossprod(param$M), 
                                        v = k, S = diag(rep(sigma2, k))))
      } else {
        #' Diagonal entries of the lower-triangular
        #' factor of Sigma: Chi-squared prior
        val <- val + k * log(2) + 2 * sum(theta[k + 1:k]) + 
          sum(dchisq(exp(2 * theta[k + 1:k]), df = (k + 2) - 
                       1:k + 1, log = TRUE))
        #' Off-diagonal entries of the factor:
        #' Normal prior
        val <- val + sum(dnorm(theta[as.integer((2 * k) + 1:(k *  (k - 1)/2))],
                               mean = 0, sd = 1, log = TRUE))
      }
      
      return(val)
    }
    initial <- function() {
      if(!Bartlett){
        return(c(rep(0, k), as.vector(diag(rep(1, k)))))
      } else{
        return(c(rep(0, k * (k+3)/2)) )
      }
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
inla.Mmodel <- function (...)    INLA::inla.rgeneric.define(inla.rgeneric.Mmodel, ...)
