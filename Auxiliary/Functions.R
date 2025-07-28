#'  ---------------------------------------------------------------------------#
#'                          Auxiliary functions                                #
#'  ---------------------------------------------------------------------------#
# #'  INLA code for factorisable autoregressive models ---------------------------------------
#' First of all, recalling some package will be useful
library(INLA)
library(INLAMSM)
library(magrittr)

##'    IMCAR model ------------------------------------------------------------#
#' Bartlett decomposition ==> Scale parameter factorised in two triangular
#' matrices; off-diagonal entries are assinged a Normal prior,
#' squares of diagonal are assigned Chi-squared prior 
#'  ==> it is possible to show that the scale parameter thus follows 
#'  a Wishart prior.

inla.rgeneric.IMCAR.Bartlett <- 
  function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                    "log.prior", "quit"), theta = NULL) {
    envir <- parent.env(environment())
    if(!exists("scale.fac", envir = envir)) assign("scale.fac", diag(k))
    if(!exists("Wishart.on.scale", envir = envir)) assign("Wishart.on.scale", TRUE, envir = envir)
    interpret.theta <- function() {
      diag.N <- sapply(theta[as.integer(1:k)], function(x) {exp(x)})
      no.diag.N <- theta[as.integer(k + 1:(k * (k - 1)/2))]
      N <- diag(diag.N, k)
      N[lower.tri(N, diag = FALSE)] <- no.diag.N
      N <- scale.fac %*% N
      Wish.mat <- N %*% t(N) 
      return(list(Wish.mat = Wish.mat))
    }
    graph <- function() {
      PREC <- matrix(1, ncol = k, nrow = k)
      G <- kronecker(PREC, Matrix::Diagonal(nrow(W), 1) +  W)
      return(G)
    }
    Q <- function() {
      param <- interpret.theta()
      if(Wishart.on.scale){
        PREC <- solve(param$Wish.mat)
      } else PREC <- param$Wish.mat
      Q <- kronecker(PREC,  Matrix::Diagonal(nrow(W),  apply(W, 1, sum)) - W)
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
      if(!exists("df", envir = envir)) df <- k+2
      param <- interpret.theta()
      val <- k * log(2) + 2 * sum(theta[1:k]) + 
        sum(dchisq(exp(2 * theta[1:k]), df = c(df:(df-k+1)), log = TRUE)) +
        sum(dnorm(theta[as.integer(k + 1:(k * (k -   1)/2))],
                             mean = 0, sd = 1, log = TRUE))
      return(val)
    }
    initial <- function(){
      if(!exists("initial.values", envir= envir )){
        return(c(rep(0, k*(k+1)/2)))
      } else {
        return(initial.values)
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

inla.IMCAR.Bartlett <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.IMCAR.Bartlett, ...)


##' PMCAR model ---------------------------------------------------------------#

inla.rgeneric.PMCAR.Bartlett <- 
  function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                    "log.prior", "quit"), theta = NULL) {
    envir <- parent.env(environment())
    if(!exists("PC", envir = envir)) PC <- FALSE
    if(!exists("Wishart.on.scale", envir = envir)) Wishart.on.scale <- TRUE
    if(!exists("scale.fac", envir = envir)) scale.fac <- diag(k)
    if(!exists("cache.done", envir=envir)){
      D <- Matrix::Diagonal(n=nrow(W), x=rowSums(as.matrix(W))) 
      assign("D", D, envir = envir)
      if(PC) {
        eigenvalues <- eigen(solve(D) %*% W)$values
        inla.pc.pmmcar.rho <- function(rho, eigenvalues, alpha = 2/3, U = 1/2){
          n <- length(eigenvalues)
          In <- Matrix::Diagonal(n = n, x = 1)
          log.p <- numeric(length(rho))
          KLD <- function(rho, eigenvalues){
            res <-  1/2 * sum(log(1 - rho*eigenvalues)) +
              1/2 * sum(1/(1 - rho * eigenvalues) - 1)
            return(res)
          }
          for(j in c(1:length(rho))){
            KLD_j <- KLD(rho = rho[j], eigenvalues = eigenvalues)
            if(KLD_j <= 0){
              message("!!! PROBLEM !!!! \n!!! NEGATIVE OR NULL KLD - MUST FIX MODEL !!!")
              return(NULL)
            }
            derivative <- 1/2 * sum( (rho[j]*eigenvalues^2)/(1 - rho[j]*eigenvalues)^2 )
            rate <- -1/sqrt(2*KLD(U, eigenvalues = eigenvalues)) * log(1 - alpha)
            log.p[j] <- dexp(x=sqrt(2*KLD_j), rate = rate, log = T) -log(2) +
              log(1/sqrt(2*KLD_j)) + log(abs(derivative))
          }
          return(sum(log.p))
        }
        assign("inla.pc.pmmcar.rho", inla.pc.pmmcar.rho, envir = envir)
        assign("eigenvalues", eigenvalues, envir = envir)
      }
      assign("cache.done", TRUE, envir = envir)
    }
    interpret.theta <- function() {
      rho <- 1/(1 + exp(-theta[1L]))
      diag.N <- sapply(theta[as.integer(2:(k+1))], function(x) {exp(x)})
      no.diag.N <- theta[as.integer( (k+1):(k*(k+1)/2) +1)]
      N <- diag(diag.N, k)
      N[lower.tri(N, diag = FALSE)] <- no.diag.N
      N <- scale.fac %*% N
      Wish.mat <-  N %*% t(N) 
      return(list(rho = rho, Wish.mat = Wish.mat))
    }
    graph <- function() {
      PREC <- matrix(1, ncol = k, nrow = k)
      G <- kronecker(PREC, Matrix::Diagonal(nrow(W), 1) +  W)
      return(G)
    }
    Q <- function() {
      param <- interpret.theta()
      if(Wishart.on.scale){
        PREC <- solve(param$Wish.mat)
      } else PREC <- param$Wish.mat
      
      Q <- kronecker(PREC, D - param$rho*W)
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
      if(!exists("df", envir = envir)) df <- k+2
      param <- interpret.theta()
      #' Uniform prior on the autoregressive parameter
      if(!PC){
        #' Uniform prior
        val <- -theta[1L] - 2 * log(1 + exp(-theta[1L]))
      } else{
        #' PC-prior
        if(!exists("alpha", envir = envir)) alpha <- 2/3
        if(!exists("U" , envir = envir)) U <- 1/2
        val <- inla.pc.pmmcar.rho(eigenvalues = eigenvalues, rho = rep(param$rho, k), 
                                  alpha = alpha, U = U) -
          theta[1L] - 2 * log(1 + exp(-theta[1L]))
      }
        #' Chi-squared on the diagonal
      val <- val + k * log(2) + 2 * sum(theta[2:(k+1)]) + 
        sum(dchisq(exp(2 * theta[2:(k+1)]), df = c(df:(df-k+1)), log = TRUE)) +
        #' Normal on the off-diagonal
        sum(dnorm(theta[as.integer( (k+1):(k*(k+1)/2) +1 )],
                             mean = 0, sd = 1, log = TRUE))
      return(val)
    }
    initial <- function(){
      if(!exists("initial.values", envir= envir )){
        return(c(0, rep(0, k*(k+1)/2)))
      } else {
        return(initial.values)
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

inla.PMCAR.Bartlett <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.PMCAR.Bartlett, ...)


##' LMCAR model ---------------------------------------------------------------#

inla.rgeneric.LMCAR.Bartlett <- 
  function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                    "log.prior", "quit"), theta = NULL) {
    envir <- parent.env(environment())
    if(!exists("scale.fac", envir = envir)) scale.fac <- diag(k) 
    if(!exists("Wishart.on.scale", envir = envir)) Wishart.on.scale <- TRUE
    if(!exists("PC", envir = envir)) PC <- FALSE
    if(!exists("cache.done", envir=envir)){
      L <- Matrix::Diagonal(n=nrow(W), x=rowSums(as.matrix(W))) -  W
      In <- Matrix::Diagonal(n = nrow(W), x = 1)
      if(PC) {
        eigenvalues <- eigen(L - In)$values
        inla.pc.lmmcar.lambda <- function(lambda, eigenvalues, alpha = 2/3, U = 1/2){
          n <- length(eigenvalues)
          In <- Matrix::Diagonal(n = n, x = 1)
          #gammas <- list()
          log.p <- numeric(length(lambda))
          KLD <- function(lambda, eigenvalues){
            res <-  1/2 * sum(log(1 + lambda*eigenvalues)) +
              1/2 * sum(1/(1 + lambda * eigenvalues) - 1)
            return(res)
          }
          for(j in c(1:length(lambda))){
            KLD_j <- KLD(lambda = lambda[j], eigenvalues = eigenvalues)
            if(KLD_j <= 0){
              message("!!! PROBLEM !!!! \n!!! NEGATIVE OR NULL KLD - MUST FIX MODEL !!!")
              return(NULL)
            }
            derivative <- 1/2 * sum( (lambda[j]*eigenvalues^2)/(1 + lambda[j]*eigenvalues)^2 )
            rate <- -1/sqrt(2*KLD(U, eigenvalues = eigenvalues)) * log(1 - alpha)
            log.p[j] <- dexp(x=sqrt(2*KLD_j), rate = rate, log = T) -log(2) +
              log(1/sqrt(2*KLD_j)) + log(abs(derivative))
          }
          return(sum(log.p))
        }
        assign("inla.pc.lmmcar.lambda", inla.pc.lmmcar.lambda, envir = envir)
        assign("eigenvalues", eigenvalues, envir = envir)
      }
      assign("L", L, envir = envir)
      assign("cache.done", TRUE, envir = envir)
    }
    interpret.theta <- function() {
      lambda <- 1/(1 + exp(-theta[1L]))
      diag.N <- sapply(theta[as.integer(2:(k+1))], function(x) {exp(x)})
      no.diag.N <- theta[as.integer( (k+1):(k*(k+1)/2) +1)]
      N <- diag(diag.N, k)
      N[lower.tri(N, diag = FALSE)] <- no.diag.N
      N <- scale.fac %*% N
      Wish.mat <- N %*% t(N) 
      return(list(lambda = lambda, Wish.mat = Wish.mat))
    }
    graph <- function() {
      PREC <- matrix(1, ncol = k, nrow = k)
      G <- kronecker(PREC, Matrix::Diagonal(nrow(W), 1) +  W)
      return(G)
    }
    Q <- function() {
      param <- interpret.theta()
      if(Wishart.on.scale){
        PREC <- solve(param$Wish.mat)
      } else PREC <- param$Wish.mat
      blockIW <- param$lambda * L +
        (1-param$lambda)*Matrix::Diagonal(n=nrow(W), x=1)
      Q <- kronecker(PREC, blockIW)
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
      if(!exists("df", envir = envir)) df <- k+2
      param <- interpret.theta()
      #' Uniform prior on the autoregressive parameter
      if(!PC){
        val <- -theta[1L] - 2 * log(1 + exp(-theta[1L]))
      } else {
        if(!exists("alpha", envir = envir)) alpha <- 2/3
        if(!exists("U" , envir = envir)) U <- 1/2
        val <- inla.pc.lmmcar.lambda(eigenvalues = eigenvalues,
                                     lambda = rep(param$lambda, k), alpha = alpha, U = U) -
          theta[1L] - 2 * log(1 + exp(-theta[1L]))
        
      }
      val <- val + 
        #' Chi-squared on the diagonal
        k * log(2) + 2 * sum(theta[2:(k+1)]) + 
        sum(dchisq(exp(2 * theta[2:(k+1)]), df = c(df:(df-k+1)), log = TRUE)) +
        #' Normal on the off-diagonal
        sum(dnorm(theta[as.integer( (k+1):(k*(k+1)/2) +1 )],
                  mean = 0, sd = 1, log = TRUE))
      return(val)
    }
    initial <- function(){
      if(!exists("initial.values", envir= envir )){
        return(c(0, rep(0, k*(k+1)/2)))
      } else {
        return(initial.values)
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

inla.LMCAR.Bartlett <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.LMCAR.Bartlett, ...)

##' BYM -----------------------------------------------------------------------#

inla.rgeneric.MBYM.Bartlett <- function(
    cmd = c("graph", "Q", "mu", "initial", "log.norm.const",
            "log.prior", "quit"), theta = NULL){
  
  envir <- parent.env(environment())
  if(!exists("scale.fac", envir = envir)) assign("scale.fac", diag(k), envir = envir) 
  if(!exists("PC", envir = envir)) PC <- FALSE
  if(!exists("Wishart.on.scale", envir = envir)) Wishart.on.scale <- TRUE
  
  if(!exists("cache.done", envir=envir)){

    if(!exists("df", envir = envir)) df <- k+2
    L_unscaled <- Matrix::Diagonal(n=nrow(W), x=rowSums(as.matrix(W))) -  W
    constr <- INLA:::inla.bym.constr.internal(L_unscaled, adjust.for.con.comp = T)
    scaleQ <- INLA:::inla.scale.model.internal(
      L_unscaled, constr = list(A = constr$constr$A, e = constr$constr$e))
    L <- scaleQ$Q
    invL <- INLA:::inla.ginv(L)
    In <- Matrix::Diagonal(n = nrow(W), x = 1)
    eigenvalues <- eigen(invL)$values
    eigenvectors <- eigen(invL)$vectors
    inla.pc.mbym.phi <- function(phi, eigenvalues, alpha = 2/3, U = 1/2){
      n <- length(eigenvalues)
      In <- Matrix::Diagonal(n = n, x = 1)
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
        KLD_j <- KLD(phi = phi[j], eigenvalues = eigenvalues)
        if(KLD_j <= 0){
          message("!!! PROBLEM !!!! \n!!! NEGATIVE OR NULL KLD - MUST FIX MODEL !!!")
          return(NULL)
        }
        derivative <- 1/2 * sum(eigenvalues - eigenvalues / (1 + phi[j] * eigenvalues))
        rate <- -1/sqrt(2*KLD(U, eigenvalues = eigenvalues)) * log(1 - alpha)
        log.p[j] <- dexp(x=sqrt(2*KLD_j), rate = rate, log = T) -log(2) +
          log(1/sqrt(2*KLD_j)) + log(abs(derivative))
      }
      return(sum(log.p))
    }

    assign("L", L, envir = envir)
    assign("invL", invL, envir = envir)
    assign("inla.pc.mbym.phi", inla.pc.mbym.phi, envir = envir)
    assign("eigenvalues", eigenvalues, envir = envir)
    assign("eigenvectors", eigenvectors, envir = envir)
    assign("cache.done", TRUE, envir = envir)
  }
  interpret.theta <- function() {
    phi <-  1/(1 + exp(-theta[1L]))
    diag.N <- sapply(theta[as.integer(2:(k+1))], function(x) {exp(x)})
    no.diag.N <- theta[as.integer( (k+1):(k*(k+1)/2) +1)]
    N <- diag(diag.N, k)
    N[lower.tri(N, diag = FALSE)] <- no.diag.N
    N <- scale.fac %*% N
    Wish.mat <-  N %*% t(N)
    if(Wishart.on.scale) {
      Sigma <- Wish.mat
      PREC <- solve(Sigma)
    } else  {
      Sigma <- solve(Wish.mat)
      PREC <-  Wish.mat
    }
    e <- eigen(Sigma)
    invM.t <- diag(1/sqrt(e$values)) %*% t(e$vectors)
    return(list(phi = phi, PREC = PREC, invM.t = invM.t))
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
      R <- eigenvectors %*% 
        solve(param$phi * Matrix::Diagonal(x = eigenvalues, n = length (eigenvalues)) +
                (1 - param$phi) * In) %*% t(eigenvectors)
      Q <- kronecker(param$PREC, R)
    } else {
      q11 <- 1/(1-param$phi) * param$PREC
      q12 <- sqrt(param$phi)/(1-param$phi) * t(param$invM.t)
      q22 <- param$phi/(1-param$phi) * Matrix::Diagonal(x=1, n=k)
      
      Q11 <- kronecker(q11, In)
      Q12 <- - kronecker(q12, In)
      Q21 <- Matrix::t(Q12)
      Q22 <- kronecker(q22, In) + kronecker(Matrix::Diagonal(x=1, n=k), L)
      Q <- rbind(cbind(Q11, Q12), cbind(Q21, Q22))
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
    if(! PC) {
      val <- -theta[1L] - 2 * log(1 + exp(-theta[1L]))
    } else {
      if(!exists("alpha", envir = envir)) alpha <- 2/3
      if(!exists("U" , envir = envir)) U <- 1/2
      val <- inla.pc.mbym.phi(eigenvalues = eigenvalues, phi = rep(param$phi, k), alpha = alpha, U = U) -
        theta[1L] - 2 * log(1 + exp(-theta[1L]))
    }
    val <- val +
      sum(dchisq(exp(2 * theta[1:k]), df = c(df:(df-k+1)), log = TRUE)) +
      sum(dnorm(theta[as.integer(k + 1:(k * (k -   1)/2))],
                mean = 0, sd = 1, log = TRUE))
    return(val)
  }
  initial <- function(){
    if(!exists("initial.values", envir= envir )){
      return(c(rep(0, 1+(k*(k+1)/2))))
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
inla.MBYM.Bartlett <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.MBYM.Bartlett, ...)



#' 
# #'  INLA code for M-model LCAR (based on bigDM)  -----------------------------

inla.LMMCAR.model <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.Mmodel.LCAR, ...)

inla.rgeneric.Mmodel.LCAR <- 
  function(cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                   "log.prior", "quit"), theta = NULL){
    
    envir <- parent.env(environment())
    if(!exists("df", envir = envir)) assign("df", k + 2, envir = envir)
    if(!exists("PC", envir = envir)) assign("PC", FALSE, envir = envir)
    if(!exists("scale.fac", envir = envir)) assign("scale.fac", diag(k))
    if(!exists("Wishart.on.scale", envir = envir)) assign("Wishart.on.scale", TRUE, envir = envir)
    if(!exists("cache.done", envir=envir)){
      L <- Matrix::Diagonal(n=nrow(W), x=rowSums(as.matrix(W))) -  W
      In <- Matrix::Diagonal(n = nrow(W), x = 1)
      if(PC) {
        eigenvalues <- eigen(L - In)$values
        inla.pc.lmmcar.lambda <- function(lambda, eigenvalues, alpha = 2/3, U = 1/2){
          n <- length(eigenvalues)
          In <- Matrix::Diagonal(n = n, x = 1)
          #gammas <- list()
          log.p <- numeric(length(lambda))
          KLD <- function(lambda, eigenvalues){
            res <-  1/2 * sum(log(1 + lambda*eigenvalues)) +
              1/2 * sum(1/(1 + lambda * eigenvalues) - 1)
            return(res)
          }
          for(j in c(1:length(lambda))){
            KLD_j <- KLD(lambda = lambda[j], eigenvalues = eigenvalues)
            if(KLD_j <= 0){
              message("!!! PROBLEM !!!! \n!!! NEGATIVE OR NULL KLD - MUST FIX MODEL !!!")
              return(NULL)
            }
            derivative <- 1/2 * sum( (lambda[j]*eigenvalues^2)/(1 + lambda[j]*eigenvalues)^2 )
            rate <- -1/sqrt(2*KLD(U, eigenvalues = eigenvalues)) * log(1 - alpha)
            log.p[j] <- dexp(x=sqrt(2*KLD_j), rate = rate, log = T) -log(2) +
              log(1/sqrt(2*KLD_j)) + log(abs(derivative))
          }
          return(sum(log.p))
        }
        assign("inla.pc.lmmcar.lambda", inla.pc.lmmcar.lambda, envir = envir)
        assign("eigenvalues", eigenvalues, envir = envir)
      }
      assign("L", L, envir = envir)
      assign("cache.done", TRUE, envir = envir)
    }
    interpret.theta <- function() {
      lambda <- 1/(1 + exp(-theta[as.integer(1:k)]))

      diag.N <- sapply(theta[as.integer(k + 1:k)], function(x) exp(x) )
      no.diag.N <- theta[as.integer(2 * k + 1:(k * (k - 1)/2))]
      N <- diag(diag.N, k)
      N[lower.tri(N, diag = FALSE)] <- no.diag.N
      N <- scale.fac %*% N
      if(Wishart.on.scale){
        Sigma <- N %*% t(N)
      } else{
        Sigma <- solve(N %*% t(N))
      }
      e <- eigen(Sigma)
      M <- t(e$vectors %*% diag(sqrt(e$values)))
      invM <- solve(M)#e$vectors %*% diag(sqrt(1/e$values))
      return(list(lambda = lambda, Sigma = Sigma, M = M, invM = invM))
    }
    graph <- function() {
      QQ <- Q()
      G <- (QQ != 0) * 1
      return(G)
    }
    Q <- function() {
      param <- interpret.theta()
      In <- Matrix::Diagonal(nrow(W), 1)
      #M.inv <- solve(param$M)
      MI <- kronecker(param$invM, In)
      BlockIW <- kronecker(diag(param$lambda), L) + kronecker(diag(1 -  param$lambda), In)
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
      if(!PC){
        #' Uniform prior
        val <- sum(-theta[as.integer(1:k)] - 2 * log(1 + exp(-theta[as.integer(1:k)])))
      } else{
        #' PC-prior
        if(!exists("alpha", envir = envir)) alpha <- 2/3
        if(!exists("U" , envir = envir)) U <- 1/2
        val <- inla.pc.lmmcar.lambda(eigenvalues = eigenvalues, lambda = param$lambda, alpha = alpha, U = U) +
          sum(-theta[as.integer(1:k)] - 2 * log(1 + exp(-theta[as.integer(1:k)])))
      }
      
      # n^2_jj ~ chisq(k-j+1) (k degrees of freedom)
      val <- val + k * log(2) + 2 * sum(theta[k + 1:k]) + 
        sum(dchisq(exp(2 * theta[k + 1:k]), df = c(df:(df-k+1)), log = TRUE))
      # n_ki ~ N(0,1)
      val <- val + sum(dnorm(theta[(2*k)+1:(k*(k-1)/2)], mean=0, sd=1, log=TRUE))
       
      return(val)
    }
    initial <- function(){
      if(!exists("initial.values", envir= envir )){
        return(c( rep(0, k*(k+3)/2))) #changes wrt bigDM's log(9)
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


# #'  INLA code for M-model PCAR (based on bigDM)  -----------------------------

inla.PMMCAR.model <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.Mmodel.PCAR, ...)

inla.rgeneric.Mmodel.PCAR <- 
  function(cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                   "log.prior", "quit"), theta = NULL){
    
    envir <- parent.env(environment())
    if(!exists("df", envir = envir)) assign("df", k + 2, envir = envir)
    if(!exists("PC", envir = envir)) assign("PC", FALSE, envir = envir)
    if(!exists("scale.fac", envir = envir)) assign("scale.fac", diag(k))
    if(!exists("Wishart.on.scale", envir = envir)) assign("Wishart.on.scale", TRUE, envir = envir)
    if(!exists("cache.done", envir=envir)){
      D <- Matrix::Diagonal(n=nrow(W), x=rowSums(as.matrix(W))) 
      if(PC) {
        eigenvalues <- eigen(solve(D) %*% W)$values
        inla.pc.pmmcar.rho <- function(rho, eigenvalues, alpha = 2/3, U = 1/2){
          n <- length(eigenvalues)
          In <- Matrix::Diagonal(n = n, x = 1)
          log.p <- numeric(length(rho))
          KLD <- function(rho, eigenvalues){
            res <-  1/2 * sum(log(1 - rho*eigenvalues)) +
              1/2 * sum(1/(1 - rho * eigenvalues) - 1)
            return(res)
          }
          for(j in c(1:length(rho))){
            KLD_j <- KLD(rho = rho[j], eigenvalues = eigenvalues)
            if(KLD_j <= 0){
              message("!!! PROBLEM !!!! \n!!! NEGATIVE OR NULL KLD - MUST FIX MODEL !!!")
              return(NULL)
            }
            derivative <- 1/2 * sum( (rho[j]*eigenvalues^2)/(1 - rho[j]*eigenvalues)^2 )
            rate <- -1/sqrt(2*KLD(U, eigenvalues = eigenvalues)) * log(1 - alpha)
            log.p[j] <- dexp(x=sqrt(2*KLD_j), rate = rate, log = T) -log(2) +
              log(1/sqrt(2*KLD_j)) + log(abs(derivative))
          }
          return(sum(log.p))
        }
        assign("inla.pc.pmmcar.rho", inla.pc.pmmcar.rho, envir = envir)
        assign("eigenvalues", eigenvalues, envir = envir)
      }
      assign("D", D, envir = envir)
      assign("cache.done", TRUE, envir = envir)
    }
    interpret.theta <- function() {
      rho <- 1/(1 + exp(-theta[as.integer(1:k)]))
      diag.N <- sapply(theta[as.integer(k + 1:k)], function(x) exp(x) )
      no.diag.N <- theta[as.integer(2 * k + 1:(k * (k - 1)/2))]
      N <- diag(diag.N, k)
      N[lower.tri(N, diag = FALSE)] <- no.diag.N
      N <- scale.fac %*% N
      if(Wishart.on.scale){
        Sigma <- N %*% t(N)
      } else{
        Sigma <- solve(N %*% t(N))
      }
      e <- eigen(Sigma)
      M <- t(e$vectors %*% diag(sqrt(e$values)))
      invM <- solve(M)#e$vectors %*% diag(sqrt(1/e$values))
      return(list(rho = rho, Sigma = Sigma, M = M, invM = invM))
    }
    graph <- function() {
      QQ <- Q()
      G <- (QQ != 0) * 1
      return(G)
    }
    Q <- function() {
      param <- interpret.theta()
      In <- Matrix::Diagonal(nrow(W), 1)
      #M.inv <- solve(param$M)
      MI <- kronecker(param$invM, In)
      BlockIW <- kronecker(diag(k), D) - kronecker(diag(param$rho), W)
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
      if(!PC){
        #' Uniform prior
        val <- sum(-theta[as.integer(1:k)] - 2 * log(1 + exp(-theta[as.integer(1:k)])))
      } else{
        #' PC-prior
        if(!exists("alpha", envir = envir)) alpha <- 2/3
        if(!exists("U" , envir = envir)) U <- 1/2
        val <- inla.pc.pmmcar.rho(eigenvalues = eigenvalues, rho = param$rho, alpha = alpha, U = U) +
          sum(-theta[as.integer(1:k)] - 2 * log(1 + exp(-theta[as.integer(1:k)])))
      }
      # n^2_jj ~ chisq(k-j+1) (k degrees of freedom)
      val <- val + k * log(2) + 2 * sum(theta[k + 1:k]) + 
        sum(dchisq(exp(2 * theta[k + 1:k]), df = c(df:(df-k+1)), log = TRUE))
      # n_ki ~ N(0,1)
      val <- val + sum(dnorm(theta[(2*k)+1:(k*(k-1)/2)], mean=0, sd=1, log=TRUE))
      return(val)
    }
    initial <- function(){
      if(!exists("initial.values", envir= envir )){
        return(c( rep(0, k*(k+3)/2))) #changes wrt bigDM's log(9)
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



# #'  INLA code for M-model extension of the BYM --------------------------------
#'
#' General function to implement the M-model extension of the BYM.
#' Allows for either uniform or PC-prior on the mixing parameter.
#' Warning: the PC-prior has an extremely high computational cost.
#' The user can also choose between dense or sparse parametrisation.
#' Here we follow a sequential approach with as many steps as the number of
#' mixing parameters. At each j-th step, the flexible model employing a total of
#' j nonzero mixing parameters is tested against the base model with only the
#' previous nonzero j-1 mixing parameters.
#' 
#' The eigenvalues of L^+ - I are required. 

#' for benchmarking, e.g.
#' theta <- c(1.32, 2.01, -0.74, -0.46, -0.16, -0.52, -0.06, -0.52, -0.08)


inla.MMBYM.model <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.Mmodel.BYM, ...)

inla.rgeneric.Mmodel.BYM <- 
  function(cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                   "log.prior", "quit"), theta = NULL){
  
  envir <- parent.env(environment())
  if(!exists("df", envir = envir)) assign("df", k + 2, envir = envir)
  if(!exists("PC", envir = envir)) assign("PC", FALSE, envir = envir)
  if(!exists("scale.fac", envir = envir)) assign("scale.fac", diag(k))
  if(!exists("Wishart.on.scale", envir = envir)) assign("Wishart.on.scale", TRUE, envir = envir)
  if(!exists("sparse", envir = envir)) assign("sparse", TRUE, envir = envir)
  if(!exists("cache.done", envir=envir)){
    #' Laplacian matrix scaling: only needs being done once
    L_unscaled <- Matrix::Diagonal(n=nrow(W), x=rowSums(as.matrix(W))) -  W
    constr <- INLA:::inla.bym.constr.internal(L_unscaled, adjust.for.con.comp = T)
    scaleQ <- INLA:::inla.scale.model.internal(
      L_unscaled, constr = list(A = constr$constr$A, e = constr$constr$e))
    L <- scaleQ$Q
    invL <- INLA:::inla.ginv(L)
    In <- Matrix::Diagonal(n = nrow(W), x = 1)
    eigenvalues <- eigen(invL - In)$values
    inla.pc.mbym.phi <- function(phi, eigenvalues, alpha = 2/3, U = 1/2){
      n <- length(eigenvalues)
      In <- Matrix::Diagonal(n = n, x = 1)
      #gammas <- list()
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
        KLD_j <- KLD(phi = phi[j], eigenvalues = eigenvalues)
        if(KLD_j <= 0){
          message("!!! PROBLEM !!!! \n!!! NEGATIVE OR NULL KLD - MUST FIX MODEL !!!")
          return(NULL)
        }
        derivative <- 1/2 * sum(eigenvalues - eigenvalues / (1 + phi[j] * eigenvalues))
        rate <- -1/sqrt(2*KLD(U, eigenvalues = eigenvalues)) * log(1 - alpha)
        log.p[j] <- dexp(x=sqrt(2*KLD_j), rate = rate, log = T) -log(2) +
          log(1/sqrt(2*KLD_j)) + log(abs(derivative))
      }
      return(sum(log.p))
    }
    assign("L", L, envir = envir)
    assign("invL", invL, envir = envir)
    assign("inla.pc.mbym.phi", inla.pc.mbym.phi, envir = envir)
    assign("eigenvalues", eigenvalues, envir = envir)
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
    N <- scale.fac %*% N
    if(Wishart.on.scale){
      Sigma <- N %*% t(N)
    } else{
      Sigma <- solve(N %*% t(N))
    }   
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
      MI <- kronecker(t(param$invM.t), In)
      BlockIW <- Matrix::bdiag(lapply(1:k, function(i) {
        solve(param$phi[i]*invL + (1-param$phi[i])*In)
      }))
      Q <- (MI %*% BlockIW) %*% kronecker(param$invM.t, In)
    } else {
      q11 <- t(param$invM.t) %*% Matrix::Diagonal(x = 1/(1 - param$phi), n = k) %*% param$invM.t
      q12 <- t(param$invM.t) %*% Matrix::Diagonal(x = sqrt(param$phi)/(1 - param$phi), n = k)
      q22 <- Matrix::Diagonal(x = param$phi/(1 - param$phi), n = k)
      
      Q11 <- kronecker(q11, In)
      Q12 <- - kronecker(q12, In)
      Q21 <- Matrix::t(Q12)
      Q22 <- kronecker(q22, In) + kronecker(Matrix::Diagonal(x=1, n=k), L)
      Q <- rbind(cbind(Q11, Q12), cbind(Q21, Q22))
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
      if(!exists("alpha", envir = envir)) alpha <- 2/3
      if(!exists("U" , envir = envir)) U <- 1/2
      val <- inla.pc.mbym.phi(eigenvalues = eigenvalues, phi = param$phi, alpha = alpha, U = U) +
        sum(-theta[as.integer(1:k)] - 2 * log(1 + exp(-theta[as.integer(1:k)])))
    }
    # n^2_jj ~ chisq(k-j+1) (k degrees of freedom)
    val <- val + k * log(2) + 2 * sum(theta[k + 1:k]) + 
      sum(dchisq(exp(2 * theta[k + 1:k]), df = c(df:(df-k+1)), log = TRUE))
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



# #'  PC-prior for the Leroux mixing parameter ---------------------------------
#'
#' This function needs the eigenvalues of L-I ---------------------------------#
#' 
#' Here we cannot apply any consideration on 
#' Laplacian scaling in that it simply cannot be scaled. 
#' Remind this:
#'  > L <- Matrix::Diagonal(x=apply(W, MARGIN = 1, FUN = sum), n = nrow(W)) - W
#'  > eigenvalues <- eigen(L - In)$values
#' ----------------------------------------------------------------------------#
inla.pc.lmmcar.xi <- function(xi, eigenvalues, alpha = 2/3, U = 1/2){
  n <- length(eigenvalues)
  In <- Matrix::Diagonal(n = n, x = 1)
  #gammas <- list()
  log.p <- numeric(length(xi))
  KLD <- function(xi, eigenvalues){
    res <-  1/2 * sum(log(1 + xi*eigenvalues)) +
      1/2 * sum(1/(1 + xi * eigenvalues) - 1)
    return(res)
  }
  for(j in c(1:length(xi))){
    KLD_j <- KLD(xi = xi[j], eigenvalues = eigenvalues)
    if(KLD_j <= 0){
      message("!!! PROBLEM !!!! \n!!! NEGATIVE OR NULL KLD - MUST FIX MODEL !!!")
      return(NULL)
    }
    derivative <- 1/2 * sum( (xi[j]*eigenvalues^2)/(1 + xi[j]*eigenvalues)^2 )
    rate <- -1/sqrt(2*KLD(U, eigenvalues = eigenvalues)) * log(1 - alpha)
    log.p[j] <- dexp(x=sqrt(2*KLD_j), rate = rate, log = T) -log(2) +
      log(1/sqrt(2*KLD_j)) + log(abs(derivative))
  }
  return(sum(log.p))
}
# #'  PC-prior for the PCAR autoregressive parameter ---------------------------
#'
#' This function needs the eigenvalues of solve(D)%*% W
#' or equivalently the "normalised" eigenvalues of W --------------------------#
#' 
#' Again, we cannot apply any consideration on 
#' Laplacian scaling in that it simply cannot be scaled. 
#' Remind this:
#'  > D <- Matrix::Diagonal(x=apply(W, MARGIN = 1, FUN = sum), n = nrow(W)) 
#'  > eigenvalues <- eigen(solve(D) %*% W)$values
#' ----------------------------------------------------------------------------#
inla.pc.pmmcar.rho <- function(rho, eigenvalues, alpha = 2/3, U = 1/2){
  n <- length(eigenvalues)
  In <- Matrix::Diagonal(n = n, x = 1)
  log.p <- numeric(length(rho))
  KLD <- function(rho, eigenvalues){
    res <-  1/2 * sum(log(1 - rho*eigenvalues)) +
      1/2 * sum(1/(1 - rho * eigenvalues) - 1)
    return(res)
  }
  for(j in c(1:length(rho))){
    KLD_j <- KLD(rho = rho[j], eigenvalues = eigenvalues)
    if(KLD_j <= 0){
      message("!!! PROBLEM !!!! \n!!! NEGATIVE OR NULL KLD - MUST FIX MODEL !!!")
      return(NULL)
    }
    derivative <- 1/2 * sum( (rho[j]*eigenvalues^2)/(1 - rho[j]*eigenvalues)^2 )
    rate <- -1/sqrt(2*KLD(U, eigenvalues = eigenvalues)) * log(1 - alpha)
    log.p[j] <- dexp(x=sqrt(2*KLD_j), rate = rate, log = T) -log(2) +
      log(1/sqrt(2*KLD_j)) + log(abs(derivative))
  }
  return(sum(log.p))
}
# #'  sample from scale posterior, adapted from bigDM --------------------------

#' This is basically a bigDM:: function, ie.
#' bigDM::Mmodel_compute_cor, generalised to work with models run manually.


 
 
Mmodel_compute_cor_bigDM <- function (model, n.sample = 10000, J) {
  o <- tryCatch({
    hyperpar.sample <- INLA::inla.hyperpar.sample(n.sample,  model, improve.marginals = TRUE)
    offset <- ncol(hyperpar.sample) - J * (J+1) / 2
    hyperpar.sample <- hyperpar.sample[, (1+offset):ncol(hyperpar.sample)]
    hyperpar.sample[, 1:J] <- exp(hyperpar.sample[,  1:J])
    hyperpar.sample <- split(hyperpar.sample ,
                               seq(nrow(hyperpar.sample)))
     
    param.sample <- lapply(hyperpar.sample, function(x) {
      N <- diag(x[seq(J)])
      N[lower.tri(N, diag = FALSE)] <- x[-seq(J)]
      Sigma <- N %*% t(N)
      Rho <- cov2cor(Sigma)
      Rho.values <- Rho[lower.tri(Rho)]
      return(list(sigma = diag(Sigma), rho = Rho.values))
    })
    cor.sample <- do.call(rbind, lapply(param.sample, 
                                        function(x) x$rho))
    cor.density <- apply(cor.sample, 2,
                         function(x) density(x,n = 500, bw = "SJ", from = -1, to = 1))
    marginals.cor <- lapply(cor.density, function(xx) cbind(x = xx$x, 
                                                            y = xx$y))
    names(marginals.cor) <- paste("rho", apply(combn(J, 2), 2, function(x) paste0(x, collapse = "")), 
                                  sep = "")
    summary.cor <- do.call(rbind, lapply(marginals.cor, 
                                         function(x) bigDM:::compute.summary(x, cdf = NULL)))
    names(summary.cor) <- c("mean", "sd",  "quant0.025", "quant0.5", "quant0.975") 
    
    var.sample <- do.call(rbind, lapply(param.sample, 
                                        function(x) x$sigma))
    var.density <- apply(var.sample, 2, function(x) density(x, 
                                                            n = 500, bw = "SJ", from = 0))
    marginals.var <- lapply(var.density, function(xx) cbind(x = xx$x, 
                                                            y = xx$y))
    names(marginals.var) <- paste("var", 1:J, sep = "")
    summary.var <- do.call(rbind, lapply(marginals.var, 
                                         function(x) bigDM:::compute.summary(x, cdf = NULL)))
    names(summary.var) <- c("mean", "sd",  "quant0.025", "quant0.5", "quant0.975") 
    
  })
  if (any(class(o[[1]]) == "error")) {
    summary.cor <- data.frame(rep(NA, ncol(combn(J, 2))),
                              rep(NA, ncol(combn(J, 2))),
                              rep(NA, ncol(combn(J,   2))), 
                              rep(NA, ncol(combn(J, 2))),
                              rep(NA, ncol(combn(J, 
                                                 2))), rep(NA, ncol(combn(J, 2))))
    colnames(summary.cor) <- c("mean", "sd", "0.025quant", 
                               "0.5quant", "0.975quant", "mode")
    rownames(summary.cor) <- paste("rho", apply(combn(J, 
                                                      2), 2, function(x) paste0(x, collapse = "")), 
                                   sep = "")
    marginals.cor <- as.list(rep(NA, ncol(combn(J, 2))))
    names(marginals.cor) <- rownames(summary.cor)
    summary.var <- data.frame(rep(NA, J), rep(NA, J), 
                              rep(NA, J), rep(NA, J), rep(NA, J), rep(NA, 
                                                                      J))
    colnames(summary.var) <- c("mean", "sd", "0.025quant", 
                               "0.5quant", "0.975quant", "mode")
    rownames(summary.var) <- paste("var", 1:J, sep = "")
    marginals.var <- as.list(rep(NA, J))
    names(marginals.var) <- rownames(summary.var)
  }
  Mmodel.compute <- list(summary.cor = summary.cor, marginals.cor = marginals.cor, 
                         summary.var = summary.var, marginals.var = marginals.var)
  return(Mmodel.compute)
  
}



Mmodel_compute_mixing <- function(model, J){
  res <- data.frame(
    do.call(rbind, lapply(
      lapply(model$marginals.hyperpar[c(1:J)], function(f){
        inla.tmarginal(fun = function(X) 1/(1 + exp(-X)), marginal = f)
        }), function(x) unlist(inla.zmarginal(x, silent = TRUE))))) %>% 
    dplyr::select(1,2,3,5,7)
  return(res)
}
 
