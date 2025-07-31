##' input data
load("C:/Users/Leonardo/Downloads/UniBA/CAV_analisi/Other/inla ticket/dd_repl.RData")
constr <- INLA:::inla.bym.constr.internal(Q = (diag(rowSums(as.matrix(W))) - W), 
                                          adjust.for.con.comp = T)
##' Model specification --------------------------------------------------------
 
inla.rgeneric.PCAR.bartlett <- 
  function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                    "log.prior", "quit"), theta = NULL) {
    
    interpret.theta <- function() {
      #' Mixing parameter, defined on [0, 1]: expit transform
      alpha <-  1/(1 + exp(-theta[1L]))
      #' Bartlett decomposition for the scale parameter:
      #' ensures determinant > 0 AND requires minimal hyperparameters number
      #' Diagonal elements: exponential transform
      diag.N <- sapply(theta[as.integer(2:(k+1))], function(x) {
        exp(x)
      })
      #' Offdiagonal elements: no transform needed
      no.diag.N <- theta[as.integer(-(1:(k+1)))]
      N <- diag(diag.N, k)
      #' Bartlett factor, usually referred to as A:
      N[lower.tri(N, diag = FALSE)] <- no.diag.N
      Sigma <- N %*% t(N)
      #' Precision parameter
      Lambda <- solve(Sigma)
      return(list(alpha = alpha, Lambda = Lambda))
    }
    graph <- function() {
      G <- kronecker(diag(k), Matrix::Diagonal(nrow(W), 1) + W)
      return(G)
    }
    #' Precision matrix. 
    Q <- function() {
      param <- interpret.theta()
      prec <- Matrix::Diagonal(x=rowSums(as.matrix(W)), n=nrow(W)) - param$alpha*W
      Q <- kronecker(param$Lambda, prec)
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
      #' Uniform prior on the mixing
      val <- - theta[1L] - 2*log(1 + exp(-theta[1L]) )
      #' Chi squared prior on the diagonal entries of the Bartlett factor
      val <- val + k * log(2) + 2 * sum(theta[1 + 1:k]) + 
        sum(dchisq(exp(2 * theta[1 + 1:k]), df = (k + 2) - 
                     1:k + 1, log = TRUE))
      #' Normal prior on the offdiagonal entries of the Bartlett factor
      val <- val + sum(dnorm(theta[-as.integer(c(1:(k+1)))],
                             mean = 0, sd = 1, log = TRUE))
      return(val)
    }
    #' Initial values: all zeroes
    initial <- function() {
      return(rep(0, k*(k+1)/2 + 1))
    }
    quit <- function() {
      return(invisible())
    }
    if (as.integer(R.version$major) > 3) {
      if (!length(theta))  theta = initial()
    }
    else {
      if (is.null(theta))  theta <- initial()
    }
    val <- do.call(match.arg(cmd), args = list())
    return(val)
  }

##' BYM model - features dense precision matrix
inla.rgeneric.BYM.dense <- 
  function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                    "log.prior", "quit"), theta = NULL) {
    envir <- parent.env(environment())
    if(!exists("cache.done", envir=envir)){
      starttime.scale <- Sys.time()
      #' Laplacian matrix scaling and eigendecomposition:
      #' burdensome operations that only need being performed once
      L_unscaled <- Matrix::Diagonal(nrow(W), rowSums(as.matrix(W))) -  W
      constr <- INLA:::inla.bym.constr.internal(L_unscaled, adjust.for.con.comp = T)
      scaleQ <- INLA:::inla.scale.model.internal(
        L_unscaled, constr = list(A = constr$constr$A, e = constr$constr$e))
      L <- scaleQ$Q
      eigenvalues <- eigen(L)$values
      eigenvectors <- eigen(L)$vectors
      endtime.scale <- Sys.time()
      cat("Time needed for scaling Laplacian matrix: ",
          round(difftime(endtime.scale, starttime.scale), 3), " seconds \n")
      assign("L", L, envir = envir)
      assign("eigenvalues", eigenvalues, envir = envir)
      assign("eigenvectors", eigenvectors, envir = envir)
      assign("cache.done", TRUE, envir = envir)
    }
    interpret.theta <- function() {
      #' Mixing parameter, defined on [0, 1]: expit transform
      phi <-  1/(1 + exp(-theta[1L]))
      #' Bartlett decomposition for the scale parameter:
      #' ensures determinant > 0 AND requires minimal hyperparameters number
      #' Diagonal elements: exponential transform
      diag.N <- sapply(theta[as.integer(2:(k+1))], function(x) {
        exp(x)
      })
      #' Offdiagonal elements: no transform needed
      no.diag.N <- theta[as.integer(-(1:(k+1)))]
      N <- diag(diag.N, k)
      #' Bartlett factor, usually referred to as A:
      N[lower.tri(N, diag = FALSE)] <- no.diag.N
      Sigma <- N %*% t(N)
      #' Precision parameter
      Lambda <- solve(Sigma)
      return(list(phi = phi, Lambda = Lambda))
    }
    graph <- function() {
      G <- kronecker(diag(k), Matrix::Diagonal(nrow(W), 1) + W)
      return(G)
    }
    #' Precision matrix. 
    Q <- function() {
      param <- interpret.theta()
      #' Eigendecomposition of the structure matrix ==> no need to invert the variance at each iteration
      evdiag <- Matrix::Diagonal(x = 1/(param$phi*eigenvalues + 1-param$phi), n = nrow(W))
      prec.marg <- eigenvectors %*% evdiag %*% t(eigenvectors)
      #VAR <- kronecker(solve(param$Lambda), param$phi*L + 
                         #(1-param$phi)*Matrix::Diagonal(n=nrow(W), x=1))
      #Q <- solve(VAR)
      Q <- kronecker(param$Lambda, prec.marg)
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
      #' Uniform prior on the mixing
      val <- - theta[1L] - 2*log(1 + exp(-theta[1L]) )
      #' Chi squared prior on the diagonal entries of the Bartlett factor
      val <- val + k * log(2) + 2 * sum(theta[1 + 1:k]) + 
        sum(dchisq(exp(2 * theta[1 + 1:k]), df = (k + 2) - 
                     1:k + 1, log = TRUE))
      #' Normal prior on the offdiagonal entries of the Bartlett factor
      val <- val + sum(dnorm(theta[-as.integer(c(1:(k+1)))],
                             mean = 0, sd = 1, log = TRUE))
      return(val)
    }
    #' Initial values: all zeroes
    initial <- function() {
      return(rep(0, k*(k+1)/2 + 1))
    }
    quit <- function() {
      return(invisible())
    }
    if (as.integer(R.version$major) > 3) {
      if (!length(theta))  theta = initial()
    }
    else {
      if (is.null(theta))  theta <- initial()
    }
    val <- do.call(match.arg(cmd), args = list())
    return(val)
  }

##' If both the BYM process and the ICAR component are modelled jointly,
##' the corresponding model has indeed sparse precision matrix.
##' However, it is singular and the model needs a sum-to-zero constraint.
inla.rgeneric.BYM.sparse <- 
  function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const",
                    "log.prior", "quit"), theta = NULL) {
  
  envir <- parent.env(environment())
  if(!exists("cache.done", envir=envir)){
    starttime.scale <- Sys.time()
    #' Laplacian matrix scaling: only needs being done once
    L_unscaled <- Matrix::Diagonal(nrow(W), rowSums(W)) -  W
    constr <- INLA:::inla.bym.constr.internal(L_unscaled, adjust.for.con.comp = T)
    scaleQ <- INLA:::inla.scale.model.internal(
      L_unscaled, constr = list(A = constr$constr$A, e = constr$constr$e))
    L <- scaleQ$Q
    endtime.scale <- Sys.time()
    cat("Time needed for scaling Laplacian matrix: ",
        round(difftime(endtime.scale, starttime.scale), 3), " seconds \n")
    assign("L", L, envir = envir)
    assign("cache.done", TRUE, envir = envir)
  }
  interpret.theta <- function() {
    #' Same as the dense version, plus the M-factorisation of the scale parameter
    #' like in the M-models
    phi <- 1/(1 + exp(-theta[1L]))
    diag.N <- sapply(theta[as.integer(2:(k+1))], function(x) {
      exp(x)
    })
    no.diag.N <- theta[as.integer(-(1:(k+1)))]
    N <- diag(diag.N, k)
    N[lower.tri(N, diag = FALSE)] <- no.diag.N
    Sigma <- N %*% t(N)
    e <- eigen(Sigma)
    M <- t(e$vectors %*% diag(sqrt(e$values)))
    invM.t <- diag(1/sqrt(e$values)) %*% t(e$vectors)
    Lambda <- t(invM.t) %*% invM.t
    return(list(phi = phi, Lambda = Lambda, invM.t = invM.t))
  }
  graph <- function() {
    QQ <- Q()
    G <- (QQ != 0) * 1
    return(G)
  }
  #' Precision matrix. Multivariate generalisation of the 
  Q <- function() {
    param <- interpret.theta()
    In <- Matrix::Diagonal(x=1, n=nrow(W))
    #' Blocks of the sparse precision matrix, as in Riebler et al. (2016)
    Q11 <- kronecker(1/(1-param$phi)*param$Lambda, In)
    Q12 <- - kronecker(sqrt(param$phi)/(1 - param$phi) * t(param$invM.t), In)
    Q21 <- Matrix::t(Q12)
    Q22 <- kronecker(diag(k), (L + param$phi/(1-param$phi)*In))
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
    #' As in the dense version
    param <- interpret.theta()
    val <- - theta[1L] - 2*log(1 + exp(-theta[1L]) )
    val <- val + k * log(2) + 2 * sum(theta[1 + 1:k]) + 
      sum(dchisq(exp(2 * theta[1 + 1:k]), df = (k + 2) - 
                   1:k + 1, log = TRUE))
    val <- val + sum(dnorm(theta[-as.integer(c(1:(k+1)))], mean = 0, sd = 1, log = TRUE))
    return(val)
  }
  initial <- function() {
    return(rep(0, k*(k+1)/2 + 1))
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

 
inla.PCAR.bartlett <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.PCAR.bartlett, ...)
inla.BYM.sparse <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.BYM.sparse, ...)
inla.BYM.dense <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.BYM.dense, ...)


##' Model running --------------------------------------------------------------

A.constr <- kronecker(Matrix::Diagonal(n=3,x=1), constr$constr$A)

library(INLA)
 
cav.pcar.bartlett <- inla(
  Y ~ 1 + X1 + X2 + X3 + 
    f(ID, model = inla.PCAR.bartlett(W = W, k=3) ),
  family = "poisson",
  offset = log(dd$offset),
  data = dd,
  num.threads = 1,
  control.compute = list(internal.opt = FALSE),
  verbose = TRUE)


cav.bym.dense <- inla(
  Y ~ 1 + X1 + X2 + X3 + 
    f(ID, model = inla.BYM.dense(W = W, k=3) ),
  family = "poisson",
  offset = log(dd$offset),
  data = dd,
  num.threads = 1,
  control.compute = list(internal.opt = FALSE),
  verbose = TRUE)
 

#' Function call crashes:
inla(
  Y ~ 1 + X1 + X2 + X3 + 
    f(ID, model = inla.BYM.sparse(W = W, k=3), 
      extraconstr = list(
        A = cbind(Matrix::Matrix(0, nrow = nrow(A.constr), ncol = ncol(A.constr)), A.constr),
        e=c(0,0,0)  )),
  family = "poisson",
  offset = log(dd$offset),
  data = dd,
  num.threads = 1,
  control.compute = list(internal.opt = FALSE),
  verbose = TRUE)


#' Function call crashes:
inla(
  Y ~ 1 + X1 + X2 + X3 + 
    f(idx, model = inla.BYM.model(W = W)),
  family = "poisson",
  offset = log(dd$nn),
  data = dd,
  control.predictor = list(A = inla.stack.A(dd_stack)),
  num.threads = 1,
  control.compute = list(internal.opt = FALSE),
  verbose = TRUE)

##' Univariate: dense versus sparse --------------------------------------------


inla.BYM.sparse.un <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.BYM.sparse.unif, ...)
inla.BYM.dense.un <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.BYM.dense.unif, ...)


inla.rgeneric.BYM.dense.unif <-   function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                                                    "log.prior", "quit"), theta = NULL) {
  
  envir <- parent.env(environment())
  if(!exists("cache.done", envir=envir)){
    starttime.scale <- Sys.time()
    #' Unscaled Laplacian matrix (marginal precision of u_1, u_2 ... u_k)
    L_unscaled <- Matrix::Diagonal(nrow(W), rowSums(W)) -  W
    constr <- INLA:::inla.bym.constr.internal(L_unscaled, adjust.for.con.comp = T)
    scaleQ <- INLA:::inla.scale.model.internal(
      L_unscaled, constr = list(A = constr$constr$A, e = constr$constr$e))
    L <- scaleQ$Q
    invL <- INLA:::inla.ginv(L)
    endtime.scale <- Sys.time()
    cat("Time needed for scaling Laplacian matrix: ",
        round(difftime(endtime.scale, starttime.scale), 3), " seconds \n")
    assign("L", L, envir = envir)
    assign("invL", invL, envir = envir)
    assign("cache.done", TRUE, envir = envir)
  }
  
  #' Standard: mapping parameters from internal scale
  interpret.theta <- function() {
    #' Mixing parameter, defined on [0, 1]: expit transform
    phi <-  1/(1 + exp(-theta[1L]))
    #' Precision parameter, internally modelled on the log scale
    tau <- exp(theta[2L])
    return(list(phi = phi, tau = tau))
  }
  graph <- function() {
    G <- Matrix::Diagonal(nrow(W), 1) + W
    return(G)
  }
  #' Precision matrix. I use the BYM2 parametrisation
  #' to have sparse precision. 
  #' Problem: Riebler et al. define a random vector of size 2n,
  #' i.e. (z' u')', where z is the overall effect and u is the ICAR component
  Q <- function() {
    param <- interpret.theta()
    variance <- 1/param$tau * param$phi * invL + 
      1/param$tau * (1-param$phi)*Matrix::Diagonal(n=nrow(W))
    Q <- solve(variance)
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
    #' Uniform prior on the mixing
    val <- - theta[1L] - 2*log(1 + exp(-theta[1L]) )
    #' PC prior on precision; term +theta[2L] needed for change of variable
    val <- val + INLA::inla.pc.dprec(prec = param$tau, lambda = 1, log = TRUE) + theta[2L]
    return(val)
  }
  #' As in the built-in model
  initial <- function() {
    return(c(-3, 4))
  }
  quit <- function() {
    return(invisible())
  }
  #' Initialise hyperparameters
  if (as.integer(R.version$major) > 3) {
    if (!length(theta)) 
      theta <- initial()
  }
  else {
    if (is.null(theta)) {
      theta <- initial()
    }
  }
  val <- do.call(match.arg(cmd), args = list())
  return(val)
}
inla.rgeneric.BYM.sparse.unif <-   function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                                                     "log.prior", "quit"), theta = NULL) {
  
  envir <- parent.env(environment())
  if(!exists("cache.done", envir=envir)){
    starttime.scale <- Sys.time()
    #' Unscaled Laplacian matrix (marginal precision of u_1, u_2 ... u_k)
    L_unscaled <- Matrix::Diagonal(nrow(W), rowSums(W)) -  W
    constr <- INLA:::inla.bym.constr.internal(L_unscaled, adjust.for.con.comp = T)
    scaleQ <- INLA:::inla.scale.model.internal(
      L_unscaled, constr = list(A = constr$constr$A, e = constr$constr$e))
    L <- scaleQ$Q
    endtime.scale <- Sys.time()
    cat("Time needed for scaling Laplacian matrix: ",
        round(difftime(endtime.scale, starttime.scale), 3), " seconds \n")
    assign("L", L, envir = envir)
    assign("cache.done", TRUE, envir = envir)
  }
  
  #' Standard: mapping parameters from internal scale
  interpret.theta <- function() {
    #' Mixing parameter, defined on [0, 1]: expit transform
    phi <-  1/(1 + exp(-theta[1L]))
    #' Precision parameter, internally modelled on the log scale
    tau <- exp(theta[2L])
    return(list(phi = phi, tau = tau))
  }
  graph <- function() {
    Q <- Q()
    G <- as(Q != 0, "dMatrix")
    return(G)
  }
  #' Precision matrix. I use the BYM2 parametrisation
  #' to have sparse precision. 
  #' Problem: Riebler et al. define a random vector of size 2n,
  #' i.e. (z' u')', where z is the overall effect and u is the ICAR component
  Q <- function() {
    param <- interpret.theta()
    #' Blocks of the sparse precision matrix, as in Riebler et al. (2016)
    Q11 <-  param$tau/(1-param$phi) * Matrix::Diagonal(n=nrow(W), x=1)
    Q12 <- - sqrt(param$phi * param$tau)/(1 - param$phi) * Matrix::Diagonal(n = nrow(W), x = 1)
    Q21 <- Q12
    Q22 <- L + (param$phi/(1-param$phi))*Matrix::Diagonal(n=nrow(W), x=1)
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
    val <- - theta[1L] - 2*log(1 + exp(-theta[1L]) )
    #' PC prior on precision; term +theta[2L] needed for change of variable
    val <- val + INLA::inla.pc.dprec(prec = param$tau, lambda = 1, log = TRUE) + theta[2L]
    return(val)
  }
  #' As in the built-in model
  initial <- function() {
    return(c(-3, 4))
  }
  quit <- function() {
    return(invisible())
  }
  #' Initialise hyperparameters
  if (as.integer(R.version$major) > 3) {
    if (!length(theta)) 
      theta <- initial()
  }
  else {
    if (is.null(theta)) {
      theta <- initial()
    }
  }
  val <- do.call(match.arg(cmd), args = list())
  return(val)
}


inla.rgeneric.BYM.dense.fixphi <-   function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                                                    "log.prior", "quit"), theta = NULL) {
  
  envir <- parent.env(environment())
  if(!exists("cache.done", envir=envir)){
    starttime.scale <- Sys.time()
    #' Unscaled Laplacian matrix (marginal precision of u_1, u_2 ... u_k)
    L_unscaled <- Matrix::Diagonal(nrow(W), rowSums(W)) -  W
    constr <- INLA:::inla.bym.constr.internal(L_unscaled, adjust.for.con.comp = T)
    scaleQ <- INLA:::inla.scale.model.internal(
      L_unscaled, constr = list(A = constr$constr$A, e = constr$constr$e))
    L <- scaleQ$Q
    invL <- INLA:::inla.ginv(L)
    endtime.scale <- Sys.time()
    cat("Time needed for scaling Laplacian matrix: ",
        round(difftime(endtime.scale, starttime.scale), 3), " seconds \n")
    assign("L", L, envir = envir)
    assign("invL", invL, envir = envir)
    assign("cache.done", TRUE, envir = envir)
  }
  
  #' Standard: mapping parameters from internal scale
  interpret.theta <- function() {
    #' Mixing parameter, defined on [0, 1]: expit transform
    phi <-  1/2
    #' Precision parameter, internally modelled on the log scale
    tau <- exp(theta[1L])
    return(list(phi = phi, tau = tau))
  }
  graph <- function() {
    G <- Matrix::Diagonal(nrow(W), 1) + W
    return(G)
  }
  #' Precision matrix. I use the BYM2 parametrisation
  #' to have sparse precision. 
  #' Problem: Riebler et al. define a random vector of size 2n,
  #' i.e. (z' u')', where z is the overall effect and u is the ICAR component
  Q <- function() {
    param <- interpret.theta()
    variance <- 1/param$tau * param$phi * invL + 
      1/param$tau * (1-param$phi)*Matrix::Diagonal(n=nrow(W))
    Q <- solve(variance)
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
    #' Uniform prior on the mixing
    #' PC prior on precision; term +theta[2L] needed for change of variable
    val <- + INLA::inla.pc.dprec(prec = param$tau, lambda = 1, log = TRUE) + theta[1L]
    return(val)
  }
  #' As in the built-in model
  initial <- function() {
    return(4)
  }
  quit <- function() {
    return(invisible())
  }
  #' Initialise hyperparameters
  if (as.integer(R.version$major) > 3) {
    if (!length(theta)) 
      theta <- initial()
  }
  else {
    if (is.null(theta)) {
      theta <- initial()
    }
  }
  val <- do.call(match.arg(cmd), args = list())
  return(val)
}
inla.rgeneric.BYM.sparse.fixphi <-   function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                                                     "log.prior", "quit"), theta = NULL) {
  
  envir <- parent.env(environment())
  if(!exists("cache.done", envir=envir)){
    starttime.scale <- Sys.time()
    #' Unscaled Laplacian matrix (marginal precision of u_1, u_2 ... u_k)
    L_unscaled <- Matrix::Diagonal(nrow(W), rowSums(W)) -  W
    constr <- INLA:::inla.bym.constr.internal(L_unscaled, adjust.for.con.comp = T)
    scaleQ <- INLA:::inla.scale.model.internal(
      L_unscaled, constr = list(A = constr$constr$A, e = constr$constr$e))
    L <- scaleQ$Q
    endtime.scale <- Sys.time()
    cat("Time needed for scaling Laplacian matrix: ",
        round(difftime(endtime.scale, starttime.scale), 3), " seconds \n")
    assign("L", L, envir = envir)
    assign("cache.done", TRUE, envir = envir)
  }
  
  #' Standard: mapping parameters from internal scale
  interpret.theta <- function() {
    #' Mixing parameter, defined on [0, 1]: expit transform
    phi <-  1/2
    #' Precision parameter, internally modelled on the log scale
    tau <- exp(theta[1L])
    return(list(phi = phi, tau = tau))
  }
  graph <- function() {
    Q <- Q()
    G <- as(Q != 0, "dMatrix")
    return(G)
  }
  #' Precision matrix. I use the BYM2 parametrisation
  #' to have sparse precision. 
  #' Problem: Riebler et al. define a random vector of size 2n,
  #' i.e. (z' u')', where z is the overall effect and u is the ICAR component
  Q <- function() {
    param <- interpret.theta()
    #' Blocks of the sparse precision matrix, as in Riebler et al. (2016)
    Q11 <-  param$tau/(1-param$phi) * Matrix::Diagonal(n=nrow(W), x=1)
    Q12 <- - sqrt(param$phi * param$tau)/(1 - param$phi) * Matrix::Diagonal(n = nrow(W), x = 1)
    Q21 <- Q12
    Q22 <- L + (param$phi/(1-param$phi))*Matrix::Diagonal(n=nrow(W), x=1)
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
    #' PC prior on precision; term +theta[2L] needed for change of variable
    val <-   INLA::inla.pc.dprec(prec = param$tau, lambda = 1, log = TRUE) + theta[1L]
    return(val)
  }
  #' As in the built-in model
  initial <- function() {
    return(4)
  }
  quit <- function() {
    return(invisible())
  }
  #' Initialise hyperparameters
  if (as.integer(R.version$major) > 3) {
    if (!length(theta)) 
      theta <- initial()
  }
  else {
    if (is.null(theta)) {
      theta <- initial()
    }
  }
  val <- do.call(match.arg(cmd), args = list())
  return(val)
}




BYM.auto <- inla(Y ~ 1 + X1 + X2 + X3 + 
                   f(idx, model = "bym2", graph = W,
                     hyper = list(phi=list(prior = "logitbeta", initial = -3,
                                           param = c(1,1)))),
                 family = "poisson",
                 offset = log(dd$nn),
                 data = dd,
                 num.threads = 1,
                 control.compute = list(internal.opt = FALSE),
                 verbose = TRUE)

 



BYM.unif.dense <- inla(
  Y ~ 1 + X1 + X2 + X3 + 
    f(idx, model = inla.BYM.dense.un(W = W), extraconstr = list(A = constr$constr$A, e = constr$constr$e)),
  family = "poisson",
  offset = log(dd$nn),
  data = dd,
  num.threads = 1,
  control.compute = list(internal.opt = FALSE),
  verbose = TRUE)

BYM.constr <- list(
  A = cbind(constr$constr$A, matrix(0, nrow=1, ncol=nrow(W)) ),
  e = constr$constr$e)

A.local <- cbind(Matrix::Diagonal(n=nrow(W), x = 1), Matrix::Matrix(0, nrow=nrow(W), ncol=ncol(W)))

BYM.unif.sparse <- inla(
  Y ~ 1 + X1 + X2 + X3 + 
    f(idx, model = inla.BYM.sparse.un(W = W), extraconstr = BYM.constr,
      values = c(1:(2*nrow(dd))) ),
  family = "poisson",
  offset = log(dd$nn),
  data = dd,
  num.threads = 1,
  control.compute = list(internal.opt = FALSE),
  verbose = TRUE)









##' Multivariate: simple case --------------------------------------------------
##' TBD
##' TBD
##' TBD

inla.rgeneric.BYM.sparse <-   function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                                                "log.prior", "quit"), theta = NULL) {
  
  envir <- parent.env(environment())
  if(!exists("cache.done", envir=envir)){
    #' Unscaled Laplacian matrix (marginal precision of u_1, u_2 ... u_k)
    L_unscaled <- Matrix::Diagonal(nrow(W), rowSums(W)) -  W
    constr <- INLA:::inla.bym.constr.internal(L_unscaled, adjust.for.con.comp = T)
    scaleQ <- INLA:::inla.scale.model.internal(
      L_unscaled, constr = list(A = constr$constr$A, e = constr$constr$e))
    #n <- nrow(W)
    L <- scaleQ$Q
    assign("L", L, envir = envir)
    assign("cache.done", TRUE, envir = envir)
  }
  
  interpret.theta <- function() {
    phi <- 1/(1 + exp(-theta[as.integer(1:k)]))
    #' Bartlett decomposition ==> First define Sigma, 
    #' then use its eigendecomposition to define M ==> 
    #' ==> the function employs k(k+1)/2 parameters, 
    #' i.e. lower-triangular factor in the Bartlett decomposition.
    diag.N <- sapply(theta[as.integer(k + 1:k)], function(x) exp(x))
    no.diag.N <- theta[as.integer(2 * k + 1:(k * (k - 1)/2))]
    N <- diag(diag.N, k)
    N[lower.tri(N, diag = FALSE)] <- no.diag.N
    Sigma <- N %*% t(N)
    e <- eigen(Sigma)
    M <- t(e$vectors) %*% diag(sqrt(e$values))
    return(list(phi = phi, M = M))
  }
  
  graph <- function() {
    G11 <- G12 <- G21 <- Matrix::Diagonal(x=1, n=nrow(W))
    G22 <- Matrix::Diagonal(nrow(W), 1) + W
    G <- rbind(cbind(G11, G12), cbind(G21, G22))
    return(G)
  }
  #' Precision matrix. I use the BYM2 parametrisation
  #' to have sparse precision. 
  #' Problem: Riebler et al. define a random vector of size 2n,
  #' i.e. (z' u')', where z is the overall effect and u is the ICAR component
  Q <- function() {
    param <- interpret.theta()
    invM <- solve(param$M)
    PREC <- invM %*% t(invM)
    Q11 <- 1/(1 - param$phi) * kronecker(PREC, Matrix::Diagonal(n = nrow(W), x  = 1))
    Q12 <- -sqrt(param$phi)/(1 - param$phi) * kronecker(param$invM, Matrix::Diagonal(n = nrow(W), x  = 1))
    Q21 <- Matrix::t(Q12)
    Q22 <- kronecker(Matrix::Diagonal(n = k, x = 1),
                     ((param$phi/(1-param$phi))* Matrix::Diagonal(n = nrow(W), x = 1) + L))
    Q <- rbind(cbind(Q11, Q12), cbind(Q21, Q22))
    
    
    
    #' Blocks of the sparse precision matrix, as in Riebler et al. (2016)
    Q11 <-  param$tau/(1-param$phi) * Matrix::Diagonal(n=nrow(W), x=1)
    Q12 <- - sqrt(param$phi * param$tau)/(1 - param$phi) * Matrix::Diagonal(n = nrow(W), x = 1)
    Q21 <- Q12
    Q22 <- L + (param$phi/(1-param$phi))*Matrix::Diagonal(n=nrow(W), x=1)
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
    val <- log.dpc.phi.bym(param$phi) - theta[1L] - 2 * log(1 + exp(-theta[1L]))
    #val <- dnorm(theta[1L], mean = 0.5, sd = sqrt(2), log = TRUE)
    #' PC prior on precision; term +theta[2L] needed for change of variable
    val <- val + INLA::inla.pc.dprec(prec = param$tau, lambda = 1, log = TRUE) + theta[2L]
    return(val)
  }
  #' As in the built-in model
  initial <- function() {
    return(c(-3, 4))
  }
  quit <- function() {
    return(invisible())
  }
  #' Initialise hyperparameters
  if (as.integer(R.version$major) > 3) {
    if (!length(theta)) 
      theta <- initial()
  }
  else {
    if (is.null(theta)) {
      theta <- initial()
    }
  }
  val <- do.call(match.arg(cmd), args = list())
  return(val)
}

inla.rgeneric.BYM.sparse.unif <-   function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                                                     "log.prior", "quit"), theta = NULL) {
  
  envir <- parent.env(environment())
  if(!exists("cache.done", envir=envir)){
    starttime.scale <- Sys.time()
    #' Unscaled Laplacian matrix (marginal precision of u_1, u_2 ... u_k)
    L_unscaled <- Matrix::Diagonal(nrow(W), rowSums(W)) -  W
    constr <- INLA:::inla.bym.constr.internal(L_unscaled, adjust.for.con.comp = T)
    scaleQ <- INLA:::inla.scale.model.internal(
      L_unscaled, constr = list(A = constr$constr$A, e = constr$constr$e))
    L <- scaleQ$Q
    endtime.scale <- Sys.time()
    cat("Time needed for scaling Laplacian matrix: ",
        round(difftime(endtime.scale, starttime.scale), 3), " seconds \n")
    assign("L", L, envir = envir)
    assign("cache.done", TRUE, envir = envir)
  }
  
  #' Standard: mapping parameters from internal scale
  interpret.theta <- function() {
    #' Mixing parameter, defined on [0, 1]: expit transform
    phi <-  1/(1 + exp(-theta[1L]))
    #' Precision parameter, internally modelled on the log scale
    tau <- exp(theta[2L])
    return(list(phi = phi, tau = tau))
  }
  graph <- function() {
    G <- kronecker(matrix(1, nrow = 2, ncol = 2), Matrix::Diagonal(nrow(W), 1) + W)
    return(G)
  }
  #' Precision matrix. I use the BYM2 parametrisation
  #' to have sparse precision. 
  #' Problem: Riebler et al. define a random vector of size 2n,
  #' i.e. (z' u')', where z is the overall effect and u is the ICAR component
  Q <- function() {
    param <- interpret.theta()
    #' Blocks of the sparse precision matrix, as in Riebler et al. (2016)
    Q11 <-  param$tau/(1-param$phi) * Matrix::Diagonal(n=nrow(W), x=1)
    Q12 <- - sqrt(param$phi * param$tau)/(1 - param$phi) * Matrix::Diagonal(n = nrow(W), x = 1)
    Q21 <- Q12
    Q22 <- L + (param$phi/(1-param$phi))*Matrix::Diagonal(n=nrow(W), x=1)
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
    val <- - theta[1L] - 2*log(1 + exp(-theta[1L]) )
    #' PC prior on precision; term +theta[2L] needed for change of variable
    val <- val + INLA::inla.pc.dprec(prec = param$tau, lambda = 1, log = TRUE) + theta[2L]
    return(val)
  }
  #' As in the built-in model
  initial <- function() {
    return(c(-3, 4))
  }
  quit <- function() {
    return(invisible())
  }
  #' Initialise hyperparameters
  if (as.integer(R.version$major) > 3) {
    if (!length(theta)) 
      theta <- initial()
  }
  else {
    if (is.null(theta)) {
      theta <- initial()
    }
  }
  val <- do.call(match.arg(cmd), args = list())
  return(val)
}


