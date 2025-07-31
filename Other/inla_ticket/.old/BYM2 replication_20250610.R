##' input data
load("C:/Users/Leonardo/Downloads/UniBA/CAV_analisi/Other/inla ticket/dd_repl.RData")
constr <- INLA:::inla.bym.constr.internal(Q = (diag(rowSums(as.matrix(W))) - W), 
                                          adjust.for.con.comp = T)
A.constr <- kronecker(Matrix::Diagonal(n=3,x=1), constr$constr$A)

##' Model specification --------------------------------------------------------
 

##' Example of properly working model, dense parametrisation.
##' This exists in literature and has been included as a term of comparison. 

Mmodel.bym2.bartlett.dense <- function(
    cmd=c("graph","Q","mu","initial","log.norm.const","log.prior","quit"),
    theta=NULL){
  envir <- parent.env(environment())
  if(!exists("cache.done", envir=envir)){
    Qi <- Matrix::Diagonal(x=colSums(W))-W
    Ue <- eigen(Qi)$vectors
    eval <- exp(mean(log(diag(INLA:::inla.ginv(Qi)))))
    eigen <- eigen(Qi)$values[-nrow(Qi)] * eval
    
    assign("Ue", Ue, envir = envir)
    #assign("eval", eval, envir = envir)
    assign("eigen", eigen, envir = envir)
    assign("cache.done", TRUE, envir = envir)
  }
  
  interpret.theta <- function(){
    alpha <- 1/(1+exp(-theta[as.integer(1:J)]))
    
    diag.N <- sapply(theta[J+1:J], function(x) {exp(x)})
    no.diag.N <- theta[2*J+1:(J*(J-1)/2)]
    
    N <- diag(diag.N) 
    N[lower.tri(N)] <- no.diag.N
    
    Covar <- N %*% t(N)
    
    e <- eigen(Covar)
    M <- t(e$vectors %*% diag(sqrt(e$values)))
    
    return(list(alpha=alpha, Covar=Covar, M=M))
  }
  
  graph <- function(){ return(Q()) }
  
  Q <- function(){
    param <- interpret.theta()
    M.inv <- solve(param$M)
    MI <- kronecker(M.inv, Ue)
    BlockIW <- 
      Matrix::bdiag(lapply(1:J, function(i) {
        Matrix::Diagonal(x=c( eigen/(eigen+param$alpha[i]*(1-eigen)), 
                              1/(1-param$alpha[i])) )
      }))
    Q <- (MI %*% BlockIW) %*% t(MI)
    Q <- INLA::inla.as.sparse(Q)
    return (Q)
  }
  
   mu <- function(){ return(numeric(0)) }
  
   log.norm.const <- function(){
    val <- numeric(0)
    return(val)
  }
  log.prior <- function(){
    param <- interpret.theta()
    # Uniform prior in (alpha.min, alpha.max) on model scale
    val <-  sum(-theta[1:J] - 2*log(1+exp(-theta[1:J])))
    # n^2_jj ~ chisq(J-j+1) (J degrees of freedom)
    val <- val + J*log(2) + 2*sum(theta[J+1:J]) + sum(dchisq(exp(2*theta[J+1:J]), df=J-1:J+1, log=TRUE))
    # n_ji ~ N(0,1)
    val <- val + sum(dnorm(theta[(2*J)+1:(J*(J-1)/2)], mean=0, sd=1, log=TRUE))
    return(val)
  }

  initial <- function(){
    if(!exists("initial.values", envir= envir )){
      return(c(rep(0, J*(J+3)/2)))
    } else {
      return(initial.values)
    }
    
  }
  

  quit <- function(){ return(invisible()) }
  
  if(!length(theta)) theta <- initial()
  val <- do.call(match.arg(cmd), args=list())
  
  return(val)
}

##' If both the BYM process and the ICAR component are modelled jointly,
##' the corresponding model has indeed sparse precision matrix.
##' However, it is singular and the model needs a sum-to-zero constraint.
Mmodel.bym2.bartlett.sparse <- 
  function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const",
                    "log.prior", "quit"), theta = NULL) {
    
    envir <- parent.env(environment())
    if(!exists("cache.done", envir=envir)){
      starttime.scale <- Sys.time()
      #' Laplacian matrix scaling: only needs being done once
      L_unscaled <- Matrix::Diagonal(n=nrow(W), x=rowSums(as.matrix(W))) -  W
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
      alpha <-  1/(1+exp(-theta[as.integer(1:J)]))
      
      diag.N <- sapply(theta[J+1:J], function(x) {exp(x)})
      no.diag.N <- theta[2*J+1:(J*(J-1)/2)]
      
      N <- diag(diag.N) 
      N[lower.tri(N)] <- no.diag.N
      
      Sigma <- N %*% t(N)    
      e <- eigen(Sigma)
      #M <- t(e$vectors %*% diag(sqrt(e$values)))
      invM.t <- diag(1/sqrt(e$values)) %*% t(e$vectors)
      Lambda <- t(invM.t) %*% invM.t
      return(list(alpha = alpha, Lambda = Lambda, invM.t = invM.t))
    }
    graph <- function() {
      QQ <- Q()
      G <- (QQ != 0) * 1
      return(G)
    }
    
    Q <- function() {
      param <- interpret.theta()
      
      In <- Matrix::Diagonal(nrow(W), 1)
      
      q11 <- t(param$invM.t) %*% Matrix::Diagonal(x = 1/(1 - param$alpha), n = J) %*% param$invM.t
      q12 <- t(param$invM.t) %*% Matrix::Diagonal(x = sqrt(param$alpha)/(1 - param$alpha), n = J)
      q22 <- Matrix::Diagonal(x = param$alpha/(1 - param$alpha), n = J)
      
      Q11 <- kronecker(q11, In)
      Q12 <- - kronecker(q12, In)
      Q21 <- Matrix::t(Q12)
      Q22 <- kronecker(q22, In) + kronecker(Matrix::Diagonal(x=1, n=J), L)
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
      val <-  sum(-theta[1:J] - 2*log(1+exp(-theta[1:J])))
      # n^2_jj ~ chisq(J-j+1) (J degrees of freedom)
      val <- val + J*log(2) + 2*sum(theta[J+1:J]) + sum(dchisq(exp(2*theta[J+1:J]), df=J-1:J+1, log=TRUE))
      # n_ji ~ N(0,1)
      val <- val + sum(dnorm(theta[(2*J)+1:(J*(J-1)/2)], mean=0, sd=1, log=TRUE))
      return(val)
    }
    initial <- function(){
      if(!exists("initial.values", envir= envir )){
        return(c(rep(0, J*(J+3)/2)))
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


inla.PMMCAR <- function(...) INLA::inla.rgeneric.define(bigDM::Mmodel_pcar, ...)
inla.BYM.sparse <- function(...) INLA::inla.rgeneric.define(Mmodel.bym2.bartlett.sparse, ...)
inla.BYM.dense <- function(...) INLA::inla.rgeneric.define(Mmodel.bym2.bartlett.dense, ...)


##' Model running --------------------------------------------------------------


library(INLA)

#' Baseline model, PCAR: works with no problem
PMMCAR <- inla(
  Y ~ 1 + X1 + X2 + X3 + 
    f(ID, model = inla.PMMCAR(W = W, J=3, alpha.min = 0, alpha.max=1,
                              initial.values = rep(0,6)) ),
  family = "poisson",
  offset = log(dd$offset),
  data = dd,
  num.threads = 1,
  control.compute = list(internal.opt = FALSE),
  verbose = TRUE)

#' Dense parametrisation BYM model, as in Urdangarin et al. (2024)
#' Is slow, but works. PMMCAR modes are given as initial values for speedup.
#' Only commented due to long computational time; uncommenting is safe. 

#  bym.dense <- inla(
#    Y ~ 1 + X1 + X2 + X3 + 
#      f(ID, model = inla.BYM.dense(W = W, J=3,
#                                   initial.values = PMMCAR$internal.summary.hyperpar$mode) ),
#    family = "poisson",
#    offset = log(dd$offset),
#    data = dd,
#    num.threads = 1,
#    control.compute = list(internal.opt = FALSE),
#    verbose = TRUE)


#' Here is the problem: the sparse parametrisation model fails, despite being practically equivalent
#' to the "dense" one, except for how constraints are defined.
#' Here, the constraint is only set upon the ICAR part.
#' Function call crashes:
bym.sparse.attempt <- inla(
  Y ~ 1 + X1 + X2 + X3 + 
    f(ID, model = inla.BYM.sparse(W = W, J=3 ), 
      extraconstr = list(
        A = cbind(Matrix::Matrix(0, nrow = nrow(A.constr), ncol = ncol(A.constr)), A.constr),
        e=c(0,0,0),
      initial.values = PMMCAR$internal.summary.hyperpar$mode)),
  family = "poisson",
  offset = log(dd$offset),
  data = dd,
  num.threads = 1,
  control.compute = list(internal.opt = FALSE),
  verbose = TRUE)


