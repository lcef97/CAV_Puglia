##' input data ----------------------------------------------------------------- 

if(! "bigDM" %in% rownames(utils::installed.packages())) install.packages("bigDM")
library(INLA)  

load("realdata_repl.RData")
dd <- realdata_repl$dd
W <- realdata_repl$W

#' Constraint
L <- Matrix::Diagonal(x=apply(W, MARGIN=1, FUN=function(x) sum(x)), n=nrow(W)) - W

constr <- INLA:::inla.bym.constr.internal( Q = L, adjust.for.con.comp = T)
A_constr <- kronecker(Matrix::Diagonal(n=3, x=1), constr$constr$A)
#' For the BYM2 model: constraints only applied to the ICAR part
constr_BYM <- list(
  A = cbind(Matrix::Matrix(0, nrow = nrow(A_constr), ncol = ncol(A_constr)),
            A_constr),
  e=c(0,0,0) )

##' Function to sample from the posterior of \Sigma -----------------------------
#' Adapted from bigDM::Mmodel_compute_cor
Mmodel_compute_cor_bigDM <- function (model, n.sample, k) {
  o <- tryCatch({
    hyperpar.sample <- INLA::inla.hyperpar.sample(n.sample,  model, improve.marginals = TRUE)
    if (nrow(model$summary.hyperpar) == k * (k+1) / 2) {
      K <- k*(k+1)/2
      hyperpar.sample[, 1:k] <- exp(hyperpar.sample[,  1:k])
    } else {
      K <- k*(k+3)/2
      hyperpar.sample[, c((k+1):(2*k))] <- exp(hyperpar.sample[, c((k+1):(2*k))])
    }
    hyperpar.sample <- split(hyperpar.sample[, c((k+1):K)], c(1:n.sample))
    param.sample <- lapply(hyperpar.sample, function(x) {
      N <- diag(x[c(1:k)])
      N[lower.tri(N, diag = FALSE)] <- x[-c(1:k)]
      Sigma <- N %*% t(N)
      Rho <- cov2cor(Sigma)
      Rho.values <- Rho[lower.tri(Rho)]
      return(list(sigma = diag(Sigma), rho = Rho.values))
    })
    cor.sample <- do.call(rbind, lapply(param.sample, function(x) x$rho))
    cor.density <- apply(cor.sample, 2, function(x) {
      density(x, n = 500, bw = "SJ", from = -1, to = 1)
    })
    marginals.cor <- lapply(cor.density, function(xx) cbind(x = xx$x,  y = xx$y))
    names(marginals.cor) <- paste("rho", apply(combn(k, 2), 2, function(x) paste0(x, collapse = "")),  sep = "")
    summary.cor <- do.call(rbind, lapply(marginals.cor,  function(x) bigDM:::compute.summary(x, cdf = NULL)))
    var.sample <- do.call(rbind, lapply(param.sample,   function(x) x$sigma))
    var.density <- apply(var.sample, 2, function(x) {
      density(x,  n = 500, bw = "SJ", from = 0)
    }) 
    marginals.var <- lapply(var.density, function(xx) cbind(x = xx$x,  y = xx$y))
    names(marginals.var) <- paste("var", 1:k, sep = "")
    summary.var <- do.call(rbind, lapply(marginals.var, function(x) {
      bigDM:::compute.summary(x, cdf = NULL)
    }))
  })
  if (any(class(o[[1]]) == "error")) {
    summary.cor <- data.frame(rep(NA, ncol(combn(k, 2))),
                              rep(NA, ncol(combn(k, 2))),
                              rep(NA, ncol(combn(k,   2))), 
                              rep(NA, ncol(combn(k, 2))),
                              rep(NA, ncol(combn(k,  2))), 
                              rep(NA, ncol(combn(k, 2))))
    colnames(summary.cor) <- c("mean", "sd", "0.025quant","0.5quant", "0.975quant", "mode")
    rownames(summary.cor) <- paste("rho", apply(combn(k,  2), 2,
                                                function(x) paste0(x, collapse = "")), sep = "")
    marginals.cor <- as.list(rep(NA, ncol(combn(k, 2))))
    names(marginals.cor) <- rownames(summary.cor)
    summary.var <- data.frame(rep(NA, k), rep(NA, k), rep(NA, k), rep(NA, k), rep(NA, k), rep(NA, k))
    colnames(summary.var) <- c("mean", "sd", "0.025quant", "0.5quant", "0.975quant", "mode")
    rownames(summary.var) <- paste("var", 1:k, sep = "")
    marginals.var <- as.list(rep(NA, k))
    names(marginals.var) <- rownames(summary.var)
  }
  res <- list(summary.cor = summary.cor, marginals.cor = marginals.cor, 
                        summary.var = summary.var, marginals.var = marginals.var)
  return(res)
}

##' Comparison/benchmarking: LCAR model ----------------------------------------
#'  Function used to implement the Leroux proper CAR model.
#'  It is strictily based on bigDM::Mmodel_lcar,
#'  and serves illustrational purposes as a benchmark -------------------------#

inla.LMMCAR.model <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.Mmodel.LCAR, ...)

inla.rgeneric.Mmodel.LCAR <- 
  function(cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                   "log.prior", "quit"), theta = NULL){
    
    envir <- parent.env(environment())
    if(!exists("df", envir = envir)) assign("df", as.numeric(k+2), envir = envir)
    if(!exists("PC", envir = envir)) assign("PC", FALSE, envir = envir)
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
      diag.N <- sapply(theta[as.integer(k + 1:k)], function(x) { exp(x)})
      no.diag.N <- theta[as.integer(2 * k + 1:(k * (k - 1)/2))]
      N <- diag(diag.N, k)
      N[lower.tri(N, diag = FALSE)] <- no.diag.N
      Sigma <- N %*% t(N)
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
        return(c(rep(0, k), rep(0, k*(k+1)/2))) #changes wrt bigDM's log(9)
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

#' Whishart prior on the scale parameter through Bartlett decomposition,
#' as in the bigDM package ==> Looks good -------------------------------------#
m.LMMCAR <- inla(
  N_ACC ~ 0+ Y_2021 + Y_2022 + Y_2023 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMMCAR.model(k = 3, W = W, initial.values = rep(0, 9),
                                    PC = F, Bartlett = T, df=5)),
  offset = log(nn), family = "poisson", data =dd,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

#' Scale matrix posterior -----------------------------------------------------#
set.seed(11)
Sigma_LMMCAR <- Mmodel_compute_cor_bigDM(m.LMMCAR, k = 3, n.sample = 5e+4)
#' Marginal variances ---------------------------------------------------------#
Sigma_LMMCAR$summary.var

#' Marginal correlations ==> REASONABLE! --------------------------------------#
Sigma_LMMCAR$summary.cor

#' Mixing parameter posterior ==> median: 0.76; 0.52; 0.49 --------------------#
sapply(c(1,2,3), function(i){
  INLA::inla.zmarginal(INLA::inla.tmarginal(
    function(x)exp(x)/(1+exp(x)), m.LMMCAR$marginals.hyperpar[[i]] ),
    silent = T)})

##' ISSUE: BYM -----------------------------------------------------------------
#'
#' General function to implement the M-model extension of the BYM.
#' Same prior choice as for the LCAR: Bartlett decomposition on the
#' marginal covariance matrix; Uniform prior on the mixing.
#' Sum-to-zero constraint on the ICAR component -------------------------------#

inla.MMBYM.model <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.Mmodel.BYM, ...)

inla.rgeneric.Mmodel.BYM <- 
  function(cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                   "log.prior", "quit"), theta = NULL){
    
    envir <- parent.env(environment())
    if(!exists("df", envir = envir)) assign("df", k + 2, envir = envir)
    if(!exists("PC", envir = envir)) assign("PC", FALSE, envir = envir)
    if(!exists("sparse", envir = envir)) assign("sparse", TRUE, envir = envir)
    if(!exists("cache.done", envir=envir)){
      starttime.scale <- Sys.time()
      #' Laplacian matrix scaling: only needs being done once
      L_unscaled <- Matrix::Diagonal(n=nrow(W), x=rowSums(as.matrix(W))) -  W
      constr <- INLA:::inla.bym.constr.internal(L_unscaled, adjust.for.con.comp = T)
      scaleQ <- INLA:::inla.scale.model.internal(
        L_unscaled, constr = list(A = constr$constr$A, e = constr$constr$e))
      L <- scaleQ$Q
      invL <- INLA:::inla.ginv(L)
      In <- Matrix::Diagonal(n = nrow(W), x = 1)
      eigenvalues <- eigen(invL - In)$values
      endtime.scale <- Sys.time()
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
      cat("Time needed for scaling Laplacian matrix: ",
          round(difftime(endtime.scale, starttime.scale), 3), " seconds \n")
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
      Sigma <- N %*% t(N)    
      e <- eigen(Sigma)
      M <- t(e$vectors %*% diag(sqrt(e$values)))
      invM <- solve(M)
      Lambda <- invM %*% t(invM)
      return(list(phi = phi, M = M, Lambda = Lambda, invM = invM))
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
        MI <- kronecker(param$invM, In)
        BlockIW <- Matrix::bdiag(lapply(1:k, function(i) {
          solve(param$phi[i]*invL + (1-param$phi[i])*In)
        }))
        Q <- (MI %*% BlockIW) %*% Matrix::t(MI)
      } else {
        q11 <- param$invM %*% Matrix::Diagonal(x = 1/(1 - param$phi), n = k) %*% t(param$invM)
        q12 <- param$invM %*% Matrix::Diagonal(x = sqrt(param$phi)/(1 - param$phi), n = k)
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
        #' PC-prior - warning: need some tuning
        if(!exists("alpha", envir = envir)) alpha <- 2/3
        if(!exists("U" , envir = envir)) U <- 1/2
        val <- inla.pc.mbym.phi(eigenvalues = eigenvalues, phi = param$phi, alpha = alpha, U = U) +
          sum(-theta[as.integer(1:k)] - 2 * log(1 + exp(-theta[as.integer(1:k)])))
      }
      #' Chi squared distribution on the squares of the diagonal Bartlett factor
      val <- val + k * log(2) + 2 * sum(theta[k + 1:k]) + 
        sum(dchisq(exp(2 * theta[k + 1:k]), df = c(df:(df-k+1)), log = TRUE))
      #' Normal prior on the off-diagonal of Bartlett factor
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

m.BYM.sparse <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MMBYM.model(k = 3, W = W, initial.values = rep(0, 9),
                                    PC = F, df=5),
      extraconstr = constr_BYM),
  offset = log(nn), family = "poisson", data =dd,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

#' Scale matrix posterior
set.seed(11)
Sigma_MMBYM <- Mmodel_compute_cor_bigDM(m.BYM.sparse, k = 3, n.sample = 5e+4)
#' Marginal variances
Sigma_MMBYM$summary.var
#' Marginal correlations ==> median 0.04; -0.94; -0.11 ------------------------#
Sigma_MMBYM$summary.cor

#' Mixing parameter posterior ==> median: 0.51; 0.80; 0.40 --------------------#
sapply(c(1,2,3), function(i){
  INLA::inla.zmarginal(INLA::inla.tmarginal(
    function(x)exp(x)/(1+exp(x)), m.BYM.sparse$marginals.hyperpar[[i]] ),
    silent = T)})



