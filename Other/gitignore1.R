
simulate.w <- function(n, p = 0.05){
  lw <- list()
  lw[[1]] <- sample(c(2:n),
                    size = sample(1:(2*ceiling(p*(n-1))), size=1),
                    replace = FALSE,
                    prob = c(2:n)^(-2))
  for(i in c(2:(n))){
    nb.0 <- which(sapply(lw, function(x) i %in% x))
    nbmin <- ifelse(length(nb.0)==0, 1, 0)
    if(i < n) {
      nb.1 <- sample( rep(c((i+1): n),2),
                      size = max(round(p*(n-i), 0), nbmin),
                      replace = FALSE,
                      prob = rep(c((i+1):n)^(-2), 2))
    } else{
      nb.1 <- NULL
    }
    lw[[i]] <- c(nb.0, nb.1)
  }
  #' correct for singletons as last entries
  if(length(lw[[n]])==0){
    lw[[n]] <- sample(c(1:(n-1)), max(round(p*(n-1),0), 1) )
    for(j in c(1:length(lw[[n]]))){
      lw[[lw[[n]][[j]]]] <- c(lw[[lw[[n]][[j]]]], n)
    }
    
  }
  W <- sapply(lw, function(x){
    vv <- numeric(n)
    vv[x]<-1
    return(vv)
  })
  return(W)
}
W <- simulate.w(n = 8, p=0.3)



eigen.S <- function(Sigma, phi, W, step){
  
  if(det(Sigma)<0) stop("Negative determinant in Sigma == ", det(Sigma))
  
  phi[c((step+1):length(phi))] <- 0
  phi1 <- phi
  phi0 <- phi
  phi0[step] <- 0
  Phi1 <- diag(phi1)
  Phi0 <- diag(phi0)
  Phihat1 <- diag(1-phi1)
  Phihat0 <- diag(1-phi0)
  
  L <- diag(rowSums(W)) - W
  invL <- MASS::ginv(L)
  
  In <- diag(nrow=nrow(invL))
  M <- diag(sqrt(eigen(Sigma)$values)) %*% t(eigen(Sigma)$vectors)
  
  S1 <- kronecker(t(M) %*% Phi1 %*% M, invL) +
    kronecker(t(M) %*% Phihat1 %*% M, In)
  S0 <- kronecker(t(M) %*% Phi0 %*% M, invL) +
    kronecker(t(M) %*% Phihat0 %*% M, In)
  
  ratio <- S1 %*% solve(S0)
  
  M_j <- M[step, ] %*% t(M[step,])
  S_j <- phi[step] * kronecker(M_j, (invL - In))
  (phi[step] * eigen(kronecker(M_j, (invL - In)) %*% solve(S0))$values) + 1
  
  return(eigen(S)$values)
}


 


diag.N <- diag(c(1.5, 0.9, 0.8, 1.2))
no.diag.N <- matrix(0, nrow = 4, ncol = 4)
no.diag.N[lower.tri(no.diag.N, diag = FALSE)] <- c(1.5, 1.75, -0.01, 2, -0.6, 0.8)
Sigma <- (diag.N + no.diag.N) %*% t(diag.N + no.diag.N)
det(Sigma)
eigen(Sigma)$vectors



S2 <- kronecker(t(M) %*% phi2 %*% M, invL) + t(M)

eigen.S(Sigma = Sigma, phi = phi, W = W)









M <- diag(sqrt(eigen(Sigma)$values)) %*% t(eigen(Sigma)$vectors)
eVec <- eigen(Sigma)$vectors
eVal <- eigen(Sigma)$values
eVec[,2] * sqrt(eVal[2])
m1 <- M[1,]
m2 <- M[2,]
M1 <- m1 %*% t(m1)
M2 <- m2 %*% t(m2)


phi <- c(0.7, 0.9, 0.4, 0.6)
phi1 <- diag(c(phi[1], 0, 0))
phi2 <- diag(c(0, phi[2], 0))
#phi[2] * m2%*%t(m2)
S2 <- kronecker(t(M) %*% (phi1 + phi2) %*% M, invL ) +
  kronecker(t(M) %*% (diag(3) - phi1 -phi2) %*% M, In ) 

S2.check <- phi[1] * kronecker(M1, invL) + phi[2]*kronecker(M2, invL) +
  kronecker(Sigma, In) - kronecker(phi[1]*M1, In) - kronecker(phi[2]*M2, In)

A <- phi[1]*M1 + phi[2]*M2

S2.alt <- kronecker(A, (invL - In)) + kronecker(Sigma, In)

eigen(S2)$values




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
    log.p[j] <- dexp(x=sqrt(2*KLD), rate = rate, log = T) +
      log(1/2*sqrt(KLD)) + log(abs(derivative))
  }
  return(sum(log.p))
}

inla.pc.mbym.phi(phi = phi, invL = invL, M = M, rate = 1.5)







## Compare the precision matrix in dense MMBYM

M.inv <- solve(M)
In <- Matrix::Diagonal(nrow(W), 1)

MI_0 <- kronecker(M.inv, In)
  #D <- rowSums(W)
BlockIW_0 <- Matrix::bdiag(lapply(1:nrow(Phi), function(i) {
  solve(phi[i]*invL + (1-phi[i])*In)
}))
Q_0 <- (MI_0 %*% BlockIW_0) %*% kronecker(t(M.inv),In)
Q_0.check <- solve( kronecker(t(M) %*% Phi %*% M, invL) + kronecker(t(M) %*% Phihat %*% M, In ))



L_0 <- diag(rowSums(W)) - W
eval <- exp(mean(log(diag(INLA:::inla.ginv(L_0)))))
constr <- INLA:::inla.bym.constr.internal(L_0, adjust.for.con.comp = T)
L <- INLA::inla.scale.model(L_0, constr = list(A = constr$constr$A, e=constr$constr$e))
eigenval <- eigen(L_0)$values[-nrow(W)]
Ue <- eigen(L_0)$vectors

MI_1 <- kronecker(M.inv, Ue)
BlockIW_1 <- 
  Matrix::bdiag(lapply(1:nrow(Phi), function(i) {
    Matrix::Diagonal(x=c(
      eigenval/(eigenval+phi[i]*(eval-eigenval)), 1/(1-phi[i]))
    )
  }))
Q_1 <- (MI_1 %*% BlockIW_1) %*% t(MI_1)





















