#Mars.R file
library(earth)
library(rpart)

mars  <- function(formula ,data, control = NULL, ...) {
  cc <- match.call()
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  mt <- attr(mf, "terms")
  x <- model.matrix(mt, mf)
  x_names <- colnames(x)
  if(is.null(control)) {
    control <- mars.control()
  }
  fwd_out <- fwd_stepwise(y, x, control)
  bwd_out <- bwd_stepwise(fwd_out, control)
  fit <- lm(y~.-1,data = data.frame(y = y,bwd_out$B))
  out <- c(list(call = cc, formula = formula, y = y, B = bwd_out$B, splits = bwd_out$splits, x_names = x_names),fit)
  class(out) <- c("mars", class(fit))
  return(out)
}

mars.control <- function(Mmax = 2, d=3,trace=FALSE) {
  control <- new_mars.control(Mmax, d, trace)
  validate_mars.control(control)
  return (control)
}

new_mars.control <- function(Mmax, d, trace) {
  structure(list(Mmax = Mmax, d = d, trace = trace), class = 'mars.control')
}

validate_mars.control <- function(control) {
  if(control$Mmax < 2) {
    warning("Input Mmax must be >= 2; setting to 2")
    Mmax <- 2
  }
}

H <- function(x,s,t){
  return(pmax(0,s*(x-t)))
}

fwd_stepwise <- function(y,x,control){
  N <- length(y) 
  n <- ncol(x) 
  B <- matrix(1, nrow = N, ncol = 1) 
  splits <- list(data.frame(m=0,v=0,s=NA,t=NA))
  M <- 1
  while(M <= control$Mmax) { 
    
    lof_best <- Inf
    for(m in 1:M) {
      yy <- setdiff(1:n, splits[[m]]$v)
      
      for(v in yy){ 
        tt <- split_points(x[,v],B[,m])
        for(t in tt) { 
          Bnew <- data.frame(B[,(1:M)[-m]],
                             Btem1=B[,m]*H(x[,v],1,t),Btem2=B[,m]*H(x[,v],-1,t)) # ** 1
          gdat <- data.frame(y=y,Bnew)
          lof <- LOF(y~.,gdat, control)
          if(lof < lof_best) { 
            lof_best <- lof
            split_best <- c(m=m,v=v,t=t) 
          } 
        } 
      } 
    } 
    m <- split_best["m"]; v <- split_best["v"]; t <- split_best["t"]
    B <- cbind(B, B[,m]*H(x[,v],1,t))
    B <- cbind(B, B[,m]*H(x[,v],-1,t))
    leftdf <- rbind(splits[[m]], c(m,v,s=-1,t)) 
    rightdf <- rbind(splits[[m]], c(m,v,s=1,t))
    splits <- c(splits,list(leftdf), list(rightdf))
    M <- M + 2
  } 
  colnames(B) <- paste0("B",(0:(ncol(B)-1)))
  fwd_list <- list(y=y,B=B,splits=splits)
  return(fwd_list)
}

bwd_stepwise <- function(bwd_in, control){
  data <- data.frame(y = bwd_in$y, bwd_in$B)
  lof_best <- LOF(y ~ .,data, control)
  Mmax <- ncol(bwd_in$B)
  KStar <- JStar <- bwd_in$B 
  splits <- KSplit <-  bwd_in$splits
  
  for(M in Mmax:2) {
    L <- KStar
    split <- KSplit
    b <- Inf
    
    for(m in 2:M) {
      K <- L[,-m]
      data2 <- data.frame(y = bwd_in$y, K)
      lof <- LOF(y ~ ., data2, control)
      
      if(lof < b) {
        b <- lof 
        KStar <- K
        KSplit <- split[-m]
      }
      
      if(lof < lof_best) {
        lof_best <- lof 
        JStar <- K 
        splits <- split[-m]
      }
      
    }
  }
  
  bwd_list <- list(y = bwd_in$y, B = JStar, splits = splits)
  return(bwd_list)
  
}

split_points <- function(xv,Bm){
  out <- sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}

LOF <- function(form,data, control) {
  NR <- nrow(data)
  NC <- ncol(data) - 1
  
  ff <- lm(form,data)
  SumRes <- sum(residuals(ff)^2)
  SumHat <- sum(hatvalues(ff)) 
  
  C <-   SumHat + control$d * NC
  out <- SumRes * (NR / (NR - C)^2)         
  
  return(out)
}

