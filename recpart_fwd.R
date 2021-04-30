recpart_fwd <- function(y,x,Mmax){
  #---------------------------------------------------
  # Error checking:
  #---------------------------------------------------
  # Initialize:
  
  N <- length(y) # sample size
  n <- ncol(x) # number of predictors
  B <- init_B(N,Mmax) # Exercise: write init_B()
  splits <- vector(mode="list",length=Mmax) # 2(a)
  #---------------------------------------------------
  # Looping for forward selection:
  if(Mmax < 2){
    warning("Mmax is less than 2, so setting it as 2\n")
    Mmax <- 2
  }
  for(M in 1:Mmax) { # contrast to indexing 2...Mmax in Friedman
    lof_best <- Inf
    for(m in 1:M) { # choose a basis function to split
      for(v in 1:n){ # select a variable to split on
        tt <- split_points(x[,v],B[,m]) # Exercise: write split_points() 
        for(t in tt) { 
          Bnew <- data.frame(B[,(1:M)[-m]],Btem1=B[,m]*(H(x[,v]-t)),
                             Btem2=B[,m]*(H(-(x[,v]-t))))
          gdat <- data.frame(y=y,Bnew)
          lof <- LOF(y~.,gdat) #  Use your LOF() from week 4
          if(lof < lof_best) { 
            lof_best <- lof
            best_splits <- c(m=m,v=v,t=t) 
          } # end if
        } # end loop over splits
      } # end loop over variables
    } # end loop over basis functions to split
    
    m <- best_splits["m"]; v <- best_splits["v"]; t <- best_splits["t"]
    B[,M+1] <- B[,m]*H(-(x[,v]-t)) # Base functions are product of hinge functions
    B[,m] <- B[,m]*H((x[,v]-t))
    
    # 2(b)
    splits[[M+1]] <- splits[[m]]
    splits[[M+1]] <- rbind(splits[[M+1]],c(s=-1,v,t))
    splits[[m]] <- rbind(splits[[m]],c(s=1,v,t))
  } # end loop over M
  
  return(list(B=B,splits=splits))
}

H <- function(values){
  res <- values >=0
  return (as.numeric(res))
}

LOF <- function(form,data) {
  ff <- lm(form,data)
  return(sum(residuals(ff)^2))
}

init_B <- function(N,Mmax) {
  B <- data.frame(matrix(NA,nrow=N,ncol=(Mmax+1)))
  B[,1] <- 1
  names(B) <- c("B0",paste0("B",1:Mmax))
  return(B)
}

split_points <- function(xv,Bm) {
  out <- sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}

set.seed(123); n <- 10
x <- data.frame(x1=rnorm(n),x2=rnorm(n))
y <- rnorm(n)
rp_fwd <- recpart_fwd(y,x,Mmax=9)
rp_fwd$splits