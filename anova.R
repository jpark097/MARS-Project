
anova.mars<-function(mout)
{
  
  A<-mout$B
  y<-mout$y
  for(i in 1: (dim(A)[2]))
  {
    variance_of_output <- var((A[,i]*mout$coefficients[i]))
    cat(paste(c("Variance of ", 
                names(mout$coefficients)[i], "is ", 
                variance_of_output, 
                " with % of explained variance ",
                100*variance_of_output/var(y), "%", "\n" )))
  }
  
}
