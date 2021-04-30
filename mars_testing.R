source("mars.R")
set.seed(123); n = 10
x = data.frame(x1=rnorm(n),x2=rnorm(n))
y = rnorm(n)
rp = fwd_stepwise(y,x,Mmax=2)