if(!require(GA))
  {install.packages("GA")
  require(GA)
  }

x<-cars$speed
y<-cars$dist
n<-length(x)

#L<-function(beta0,beta1,sigma2) -n*log(sigma2)-sum((beta0+beta1*x-y)^2)/sigma2
#this form Likelihood function can't prevent sigma2 from being negtive during optimization

L<-function(beta0,beta1,sigma) -sum(dnorm(y,beta0+beta1*x,sigma,log = TRUE))
k.mle<-coef(mle(L,start = list(beta0=1,beta1=0,sigma=1)))
k.mle<-c(k.mle[1],k.mle[2])

f<-function(k) -sum((k[1]+k[2]*x-y)^2)
k.ga<-matrix(NA,30,2)
#coef gained by GA, iter num is 30
monitor <- function(obj) 
{ 
  obj@fitnessValue <- max(obj@fitness, na.rm = TRUE)
  valueAt <- which(obj@fitness == obj@fitnessValue)
  solution <- obj@population[valueAt,,drop=FALSE]
  if(nrow(solution) > 1)
  { # find unique solutions to precision given by default tolerance
    eps <- gaControl("eps")
    solution <- unique(round(solution/eps)*eps, margin = 1)
  }
  k.ga[obj@iter,]<<-solution
}
Result=ga("real-valued",f,
          lower=c(0,-40),
          upper = c(100,40),
          maxiter = 30,
          monitor = monitor)
plot(x,y)
for(i in 1:30){ 
  par(new=TRUE)
  plot(function(x){k.ga[i,1]+k.ga[i,2]*x})
}
