x=cars$speed
y=cars$dist
f=function(k) -sum((k[1]*x+k[2]-y)^2)
solus=matrix(NA,30,2)
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
  solus[obj@iter,]<<-solution
}
Result=ga("real-valued",f,
          lower=c(0,-40),
          upper = c(100,40),
          maxiter = 30,
          monitor = monitor)
plot(x,y)
for(i in 1:30)
  {  i=2
  par(new=TRUE)
  plot(function(x){solus[i,1]*x+solus[i,2]},0,25)}
