setwd("/Users/willphillips/Desktop/Introduction to Biocomputing/Ex10/Biocomp-Fall2018-181109-Exercise10")
dat=read.csv("data.txt")

quadratic=function(p,x,y){                                      #Creating the first model to test
  a=p[1]
  b=p[2]
  c=p[3]
  sigma=exp(p[4])
  
  quadnumb=a+b*x+c*x^2
  
  nll=-sum(dnorm(x=y,mean = quadnumb,sd=sigma, log = TRUE))
  return(nll)
}
initialguessq=(c(1,1,1,1))                                      #Initial guesses for the first function
fitq=optim(par = initialguessq,fn=quadratic,x=dat$x,y=dat$y)    #the optimization function for the first model
print(fitq)



linear=function(p,x,y){                                         #Creating the second model to test
  a=p[1]
  b=p[2]
  sigma=exp(p[3])
  
  linearnumb=a+b*x
  
  nll=-sum(dnorm(x=y,mean = linearnumb,sd=sigma, log = TRUE))
  return(nll)
}
initialguessl=c(1,1,1)                                          #Initial guesses for the second model
fitl=optim(par = initialguessl, fn=linear,x=dat$x, y=dat$y)     #Optimization function for the second model
print(fitl)

models=c(fitl$value,fitq$value)                                 #Variable holding the negative log likelihoods of the two models


if(fitl$value<fitq$value){                                      #Conditional to determine which model has the lower value of negative log likelihood. Whichever model has the lower value is printed
  print("linear model is superior")
}else{
  if(fitl$value==fitq$value){
    print("Models are equivalent")
  }else{
    print("quadratic model is superior")
  }
}

