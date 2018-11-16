## IAN KLUPAR -- INTRO BIOCOMP -- HOMEWORK 10 ##

rm(list=ls())
setwd("~/Documents/r/intro-biocomp/Biocomp-Fall2018-181109-Exercise10")

##-------------------Question 1-------------------
df <- read.csv("data.txt", header = T, sep = ',')
head(df)
plot(x~y, data = df)

# Quadratic model
quadratic <- function(p,x,y){
  a=p[1]
  b=p[2]
  c=p[3]
  sigma=exp(p[3])
  pred=a+(b*x)+(c*x*x)
  nll=-sum(dnorm(x=y,mean=pred,sd=sigma,log=TRUE))
  return(nll)
}
# Linear model
linear <- function(p,x,y){
  a=p[1]
  b=p[2]
  sigma=exp(p[3])
  pred=a+b*x
  nll=-sum(dnorm(x=y,mean=pred,sd=sigma,log=TRUE))
  return(nll)
}
# Initial random parameters
quadraticGuess=c(1,1,1)
linearGuess=c(1,1,1)

# run the optim function
quadFit=optim(par=quadraticGuess,fn=quadratic,x=df$x,y=df$y)
lineFit=optim(par=linearGuess,fn=linear,x=df$x,y=df$y)

# Test statistic
teststat=2*(quadFit$value-lineFit$value)
teststat

ans=length(quadFit$par)-length(lineFit$par)
1-pchisq(teststat,ans)
# 0 means that they are not different enough?

##-------------------Question 2-------------------
rm(list=ls())
library(deSolve)
library(ggplot2)

sim <- function(t,y,p){
  N1 = y[1]
  N2 = y[2]
  
  r1 = p[1]
  r2 = p[2]
  a11 = p[3]
  a12 = p[4]
  a22 = p[5]
  a21 = p[6]
  
  dNdt1 = r1*(1 - N1*a11 - N2*a12)*N1
  dNdt2 = r2*(1 - N2*a22 - N1*a21)*N2
  
  return(list(c(dNdt1,dNdt2)))
}
# Case 1
times = 1:100
# intitial population values
N0 = c(10,1)

# params = c( r1, r2, a11, a12, a22, a21)
params1=c(0.1, 0.1, 1, 0.5, 1, 0.5)
sim1 <- ode(y=N0, times=times, func=sim, parms=params1)
output1=data.frame(time=sim1[,1],N=sim1[,2:3])
output1

# plot
ggplot(output1,aes(x=time,y=N.1))+
  geom_line()+
  geom_line(data=output1,mapping=aes(x=time,y=N.2),col='red')+
  theme_classic()

# Case 2
# hole N0 constant but change parameters
times = 1:1000
# intitial population values
N0 = c(10,1)

# params = c( r1, r2, a11, a12, a22, a21)
params2=c(0.1, 0.1, .01, .005, .01, .005)
sim2 <- ode(y=N0,times=times,func=sim,parms=params2)
output2=data.frame(time=sim2[,1],N=sim2[,2:3])
output2

# plot
ggplot(output2,aes(x=time,y=N.1))+
  geom_line()+
  geom_line(data=output2,mapping=aes(x=time,y=N.2),col='red')+
  theme_classic()

# Case 3
# keep the parameters the same but change N0
times = 1:100
# intitial population values
N0 = c(9,10)

# params = c( r1, r2, a11, a12, a22, a21)
params3=   c(0.1, 0.1, .01, .005, .01, .005 )
sim3 <- ode(y=N0,times=times,func=sim,parms=params3)
output3=data.frame(time=sim3[,1],N=sim3[,2:3])
output3

# plot
ggplot(output3,aes(x=time,y=N.1))+
  geom_line()+
  geom_line(data=output3,mapping=aes(x=time,y=N.2),col='red')+
  theme_classic()

# as long as a12 < a11 and a21 < a22, the populations seem to be able to coexist










