setwd("/Users/willphillips/Desktop/Introduction to Biocomputing/Ex10/Biocomp-Fall2018-181109-Exercise10")
dat=read.csv("data.txt")
library(deSolve)
library(ggplot2)

pop1=function(t,y,p){               #creating the funciton to model the growth/competition
  N1=y[1]                           
  N2=y[2]
  R1=p[1]
  R2=p[2]
  a11=p[3]
  a22=p[4]
  a12=p[5]
  a21=p[6]
  
  dN1dt=R1*(1-(N1*a11)-(N2*a12))*N1
  dN2dt=R2*(1-(N2*a22)-(N1*a21))*N2
  
  return(list(c(dN1dt,dN2dt)))
}
#case#= R1,R2,a11,a22,a12,a21
#a12<a11, a21<a22
case1=c(1,1,0.5,0.5,3,3)              #The case where a11 and a22 are less than a12 and a21
case2=c(1,1,4,2,0.2,0.1)              #The case where a11 and a22 are greater than a12 and a21
case3=c(1,1,1,2,1,2)                  #The case where a11=a12 and a22=a21
N0=c(1,1)                             #Initial conditions
time=1:100                            #Time

sim1=ode(y=N0,times = time, func = pop1, parms = case1)         #Running the simulations
sim2=ode(y=N0,times = time, func = pop1, parms = case2)
sim3=ode(y=N0,times = time, func = pop1, parms = case3)


Output1=data.frame(time=sim1[,1],N1=sim1[,2], N2=sim1[,3])      #putting the simulation results in a data frame
Output2=data.frame(time=sim2[,1],N1=sim2[,2], N2=sim2[,3])
Output3=data.frame(time=sim3[,1],N1=sim3[,2], N2=sim3[,3])

a=ggplot(Output1, aes(x=time, y=N))+geom_line(aes(x=time, y=Output1[,2]), color="black")+geom_line(aes(x=time,y=Output1[,3]),color="red")      #Plots where N1 is in black and N2 is in red
b=ggplot(Output2, aes(x=time, y=N))+geom_line(aes(x=time, y=Output2[,2]), color="black")+geom_line(aes(x=time,y=Output2[,3]),color="red")
d=ggplot(Output3, aes(x=time, y=N))+geom_line(aes(x=time, y=Output3[,2]), color="black")+geom_line(aes(x=time,y=Output3[,3]),color="red")
