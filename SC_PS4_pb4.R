#t=0
x=c(10)
wt=rnorm(101)
wt[1]<-0
vt=rnorm(101)
y=c(x[1]+vt[1])

#Simulation for t=1,...,100
for(i in 2:101){
  x<-rbind(x,c(0.9*x[i-1]+wt[i]))
  y<-rbind(y,c(x[i] + vt[i]))
}


E=c(0) #E = E[xt|Ft]
e=c(0) #e = E[xt|Ft-1]
V=c(10) #V = Var[xt|Ft]
v=c(10) #v = var[xt|Ft-1]


#Kalman filter algorithm
for (t in 2:101) {
  #Prediction
  e<-rbind(e,0.9*E[t-1])
  v<-rbind(v,0.81*V[t-1] + 1)
  
  S=v[t]+1
  K=v[t]/S
  
  #Update
  E<- rbind(E, e[t] + K*(y[t]-e[t]))
  V<- rbind(V, (1 - K)*v[t])
}

par(mfrow=c(2,1))
plot(x, xlab = "t", ylab = "xt", col="red")
points(E, col="blue")

r=x-E #xt-E[xt|Ft]

plot(r[2:101], xlab = "t", ylab = "xt-E[xt|Ft]") #t=0 is an outlier
abline(0,0)

print('the mean and the variance of the residual are')
mean(r)
var(r)
