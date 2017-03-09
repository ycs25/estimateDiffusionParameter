estfc3<-function(Y) {

  for (t in 1:1000) {
    x1=X[t]
    x2=X[t+1]
    n=1
    g1=0
    s1=0
    s2=0
    for (m in 0:n) {
      t1=choose(n,m)*gamma(Y[1]+Y[2]+n+m-2)/gamma(Y[2]+m)*((x1-1)^m)
      s1=s1+t1
      t2=choose(n,m)*gamma(Y[1]+Y[2]+n+m-2)/gamma(Y[2]+m)*((x2-1)^m)
      s2=s2+t2
    }
    p2<-sqrt(gamma(Y[2]+n)*(2*n+Y[1]+Y[2]-1)*gamma(Y[2])
             *gamma(Y[1])/(factorial(n)*gamma(Y[1]+Y[2]+n-1)
                           *gamma(Y[1]+Y[2])*gamma(Y[1]+n)))*s2
    p1<-sqrt(gamma(Y[2]+n)*(2*n+Y[1]+Y[2]-1)*gamma(Y[2])
             *gamma(Y[1])/(factorial(n)*gamma(Y[1]+Y[2]+n-1)
                           *gamma(Y[1]+Y[2])*gamma(Y[1]+n)))*s1
    l=-n*(1+(n-1)/(Y[1]+Y[2]))*0.01
    h=p2-exp(l)*p1
    g1=g1+h
  }
  
  for (t in 1:1000) {
    x1=X[t]
    x2=X[t+1]
    n=2
    g2=0
    s1=0
    s2=0
    for (m in 0:n) {
      t1=choose(n,m)*gamma(Y[1]+Y[2]+n+m-2)/gamma(Y[2]+m)*((x1-1)^m)
      s1=s1+t1
      t2=choose(n,m)*gamma(Y[1]+Y[2]+n+m-2)/gamma(Y[2]+m)*((x2-1)^m)
      s2=s2+t2
    }
    p2<-sqrt(gamma(Y[2]+n)*(2*n+Y[1]+Y[2]-1)*gamma(Y[2])
             *gamma(Y[1])/(factorial(n)*gamma(Y[1]+Y[2]+n-1)
                           *gamma(Y[1]+Y[2])*gamma(Y[1]+n)))*s2
    p1<-sqrt(gamma(Y[2]+n)*(2*n+Y[1]+Y[2]-1)*gamma(Y[2])
             *gamma(Y[1])/(factorial(n)*gamma(Y[1]+Y[2]+n-1)
                           *gamma(Y[1]+Y[2])*gamma(Y[1]+n)))*s1
    l=-n*(1+(n-1)/(Y[1]+Y[2]))*0.01
    h=p2-exp(l)*p1
    g2=g2+h
  }
  
  return(c(F1=g1,F2=g2))
}

library(rootSolve)
load("simu.RData")
fs<-multiroot(estfc3,c(1,1))
ss<-multiroot(estfc3,c(2,2))

Y=c(3,5)
V=estfc3(Y)
sum(V)