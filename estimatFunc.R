estimatfunc<-function(a) {
  G=0
  for (t in 1:999) {
    x1=X[t]
    x2=X[t+1]
    for (n in 0:2) {
      g=0
      s1=0
      s2=0
      for (m in 0:n) {
        t1=choose(n,m)*gamma(a+b+n+m+1)/gamma(a+m+1)*((x1-1)^m)
        s1=s1+t1
        t2=choose(n,m)*gamma(a+b+n+m+1)/gamma(a+m+1)*((x2-1)^m)
        s2=s2+t2
      }
      p2<-sqrt(gamma(a+n+1)*(2*n+a+b+1)*gamma(a+1)
              *gamma(b+1)/(factorial(n)*gamma(a+b+n+1)
                           *gamma(a+b+2)*gamma(b+n+1)))*s2
      p1<-sqrt(gamma(a+n+1)*(2*n+a+b+1)*gamma(a+1)
               *gamma(b+1)/(factorial(n)*gamma(a+b+n+1)
                            *gamma(a+b+2)*gamma(b+n+1)))*s1
      h=p2-exp(-c*n*(1+(n-1)/(a+b))*d)*p1
      g=g+h
    }
    G=G+g
  }
  return(G)
}