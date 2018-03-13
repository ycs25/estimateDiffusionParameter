library(Sim.DiffProc)
xt<-snssde1d(drift=expression((3/8-x)),diffusion=expression(sqrt(2/8*x*(1-x))),M=1,x0=3/8,t0=0,T=100,N=10000)
X=xt$X

est<-function(Y) {
  
  g1=0;g2=0;g3=0;Z=X-1;
  for (t in 1:10000) {
    x1=Z[t]
    x2=Z[t+1]
    
    h1=(Y[2]*x2-Y[1])-exp(-Y[3]*0.01)*(Y[2]*x1-Y[1])
    
    g1=g1+h1
    
    h2=(Y[2]+1)*(Y[2]+2)*x2^2-2*(Y[2]+1)*(Y[1]+1)*x2+(Y[1])*(Y[1]+1)-
      exp(-Y[3]*2*0.01*(1+1/Y[2]))*
      (Y[2]+1)*(Y[2]+2)*x1^2-2*(Y[2]+1)*(Y[1]+1)*x1+(Y[1])*(Y[1]+1)
    
    g2=g2+h2
    
    h3=(Y[2]+2)*(Y[2]+3)*(Y[2]+4)*x2^3-3*(Y[2]+2)*(Y[1]+2)*x2^2+3*(Y[2]+2)*(Y[1]+1)*(Y[1]+2)*x2-(Y[1])*(Y[1]+1)*(Y[1]+2)-
      exp(-Y[3]*3*0.01*(1+1/Y[2]))*
      (Y[2]+2)*(Y[2]+3)*(Y[2]+4)*x1^3-3*(Y[2]+2)*(Y[1]+2)*x1^2+3*(Y[2]+2)*(Y[1]+1)*(Y[1]+2)*x1-(Y[1])*(Y[1]+1)*(Y[1]+2)
    
    g3=g3+h3
  }
  
  
  return(c(F1=g1,F2=g2,F3=g3))
}

library(rootSolve)
fs<-multiroot(est,c(1,1,1))
ss<-multiroot(est,c(3,8,1))

Y=c(3,8,1)
V=est(Y)