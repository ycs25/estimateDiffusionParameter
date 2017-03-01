l=1;u=5;z=l
while (estimatfunc(z)>0.1) {
  if (estimatfunc(u)>estimatfunc(l)) {
    u=(l+u)/2
  } else {
    l=(l+u)/2
  }
  z=(l+u)/2
}
z