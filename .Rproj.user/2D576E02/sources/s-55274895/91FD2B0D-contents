run_model = function(times=1:50, r = .4) {
  # Parameters
  max_time = times[length(times)]
  K = 1
  
  N = vector('numeric',length=max_time)
  N[1] = 0.1
  
  # Iterate model
  for (t in 1:max_time){
    N[t+1] = N[t] + r*N[t]*(1 - N[t]/K)
    if (N[t+1] < 0) {
      print(paste("model crashed for r=", r))
      return()
    }
  }
  return(N[times])
}

## Q2

plot(x=rep(1, 11), y=run_model(times=40:50), ylim=c(0,2), xlim=c(1, 3))
for (r in seq(1, 3, by=.01)) {
  points(x=rep(r, 11), y=run_model(times=40:50, r=r))
}
