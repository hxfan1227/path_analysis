trendMK <- function(timeserial){
  n <- length(timeserial)
  s=0
  for (k in 1:(n-1)){
    for (j in (k+1):n){
      s=s+sign(timeserial[j]-timeserial[k])
    }
  }
  v = ( n * ( n - 1 ) * ( 2 * n + 5 ) ) / 18
  if (s==0){
    z=0
  }
  else{
    if (s>0){
      z = ( s - 1 ) / sqrt( v )
    }
    else z = ( s + 1 ) / sqrt( v )
  }
  return(z)
}