sen_slope <- function(timeserial, dt = 1, display = F, nor = 1.96){
  n <- length(timeserial)
  ndash <- n * ( n - 1 ) / 2
  s <- matrix(0, nrow = ndash, ncol = 1)
  i <- 1
  for (k in 1:(n-1)){
    for (j in (k+1):n){
      s[i] <- (timeserial[j] - timeserial[k]) / (j - k) / dt
      i <- i + 1
    }
  }
  sl <- median(s)
  v <- ( n * ( n - 1 ) * ( 2 * n + 5 ) ) / 18
  m1 <- trunc( ( ndash - nor * sqrt( v ) ) / 2 )
  m2 <- trunc( ( ndash + nor * sqrt( v ) ) / 2 )
  s <- sort(s)
  lcl <- s[m1]
  ucl <- s[m2+1]
  if (display){
    message("Slope Estimate = ", sl,"\n", 
            "Lower Confidence Limit = ", lcl, "\n" ,
            "Upper Confidence Limit = ", ucl)
  }
  else return (data.frame(slope =sl,
                          Ucl = ucl,
                          Lcl = lcl))
}
