eqn <- function(a, b, r2){
  if(b >= 0){
    as.character(as.expression(
      substitute(italic(y) == a + b * italic(x) * "," ~~ italic(R)^2 == r2,
                 list(a  = a,
                      b  = b, 
                      r2 = r2))
    ))
  } else {
    as.character(as.expression(
      substitute(italic(y) == a - b * italic(x) * "," ~~ italic(R)^2 == r2,
                 list(a  = a,
                      b  = abs(b), 
                      r2 = r2))
    ))
  }
  
}