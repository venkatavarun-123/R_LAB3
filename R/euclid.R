#'Euclidean algorithm to find greatest common divisor.
#'@param x a numeric scalar.
#'@param y another numeric scalar.
#'@description The algorithm replaces larger of the two numbers by the remainder when divided by smaller of two numbers and stops when remainder equals zero.
#'@return The greatest common divisor of two numbers passed in as arguments.
#'@source https://en.wikipedia.org/wiki/Euclidean_algorithm.
#'@export
euclidean<-function(x,y)
{
  if(is.numeric(x)==TRUE && is.numeric(y)==TRUE && length(x)==1L && length(y)==1L)
  {
    if(x==0)
      return(abs(y))
  return(euclidean(y%%x,x))
  }
  else
    stop()
}

