#' Function to get greatest common divisor
#' Authors: Maria Treesa Sebastian(marse306), Brian Masinde(brima748), Omkar(omkbh878)
#' @param x as numeric
#' @param y as numeric
#'
#' @return GCD of x and y .
#' @references <https://en.wikipedia.org/wiki/Euclidean_algorithm>
#' @export
#'
#' @examples euclidean(100,10000)
#'
euclidean <- function(x,y){
  stopifnot(is.numeric(x) || is.numeric(y))
  i<-1
    repeat{
      temp<-y
      y<- x%%y
      x<-temp
      if(y==0) break
    }
  return(x)
}



