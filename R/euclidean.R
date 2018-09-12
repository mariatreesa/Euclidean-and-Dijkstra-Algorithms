# LAB3
# Authors: Maria Treesa Sebastian(marse306), Brian Masinde, Omkar
# Greatest commmon divisor
euclidean <- function(a,b){
  while(b != 0){
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}
