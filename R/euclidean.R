# LAB3
# Authors: Maria Treesa Sebastian(marse306), Brian Masinde, Omkar(omkbh878)
# Greatest commmon divisor
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

euclidean(123612, 13892347912)
#[1] 4
euclidean(100, 1000)
#[1] 100

