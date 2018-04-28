

# Assignment - 02

#1a
tmpFn1 <- function(xVec){
      a <- xVec^(1: length(xVec))
      return(a)

}
tmpFn1(1:5)

tmpFn2 <- function(xVec2){
      
      n = length(xVec2)
      
      b <-(xVec2^(1:n)/(1:n))
      
      return(b)
}
tmpFn2(1:5)
#1b.

tmpFn3 <- function(xVec3,n){
     d <- (1+sum(xVec3^(1:n)/(1:n)))
     return(d)
}


#2: tmpFn(xVec); x = (x1....xn); x1+x2+x3/3;

tmpFn <- function (xVec){
    e <- xVec[1:(length(xVec) -2)] # gives values from 1:length of xVec
    f <- xVec[2:(length(xVec) -1)] # gives values from 2:length of xvec except last one
    g <- xVec[3:(length(xVec))]    # Gives frp, 3 to end of length of a xVec
    h <- (e+g+h)/3              # Takes the each value eg: 1 from e, 2 from f, 3 from g and averages it. 
    return(h)
}    

tmpFn(c(1:5, 6:1))


#3. Continous function: f(x) = {x2 + 2x + 3 if x<0; x+3 if 0 <- x <2; x2+4x -7 if 2 <- x}

F <- function (x) {
   d <- rep(0,length(x))
   
   for (i in 1: length(x)){
       a <- x[i]
       
       if (a < 0){
         d[i] <- (a^2 + 2*a[i] + 3)
       }
        
       if ( a < 2 & a >= 0){
         d[i] <- (a +3)
       }
       if (a >= 2){
         d[i] <- (a^2 + 4*a - 7)
       }
   }
       e <- d
       return(e)
}       

x <- F(-3:3)
plot(x)


#4 Write a function which takes a single argument which is a matrix. The function should return a matrix
#which is the same as the function argument but every odd number is doubled.

F4 <- function(x){
  k <- nrow(x)
  x <- matrix(x)
  d <- rep(0,length(x))
     for (i in 1: length(x)){
      a <- x[i]
      if (a %% 2 == 1){ 
          d[i] <- (a*2)
      } else d[i] <- matrix(x[i])
   }
      d <- matrix(d, nrow = k)
      return(d)
}

x <- matrix(c(1, 5, -2, 1, 2,-1, 3, 6, -3), nrow = 3)

F4(x)
  

# 5. 
  
F5 <- function (n,k){
  a <- rep(0, n*5)
  b <- matrix(a, nrow = n)    # Creates a n by n matrix
  b[abs(col(b) - row(b)) == 1] <- 1   # subtracts the col and row values and replaces with 1 when the output is 1
    for (i in 1:n){
    for(j in 1:n){
      if(i == j){
        b[i,j] <- k # Whenever bot the values are same it is replaced by k (which is an input)
      }
    }
  }
  return(b)
}
F5(5,2)

# 6. 

