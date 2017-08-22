LagrangePolynomial <- function(x,y) {
  len <- length(x)
  if(len != length(y))
    stop("Length not equal!")
  
  if(len < 2)
    stop("Dim size must be more than 1!")
  
  if(any(table(x)>1))
    stop("Number cannot be same!")
  
  #Initialize each term
  xx <- paste("(","a -",x,")")
  m <- c(rep(0,len))
  #Generate each term
  for(i in 1:len) {
    numerator <- "1"
    for(j in 1:len) {
      if(i != j) {
        numerator <- paste(numerator,"*",xx[j])
      }
    }
    denominator <- prod((x[i]-x)[-i])
    numerator <- paste(numerator,"/",denominator)
    m[i]<-numerator 
  }
  
  #Combine the exrpession
  m <- paste(m,"*",y)
  r <- paste(m,collapse="+")
  
  #Construct the function
  fbody <- paste("{ return(",r,")}")
  f <- function(a) {}
  
  #Fill the function's body
  body(f) <- parse(text=fbody)
  
  return(f)
}

a=c(1,2,3,4)
b=c(10,20,30,40) 
f <- LagrangePolynomial(a,b) 
f(1.5) 
