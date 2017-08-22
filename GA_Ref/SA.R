library("GenSA")

# Try Rastrgin function (The objective function value for global minimum
# is 0 with all components of par are 0.)
Rastrigin <- function(x) {
  sum(x^2 - 10 * cos(2 * pi  * x)) + 10 * length(x)
}

set.seed(1234) # The user can use any seed.
dimension <- 30
global.min <- 0
tol <- 1e-13
lower <- rep(-5.12, dimension)
upper <- rep(5.12, dimension)
out <- GenSA(lower = lower, upper = upper, fn = Rastrigin,
             control=list(threshold.stop=global.min+tol,verbose=TRUE))
out[c("value","par","counts")]

# GenSA will stop after running for about 2 seconds
# Note: The time for solving this problem by GenSA may vary
# depending on the computer used. 
set.seed(1234) # The user can use any seed.
dimension <- 30
global.min <- 0
tol <- 1e-13
lower <- rep(-5.12, dimension)
upper <- rep(5.12, dimension)
out <- GenSA(lower = lower, upper = upper, fn = Rastrigin,
             control=list(max.time=2))
out[c("value","par","counts")]

x<-seq(-5.12,5.12,length=100)
y<-x
f<-function(x,y) { 20+(x^2-10*cos(2*3.14*x))+(y^2-10*cos(2*3.14*y)) }
z<-outer(x,y,f)
z[is.na(z)]<-1
persp(x,y,z,theta=30,phi=30,expand=0.5,col="red",ltheta=90,shade=0.50,ticktype="detailed",d=5,r=1)

