#NAMA : DIMAS SETYAWAN RAMADHANSYAH
#NIM  : 17523152

#UAS

#NO 1
(C) y = -91.6 + 1.7*x

#NO 2
(A) 54.1

#NO 3
(C) 1 - 0.07916667*x + 2.19375*x^2 - 0.9958333*x^3 + 0.13125*x^4

xk=c(2,2.75,4) 
fxk=c(0.5,0.36,0.25) 
px(3,xk,fxk) 

library(polynom) 

a <- c(0,1,2,3,4) 
b <- c(1,2.25,3.75,4.25,5.65) 
poly.calc(a,b) 

f1 <- function(ik){ 
  return (1 - 0.07916667*ik + 2.19375*ik^2 - 0.9958333*ik^3 + 0.13125*ik^4) 
}

#NO 4
(B) 4.168677441875

xk=c(2,2.75,4) 
fxk=c(0.5,0.36,0.25) 
px(3,xk,fxk) 

library(polynom) 

a <- c(0,1,2,3,4) 
b <- c(1,2.25,3.75,4.25,5.65) 
poly.calc(a,b) 

f1 <- function(ik){ 
  return (1 - 0.07916667*ik + 2.19375*ik^2 - 0.9958333*ik^3 + 0.13125*ik^4) 
} 

f1(2.75)

#NO 5
(C) plot(xi,yi) 
curve(f1,add=TRUE)

yi <- c(1,2,3,4,5) 
xi <- c(0,1,2,3,4) 
plot(xi,yi) 
curve(f1,add=TRUE)

#NO 6
(C) 7

#NO 7
(D) Tidak ada

#NO 8
(B) Ketika nilai b=p

#NO 9
(B) Relative error

#NO 10
(D) 14-13

#NO 11
(B) -5.666667

library(pracma) 
fun <- function(x){ 
  return (x^2-6) 
} 
a1=0 
a2=1 
trapzfun(fun,a1,a2)

#NO 12
(D) 3.083333

library(pracma) 
fun <- function(x){ 
  return (x^3+4*x^2-10) 
} 
a1=1
a2=2 
trapzfun(fun,a1,a2)

#NO 13
(A) L <- h/2*(f0 + 2 * sum(fi)-1 + fn)

#No 14
(B) 0.335

h <- 0.1 
x <- seq(0,1, by=h) 
f <- function(x){ 
  return (x^2) 
} 

f0 <- f(x[1]) 
fi <- sapply(x[2:10],f) 
fn <- f(x[length(x)]) 

trap <- function(f0,fi,fn,h){ 
  L <- h * (f0 + 2 * sum(fi) + fn)/2 
  return (L) 
} 
trap(f0,fi,fn,h)

#NO 15
(A) fi <-sapply(x[2:5])

h <- 0.2 
x <- seq(0,1, by=h) 
f <- function(x){ 
  return (x^2) 
} 

f0 <- f(x[1]) 
fi <- sapply(x[2:5],f) 
fn <- f(x[length(x)]) 

trap <- function(f0,fi,fn,h){ 
  L <- h * (f0 + 2 * sum(fi) + fn)/2 
  return (L) 
} 
trap(f0,fi,fn,h)