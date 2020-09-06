# if-else,  for,  while,  repeat
#g0=PN(f0)
#dist = 'BN' , 'B' , 'P'
#par = c(r,p) en caso de 'BN'
#par = c(n,p) en caso de 'B'
#par = lambda en caso de 'P'
#P(Y=k) = f(k) 
#f(k)= función de proba 
###########################
#Ejemplo de f(x) para el
#ejemplo 4.8 del libro de Dickson
######
f <- function(x){(0.6*0.4^(x-1))*(x>0)}
######
dist <- 'BN'
r <- 5
p <- 0.2
# f0 <- 0.1
if(dist == 'BN'){
  a <- 1-p
  b <- (r-1)*(1-p)   
# g0 <- (p/(1-(1-p)*f0))^r
  g0 <- (p/(1-(1-p)*f(0)))^r
}
dist <- 'B'
n <- 4
p <- 0.4
f0 <- 0.3
if(dist == 'B'){
  a <- -p/(1-p)
  b <- (n+1)*p/(1-p)   
# g0 <- (1-p+p*f0)^n
  g0 <- (1-p+p*f(0))^n
}
dist <- 'P'
lambda <- 2
f0 <- 0
if(dist == 'P'){
  a <- 0
  b <- lambda    
# g0 <- exp(-lambda*(1-f0))
  g0 <- exp(-lambda*(1-f(0)))
}
#########################
#########################
C <- 1/(1-a*f(0))
g <- numeric(r+1)
g[1] <- g0
 for(i in 1:r){
                        aux <- 0
                        for(k in 0:(i-1)){
                                aux <- aux + C*(a+((i-k)/i)*b)*f(i-k)*g[k+1]
                        }
                        g[i+1] <- aux
                }
                
