# These functions is adapted from ST5 in gamlss.dist that was written by Bob Rigby and Mikis Stasinopoulos
#' Jones Faddy's Skew-t Distribution
#' @export 
#' @name jfst
#' @importFrom stats pbeta qbeta
#' @param x,q vector of quantiles. 
#' @param p vectors of probabilities.
#' @param n number of observations.
#' @param mu a location parameter.
#' @param sigma a scale parameter.
#' @param alpha a shape parameter (skewness).
#' @param beta a shape parameter (kurtosis).
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p)
#' The default value of this parameter is FALSE 
#' @param lower.tail logical;if TRUE (default), probabilities are
#' \eqn{P\left[ X\leq x\right]}, otherwise, \eqn{P\left[ X>x\right] }.
#' 
#' @description
#' To calculate density function, distribution function, quantile function, and  build data from random generator function 
#' for the Jones-Faddy's Skew-t Distribution. 
#' 
#' @return 
#' \code{djfst} gives the density , \code{pjfst} gives the distribution function,
#' \code{qjfst} gives quantiles function, \code{rjfst} generates random numbers.
#' @author Anisa' Faoziah
#' @keywords distribution
#' @concept Univariate
#' @concept Continuous
#' 
#' 

#' @details
#' Jones-Faddy's Skew-t Distribution 
#' 
#' 
#' 
#' The Jones-Faddy's Skew-t distribution with parameters \eqn{\mu}, \eqn{\sigma},\eqn{\alpha}, and \eqn{\beta}
#' has density:
#' \deqn{f(x |\mu,\sigma,\beta,\alpha)= \frac{c}{\sigma} {\left[{1+\frac{z}{\sqrt{\alpha+\beta+z^2}}}\right]}^{\alpha+\frac{1}{2}}
#' {\left[{1-\frac{z}{\sqrt{\alpha+\beta+z^2}}}\right]}^{\beta+\frac{1}{2}}}
#' where \eqn{-\infty<x<\infty,  -\infty<\mu<\infty,  \sigma>0, \alpha>0, \beta>0,} 
#' \eqn{z =\frac{x-\mu}{\sigma} }, \eqn{ c = {\left[2^{\left(\alpha+\beta-1\right)} {\left(\alpha+\beta\right)^{\frac{1}{2}}} B(a,b)\right]}^{-1} },
#'  
#' 
#' @references 
#' Jones, M.C. and Faddy, M. J. (2003) A skew extension of the t distribution,
#'   with applications. Journal of the Royal Statistical Society, 
#'   Series B, 65, pp 159-174
#'   
#'  Rigby, R.A. and Stasinopoulos, M.D. and Heller, G.Z. and De Bastiani, F. 
#'   (2019) Distributions for Modeling Location, Scale, 
#'   and Shape: Using GAMLSS in R.CRC Press

#' @rdname jfst
#' @examples
#' djfst(4, mu=0, sigma=1, alpha=2, beta=2)
djfst<- function(x,mu=0, sigma=1,alpha=2, beta=2, log=FALSE){
  ifelse(is.na(alpha),stop(" alpha must be not missing value"),NA)
  ifelse(is.na(beta),stop(" beta must be not missing value"),NA)
  ifelse(is.na(mu),stop(" mu must be not missing value"),NA)
  ifelse(is.na(sigma),stop(" sigma must be not missing value"),NA)
  if (any(sigma <= 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha <= 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  
    if (any(beta <= 0)) 
    stop(paste("beta must be positive", "\n", ""))
  
  
#    nu  <-2/beta
#    lam <- (2*alpha)/(beta*sqrt(2*beta+(alpha*alpha))) 
#    a <- (nu + lam)/2
#    b <- (nu - lam)/2
a<-alpha
b<-beta
      z <- (x-mu)/sigma
    rz <-  z / sqrt(a + b + (z*z))
    rz[is.infinite(z)]<-ifelse(z[is.infinite(z)]<0,-1,1)
    p <- (a + 0.5) * (log(1+rz)) + (b + 0.5) * (log(1-rz)) - ((a + b - 1) * log(2)) -  (0.5 * log (a + b)) - lbeta(a,b) - log(sigma)
    #p <- dST5(x,mu=mu,sigma=sigma, nu=alpha,tau=beta, log=log)
    if(log){
      p <- p
    }else{
      p <- exp(p)
    }
    return (p)
}

#' @export
#' @rdname jfst
#' @examples
#' pjfst(4, mu=0, sigma=1, alpha=2, beta=2)
pjfst <- function(q,mu=0, sigma=1,alpha=2, beta=2,lower.tail=TRUE, log.p=FALSE){
  
  ifelse(is.na(alpha),stop(" alpha must be not missing value"),NA)
  ifelse(is.na(beta),stop(" beta must be not missing value"),NA)
  ifelse(is.na(mu),stop(" mu must be not missing value"),NA)
  ifelse(is.na(sigma),stop(" sigma must be not missing value"),NA)
  if (any(sigma <= 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha <= 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  
  if (any(beta <= 0)) 
    stop(paste("beta must be positive", "\n", ""))
  
  
  #    nu  <-2/beta
  #    lam <- (2*alpha)/(beta*sqrt(2*beta+(alpha*alpha))) 
  #    a <- (nu + lam)/2
  #    b <- (nu - lam)/2
  a<-alpha
  b<-beta
    z <- (q-mu)/sigma
    rz <-  z / sqrt(a + b + (z*z))
    rz[is.infinite(z)] <- ifelse(z[is.infinite(z)] < 0,-1,1)
    r <- 0.5 * (1 + rz)
    p <- pbeta(r,a,b)
    if (lower.tail) {
      p <- p
    }else{
      p <- 1 - p
    }
    
    if (log.p) { 
      p <- log(p)
    }else{
      p <- p
    }
    return(p)
  
  
}

#' @export
#' @rdname jfst
#' @examples
#' qjfst(0.4, mu=0, sigma=1, alpha=2, beta=2)
qjfst<-function(p,mu=0,sigma=1, alpha = 2, beta=2,lower.tail=TRUE,log.p=FALSE){
  ifelse(is.na(alpha),stop(" alpha must be not missing value"),NA)
  ifelse(is.na(beta),stop(" beta must be not missing value"),NA)
  ifelse(is.na(mu),stop(" mu must be not missing value"),NA)
  ifelse(is.na(sigma),stop(" sigma must be not missing value"),NA)
  if (any(sigma <= 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha <= 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  if (any(beta <= 0)) 
    stop(paste("beta must be positive", "\n", ""))
  if (any(p <= 0) | any(p >= 1)) 
    stop(paste("p must be between 0 and 1", "\n", "")) 
 #   nu  = 2/beta
#    lam = (2*alpha)/(beta*sqrt(2*beta+(alpha*alpha))) 
#    a = (nu + lam)/2
#    b = (nu - lam)/2
 a<-alpha
 b<-beta
     balpha = qbeta(p,a,b)
    zalpha =(sqrt(a+b))*(2*balpha-1)/(2*sqrt(balpha*(1-balpha)))
    p = mu + sigma * zalpha
    if(lower.tail){
      p <- p
    }else{
      p <- 1-p
    }
    
    if(log.p){
      p <- exp(p)
    }else{
      p <- p
    }
    return (p)
  
}


#' @export
#' @rdname jfst
#' @examples
#' r=rjfst(10000, mu=0, sigma=1, alpha=2, beta=2)
#' head(r)
#' hist(r, xlab = 'jfst random number', ylab = 'Frequency', 
#' main = 'Distribution of jfst Random Number ')
rjfst <- function(n,mu=0, sigma=1, alpha=2, beta=2){
#.<-rbeta<-NULL 
  ifelse(is.na(alpha),stop(" alpha must be not missing value"),NA)
  ifelse(is.na(beta),stop(" beta must be not missing value"),NA)
  ifelse(is.na(mu),stop(" mu must be not missing value"),NA)
  ifelse(is.na(sigma),stop(" sigma must be not missing value"),NA)
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(beta < 0)) 
    stop(paste("beta must be positive", "\n", ""))  
    n <- ceiling(n)
  #  u<-rchisq(n,2*alpha)
  #  v<-rchisq(n,2*beta)
  #  t<-sqrt(alpha+beta)*(u-v)/(2*sqrt(u*v))
  #  rb<-rbeta(n,alpha,beta)
   # t<-sqrt(alpha+beta)*(2*rb-1)/(2*sqrt(rb*(1-rb)))
   # r = mu + sigma * t
   r <- qjfst(runif(n), mu=mu, sigma=sigma, alpha=alpha, beta=beta)
    #x<-rST5(n, mu=mu, sigma=sigma, nu=alpha, tau=beta)
    return(r)
  
}
