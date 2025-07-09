#'
#' MSNBurr-IIa distribution.
#' @importFrom Rmpfr log1mexp log1pexp
#' @export
#' @name msnburr2a
#' @param x,q vector of quantiles. 
#' @param mu a location parameter.
#' @param sigma a scale parameter.
#' @param alpha a shape parameter
#' @param p vectors of probabilities.
#' @param n number of observations.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p),
#' The default value of this parameter is FALSE. 
#' @param lower.tail logical;if TRUE (default), probabilities are
#' \eqn{P\left[ X\leq x\right]}, otherwise, \eqn{P\left[ X>x\right] }.
#' 
#' @description
#' To calculate density function, distribution function, quantile function, and  build data from random generator function 
#' for the MSNBurr distribution. 

#' @keywords distribution
#' @concept Univariate
#' @concept Continuous
#' 

#' @return \code{dmsnburr2a} gives the density, \code{pmsnburr2a} gives the distribution
#' function, \code{qmsnburr2a} gives the quantile function and \code{rmsnburr2a} generates
#' random numbers.
#'  
#' @author Achmad Syahrul Choir and Nur Iriawan
#' @details
#' MSNBurr-IIa Distribution 
#' 
#' The MSNBurr-IIa distribution with parameters \eqn{\mu}, \eqn{\sigma}, and \eqn{\alpha}
#' has density:
#' \deqn{f(x |\mu,\sigma,\alpha)=\frac{\omega}{\sigma}\exp{\left(\omega{\left(\frac{x-\mu}{\sigma}\right)}\right)}{{\left(1+\frac{\exp{\left(\omega{(\frac{x-\mu}{\sigma})}\right)}}{\alpha}\right)}^{-(\alpha+1)}}}
#' where \eqn{-\infty < x < \infty, -\infty < \mu< \infty, \sigma>0, \alpha>0, 
#' \omega = \frac{1}{\sqrt{2\pi}} {\left(1+\frac{1}{\alpha}\right)^{\alpha+1}}}
#

#' @references 
#' Choir, A. S. (2020). The New Neo-Normal Distributions and their Properties. Dissertation. Institut Teknologi Sepuluh Nopember.
#' 
#' @examples
#' library("neodistr")
#' dmsnburr2a(7, mu=0, sigma=1, alpha=0.1)
#' plot(function(x) dmsnburr2a(x, alpha=0.1), -3, 20,
#' main = "Right Skew MSNBurr-IIa Density ",ylab="density")
dmsnburr2a<-function(x,mu=0,sigma=1,alpha=1,log=FALSE){
 
  ifelse(is.na(alpha),stop(" alpha,must be not missing value"),NA)
  ifelse(is.na(mu),stop(" alpha,must be not missing value"),NA)
  ifelse(is.na(sigma),stop(" alpha,must be not missing value"),NA)
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha < 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  
  omega<-((1+1/alpha)^(alpha+1))/sqrt(2*pi)
  zo<--omega*((x-mu)/sigma)
  
    lp<-log(omega)-log(sigma)+(alpha+1)*log(alpha)+alpha*zo-((alpha+1)*log1p(alpha*exp(zo)))
    lp[is.infinite(zo)]<--Inf
  if (log){
    return(lp)
  }else{
    return(exp(lp))
  }
  

}

#' @export
#' @rdname msnburr2a
#' @examples
#' p=pmsnburr2a(4, mu=0, sigma=1, alpha=1)
#' p
pmsnburr2a<-function(q,mu=0,sigma=1,alpha=1,lower.tail=TRUE,log.p=FALSE){
  ifelse(is.na(alpha),stop(" alpha,must be not missing value"),NA)
  ifelse(is.na(mu),stop(" alpha,must be not missing value"),NA)
  ifelse(is.na(sigma),stop(" alpha,must be not missing value"),NA)
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha < 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  
  omega<-(1/sqrt(2*pi))*(1+(1/alpha))^(alpha+1)
  zoa<-omega*((q-mu)/sigma)-log(alpha)
  qs<-1-exp(-alpha*log1pexp(zoa))
  if (lower.tail) {
    if(log.p){
      return (log(qs))
    }else{
      return (qs)
    }
  }
  else {
    if(log.p){
      return (log(1 - qs))
    }else{
      return (1-qs)
    }
  }
  
}

#' @export
#' @rdname msnburr2a
#' @examples
#' q=qmsnburr2a(p, mu=0, sigma=1, alpha=1)
#' q
#' qmsnburr2a(0.5, mu=0, sigma=1, alpha=1)
qmsnburr2a<-function(p,mu=0,sigma=1,alpha=1,lower.tail=TRUE,log.p=FALSE){
  ifelse(is.na(alpha),stop(" alpha,must be not missing value"),NA)
  ifelse(is.na(mu),stop(" alpha,must be not missing value"),NA)
  ifelse(is.na(sigma),stop(" alpha,must be not missing value"),NA)
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha < 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  if (any(p <= 0) | any(p >= 1)) 
    stop(paste("p must be between 0 and 1", "\n", ""))
  if (lower.tail) {
    if (log.p){
      tp <- exp(p)
    }else{
      tp <- p
    }
  }
  else {
    if (log.p){
      tp <- 1-exp(p)
    }
    else{
      tp <- 1-p
    }
  }
  omega<-(1/sqrt(2*pi))*(1+(1/alpha))^(alpha+1)
  return (mu+(sigma/omega)*(log(alpha)+log(((1-tp)^(-1/alpha))-1)))
 
}


    
#' MSNBurr-IIa Distribution 
#' @export
#' @rdname msnburr2a
#' @examples
#' r=rmsnburr2a(10000, mu=0, sigma=1, alpha=0.1)
#' head(r)
#' hist(r, xlab = 'MSNBurr random number', ylab = 'Frequency', 
#' main = 'Distribution of MSNBurr-IIa Random Number ')
rmsnburr2a<-function(n,mu=0,sigma=1,alpha=1){
  runif<-NULL
  ifelse(is.na(alpha),stop(" alpha,must be not missing value"),NA)
  ifelse(is.na(mu),stop(" alpha,must be not missing value"),NA)
  ifelse(is.na(sigma),stop(" alpha,must be not missing value"),NA)
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha < 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  omega=(1/sqrt(2*pi))*(1+(1/alpha))^(alpha+1)
  return (mu+(sigma/omega)*(log(alpha)+log(((1-runif(n,0,1))^(-1/alpha))-1)))
 
  
}
