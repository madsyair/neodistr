#' MSNBurr Distribution
#' @name msnburr
#' @export
#' @importFrom Rmpfr log1mexp log1pexp 
#' @param x,q vector of quantiles. 
#' @param p vectors of probabilities.
#' @param n number of observations.
#' @param mu a location parameter.
#' @param sigma a scale parameter.
#' @param alpha a shape parameter.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p)
#' The default value of this parameter is FALSE. 
#' @param lower.tail logical;if TRUE (default), probabilities are
#' \eqn{P\left[ X\leq x\right]}, otherwise, \eqn{P\left[ X>x\right] }.
#' 
#' @description
#' To calculate density function, distribution funcion, quantile function, and  build data from random generator function 
#' for the MSNBurr Distribution. 
#' @keywords distribution
#' @concept Univariate
#' @concept Continuous
#' 
#' @return 
#' \code{dmsnburr} gives the density , \code{pmsnburr} gives the distribution function,
#' \code{qmsnburr} gives quantiles function, \code{rmsnburr} generates random  numbers.
#'
#' @author Achmad Syahrul Choir and Nur Iriawan
#' @details
#' MSNBurr Distribution 
#' 
#' The MSNBurr distribution with parameters \eqn{\mu}, \eqn{\sigma},and \eqn{\alpha}
#' has density:
#' \deqn{f(x |\mu,\sigma,\alpha)=\frac{\omega}{\sigma}\exp{\left(\omega{\left(\frac{x-\mu}{\sigma}\right)}\right)}{{\left(1+\frac{\exp{\left(\omega{(\frac{x-\mu}{\sigma})}\right)}}{\alpha}\right)}^{-(\alpha+1)}}}
#' where \eqn{-\infty < x < \infty, -\infty < \mu< \infty, \sigma>0, \alpha>0, 
#' \omega = \frac{1}{\sqrt{2\pi}} {\left(1+\frac{1}{\alpha}\right)^{\alpha+1}}}
#'
#' @references 
#' Iriawan, N. (2000). Computationally Intensive Approaches to Inference in Neo-Normal Linear Models. Curtin University of Technology.
#' 
#' Choir, A. S. (2020). The New Neo-Normal Distributions and their Properties. Disertation. Institut Teknologi Sepuluh Nopember.
#' 

#' @examples
#' library("neodistr")
#' dmsnburr(0, mu=0, sigma=1, alpha=0.1)
#' plot(function(x) dmsnburr(x, alpha=0.1), -20, 3,
#' main = "Left Skew MSNBurr Density ",ylab="density")



dmsnburr<-function(x,mu=0,sigma=1,alpha=1,log=FALSE){
  ifelse(is.na(alpha),stop(" alpha must be not missing value"),NA)
  ifelse(is.na(mu),stop(" mu must be not missing value"),NA)
  ifelse(is.na(sigma),stop("sigma  must be not missing value"),NA)
  
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha < 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  
  omega<-((1+1/alpha)^(alpha+1))/sqrt(2*pi)
  zo<--omega*((x-mu)/sigma)
  zoa<-zo-log(alpha)
  
  lp<-log(omega)-log(sigma)+zo-(alpha+1)*log1pexp(zoa)
  lp[(is.infinite(zo))]<--Inf
  
  if(log){
    return(lp)
  }else{
    return(exp(lp))
  }
  
  
  
}

#' @export
#' @rdname msnburr
#' @examples
#' pmsnburr(7, mu=0, sigma=1, alpha=1)
pmsnburr<-function(q,mu=0,sigma=1,alpha=1,lower.tail=TRUE,log.p=FALSE){
  ifelse(is.na(alpha),stop(" alpha must be not missing value"),NA)
  ifelse(is.na(mu),stop(" mu must be not missing value"),NA)
  ifelse(is.na(sigma),stop("sigma  must be not missing value"),NA)
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha < 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  
  omega<-((1+1/alpha)^(alpha+1))/sqrt(2*pi)
  zoa<--omega*((q-mu)/sigma)-log(alpha)
  qs<--alpha*log1pexp(zoa)
  if (lower.tail==TRUE) {
    if(log.p==TRUE){
      return(qs)
    }else{
      return(exp(qs))
    }
  }  else {
    if(log.p==TRUE){
      return(log1mexp(qs))
    }else{
      return(exp(log1mexp(qs)))
    }
  }
  
  
}

#' @export
#' @rdname msnburr
#' @examples
#' qmsnburr(0.6, mu=0, sigma=1, alpha=1)
qmsnburr<-function(p,mu=0,sigma=1,alpha=1,lower.tail=TRUE,log.p=FALSE)
{
  ifelse(is.na(alpha),stop(" alpha must be not missing value"),NA)
  ifelse(is.na(mu),stop(" mu must be not missing value"),NA)
  ifelse(is.na(sigma),stop("sigma  must be not missing value"),NA)
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha < 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  if (any(p <= 0) | any(p >= 1)) 
    stop(paste("p must be between 0 and 1", "\n", ""))  
  if (lower.tail==TRUE) {
    if (log.p==TRUE){
      tp <- exp(p)
    }else{
      tp <- p
    }
  } else {
    if (log.p){
      tp <- 1-exp(p)
    }
    else{
      tp <- 1-p
    }
  }
  omega<-((1+1/alpha)^(alpha+1))/sqrt(2*pi)
  return (mu-(sigma/omega*(log(alpha)+log((tp^(-1/alpha))-1))))
  
}

#' MSNBurr Distribution
#' @export
#' @rdname msnburr
#' @examples
#'r<- rmsnburr(10000, mu=0, sigma=1, alpha=1)
#'head(r)
#'hist(r, xlab = 'MSNBurr random number', ylab = 'Frequency', 
#'main = 'Distribution of MSNBurr Random Number ')
rmsnburr<-function(n,mu=0,sigma=1,alpha=1){
  runif<-NULL
  ifelse(is.na(alpha),stop(" alpha must be not missing value"),NA)
  ifelse(is.na(mu),stop(" mu must be not missing value"),NA)
  ifelse(is.na(sigma),stop("sigma  must be not missing value"),NA)
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha < 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  omega<-((1+1/alpha)^(alpha+1))/sqrt(2*pi)
  return (mu-(sigma/omega)*(log(alpha)+log((runif(n,0,1)^(-1/alpha))-1)))
  
}