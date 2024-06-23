#' GMSNBurr distribution
#' @export
#' @importFrom stats rchisq runif
#' @name gmsnburr
#' @param x,q vector of quantiles. 
#' @param p vectors of probabilities.
#' @param n number of observations.
#' @param mu a  location parameter. 
#' @param sigma a scale parameter.
#' @param alpha a shape parameter.
#' @param beta a shape parameter.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p)
#' The default value of this parameter is FALSE. 
#' @param lower.tail logical;if TRUE (default), probabilities are
#' \eqn{P\left[ X\leq x\right]}, otherwise, \eqn{P\left[ X>x\right] }.
#' @author Achmad Syahrul Choir
#' @description
#' To calculate density function, distribution funcion, quantile function, and  build data from random generator function 
#' for the GMSNBurr Distribution. 
#' 
#' @keywords distribution
#' @concept Univariate
#' @concept Continuous
#' 
#' 
#' @return 
#' \code{dgmsnburr} gives the density , \code{pgmasnburr} gives the distribution function,
#' \code{qgmsnburr} gives quantiles function, \code{rgmsnburr} generates random numbers.


#' @details
#' GMSNBurr Distribution 
#' 
#' The GMSNBurr distribution with parameters \eqn{\mu}, \eqn{\sigma},\eqn{\alpha}, and \eqn{\beta}
#' has density:
#' \deqn{f(x |\mu,\sigma,\alpha,\beta) =  {\frac{\omega}{{B(\alpha,\beta)}\sigma}}{{\left(\frac{\beta}{\alpha}\right)}^\beta} {{\exp{\left(-\beta \omega {\left(\frac{x-\mu}{\sigma}\right)}\right)} {{\left(1+{\frac{\beta}{\alpha}} {\exp{\left(-\omega {\left(\frac{x-\mu}{\sigma}\right)}\right)}}\right)}^{-(\alpha+\beta)}}}}}
#' where \eqn{-\infty<x<\infty,  -\infty<\mu<\infty,  \sigma>0, \alpha>0, \beta>0} 
#' and  \eqn{\omega = {\frac{B(\alpha,\beta)}{\sqrt{2\pi}}}{{\left(1+{\frac{\beta}{\alpha}}\right)}^{\alpha+\beta}}{\left(\frac{\beta}{\alpha}\right)}^{-\beta}}
#'
#' @references
#' Choir, A. S. (2020). The New Neo-Normal Distributions and their Properties. Disertation. Institut Teknologi Sepuluh Nopember.
#'
#' Iriawan, N. (2000). Computationally Intensive Approaches to Inference in Neo-Normal Linear Models. Curtin University of Technology.
#' 
#' @examples
#' library("neodistr")
#' dgmsnburr(0, mu=0, sigma=1, alpha=1,beta=1)
dgmsnburr<-function(x,mu=0,sigma=1,alpha=1,beta=1, log=FALSE){
  ifelse(is.na(alpha),stop(" alpha must be not missing value"),NA)
  ifelse(is.na(beta),stop(" beta must be not missing value"),NA)
  ifelse(is.na(mu),stop(" mu must be not missing value"),NA)
  ifelse(is.na(sigma),stop(" sigma must be not missing value"),NA)
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha < 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  if (any(beta < 0)) 
    stop(paste("beta must be positive", "\n", ""))

  lomega=-0.5*log(2*pi)+lbeta(alpha,beta)-beta*(log(beta)-log(alpha))+(alpha+beta)*log1p(beta/alpha)
  omega=exp(lomega)
  zo<--omega*((x-mu)/sigma)
  zoa<-zo+log(beta)-log(alpha)

  lp<- lomega-log(sigma)+beta*(log(beta)-log(alpha))+beta*zo-((alpha+beta)*log1pexp(zoa))-lbeta(alpha,beta)
  lp[(is.infinite(zo))]<--Inf
  if(log){
    return(lp)
  }else{
    return(exp(lp))
  }
  
}

#' @export
#' @rdname gmsnburr
#' @examples
#' pgmsnburr(4, mu=0, sigma=1, alpha=1, beta=1)
pgmsnburr<-function(q,mu=0,sigma=1,alpha=1, beta=1,lower.tail=TRUE,log.p=FALSE){
  pbeta<-NULL
  ifelse(is.na(alpha),stop(" alpha must be not missing value"),NA)
  ifelse(is.na(beta),stop(" beta must be not missing value"),NA)
  ifelse(is.na(mu),stop(" mu must be not missing value"),NA)
  ifelse(is.na(sigma),stop(" sigma must be not missing value"),NA)
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha < 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  if (any(beta < 0)) 
    stop(paste("beta must be positive", "\n", ""))
  
  lomega=-0.5*log(2*pi)+lbeta(alpha,beta)-beta*(log(beta)-log(alpha))+(alpha+beta)*log1p(beta/alpha)
  omega=exp(lomega)
  epart=exp(-omega*((q-mu)/sigma))
  ep=1/(1+(beta/alpha)*epart)
  return(pbeta(ep,alpha,beta,lower.tail=lower.tail,log.p=log.p))
  
}

#' @export
#' @rdname gmsnburr
#' @examples
#' qgmsnburr(0.4, mu=0, sigma=1, alpha=1, beta=1)
qgmsnburr<-function(p,mu=0,sigma=1,alpha=1,beta=1,lower.tail=TRUE, log.p=FALSE){
  qf<-NULL
  ifelse(is.na(alpha),stop(" alpha must be not missing value"),NA)
  ifelse(is.na(beta),stop(" beta must be not missing value"),NA)
  ifelse(is.na(mu),stop(" mu must be not missing value"),NA)
  ifelse(is.na(sigma),stop(" sigma must be not missing value"),NA)
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha < 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  if (any(beta < 0)) 
    stop(paste("beta must be positive", "\n", ""))
  if (any(p <= 0) | any(p >= 1)) 
    stop(paste("p must be between 0 and 1", "\n", ""))
# if (lower.tail==TRUE) {
#   if (log.p==TRUE){
#      tp <- exp(p)
#    }else{
#      tp <- p
#    }
#  } else {
#    if (log.p){
#      tp <- 1-exp(p)
#    }
#    else{
#      tp <- 1-p
#     }
#   }
  lomega=-0.5*log(2*pi)+lbeta(alpha,beta)-beta*(log(beta)-log(alpha))+(alpha+beta)*log1p(beta/alpha)
  omega=exp(lomega)
  # ib<-qbeta(tp,alpha, beta );
  # s <- (1/ib)-1
  #  return (mu-(sigma/omega)*(beta/alpha)*log(s))
  zf <-qf(p,2*beta,2*alpha,lower.tail=!lower.tail,log.p=log.p)
  return (mu-(sigma/omega)*log(zf))
}

#' @export
#' @rdname gmsnburr
#' @examples
#' r=rgmsnburr(10000, mu=0, sigma=1, alpha=1, beta=1)
#' head(r)
#' hist(r, xlab = 'GMSNBurr random number', ylab = 'Frequency', 
#' main = 'Distribution of GMSNBurr Random Number ')
rgmsnburr<-function(n, mu=0, sigma=1, alpha=1, beta=1){
  rf<-NULL
  ifelse(is.na(alpha),stop(" alpha must be not missing value"),NA)
  ifelse(is.na(beta),stop(" beta must be not missing value"),NA)
  ifelse(is.na(mu),stop(" mu must be not missing value"),NA)
  ifelse(is.na(sigma),stop(" sigma must be not missing value"),NA)
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha < 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  if (any(beta < 0)) 
    stop(paste("beta must be positive", "\n", ""))
  lomega=-0.5*log(2*pi)+lbeta(alpha,beta)-beta*(log(beta)-log(alpha))+(alpha+beta)*log1p(beta/alpha)
  omega=exp(lomega)
  zf <-rf(n,2*beta,2*alpha)
  #z1=rchisq(n,2*alpha)/(2*alpha);
  #z2=rchisq(n,2*beta)/(2*beta);
  #logzf=log(z2)-log(z1);
  #return(mu-(sigma/omega)*logzf)
  return (mu-(sigma/omega)*log(zf))
  
  
}

rgmsnburr_old<-function(n,mu=0,sigma=1,alpha=1,beta=1){
  rbeta<-NULL
  ifelse(is.na(alpha),stop(" alpha must be not missing value"),NA)
  ifelse(is.na(beta),stop(" beta must be not missing value"),NA)
  ifelse(is.na(mu),stop(" mu must be not missing value"),NA)
  ifelse(is.na(sigma),stop(" sigma must be not missing value"),NA)
  
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha < 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  if (any(beta < 0)) 
    stop(paste("beta must be positive", "\n", ""))
  lomega<--0.5*log(2*pi)+lbeta(alpha,beta)-beta*(log(beta)-log(alpha))+(alpha+beta)*log1p(beta/alpha)
  omega<-exp(lomega)
  rb<-rbeta(n,beta,alpha)
  z<-(alpha*rb)/(beta*(1-rb))
  return (mu-(sigma/omega)*log(z))
  
  
}
