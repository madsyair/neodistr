# These functions is adapted from SE4 in gamlss.dist that was written by Bob Rigby and Mikis Stasinopoulos
#' Jones Skew Exponential Power

#' @name jsep
#' @importFrom stats pbeta qbeta
#' @param x,q vector of quantiles. 
#' @param p vectors of probabilities.
#' @param n number of observations.
#' @param mu a location parameter.
#' @param sigma a scale parameter.
#' @param alpha a shape parameter (left tail heaviness parameter).
#' @param beta a shape parameter (right tail heaviness parameter).
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p)
#' The default value of this parameter is FALSE 
#' @param lower.tail logical;if TRUE (default), probabilities are
#' \eqn{P\left[ X\leq x\right]}, otherwise, \eqn{P\left[ X>x\right] }.
#' 
#' @description
#' To calculate density function, distribution function, quantile function, and  build data from random generator function 
#' for the Jones Skew Exponential Power
#' 
#' @return 
#' \code{djsep} gives the density , \code{pjsep} gives the distribution function,
#' \code{qjsep} gives quantiles function, \code{rjsep} generates random numbers.
#' @author Meischa Zahra Nur Adhelia
#' @keywords distribution
#' @concept Univariate
#' @concept Continuous
#' 
#' 

#' @details
#' Jones Skew Exponential Power
#' 
#' The Jones Skew Exponential Power with parameters \eqn{\mu}, \eqn{\sigma},\eqn{\alpha}, and \eqn{\beta}
#' has density:
#' \deqn{
#'     f(y | \mu, \sigma, \alpha, \beta) = \left\{
#'       \begin{array}{ll}
#'         \frac{c}{\sigma} \exp\left(-|z|^{\alpha}\right), & \text{if } y < \mu \\
#'         \frac{c}{\sigma} \exp\left(-|z|^{\beta}\right), & \text{if } y \geq \mu
#'       \end{array}
#'     \right.
#'   }
#'   where:
#'   \deqn{z = \frac{y - \mu}{\sigma},}
#'   \deqn{c = \left[ \Gamma(1 + \beta^{-1}) + \Gamma(1 + \alpha^{-1}) \right]^{-1}.}  
#' 
#' @references 
#'  Rigby, R.A. and Stasinopoulos, M.D. and Heller, G.Z. and De Bastiani, F. 
#'   (2019) Distributions for Modeling Location, Scale, 
#'   and Shape: Using GAMLSS in R.CRC Press

#' @rdname jsep
#' @examples
#' djsep(4, mu=0, sigma=1, alpha=2, beta=2)
#' @export 
djsep <- function(x, mu=0, sigma=1, alpha=2, beta=2, log=FALSE)
{
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
  
  a<-alpha
  b<-beta
  
  lk1 <- lgamma(1+(1/alpha))
  lk2 <- lgamma(1+(1/beta))
  k1 <- exp(lk1)  
  k2 <- exp(lk2)     
  z <- (x-mu)/sigma
  
  loglik1 <- -((abs(z)^a))
  loglik2 <- -((abs(z)^b))
  loglik <- ifelse(x < mu, loglik1, loglik2)
  loglik <- loglik - log(k1+k2) - log(sigma)
  fy <- if(log==FALSE) exp(loglik) else loglik 
  #return(fy)
  if (log){
    return(loglik)
  } else{
    return(exp(loglik))
  }
}    


#' @rdname jsep
#' @examples
#' pjsep(4, mu=0, sigma=1, alpha=2, beta=2)
#' @export
pjsep <- function(q, mu=0, sigma=1, alpha=2, beta=2, lower.tail = TRUE, log.p = FALSE)
{  
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
  
  a<-alpha
  b<-beta
  
  lk1 <- lgamma(1+(1/a))
  lk2 <- lgamma(1+(1/b))
  lk <- lk2 - lk1
  k <- exp(lk)       
  z <- (q-mu)/sigma
  s1 <- (abs(z)^a)
  s2 <- (abs(z)^b)
  cdf1 <- 1-pgamma(s1,shape=1/a,scale=1)
  cdf2 <- 1+k*pgamma(s2,shape=1/b,scale=1)
  cdf <- ifelse(q < mu, cdf1, cdf2)
  cdf <- cdf/(1+k)
  if(lower.tail==TRUE) cdf  <- cdf else  cdf <- 1-cdf  
  if(log.p==FALSE) cdf  <- cdf else  cdf <- log(cdf) 
  cdf    
}


#' @rdname jsep
#' @examples
#' qjsep(0.5, mu=0, sigma=1, alpha=2, beta=2)
#' @export
qjsep <- function(p, mu=0, sigma=1, alpha=2, beta=2, lower.tail = TRUE, log.p = FALSE)
{ 
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
  if (any(p <= 0)|any(p >= 1))  stop(paste("p must be between 0 and 1", "\n", ""))  
  
  a<-alpha
  b<-beta
  
  if (log.p==TRUE) p <- exp(p) else p <- p
  
  if (lower.tail==TRUE) p <- p else p <- 1-p
  lk1 <- lgamma(1+(1/a))
  lk2 <- lgamma(1+(1/b))
  lk <- lk2 - lk1
  k <- exp(lk)       
  suppressWarnings(q1 <- mu - sigma*((qgamma( 1-p*(1+k), shape=1/a, scale=1))^(1/a)))
  suppressWarnings(q2 <- mu + sigma*((qgamma( (-1/k)*(1-p*(1+k)), shape=1/b, scale=1))^(1/b)))
  q <- ifelse(p < (1/(1+k)), q1, q2)
  q[p == 0] <- -Inf
  q[p == 1] <- Inf
  q[p <  0] <- NaN
  q[p >  1] <- NaN
  return(q)
}


#' @rdname jsep
#' @examples
#' rjsep(4, mu=0, sigma=1, alpha=2, beta=2)
#' @export
rjsep <- function(n, mu=0, sigma=1, alpha=2, beta=2)
{
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
  
  a<-alpha
  b<-beta
  
  n <- ceiling(n)
  p <- runif(n)
  r <- qjsep(p,mu=mu,sigma=sigma,alpha=a,beta=b)
  return(r)
}