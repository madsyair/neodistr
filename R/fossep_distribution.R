# These functions is adapted from SEP3 in gamlss.dist that was written by Bob Rigby and Mikis Stasinopoulos
#' Fernandez-Osiewalski-Steel Skew Exponential Power Distribution
#' @export 
#' @name fossep
#' @importFrom stats pgamma qgamma
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
#' for the Fernandez-Osiewalski-Steel Skew Exponential Power Distribution. 
#' 
#' @return 
#' \code{dfossep} gives the density , \code{pfossep} gives the distribution function,
#' \code{qfossep} gives quantiles function, \code{rfossep} generates random numbers.
#' @author Almira Utami
#' @keywords distribution
#' @concept Univariate
#' @concept Continuous
#' 
#' 

#' @details
#' Fernandez-Osiewalski-Steel Skew Exponential Power Distribution 
#' 
#' 
#' 
#' The Fernandez-Osiewalski-Steel Skew Exponential Power distribution with parameters \eqn{\mu}, \eqn{\sigma},\eqn{\alpha}, and \eqn{\beta}
#' has density:
#' \deqn{f(x |\mu,\sigma,\beta,\alpha) = \frac{c}{\sigma} \exp \left( - \frac{1}{2} \left| v z \right|^\tau \right) \quad \text{if } x < \mu}
#' \deqn{f(x |\mu,\sigma,\beta,\alpha) = \frac{c}{\sigma} \exp \left( - \frac{1}{2} \left| \frac{v}{z} \right|^\tau \right) \quad \text{if } x \ge \mu}
#' \deqn{ \text{where } -\infty < y < \infty, \ -\infty < \mu < \infty, \ \sigma > 0, \ \alpha > 0, \ \beta > 0}
#' \deqn{ z = \frac{x - \mu}{\sigma}}
#' \deqn{ c = v \tau \left[ (1 + v^2) 2^{\frac{1}{\tau}} \Gamma \left( \frac{1}{\tau} \right) \right]^{-1}}
#'
#' @references 
#'  Fernandez, C., Osiewalski, J., & Steel, M. F. (1995) Modeling and inference with v-spherical distributions.
#'  Journal of the American Statistical Association, 90(432), pp 1331-1340.
#'
#'  Rigby, R.A. and Stasinopoulos, M.D. and Heller, G.Z. and De Bastiani, F. 
#'   (2019) Distributions for Modeling Location, Scale, 
#'   and Shape: Using GAMLSS in R.CRC Press

#' @rdname fossep
#' @examples
#' dfossep(4, mu=0, sigma=1, alpha=2, beta=2)
dfossep<- function(x,mu=0, sigma=1,alpha=2, beta=2, log=FALSE){
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
  z <- (x-mu)/sigma

  loglik1 <- -0.5*(a*abs(z))^b
  loglik2 <- -0.5*(abs(z)/a)^b
  
  loglik <- ifelse(x < mu, loglik1, loglik2)

  loglik <- loglik-log(sigma)+log(a)-log1p(a^2)-(1/b)*log(2)-lgamma(1+1/b)
  
  if (log) {
    return(loglik)
  } else {
    return(exp(loglik))
  }
  
  
}

#' @export
#' @rdname fossep
#' @examples
#' pfossep(4, mu=0, sigma=1, alpha=2, beta=2)
pfossep <- function(q,mu=0, sigma=1,alpha=2, beta=2,lower.tail=TRUE, log.p=FALSE){
  
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
  z <- (q-mu)/sigma
  k <- a^2       
  z1 <- a*(q-mu)/(sigma*(2^(1/b)))
  z2 <- (q-mu)/(sigma*a*(2^(1/b)))
  s1 <- (abs(z1)^b)
  s2 <- (abs(z2)^b)
  cdf1 <- 1-pgamma(s1,1/b,1)
  cdf2 <- 1+k*pgamma(s2,1/b,1)
  cdf <- ifelse(q < mu, cdf1, cdf2)
  cdf <- cdf/(1+k)
  
  if (log.p) {
    return(log(cdf))
  } else {
    return(cdf)
  }
}

#' @export
#' @rdname fossep
#' @examples
#' qfossep(0.4, mu=0, sigma=1, alpha=2, beta=2)
qfossep <- function(p, mu = 0, sigma = 1, alpha = 2, beta = 2, lower.tail = TRUE, log.p = FALSE) {
  ifelse(is.na(alpha), stop("alpha must be not missing value"), NA)
  ifelse(is.na(beta), stop("beta must be not missing value"), NA)
  ifelse(is.na(mu), stop("mu must be not missing value"), NA)
  ifelse(is.na(sigma), stop("sigma must be not missing value"), NA)
  if (any(sigma <= 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(alpha <= 0)) 
    stop(paste("alpha must be positive", "\n", ""))
  if (any(beta <= 0)) 
    stop(paste("beta must be positive", "\n", ""))
  if (any(p <= 0) | any(p >= 1)) 
    stop(paste("p must be between 0 and 1", "\n", "")) 
  
  a <- alpha
  b <- beta
  k <- a^2  
  
  suppressWarnings(q1 <- mu - (sigma * (2^(1/b)) / a) * (qgamma(1 - p * (1 + k), 1/b, 1))^(1/b))
  suppressWarnings(q2 <- mu + (sigma * a * (2^(1/b))) * (qgamma((-1/k) * (1 - p * (1 + k)), 1/b, 1))^(1/b))

  q <- ifelse(p < (1 / (1 + k)), q1, q2)
  
  q[p == 0] <- -Inf
  q[p == 1] <- Inf
  q[p <  0] <- NaN
  q[p >  1] <- NaN
  
  if (lower.tail) {
    p <- p
  } else {
    p <- 1 - p
  }
  
  if (log.p) {
    p <- exp(p)
  } else {
    p <- p
  }
  return(q)
}

#' @export
#' @rdname fossep
#' @examples
#' rfossep(4, mu=0, sigma=1, alpha=2, beta=2)
# Fungsi rfossep yang sudah diperbaiki
rfossep <- function(n, mu = 0, sigma = 1, alpha = 2, beta = 2) {
  ifelse(is.na(alpha), stop("alpha must be not missing value"), NA)
  ifelse(is.na(beta), stop("beta must be not missing value"), NA)
  ifelse(is.na(mu), stop("mu must be not missing value"), NA)
  ifelse(is.na(sigma), stop("sigma must be not missing value"), NA)
  if (any(alpha < 0)) 
    stop(paste("alpha must be positive", "\n", ""))  
  if (any(sigma < 0))  
    stop(paste("sigma must be positive", "\n", ""))
  if (any(beta < 0)) 
    stop(paste("beta must be positive", "\n", ""))  
  n <- ceiling(n)
  r <- qfossep(runif(n), mu = mu, sigma = sigma, alpha = alpha, beta = beta)
  return(r)
}