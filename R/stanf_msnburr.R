#' Stan function of MSNBurr Distribution
#'
#' @name stanf_msnburr
#' @description 
#' Provides Stan code for the MSNBurr distribution (Log-density, CDF, RNG, etc.)
#' with support for both vectorized and scalar modes.
#' 
#' @param vectorize logical; if TRUE, Vectorize Stan code of MSNBurr distribution are given 
#' The default value of this parameter is TRUE
#' @param rng logical; if TRUE,  Stan code of quantile and 
#' random number generation of MSNBurr distribution are given 
#' The default value of this parameter is TRUE
#' @author Achmad Syahrul Choir and Nur Iriawan
#' @return
#' \code{msnburr_lpdf} gives the log of density, \code{msnburr_cdf} gives the distribution
#' function, \code{msnburr_lcdf} gives the log of distribution function, \code{msnburr_lccdf} gives the complement of log distribution function (1-msnburr_lcdf),
#'  and \code{msnburr_rng} generates
#' random deviates.
#' 
#'@details
#'  MSNBurr Distribution has density:
#' \deqn{f(y |\mu,\sigma,\alpha)=\frac{\omega}{\sigma}\exp{\left(-\omega{\left(\frac{y-\mu}{\sigma}\right)}\right)}{{\left(1+\frac{\exp{\left(-\omega{(\frac{y-\mu}{\sigma})}\right)}}{\alpha}\right)}^{-(\alpha+1)}}}
#' where \eqn{-\infty < y < \infty, -\infty < \mu< \infty, \sigma>0, \alpha>0, 
#' \omega = \frac{1}{\sqrt{2\pi}} {\left(1+\frac{1}{\alpha}\right)^{\alpha+1}}}
#'
#'This function gives stan code  of log density, cumulative distribution, log of cumulative distribution, log complementary cumulative distribution, quantile, random number of MSNBurr distribution 
#'
#' @references 
#' Iriawan, N. (2000). Computationally Intensive Approaches to Inference in Neo-Normal Linear Models. Curtin University of Technology.
#' Choir, A. S. (2020). The New Neo-Normal Distributions and their Properties. Dissertation. Institut Teknologi Sepuluh Nopember.
#' 
#' @examples 
#' \dontrun{
#' library (neodistr)
#' library(rstan)
#' #inputting data
#' set.seed(136)
#' dt <- neodistr::rmsnburr(100,0,1,0.5) # random generating MSNBurr data 
#' dataf <- list(
#'   n = 100,
#'   y = dt
#' )
#' #### not vector  
#' ##Calling the function of the neo-normal distribution that is available in the package.
#' func_code<-paste(c("functions{",neodistr::stanf_msnburr(vectorize=FALSE),"}"),collapse="\n")
#' #define stan model code
#' model<-"
#'  data {
#'  int<lower=1> n;
#'  vector[n] y;
#'  }
#'  parameters {
#'  real mu;
#'  real <lower=0> sigma;
#'  real <lower=0> alpha;
#'  
#'  }
#'  model {
#'  for(i in 1:n){
#'  y[i]~msnburr(mu,sigma,alpha);
#'  }
#'  mu~cauchy(0,1);
#'  sigma~cauchy(0,2.5);
#'  alpha~cauchy(0,1);
#'  }
#'   "
#' #merge stan model code and selected neo-normal stan function
#' fit_code<-paste(c(func_code,model,"\n"),collapse="\n") 
#' 
#' # Create the model using stan function
#' fit1 <- stan(
#'   model_code = fit_code,  # Stan program
#'   data = dataf,    # named list of data
#'   chains = 2,             # number of Markov chains
#'   #warmup = 5000,          # number of warmup iterations per chain
#'   iter = 10000,           # total number of iterations per chain
#'   cores = 2              # number of cores (could use one per chain)
#' )
#' 
#' # Showing the estimation results of the parameters that were executed using the Stan file
#' print(fit1, pars=c("mu", "sigma", "alpha", "lp__"), probs=c(.025,.5,.975))
#' 
#' 
#' # Vector
#' ##Calling the function of the neo-normal distribution that is available in the package.
#' func_code_vector<-paste(c("functions{",neodistr::stanf_msnburr(vectorize=TRUE),"}"),collapse="\n")
#' # define stan model as vector
#' model_vector<-"
#' data {
#'   int<lower=1> n;
#'   vector[n] y;
#' }
#' parameters {
#'   real mu;
#'   real <lower=0> sigma;
#'   real <lower=0> alpha;
#' }
#' model {
#'   y~msnburr(rep_vector(mu,n),sigma,alpha);
#'   mu~cauchy(0,1);
#'   sigma~cauchy(0,2.5);
#'   alpha~cauchy(0,1);
#' }
#' "
#' #merge stan model code and selected neo-normal stan function
#' fit_code_vector<-paste(c(func_code_vector,model_vector,"\n"),collapse="\n")
#' 
#' # Create the model using stan function
#' fit2 <- stan(
#'   model_code = fit_code_vector,  # Stan program
#'   data = dataf,    # named list of data
#'   chains = 2,             # number of Markov chains
#'   #warmup = 5000,          # number of warmup iterations per chain
#'   iter = 10000,           # total number of iterations per chain
#'   cores = 2             # number of cores (could use one per chain)
#' )
#' 
#' @export
stanf_msnburr <- function(vectorize = TRUE, rng = TRUE) {
  
  sig_y  <- if (vectorize) "vector" else "real"
  sig_mu <- if (vectorize) "vector" else "real"
  loc_v  <- if (vectorize) "vector[N]" else "real"
  
  common_check <- '
    if (alpha <= 0) 
       reject("alpha <= 0; found alpha =", alpha);
    if (sigma <= 0) 
        reject("sigma <= 0; found sigma =", sigma);
    lomega = -0.5 * log(2 * pi()) + (alpha + 1.0) * log1p(1.0 / alpha);
    omega = exp(lomega);'

  dist_code <- paste0('
    real msnburr_lpdf(', sig_y, ' y, ', sig_mu, ' mu, real sigma, real alpha) {
      int N = ', if (vectorize) "rows(y)" else "1", ';
      real lomega;
      real omega;
      ', loc_v, ' lp;
      ', loc_v, ' zo;
      ', loc_v, ' zoa;
      ', common_check, '
      zo = -omega * ((y - mu) / sigma);
      zoa = zo - log(alpha);
      lp = ', if (vectorize) "rep_vector((lomega - log(sigma)), N)" else "(lomega - log(sigma))", ' + zo - ((alpha + 1.0) * log1p_exp(zoa));
      ', if (vectorize) 'return sum(lp);' 
         else 'return lp;', '
    }

    real msnburr_cdf(', sig_y, ' y, ', sig_mu, ' mu, real sigma, real alpha) {
      int N = ', if (vectorize) "rows(y)" else "1", ';
      real lomega;
      real omega;
      ', loc_v, ' zoa;
      ', common_check, '
      zoa = -omega * ((y - mu) / sigma) - log(alpha);
      return exp(', if (vectorize) "sum(-alpha * log1p_exp(zoa))" else "(-alpha * log1p_exp(zoa))", ');
    }

    real msnburr_lcdf(', sig_y, ' y, ', sig_mu, ' mu, real sigma, real alpha) {
      int N = ', if (vectorize) "rows(y)" else "1", ';
      real lomega;
      real omega;
      ', loc_v, ' zoa;
      ', common_check, '
      zoa = -omega * ((y - mu) / sigma) - log(alpha);
      return ', if (vectorize) "-sum(alpha * log1p_exp(zoa))" else "-alpha * log1p_exp(zoa)", ';
    }

    real msnburr_lccdf(', sig_y, ' y, ', sig_mu, ' mu, real sigma, real alpha) {
      int N = ', if (vectorize) "rows(y)" else "1", ';
      real lomega;
      real omega;
      ', loc_v, ' zoa;
      ', common_check, '
      zoa = -omega * ((y - mu) / sigma) - log(alpha);
      return ', if (vectorize) "sum(log1m_exp(-(alpha * log1p_exp(zoa))))" else "log1m_exp(-(alpha * log1p_exp(zoa)))", ';
    }
  ')

  qr_code <- '
    real msnburr_quantile(real p, real mu, real sigma, real alpha) {
      real lomega;
      real omega;
      real log_term;
      real inner;
      if (alpha <= 0) reject("alpha <= 0; found alpha =", alpha);
      if (sigma <= 0) reject("sigma <= 0; found sigma =", sigma);
      if (p < 0 || p > 1) reject("p < 0 or p > 1, found p = ", p);
      lomega = -0.5 * log(2 * pi()) + (alpha + 1.0) * log1p(1.0 / alpha);
      omega = exp(lomega);
      log_term = -log(p) / alpha;
      inner = (log_term > 20.0) ? log_term : log(expm1(log_term));
      inner = fmin(inner, 700.0);
      return mu - (sigma / omega) * (log(alpha) + inner);
    }

    real msnburr_rng(real mu, real sigma, real alpha) {
      if (alpha <= 0 || sigma <= 0) reject("Invalid parameters in RNG");
      return msnburr_quantile(uniform_rng(1e-12, 1.0 - 1e-12), mu, sigma, alpha);
    }
  '

  out <- if (rng) paste0(dist_code, qr_code) else dist_code
  # Clean up formatting: Ensure a newline follows every semicolon
 # out <- gsub(";[ ]*", ";\n", out)
  return(out)
}
