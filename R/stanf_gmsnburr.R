#' Stan function of GMSNBurr Distribution
#' @name stanf_gmsnburr
#'
#' @description
#' Stan code of GMSNBurr distribution for custom distribution in stan
#' @param vectorize logical; if TRUE, Vectorize Stan code of GMSNBurr distribution are given 
#' The default value of this parameter is TRUE
#' @param rng logical; if TRUE,  Stan code of quantile and 
#' random number generation of GMSNBurr distribution are given 
#' The default value of this parameter is TRUE
#' @return
#'\code{msnburr_lpdf} gives the stans's code of log of density, \code{msnburr_cdf} gives the stans's code of distribution
#' function, \code{gmsnburr_lcdf} gives the stans's code of log of distribution function, \code{gmsnburr_lccdf} gives the stans's code of complement of log distribution function (1-gmsnburr_lcdf),
#'  and \code{gmsnburr_rng} the stans's code of generates random numbers.
#' @author Achmad Syahrul Choir
#' @details
#'  GMSNBurr Distribution has density:
#' \deqn{f(y |\mu,\sigma,\alpha,\beta) =  {\frac{\omega}{{B(\alpha,\beta)}\sigma}}{{\left(\frac{\beta}{\alpha}\right)}^\beta} {{\exp{\left(-\beta \omega {\left(\frac{y-\mu}{\sigma}\right)}\right)} {{\left(1+{\frac{\beta}{\alpha}} {\exp{\left(-\omega {\left(\frac{y-\mu}{\sigma}\right)}\right)}}\right)}^{-(\alpha+\beta)}}}}}
#' where \eqn{-\infty<y<\infty,  -\infty<\mu<\infty,  \sigma>0, \alpha>0, \beta>0} 
#' and  \eqn{\omega = {\frac{B(\alpha,\beta)}{\sqrt{2\pi}}}{{\left(1+{\frac{\beta}{\alpha}}\right)}^{\alpha+\beta}}{\left(\frac{\beta}{\alpha}\right)}^{-\beta}}
#' 
#' This function gives stan code  of log density, cumulative distribution, log of cumulative distribution, log complementary cumulative distribution, quantile, random number of GMSNBurr distribution
#' 
#' @references 
#' Choir, A. S. (2020). The New Neo-Normal Distributions and their Properties. Dissertation. Institut Teknologi Sepuluh Nopember.
#' @examples 
#' \dontrun{
#' library(neodistr)
#' library(rstan)
#' #inputting data
#' set.seed(136)
#' dt <- rgmsnburr(100,0,1,0.5,0.5) # random generating MSNBurr-IIA data 
#' dataf <- list(
#'   n = 100,
#'   y = dt
#' )
#' #### not vector  
#' ##Calling the function of the neo-normal distribution that is available in the package.
#' func_code<-paste(c("functions{",neodistr::stanf_gmsnburr(vectorize=FALSE),"}"),collapse="\n")
#' #define stan model code
#' model<-"
#'   data {
#'   int<lower=1> n;
#'   vector[n] y;
#'   }
#'   parameters {
#'   real mu;
#'   real <lower=0> sigma;
#'   real <lower=0> alpha;
#'   real <lower=0> beta; 
#'   }
#'   model {
#'   for(i in 1:n){
#'   y[i]~gmsnburr(mu,sigma,alpha,beta);
#'   }
#'   mu~cauchy(0,1);
#'   sigma~cauchy(0,2.5);
#'   alpha~cauchy(0,1);
#'   beta~cauchy(0,1);
#'   }
#'    "
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
#'   cores = 2,              # number of cores (could use one per chain)
#'   control = list(         #control samplers behavior
#'     adapt_delta=0.9
#'   )
#' )
#' 
#' # Showing the estimation results of the parameters that were executed using the Stan file
#' print(fit1, pars=c("mu", "sigma", "alpha", "beta","lp__"), probs=c(.025,.5,.975))
#' 
#' 
#' # Vector
#' ##Calling the function of the neo-normal distribution that is available in the package.
#' func_code_vector<-paste(c("functions{",neodistr::stanf_gmsnburr(vectorize=TRUE),"}"),collapse="\n")
#' # define stan model as vector
#' model_vector<-"
#'  data {
#'    int<lower=1> n;
#'    vector[n] y;
#'  }
#'  parameters {
#'    real mu;
#'    real <lower=0> sigma;
#'    real <lower=0> alpha;
#'    real <lower=0> beta;
#'  }
#'  model {
#'    y~gmsnburr(rep_vector(mu,n),sigma,alpha,beta);
#'    mu~cauchy(0,1);
#'    sigma~cauchy(0,2.5);
#'    alpha~cauchy(0,1);
#'    beta~cauchy(0,1);
#'  }
#'  "
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
#'   cores = 2,              # number of cores (could use one per chain)
#'   control = list(         #control samplers behavior
#'     adapt_delta=0.9
#'   )
#' )
#' 
#' # Showing the estimation results of the parameters 
#' print(fit2, pars=c("mu", "sigma", "alpha","beta",  "lp__"), probs=c(.025,.5,.975))
#' }
#' @export
stanf_gmsnburr <- function(vectorize = TRUE, rng = TRUE) {

  # 1. Type Signatures for Arguments
  sig_y  <- if (vectorize) "vector" else "real"
  sig_mu <- if (vectorize) "vector" else "real"

  # 2. Local Variable Declarations
  loc_v <- if (vectorize) "vector[N]" else "real"

  # 3. Validation & Common Math Logic
  #    omega = B(a,b)/sqrt(2*pi) * (1+b/a)^(a+b) * (b/a)^(-b)
  #    log(omega) = lbeta(a,b) - 0.5*log(2*pi) + (a+b)*log1p(b/a) - b*(log(b)-log(a))
  common_check <- '
    if (alpha <= 0) { reject("alpha <= 0; found alpha =", alpha); }
    if (beta  <= 0) { reject("beta  <= 0; found beta  =", beta);  }
    if (sigma <= 0) { reject("sigma <= 0; found sigma =", sigma); }
    lomega = -0.5 * log(2 * pi()) + lbeta(alpha, beta)
             - beta * (log(beta) - log(alpha))
             + (alpha + beta) * log1p(beta / alpha);
    omega = exp(lomega);'

  # 4. Distribution Functions
  #    Key variables:
  #      zo  = -omega * (y - mu) / sigma
  #      zoa = zo + log(beta/alpha)
  #
  #    LPDF:  lomega - log(sigma) + beta*log(beta/alpha) - lbeta(alpha,beta)
  #           + beta*zo - (alpha+beta)*log1p_exp(zoa)
  #
  #    CDF via incomplete beta:
  #      Let ep = 1/(1 + (beta/alpha)*exp(zo)) = sigmoid(-zoa) = exp(-log1p_exp(zoa))
  #      Then CDF = I_ep(alpha, beta)  [regularized incomplete beta]

  dist_code <- paste0('
    real gmsnburr_lpdf(', sig_y, ' y, ', sig_mu, ' mu, real sigma, real alpha, real beta) {
      int N = ', if (vectorize) "rows(y)" else "1", ';
      real lomega;
      real omega;
      ', loc_v, ' lp;
      ', loc_v, ' zo;
      ', loc_v, ' zoa;
      ', if (vectorize) 'if (rows(y) != rows(mu)) { reject("y and mu must have same length"); }' else '', '
      ', common_check, '
      zo = -omega * ((y - mu) / sigma);
      zoa = zo + log(beta) - log(alpha);
      lp = ', if (vectorize)
        'rep_vector((lomega - log(sigma) + beta * (log(beta) - log(alpha)) - lbeta(alpha, beta)), N)'
      else
        '(lomega - log(sigma) + beta * (log(beta) - log(alpha)) - lbeta(alpha, beta))',
      ' + beta * zo - (alpha + beta) * log1p_exp(zoa);
      ', if (vectorize)
        'for(i in 1:N) { if(is_inf(zo[i])) lp[i] = negative_infinity(); }
      return sum(lp);'
      else
        'if(is_inf(zo)) lp = negative_infinity();
      return lp;', '
    }

    real gmsnburr_lcdf(', sig_y, ' y, ', sig_mu, ' mu, real sigma, real alpha, real beta) {
      int N = ', if (vectorize) "rows(y)" else "1", ';
      real lomega;
      real omega;
      ', loc_v, ' zo;
      ', loc_v, ' zoa;
      ', loc_v, ' ep;
      ', if (vectorize) 'if (rows(y) != rows(mu)) { reject("y and mu must have same length"); }' else '', '
      ', common_check, '
      zo = -omega * ((y - mu) / sigma);
      zoa = zo + log(beta) - log(alpha);
      ep = exp(-log1p_exp(zoa));
      return beta_lcdf(ep | alpha, beta);
    }

    real gmsnburr_lccdf(', sig_y, ' y, ', sig_mu, ' mu, real sigma, real alpha, real beta) {
      int N = ', if (vectorize) "rows(y)" else "1", ';
      real lomega;
      real omega;
      ', loc_v, ' zo;
      ', loc_v, ' zoa;
      ', loc_v, ' ep;
      ', if (vectorize) 'if (rows(y) != rows(mu)) { reject("y and mu must have same length"); }' else '', '
      ', common_check, '
      zo = -omega * ((y - mu) / sigma);
      zoa = zo + log(beta) - log(alpha);
      ep = exp(-log1p_exp(zoa));
      return beta_lccdf(ep | alpha, beta);
    }

    real gmsnburr_cdf(', sig_y, ' y, ', sig_mu, ' mu, real sigma, real alpha, real beta) {
      return exp(gmsnburr_lcdf(y, mu, sigma, alpha, beta));
    }
  ')

  # 5. Quantile and RNG Logic
  qr_code <- '
    real gmsnburr_quantile(real p, real mu, real sigma, real alpha, real beta) {
      real lomega;
      real omega;
      real ib;
      real log_s;
      if (alpha <= 0 || beta <= 0 || sigma <= 0) { reject("Invalid parameters"); }
      if (p <= 0 || p >= 1) { reject("p must be in (0,1); found p = ", p); }

      lomega = -0.5 * log(2 * pi()) + lbeta(alpha, beta)
               - beta * (log(beta) - log(alpha))
               + (alpha + beta) * log1p(beta / alpha);
      omega = exp(lomega);

      ib = inv_inc_beta(alpha, beta, p);

      // s = (alpha/beta) * ((1 - ib) / ib)
      // Compute in log-space to avoid overflow when ib ≈ 0
      log_s = log(alpha) - log(beta) + log1m(ib) - log(ib);

      return mu - (sigma / omega) * log_s;
    }

    real gmsnburr_rng(real mu, real sigma, real alpha, real beta) {
      return gmsnburr_quantile(uniform_rng(1e-12, 1.0 - 1e-12), mu, sigma, alpha, beta);
    }
  '

  # Merge and return
  return(if (rng) paste0(dist_code, qr_code) else dist_code)
}

}
