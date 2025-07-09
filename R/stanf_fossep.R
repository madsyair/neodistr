# These functions is translated from SEP3 in gamlss.dist that was written by Bob Rigby and Mikis Stasinopoulos

#' Stan function of Fernandez-Osiewalski-Steel Skew Exponential Power Distribution 

#' @name stanf_fossep
#' @description
#' Stan code of fossep distribution for custom distribution in stan
#' @param vectorize logical; if TRUE, Vectorize Stan code of Fernandez-Osiewalski-Steel Skew Exponential Power distribution are given 
#' The default value of this parameter is TRUE
#' @return
#' \code{fossep_lpdf} gives stan's code of the log of density, \code{fossep_cdf} gives stan's code of the distribution
#' function, \code{fossep_lcdf} gives stan's code of the log of distribution function and \code{fossep_lccdf} 
#' gives the stans's code of complement of log distribution function (1-fossep_lcdf)
#' @author Almira Utami and Achmad Syahrul Choir
#' @details
#'  Fernandez-Osiewalski-Steel Skew Exponential Power Distribution has density:
#' \deqn{f(y |\mu,\sigma,\alpha,\beta) = \frac{c}{\sigma} \exp \left( - \frac{1}{2} \left| v z \right|^\beta \right) \quad \text{if } y < \mu}
#' \deqn{f(y |\mu,\sigma,\alpha,\beta) = \frac{c}{\sigma} \exp \left( - \frac{1}{2} \left| \frac{v}{z} \right|^\beta \right) \quad \text{if } y \ge \mu}
#' \deqn{ \text{where } -\infty < y < \infty, \ -\infty < \mu < \infty, \ \sigma > 0, \ \alpha > 0, \ \beta > 0}
#' \deqn{ z = \frac{y - \mu}{\sigma}}
#' \deqn{ c = v \beta \left[ (1 + v^2) 2^{\frac{1}{\beta}} \Gamma \left( \frac{1}{\beta} \right) \right]^{-1}}
#'  
#' This function gives stan code  of log density, cumulative distribution, log of cumulative distribution, 
#' log complementary cumulative distribution of Fernandez-Osiewalski-Steel Skew Exponential Power Distribution 
#' @references 
#' Fernandez, C., Osiewalski, J., & Steel, M. F. (1995) Modeling and inference with v-spherical distributions. 
#' Journal of the American Statistical Association, 90(432), pp 1331-1340
#'
#' Rigby, R.A. and Stasinopoulos, M.D. and Heller, G.Z. and De Bastiani, F. (2019) Distributions for Modeling Location, 
#' Scale, and Shape: Using GAMLSS in R.CRC Press
#' @examples
#' \dontrun{
#' library (neodistr)
#' library (rstan)
#' 
#' # inputting data
#' set.seed(400)
#' dt <- neodistr::rfossep(100,mu=0, sigma=1, alpha = 2, beta = 2) # random generating fossep data
#' dataf <- list(
#'  n = 100,
#'  y = dt
#'  )
#'  
#'  
#' #### Vector
#' ## Calling the function of the neonormal distribution that is available in the package.
#' func_code_vector<-paste(c("functions{",neodistr::stanf_fossep(vectorize=TRUE),"}"),collapse="\n")
#' 
#' # Define Stan Model Code
#' model_vector <-"
#'     data{
#'       int<lower=1> n;
#'       vector[n] y;
#'     }
#'     parameters{
#'       real mu;
#'       real <lower=0> sigma;
#'       real <lower=0> alpha;
#'       real <lower=0>beta;
#'     }
#'     model {
#'       y ~ fossep(rep_vector(mu,n),sigma, alpha, beta);
#'       mu ~ cauchy (0,1);
#'       sigma ~ cauchy (0, 1);
#'       alpha ~ lognormal(0,2.5);
#'       beta ~ lognormal(0,2.5);
#'       
#'     }
#'  "
#'  
#'  # Merge stan model code and selected neo-normal stan function
#' fit_code_vector <- paste (c(func_code_vector,model_vector,"\n"), collapse = "\n")
#' 
#' # Create the model using Stan Function
#' fit2 <- stan(
#'     model_code = fit_code_vector,  # Stan Program
#'     data = dataf,                  # named list data
#'     chains = 2,                    # number of markov chains
#'     warmup = 5000,                 # total number of warmup iterarions per chain
#'     iter = 10000,                  # total number of iterations iterarions per chain
#'     cores = 2,                     # number of cores (could use one per chain)
#'     control = list(                # control sampel behavior
#'       adapt_delta = 0.99
#'     ),
#'     refresh = 1000                 # progress has shown if refresh >=1, else no progress shown
#' )
#' 
#' # Showing the estimation result of the parameters that were executed using the Stan file
#' print(fit2, pars = c("mu", "sigma", "alpha", "beta", "lp__"), probs=c(.025,.5,.975))
#'  }

#' @export
stanf_fossep<-function(vectorize=TRUE){
  if(vectorize){
    ## LPDF 
    dist<-'real fossep_lpdf(vector y, vector mu, real sigma, real alpha, real beta){
      int N=rows(y);
      real a = alpha;
      real b = beta;
      
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma);
      if(alpha<=0)
        reject ("alpha<=0, found alpha = ", alpha);
      if (beta<=0)
        reject ("beta<=0; found beta =", beta);

      vector[N] z = (y - mu) / sigma;
      vector[N] loglik1 = -0.5 * pow(a * abs(z), b);
      vector[N] loglik2 = -0.5 * pow(abs(z) / a, b);
      vector[N] loglik;
      for (i in 1:N) {
        loglik[i] = (y[i] < mu[i]) ? loglik1[i] : loglik2[i];
      }
    
      return sum(loglik) + N * (log(a) - log1p(a^2) - (1 / b) * log(2) - lgamma(1 + 1 / b) - log(sigma));
    }
    
    real fossep_cdf(vector y, vector mu, real sigma, real alpha, real beta){
      int N=rows(y);
      real a = alpha;
      real b = beta;
      real k = square(a);
      
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma);
      if(alpha<=0)
        reject ("alpha<=0, found alpha = ", alpha);
      if (beta<=0)
        reject ("beta<=0; found beta =", beta);
        
      vector[N] z1 = a * (y - mu) / (sigma * pow(2, 1 / b));
      vector[N] z2 = (y - mu) / (sigma * a * pow(2, 1 / b));
      vector[N] s1 = pow(fabs(z1), b);
      vector[N] s2 = pow(fabs(z2), b);
      vector[N] cdf1 = 1 - gamma_p(1 / b, s1);
      vector[N] cdf2 = 1 + k * gamma_p(1 / b, s2);
      vector[N] cdf;
        
      for (i in 1:N){
        cdf[i]=(y[i]<mu[i]) ? cdf1[i]:cdf2[i];
      }

      return sum(cdf) / (1 + k);
    }
    
    real fossep_lcdf(vector y, vector mu, real sigma, real alpha, real beta){
      int N=rows(y);
      real a = alpha;
      real b = beta;
      real k = square(a);
      
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma);
      if(alpha<=0)
        reject ("alpha<=0, found alpha = ", alpha);
      if (beta<=0)
        reject ("beta<=0; found beta =", beta);
      
      vector[N] z1 = a * (y - mu) / (sigma * pow(2, 1 / b));
      vector[N] z2 = (y - mu) / (sigma * a * pow(2, 1 / b));
      vector[N] s1 = pow(fabs(z1), b);
      vector[N] s2 = pow(fabs(z2), b);
      vector[N] lcdf1 = log1m(gamma_p(1/b,s1));
      vector[N] lcdf2 = log1p(k*gamma_p(1/b,s2));
      vector[N] lcdf;
      
      for (i in 1:N){
        lcdf[i]=(y[i]<mu[i]) ? lcdf1[i]:lcdf2[i];
      }
      
      return sum(lcdf) - N * log1p(k);
    }
    
    real fossep_lccdf(vector y, vector mu, real sigma, real alpha, real beta){
      int N=rows(y);
      real a = alpha;
      real b = beta;
      real k = square(a);

      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma);
      if(alpha<=0)
        reject ("alpha<=0, found alpha = ", alpha);
      if (beta<=0)
        reject ("beta<=0; found beta =", beta);
      
      vector[N] z1 = a * (y - mu) / (sigma * pow(2, 1 / b));
      vector[N] z2 = (y - mu) / (sigma * a * pow(2, 1 / b));
      vector[N] s1 = pow(fabs(z1), b);
      vector[N] s2 = pow(fabs(z2), b);
      vector[N] lccdf1 = log(k + gamma_p(1/b,s1));
      vector[N] lccdf2 = log(k) + log1m(gamma_p(1/b,s2));
      vector[N] lccdf;
      
      for (i in 1:N){
        lccdf[i]=(y[i]<mu[i]) ? lccdf1[i]:lccdf2[i];
      }  
      
      return sum(lccdf) - N * log1p(k);
    }
    '
  }else{
    
    ## LPDF 
    dist<-'real fossep_lpdf(real y, real mu, real sigma, real alpha, real beta){
      real a = alpha;
      real b = beta;
      
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma);
      if(alpha<=0)
        reject ("alpha<=0, found alpha = ", alpha);
      if (beta<=0)
        reject ("beta<=0; found beta =", beta);
      real z = (y - mu) / sigma;
      real loglik1 = -0.5 * pow(a * abs(z), b);
      real loglik2 = -0.5 * pow(abs(z) / a, b);
      real loglik;
      loglik = (y < mu) ? loglik1 : loglik2;
      
      return loglik +  log(a) - log1p(a^2) - (1 / b) * log(2) - lgamma(1 + 1 / b) - log(sigma);
    }
    
    real fossep_cdf(real y, real mu, real sigma, real alpha, real beta){
      real a = alpha;
      real b = beta;
      real k = square(a);
      
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma);
      if(alpha<=0)
        reject ("alpha<=0, found alpha = ", alpha);
      if (beta<=0)
        reject ("beta<=0; found beta =", beta);
        
      real z1 = a * (y - mu) / (sigma * pow(2, 1 / b));
      real z2 = (y - mu) / (sigma * a * pow(2, 1 / b));
      real s1 = pow(fabs(z1), b);
      real s2 = pow(fabs(z2), b);
      real cdf1 = 1 - gamma_p(1 / b, s1);
      real cdf2 = 1 + k * gamma_p(1 / b, s2);
      real cdf = (y < mu) ? cdf1 : cdf2;
      
      return cdf / (1 + k);
    }
    
    real fossep_lcdf(real y, real mu, real sigma, real alpha, real beta){
      real a = alpha;
      real b = beta;
      real k = square(a);
      
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma);
      if(alpha<=0)
        reject ("alpha<=0, found alpha = ", alpha);
      if (beta<=0)
        reject ("beta<=0; found beta =", beta);
      
      real z1 = a * (y - mu) / (sigma * pow(2, 1 / b));
      real z2 = (y - mu) / (sigma * a * pow(2, 1 / b));
      real s1 = pow(fabs(z1), b);
      real s2 = pow(fabs(z2), b);
      real lcdf1 = log1m(gamma_p(1/b,s1));
      real lcdf2 = log1p(k*gamma_p(1/b,s2));
      real lcdf = (y < mu) ? lcdf1 : lcdf2;
      
      return lcdf -  log1p(k);
    }
    
    real fossep_lccdf(real y, real mu, real sigma, real alpha, real beta){
      real a = alpha;
      real b = beta;
      real k = square(a);

      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma);
      if(alpha<=0)
        reject ("alpha<=0, found alpha = ", alpha);
      if (beta<=0)
        reject ("beta<=0; found beta =", beta);
      
      real z1 = a * (y - mu) / (sigma * pow(2, 1 / b));
      real z2 = (y - mu) / (sigma * a * pow(2, 1 / b));
      real s1 = pow(fabs(z1), b);
      real s2 = pow(fabs(z2), b);
      real lccdf1 = log(k + gamma_p(1/b,s1));
      real lccdf2 = log(k) + log1m(gamma_p(1/b,s2));
      real lccdf = (y < mu) ? lccdf1:lccdf2;
        
      return lccdf - log1p(k);
    }
    '
  }
}
