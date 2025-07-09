# These functions is translated from ST5 in gamlss.dist that was written by Bob Rigby and Mikis Stasinopoulos

#' Stan function of Jones Skew Exponential Power

#' @name stanf_jsep
#' @description
#' Stan code of jsep distribution for custom distribution in stan
#' @param vectorize logical; if TRUE, Vectorize Stan code of Jones and faddy distribution are given 
#' The default value of this parameter is TRUE
#' @return
#' \code{jsep_lpdf} gives stan's code of the log of density, \code{jsep_cdf} gives stan's code of the distribution
#' function, and \code{jsep_lcdf} gives stan's code of the log of distribution function
#' @author Meischa Zahra Nur Adhelia and Achmad Syahrul Choir
#' @details
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
#'   
#' @examples
#' \dontrun{
#' library (neodistr)
#' library (rstan)
#' 
#' # inputting data
#' set.seed(400)
#' dt <- neodistr::rjsep(100,mu=0, sigma=1, alpha = 2, beta = 2) # random generating jsep data
#' dataf <- list(
#'  n = 100,
#'  y = dt
#'  )
#'  
#'  
#' #### not vector
#' ## Calling the function of the neo-normal distribution that is available in the package.
#' func_code<-paste(c("functions{",neodistr::stanf_jsep(vectorize=TRUE),"}"),collapse="\n")
#' 
#' # Define Stan Model Code
#' model <-"
#'     data{
#'       int<lower=1> n;
#'       vector[n] y;
#'     }
#'     parameters{
#'       real mu;
#'       real <lower=0> sigma;
#'       real <lower=0> alpha;
#'       real <lower=0> beta;
#'     }
#'     model {
#'       y ~ jsep(rep_vector(mu,n),sigma, alpha, beta);
#'       mu ~ cauchy(0,1);
#'       sigma ~ cauchy(0, 2.5);
#'       alpha ~ lognormal(0,5);
#'       beta ~ lognormal(0,5);
#'       
#'     }
#' "
#' 
#' # Merge stan model code and selected neo-normal stan function
#' fit_code <- paste (c(func_code,model,"\n"), collapse = "\n")
#' 
#' # Create the model using Stan Function
#' fit1 <- stan(
#'     model_code = fit_code,  # Stan Program
#'     data = dataf,           # named list data
#'     chains = 2,             # number of markov chains
#'     warmup = 5000,          # total number of warmup iterarions per chain
#'     iter = 10000,           # total number of iterations iterarions per chain
#'     cores = 2,              # number of cores (could use one per chain)
#'     control = list(         # control sampel behavior
#'       adapt_delta = 0.99
#'     ),
#'     refresh = 1000          # progress has shown if refresh >=1, else no progress shown
#' )
#' 
#' # Showing the estimation result of the parameters that were executed using the Stan file
#' print(fit1, pars = c("mu", "sigma", "alpha", "beta", "lp__"), probs=c(.025,.5,.975))
#' 
#' 
#' #### Vector
#' ## Calling the function of the neonormal distribution that is available in the package.
#' func_code_vector<-paste(c("functions{",neodistr::stanf_jsep(vectorize=TRUE),"}"),collapse="\n")
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
#'       y ~ jsep(rep_vector(mu,n),sigma, alpha, beta);
#'       mu ~ cauchy (0,1);
#'       sigma ~ cauchy (0, 2.5);
#'       alpha ~ lognormal(0,5);
#'       beta ~ lognormal(0,5);
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
stanf_jsep<-function(vectorize=TRUE){
  if(vectorize){
    #PDF
    dist<-'real jsep_lpdf(vector y, vector mu, real sigma, real alpha, real beta){
      int N=rows(y);
      //  real nu;
      //  real lam;
      real a;
      real b;
      vector[N] z;
      vector[N] loglik;
  
      real log_likelihood;
       if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma);
      if(alpha<=0)
        reject ("alpha<=0, found beta = ", alpha);
      if (beta<=0)
        reject ("beta<=0; found beta =", beta);
      a=alpha;
      b=beta;
      z=(y-mu)/sigma;
  
      real lk1 = lgamma(1+1/a);
      real lk2 = lgamma(1+1/b) ;
      real k1 = exp(lk1); 
      real k2 = exp(lk2);
      vector[N] loglik1 = - pow(fabs(z), a);
      vector[N] loglik2 = - pow(fabs(z), b);
      for (i in 1:N){
            loglik[i]=(y[i]<mu[i]) ? loglik1[i]:loglik2[i];
      }
     
      log_likelihood = sum(loglik) - N*log(k1 + k2) - N*log(sigma);
    
      return log_likelihood;
    }

    //CDF
    real jsep_cdf(vector y, vector mu, real sigma, real alpha, real beta)
    {
      int N=rows(y);
      //  real nu;
      //  real lam;
        real  a;
        real  b;  
        vector[N] z;
        if (sigma<=0)
          reject("sigma<=0; found sigma =", sigma);
        if(alpha<=0)
          reject ("alpha<=0, found alpha = ", alpha);
        if (beta<=0)
          reject ("beta<=0; found beta =", beta);
        z=(y-mu)/sigma;
        a=alpha;
        b=beta;
      
      // c = pow((exp(lgamma(1+pow(b,-1))) + exp(lgamma(1+pow(a,-1)))),-1)
      //log_cdf += -log(exp(lgamma(1+pow(b,-1))) + exp(lgamma(1+pow(a,-1)))) - log(a) + lgamma(1/a) + log1m(gamma_p(1/a,pow(fabs(z[i]), a))); // Nilai absolut z dipangkatkan a        
         
        real lk1 = lgamma(1+1/a);
        real lk2 = lgamma(1+1/b);
        real lk = lk2 - lk1;
        real k = exp(lk);
        vector[N] s1 = pow(fabs(z), a);
        vector[N] s2 = pow(fabs(z), b);
        vector[N] cdf1 = 1 - gamma_p(1/a,s1);
        vector[N] cdf2 = 1 + k * gamma_p(1/b,s2);
        vector[N] cdf;
        
        for (i in 1:N){
              cdf[i]=(y[i]<mu[i]) ? cdf1[i]:cdf2[i];
              cdf[i] = cdf[i]/(1+k);
        }
        
        real result = sum(cdf);
        
        return result;
     }

    //Log CDF
    real jsep_lcdf(vector y, vector mu, real sigma, real alpha, real beta)
    {
      int N=rows(y);
      //  real nu;
      //  real lam;
      real  a;
      real  b;  
      vector[N] z;
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma);
      if(alpha<=0)
        reject ("alpha<=0, found alpha = ", alpha);
      if (beta<=0)
        reject ("beta<=0; found beta =", beta);
      z=(y-mu)/sigma;
      a=alpha;
      b=beta;
      real lk1 = lgamma(1+pow(a,-1));
      real lk2 = lgamma(1+pow(b,-1));
      real lk = lk2 - lk1;
      real k = exp(lk);
      vector[N] s1 = pow(fabs(z), a);
      vector[N] s2 = pow(fabs(z), b);
      vector[N] log_cdf1 = log1m(gamma_p(1/a,s1));
      vector[N] log_cdf2 = log1p(k * gamma_p(1/b,s2));
      
      
      //vector[N] log_cdf3 = (y < mu) .* log_cdf1 + (y >= mu) .* log_cdf2;
      //vector[N] log_cdf4 = log_cdf3-log1p(k);
      
      vector[N] log_cdf3;
      vector[N] log_cdf4;
      for (i in 1:N){
          log_cdf3[i]=(y[i]<mu[i]) ? log_cdf1[i]:log_cdf2[i];
          log_cdf4[i] = log_cdf3[i]-log1p(k);
      }
      
      real log_cdf = sum(log_cdf4);
    
      return log_cdf;
    }

    //log CCDF
    real jsep_lccdf (vector y, vector mu, real sigma, real alpha, real beta)
    {
      int N=rows(y);
      //  real nu;
      //  real lam;
      real  a;
      real  b;  
      vector[N] z;
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma);
      if(alpha<=0)
        reject ("alpha<=0, found alpha = ", alpha);
      if (beta<=0)
        reject ("beta<=0; found beta =", beta);
      z=(y-mu)/sigma;
      a=alpha;
      b=beta;
      
      real lk1 = lgamma(1+pow(a,-1));
      real lk2 = lgamma(1+pow(b,-1));
      real lk = lk2 - lk1;
      real k = exp(lk);
      vector[N] s1 = pow(fabs(z), a);
      vector[N] s2 = pow(fabs(z), b);
      
      vector[N] lccdf1 =log(k+gamma_p(1/a, s1));
      vector[N] lccdf2 =log(k)+log1m(gamma_p(1/b, s2));

      vector[N] lccdf;
      for (i in 1:N){
        lccdf[i]=(y[i]<mu[i]) ? lccdf1[i]:lccdf2[i];
      }
      lccdf = lccdf-log1p(k);
      
      real log_ccdf = sum(lccdf);
      return log_ccdf;
    }
    '
  }
  else{
    #PDF
    dist<-'real jsep_lpdf(real y, real mu, real sigma, real alpha, real beta){
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma);
      if(alpha<=0)
        reject ("alpha<=0, found alpha = ", alpha);
      if (beta<=0)
        reject ("beta<=0; found beta =", beta);
      real a=alpha;
      real b=beta;
      real z=(y-mu)/sigma;
      real lk1 = lgamma(1+1/a);
      real lk2 = lgamma(1+1/b) ;
      real k1 = exp(lk1); 
      real k2 = exp(lk2);
      real loglik=(y<mu) ? - pow(fabs(z), a): - pow(fabs(z), b);
      return loglik - log(k1 + k2) - log(sigma);
    }
    //CDF
    real jsep_cdf(real y, real mu, real sigma, real alpha, real beta)
    {
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma);
      if(alpha<=0)
        reject ("alpha<=0, found alpha = ", alpha);
      if (beta<=0)
        reject ("beta<=0; found beta =", beta);
      real a=alpha;
      real b=beta;
      real z=(y-mu)/sigma;
      
      // c = pow((exp(lgamma(1+pow(b,-1))) + exp(lgamma(1+pow(a,-1)))),-1)
      //log_cdf += -log(exp(lgamma(1+pow(b,-1))) + exp(lgamma(1+pow(a,-1)))) - log(a) + lgamma(1/a) + log1m(gamma_p(1/a,pow(fabs(z), a))); // Nilai absolut z dipangkatkan a        
      
      real lk1 = lgamma(1+1/a);
      real lk2 = lgamma(1+1/b);
      real lk = lk2 - lk1;
      real k = exp(lk);
      real s1 = pow(fabs(z), a);
      real s2 = pow(fabs(z), b);
      real cdf1 = 1 - gamma_p(1/a,s1);
      real cdf2 = 1 + k * gamma_p(1/b,s2);
      
      return (y<mu) ? cdf1/(1+k):cdf2/(1+k);
    }
    
    //Log CDF
    real jsep_lcdf(real y, real mu, real sigma, real alpha, real beta)
    {
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma);
      if(alpha<=0)
        reject ("alpha<=0, found alpha = ", alpha);
      if (beta<=0)
        reject ("beta<=0; found beta =", beta);
      real a=alpha;
      real b=beta;
      real z=(y-mu)/sigma;
      real lk1 = lgamma(1+pow(a,-1));
      real lk2 = lgamma(1+pow(b,-1));
      real lk = lk2 - lk1;
      real k = exp(lk);
      real s1 = pow(fabs(z), a);
      real s2 = pow(fabs(z), b);
      real log_cdf1 = log1m(gamma_p(1/a,s1));
      real log_cdf2 = log1p(k * gamma_p(1/b,s2));
      
      return (y<mu) ? log_cdf1 - log1p(k):log_cdf2 - log1p(k);
    }
    
    //log CCDF
    real jsep_lccdf (real y, real mu, real sigma, real alpha, real beta)
    {
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma);
      if(alpha<=0)
        reject ("alpha<=0, found alpha = ", alpha);
      if (beta<=0)
        reject ("beta<=0; found beta =", beta);
      real a=alpha;
      real b=beta;
      real z=(y-mu)/sigma;
      real lk1 = lgamma(1+pow(a,-1));
      real lk2 = lgamma(1+pow(b,-1));
      real lk = lk2 - lk1;
      real k = exp(lk);
      real s1 = pow(fabs(z), a);
      real s2 = pow(fabs(z), b);
      
      return (y<mu) ? log(k+gamma_p(1/a, s1)) - log1p(k):log(k)+log1m(gamma_p(1/b, s2)) - log1p(k);
    }
    '
  }
}