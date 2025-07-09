#' Stan function of MSNBurr-IIa Distribution
#' @name stanf_msnburr2a
#'
#' @description
#' Stan code of MSNBurr-IIa distribution for custom distribution in stan
#'
#' @param vectorize logical; if TRUE, Vectorize Stan code of MSNBurr-IIa distribution are given 
#' The default value of this parameter is TRUE
#' @param rng logical; if TRUE,  Stan code of quantile and 
#' random number generation of MSNBurr-IIa distribution are given 
#' The default value of this parameter is TRUE
#' @author Achmad Syahrul Choir and Nur Iriawan
#' @return
#' \code{msnburr_lpdf} gives the log of density, \code{msnburr_cdf} gives the distribution
#' function, \code{msnburr_lcdf} gives the log of distribution function, \code{msnburr_lccdf} gives the complement of log distributionfunction (1-msnburr_lcdf),
#'  and \code{msnburr_rng} generates
#' random deviates.
#' 
#'@details
#'  MSNBurr-IIa Distribution has density function:
#' \deqn{f(y |\mu,\sigma,\alpha)=\frac{\omega}{\sigma}\exp{\left(\omega{\left(\frac{y-\mu}{\sigma}\right)}\right)}{{\left(1+\frac{\exp{\left(\omega{(\frac{y-\mu}{\sigma})}\right)}}{\alpha}\right)}^{-(\alpha+1)}}}
#' where \eqn{-\infty < y < \infty, -\infty < \mu< \infty, \sigma>0, \alpha>0, 
#' \omega = \frac{1}{\sqrt{2\pi}} {\left(1+\frac{1}{\alpha}\right)^{\alpha+1}}}
#'This function gives stan code  of log density, cumulative distribution, log of cumulative distribution, log complementary cumulative distribution, quantile, random number of MSNBurr-IIa distribution 


#' @references 
#' Choir, A. S. (2020). The New Neo-Normal Distributions and their Properties. Dissertation. Institut Teknologi Sepuluh Nopember.
#' 
#' @examples 
#' \dontrun{
#' library (neodistr)
#' library(rstan)
#' #inputting data
#' set.seed(136)
#' dt <- neodistr::rmsnburr2a(100,0,1,0.5) # random generating MSNBurr-IIA data 
#' dataf <- list(
#'   n = 100,
#'   y = dt
#' )
#' #### not vector  
#' ##Calling the function of the neo-normal distribution that is available in the package.
#' func_code<-paste(c("functions{",neodistr::stanf_msnburr2a(vectorize=FALSE),"}"),collapse="\n")
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
#'  y[i]~msnburr2a(mu,sigma,alpha);
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
#' func_code_vector<-paste(c("functions{",neodistr::stanf_msnburr2a(vectorize=TRUE),"}"),collapse="\n")
#' # define stan model as vector
#' model_vector<-"
#' data {
#'   int<lower=1> n;
#'   vector[n] y;
#' }
#' parameters {
#'   real mu;
#'   real <lower=0> sigma;
#'   real  alpha;
#' }
#' model {
#'   y~msnburr2a(rep_vector(mu,n),sigma,alpha);
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
#'   cores = 2              # number of cores (could use one per chain)
#' )
#' 
#' # Showing the estimation results of the parameters that were executed using the Stan file
#' print(fit2, pars=c("mu", "sigma", "alpha",  "lp__"), probs=c(.025,.5,.975))
#' 
#' }

#' @export
stanf_msnburr2a<-function(vectorize=TRUE,rng=TRUE){
  if(vectorize){
dist<-  'real msnburr2a_lpdf(vector y,  vector mu, real sigma,real alpha) {
  // msnburr2a log pdf
  int N = rows(y);
  real omega;
  vector[N] lp;
  vector[N] zo;
   if (alpha<=0)
    reject("alpha<=0; found alpha =", alpha);
  if (sigma<=0)
    reject("sigma<=0; found sigma =", sigma);
  omega=(1/sqrt(2*pi()))*pow(1+(1/alpha),(alpha+1));
  zo=-omega*((y-mu)/sigma);
  lp=rep_vector((log(omega)-log(sigma)+(alpha+1)*log(alpha)),N)+(alpha*zo)-((alpha+1)*log1p(alpha*exp(zo)));
   for (i in 1:N){
    if(is_inf(zo[i])==1){
    lp[i]=negative_infinity();
    }
    }
    return sum(lp);
  //return N*log(omega)-N*log(sigma)+N*(alpha+1)*log(alpha)+sum(alpha*zo)-sum((alpha+1)*log1p(alpha*exp(zo)));
 //return N*log(omega)-N*log(sigma)+sum(zo)-sum((alpha+1)*log1p_exp(zoa));
 
  }

  //msnburr2a cdf
  real msnburr2a_cdf(vector y, vector mu, real sigma,real alpha) {
    int N = rows(y);
    real omega;
    vector[N] zoa;
  
    if (alpha<=0)
      reject("alpha<=0; found alpha =", alpha);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    omega=(1/sqrt(2*pi()))*pow(1+(1/alpha),(alpha+1));
    zoa=(omega*((y-mu)/sigma)-log(alpha));
    return exp(sum(log1m_exp(-alpha*log1p_exp(zoa))));
   }
  
  // msnburr2a log cdf
  real msnburr2a_lcdf(vector y, vector mu, real sigma,real alpha ) {
    int N = rows(y);
    real omega;
    vector[N] zoa;
    if (alpha<=0)
      reject("alpha<=0; found alpha =", alpha);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    omega=(1/sqrt(2*pi()))*pow(1+(1/alpha),(alpha+1));
    zoa=(omega*((y-mu)/sigma)-log(alpha));
    return  (sum(log1m_exp(-alpha*log1p_exp(zoa))));
    
  }
  
  // msnburr2a log ccdf
  real msnburr2a_lccdf(vector y, vector mu, real sigma,real alpha) {
    int N = rows(y);
    real omega;
    vector[N] zoa;
    if (alpha<=0)
      reject("alpha<=0; found alpha =", alpha);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    omega=(1/sqrt(2*pi()))*pow(1+(1/alpha),(alpha+1));
    zoa=(omega*((y-mu)/sigma)-log(alpha));
    return (sum(-alpha*log1p_exp(zoa)));
    }'
}else{
  dist<-
    'real msnburr2a_lpdf(real y,  real mu, real sigma,real alpha) {
  // msnburr2a log pdf
  real omega;
  real zo;
  real zoa; 
  real lp;
  if (alpha<=0)
    reject("alpha<=0; found alpha =", alpha);
  if (sigma<=0)
    reject("sigma<=0; found sigma =", sigma);
  omega=(1/sqrt(2*pi()))*pow(1+(1/alpha),(alpha+1));
   zo=-omega*((y-mu)/sigma);
   if(is_inf(zo)==1){
   lp=negative_infinity();
   }else{
   lp=log(omega)-log(sigma)+(alpha+1)*log(alpha)+(alpha*zo)-((alpha+1)*log1p(alpha*exp(zo)));
   }
   return lp;
 // return log(omega)-log(sigma)+(alpha+1)*log(alpha)+(alpha*zo)-((alpha+1)*log1p(alpha*exp(zo)));
      }

  //msnburr2a cdf
  real msnburr2a_cdf(real y, real mu, real sigma,real alpha) {
     real omega;
    real zoa;
  
    if (alpha<=0)
      reject("alpha<=0; found alpha =", alpha);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    omega=(1/sqrt(2*pi()))*pow(1+(1/alpha),(alpha+1));
    zoa=(omega*((y-mu)/sigma)-log(alpha));
    return exp(log1m_exp(-alpha*log1p_exp(zoa)));
   }
  
  // msnburr2a log cdf
  real msnburr2a_lcdf(real y, real mu, real sigma,real alpha ) {
    real omega;
    real zoa;
    if (alpha<=0)
      reject("alpha<=0; found alpha =", alpha);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    omega=(1/sqrt(2*pi()))*pow(1+(1/alpha),(alpha+1));
    zoa=(omega*((y-mu)/sigma)-log(alpha));
    return  log1m_exp(-alpha*log1p_exp(zoa));
    
  }
  
  // msnburr2a log ccdf
  real msnburr2a_lccdf(real y, real mu, real sigma,real alpha) {
 
    real omega;
    real zoa;
    if (alpha<=0)
      reject("alpha<=0; found alpha =", alpha);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    omega=(1/sqrt(2*pi()))*pow(1+(1/alpha),(alpha+1));
    zoa=(omega*((y-mu)/sigma)-log(alpha));
    return ((-alpha*log1p_exp(zoa)));
    }
  '

}
qr<- 
'  //msnburr2a quantile
  real msnburr2a_quantile (real p, real mu, real sigma, real alpha){
    real omega;
      if (alpha<=0)
        reject("alpha<=0; found alpha =", alpha);
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma);
      if(p < 0||p > 1)
        reject("p<0 or p>1, found p = ", p);
      omega = (1/sqrt(2*pi()))*(1+(1/alpha))^(alpha+1);
      return mu+(sigma/omega)*(log(alpha)+log(pow((1-p),(-1/alpha))-1));
      
  }
  
  //msnburr2a_rng
  real msnburr2a_rng(real mu, real sigma,real alpha) {
    if (alpha<=0)
      reject("alpha<=0; found alpha =", alpha);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    return msnburr2a_quantile(uniform_rng(0,1), mu, sigma, alpha);
  }'
if(rng){
  paste0(dist,qr)
}else{
  dist 
}
}

