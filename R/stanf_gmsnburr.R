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
stanf_gmsnburr<-function(vectorize =TRUE,rng=TRUE){
  if(vectorize ){
    dist<-
  'real gmsnburr_lpdf(vector y, vector mu,real sigma,real alpha, real beta) {
    // gmsnburr log pdf
     if (alpha<=0)
      reject("alpha<=0; found alpha =", alpha);
    if (beta<=0)
      reject("beta<=0; found beta =", beta);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    int N = rows(y);
    vector[N] lp;
    real lomega=-0.5*log(2*pi())+lbeta(alpha,beta)-beta*(log(beta/alpha))+(alpha+beta)*log1p(beta/alpha);
    real omega=exp(lomega);
    //vector[N] epart=exp(-omega*((y-mu)/sigma));
    vector[N] zo=-omega*((y-mu)/sigma);
    vector[N] zoa=zo+log(beta)-log(alpha);
 
    lp=rep_vector((lomega-log(sigma)+beta*(log(beta)-log(alpha))-lbeta(alpha,beta)),N)+beta*zo-((alpha+beta)*log1p_exp(zoa));
//    lp=rep_vector(lomega-log(sigma)+beta*(log(beta/alpha))-N*lbeta(alpha,beta),N)-(beta*omega((y-mu)/sigma))-((alpha+beta)*log1p((beta/alpha)*epart));

    for (i in 1:N){
    if(is_inf(zo[i])==1){
    lp[i]=negative_infinity();
    }
    }
    return sum(lp);
// return N*lomega-N*log(sigma)+N*beta*(log(beta)-log(alpha))+sum(beta*zo)-sum((alpha+beta)*log1p_exp(zoa))-N*lbeta(alpha,beta);
 //    return N*lomega-N*log(sigma)+N*beta*(log(beta/alpha))-sum(beta*omega*((y-mu)/sigma))-sum((alpha+beta)*log1p((beta/alpha)*epart))-N*lbeta(alpha,beta);

  }
  
  //gmsnburr cdf
  real gmsnburr_cdf(vector y, vector mu, real sigma,real alpha,real beta) {
    int N = rows(y);
    real lomega;
    real omega;
    if (alpha<=0)
      reject("alpha<=0; found alpha =", alpha);
    if (beta<=0)
      reject("beta<=0; found beta =", beta);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    lomega=-0.5*log(2*pi())+lbeta(alpha,beta)-beta*(log(beta)-log(alpha))+(alpha+beta)*log1p(beta/alpha);
    omega=exp(lomega);
   
    vector[N] zo=-omega*((y-mu)/sigma);
    vector[N] zoa=zo+log(beta)-log(alpha);
    
    vector[N] ep=exp(-log1p_exp(zoa));
      
    return exp(beta_lcdf(ep|alpha,beta));
  }
  
  //gmsnburr log cdf
  real gmsnburr_lcdf(vector y, vector mu, real sigma,real alpha,real beta) {
    // generalised gmsnburr log cdf
    int N = rows(y);
    real lomega;
    real omega;
    if (alpha<=0)
      reject("alpha<=0; found alpha =", alpha);
    if (beta<=0)
      reject("beta<=0; found beta =", beta);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    lomega=-0.5*log(2*pi())+lbeta(alpha,beta)-beta*(log(beta)-log(alpha))+(alpha+beta)*log1p(beta/alpha);
    omega=exp(lomega);
   // epart=exp(-omega*((y-mu)/sigma));
    //for (i in 1:N){
    //  ep=rep_vector(1,N)./(1+(beta/alpha)*epart);
  
      //}
      
    vector[N] zo=-omega*((y-mu)/sigma);
    vector[N] zoa=zo+log(beta)-log(alpha);
    
    vector[N] ep=exp(-log1p_exp(zoa));
  
    return (beta_lcdf(ep|alpha,beta));
  }
  
  //gmsnburr log ccdf
  real gmsnburr_lccdf(vector y, vector mu, real sigma,real alpha,real beta) {
    int N = rows(y);
    real lomega;
    real omega;
  //  vector[N] epart;
  //  vector[N] ep;
    if (alpha<=0)
      reject("alpha<=0; found alpha =", alpha);
    if (beta<=0)
      reject("beta<=0; found beta =", beta);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    lomega=-0.5*log(2*pi())+lbeta(alpha,beta)-beta*(log(beta)-log(alpha))+(alpha+beta)*log1p(beta/alpha);
    omega=exp(lomega);
    //epart=exp(-omega*((y-mu)/sigma));
    //for (i in 1:N){
    //  ep=rep_vector(1,N)./(1+(beta/alpha)*epart);
  
      //}
      
    vector[N] zo=-omega*((y-mu)/sigma);
    vector[N] zoa=zo+log(alpha)-log(beta);
    
    vector[N] ep=exp(-log1p_exp(zoa));
  
    return (beta_lccdf(ep|alpha,beta));
  }'
  }else{
  dist<-'real gmsnburr_lpdf(real y, real mu,real sigma,real alpha, real beta) {
    // gmsnburr log pdf
  real lp;
    real lomega=-0.5*log(2*pi())+lbeta(alpha,beta)-beta*(log(beta/alpha))+(alpha+beta)*log1p(beta/alpha);
    real omega=exp(lomega);
    real zo=-omega*((y-mu)/sigma);
    real zoa=zo+log(beta)-log(alpha);
    if (alpha<=0)
      reject("alpha<=0; found alpha =", alpha);
    if (beta<=0)
      reject("beta<=0; found beta =", beta);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
      if(is_inf(zo)==1){
        lp=negative_infinity();
      }else{
        lp=lomega-log(sigma)+beta*(log(beta)-log(alpha))+(beta*zo)-(alpha+beta)*log1p_exp(zoa)-lbeta(alpha,beta);
        }
    
    return lp;
  }
  
  //gmsnburr cdf
  real gmsnburr_cdf(real y, real mu, real sigma,real alpha,real beta) {
    real lomega;
    real omega;
    if (alpha<=0)
      reject("alpha<=0; found alpha =", alpha);
    if (beta<=0)
      reject("beta<=0; found beta =", beta);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    lomega=-0.5*log(2*pi())+lbeta(alpha,beta)-beta*(log(beta)-log(alpha))+(alpha+beta)*log1p(beta/alpha);
    omega=exp(lomega);
   
    real zo=-omega*((y-mu)/sigma);
    real zoa=zo+log(beta)-log(alpha);
    
    real ep=exp(-log1p_exp(zoa));
      
    return exp(beta_lcdf(ep|alpha,beta));
  }
  
  //gmsnburr log cdf
  real gmsnburr_lcdf(real y, real mu, real sigma,real alpha,real beta) {
    // generalised gmsnburr log cdf
    real lomega;
    real omega;
    if (alpha<=0)
      reject("alpha<=0; found alpha =", alpha);
    if (beta<=0)
      reject("beta<=0; found beta =", beta);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    lomega=-0.5*log(2*pi())+lbeta(alpha,beta)-beta*(log(beta)-log(alpha))+(alpha+beta)*log1p(beta/alpha);
    omega=exp(lomega);
   // epart=exp(-omega*((y-mu)/sigma));
    //for (i in 1:N){
    //  ep=rep_vector(1,N)./(1+(beta/alpha)*epart);
  
      //}
      
    real zo=-omega*((y-mu)/sigma);
    real zoa=zo+log(beta)-log(alpha);
    
    real ep=exp(-log1p_exp(zoa));
  
    return (beta_lcdf(ep|alpha,beta));
  }
  
  //gmsnburr log ccdf
  real gmsnburr_lccdf(real y, real mu, real sigma,real alpha,real beta) {
    real lomega;
    real omega;
  //  vector[N] epart;
  //  vector[N] ep;
    if (alpha<=0)
      reject("alpha<=0; found alpha =", alpha);
    if (beta<=0)
      reject("beta<=0; found beta =", beta);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    lomega=-0.5*log(2*pi())+lbeta(alpha,beta)-beta*(log(beta)-log(alpha))+(alpha+beta)*log1p(beta/alpha);
    omega=exp(lomega);
    //epart=exp(-omega*((y-mu)/sigma));
    //for (i in 1:N){
    //  ep=rep_vector(1,N)./(1+(beta/alpha)*epart);
  
      //}
      
    real zo=-omega*((y-mu)/sigma);
    real zoa=zo+log(beta)-log(alpha);
    
    real ep=exp(-log1p_exp(zoa));
  
    return (beta_lccdf(ep|alpha,beta));
  }'
  }
qr<-'
  //gmsnburr quantile
  real gmsnburr_quantile(real p, real mu, real sigma, real alpha, real beta){
  real omega;
  real lomega;
  //real zf;
  if (alpha<=0)
    reject("alpha<=0; found alpha =", alpha);
  if (beta<=0)
    reject("beta<=0; found beta =", beta);
  if (sigma<=0)
    reject("sigma<=0; found sigma =", sigma);
  if(p < 0||p > 1)
    reject("p<0 or p>1, found p = ", p);
    
  lomega=-0.5*log(2*pi())+lbeta(alpha,beta)-beta*(log(beta)-log(alpha))+(alpha+beta)*log1p(beta/alpha);
  omega=exp(lomega);
  real ib=inv_inc_beta(alpha, beta,  p);
  real s = (alpha/beta)*((1/ib)-1);
 // zf <-qf(p,2*beta,2*alpha,lower.tail=!lower.tail,log.p=log.p);
  return (mu-(sigma/omega)*log(s));
      
  }
  
  //gmsnburr rng
  real gmsnburr_rng(real mu, real sigma,real alpha,real beta) {
  // real lomega;
//  real omega;
  //  real logzf;
  //  real z1;
  //  real z2;
  //   real rb;
    // real z;
    if (alpha<=0)
      reject("alpha<=0; found alpha =", alpha);
    if (beta<=0)
      reject("beta<=0; found beta =", beta);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
  //  lomega=-0.5*log(2*pi())+lbeta(alpha,beta)-beta*(log(beta)-log(alpha))+(alpha+beta)*log1p(beta/alpha);
  //  omega=exp(lomega);
  //  rb=beta_rng(beta,alpha);
  //  z=(alpha*rb)/(beta*(1-rb));
  //  return (mu-(sigma/omega)*log(z))
    
    
  // z1=chi_square_rng(2*alpha)/(2*alpha);
   //z2=chi_square_rng(2*beta)/(2*beta);
   //logzf=log(z2)-log(z1);
  //  return (mu-(sigma/omega)*logzf);
    return gmsnburr_quantile(uniform_rng(0,1), mu, sigma, alpha, beta);
  }'
if(rng){
paste0(dist,qr)
}else{
dist 
}
}
