# These functions is translated from ST5 in gamlss.dist that was written by Bob Rigby and Mikis Stasinopoulos

#' Stan function of Jones and Faddys Skew-t Distribution 

#' @name stanf_jfst
#' @description
#' Stan code of JFST distribution for custom distribution in stan
#' @param vectorize logical; if TRUE, Vectorize Stan code of Jones and faddy distribution are given 
#' The default value of this parameter is TRUE
#' @param rng logical; if TRUE,  Stan code of quantile and 
#' random number generation of Jones and faddy distribution are given 
#' The default value of this parameter is TRUE
#' @return
#' \code{jfst_lpdf} gives stan's code of the log of density, \code{jfst_cdf} gives stan's code of the distribution
#' function, \code{jfst_lcdf} gives stan's code of the log of distribution function and \code{jfst_rng} gives stan's code of generates
#' random numbers.
#' @author Anisa' Faoziah and Achmad Syahrul Choir
#' @details
#'  Jones-Faddy’s Skew-t distribution has density:
#' \deqn{f(y |\mu,\sigma,\beta,\alpha)= \frac{c}{\sigma} {\left[{1+\frac{z}{\sqrt{\alpha+\beta+z^2}}}\right]}^{\alpha+\frac{1}{2}}
#' {\left[{1-\frac{z}{\sqrt{\alpha+\beta+z^2}}}\right]}^{\beta+\frac{1}{2}}}
#' where \eqn{-\infty<y<\infty,  -\infty<\mu<\infty,  \sigma>0, \alpha>0, \beta>0,} 
#' \eqn{z =\frac{y-\mu}{\sigma} }, \eqn{ c = {\left[2^{\left(\alpha+\beta-1\right)} {\left(\alpha+\beta\right)^{\frac{1}{2}}} B(a,b)\right]}^{-1} },
#'  
#' This function gives stan code  of log density, cumulative distribution, log of cumulatif distribution, log complementary cumulative distribution,
#'  quantile, random number of Jones-Faddy's Skew-t distribution
#' @references 
#' Jones, M.C. and Faddy, M. J. (2003) A skew extension of the t distribution, with applications. Journal of the Royal Statistical Society, Series B, 65, pp 159-174
#'
#' Rigby, R.A. and Stasinopoulos, M.D. and Heller, G.Z. and De Bastiani, F. (2019) Distributions for Modeling Location, Scale, and Shape: Using GAMLSS in R.CRC Press
#' @examples
#' \donttest{
#' library (neodistr)
#' library (rstan)
#' 
#' # inputting data
#' set.seed(400)
#' dt <- neodistr::rjfst(100,mu=0, sigma=1, alpha = 2, beta = 2) # random generating JFST data
#' dataf <- list(
#'  n = 100,
#'  y = dt
#'  )
#'  
#'  
#' #### not vector
#' ## Calling the function of the neo-normal distribution that is available in the package.
#' func_code<-paste(c("functions{",neodistr::stanf_jfst(vectorize=FALSE),"}"),collapse="\n")
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
#'       for(i in 1 : n){
#'       y[i] ~ jfst(mu,sigma, alpha, beta);
#'       }
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
#' func_code_vector<-paste(c("functions{",neodistr::stanf_jfst(vectorize=TRUE),"}"),collapse="\n")
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
#'       y ~ jfst(rep_vector(mu,n),sigma, alpha, beta);
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

stanf_jfst<-function(vectorize=TRUE,rng=TRUE){
  if(vectorize){
dist<-'real jfst_lpdf(vector y, vector mu, real sigma, real alpha, real beta)
{
  int N=rows(y);
//  real nu;
//  real lam;
  real a;
  real b;
  vector[N] z;
  vector[N] rz;
  real result;
   if (sigma<=0)
    reject("sigma<=0; found sigma =", sigma);
  if(alpha<=0)
    reject ("alpha<=0, found beta = ", alpha);
  if (beta<=0)
    reject ("beta<=0; found beta =", beta);
//     nu =2/beta;
//    lam=2*alpha/(beta*sqrt(2*beta+alpha*alpha));
//      a =(nu+lam)/2;
//      b = (nu-lam)/2;
a=alpha;
b=beta;
    z=(y-mu)/sigma;
      rz=z./(sqrt(a+b+(z.*z)));
    for (i in 1:N){
      if(is_inf(z[i])==1){
   //   if(z[i]<0){
    //  rz[i]=-1;
  //    }else{
    //  rz[i]=1;
   //   }
    rz[i]=z[i]<0?-1:1;
      }
    }
   return (a+0.5)*sum(log1p(rz))+(b+0.5)*sum(log1m(rz))-N*((a+b-1)*log(2))-N*(0.5*log(a+b))-N*lbeta(a,b)-N*log(sigma);
}

//CDF
real jfst_cdf(vector y, vector mu, real sigma, real alpha, real beta)
{
  int N=rows(y);
//  real nu;
//  real lam;
  real  a;
  real  b;  
  vector[N] z;
  vector[N] r;
    vector[N] rz;
   if (sigma<=0)
    reject("sigma<=0; found sigma =", sigma);
  if(alpha<=0)
    reject ("alpha<=0, found alpha = ", alpha);
  if (beta<=0)
    reject ("beta<=0; found beta =", beta);
    z=(y-mu)/sigma;
//     nu =2/beta;
//    lam=2*alpha/(beta*sqrt(2*beta+alpha*alpha));
//      a =(nu+lam)/2;
//      b = (nu-lam)/2;
      a=alpha;
      b=beta;
  //menggunakan element waist
  
 rz=z./(sqrt(a+b+(z.*z)));
      for (i in 1:N){
      if(is_inf(z[i])==1){
   //   if(z[i]<0){
    //  rz[i]=-1;
  //    }else{
    //  rz[i]=1;
   //   }
    rz[i]=z[i]<0?-1:1;
      }
    }
   r = 0.5*(1 + rz);
  
  return exp(beta_lcdf(r|a,b));
}

//Log CDF
real jfst_lcdf(vector y, vector mu, real sigma, real alpha, real beta)
{
  int N=rows(y);
//  real nu;
//  real lam;
  real  a;
  real  b;
  vector[N] z;
  vector[N] r;
    vector[N] rz;
   if (sigma<=0)
    reject("sigma<=0; found sigma =", sigma);
  if(alpha<=0)
    reject ("alpha<=0, found alpha = ", alpha);
  if (beta<=0)
    reject ("beta<=0; found beta =", beta);
//     nu =2/beta;
//    lam=2*alpha/(beta*sqrt(2*beta+alpha*alpha));
//      a =(nu+lam)/2;
//      b = (nu-lam)/2;
a=alpha;
b=beta;
      z=(y-mu)/sigma;
  
 rz=z./(sqrt(a+b+(z.*z)));
      for (i in 1:N){
      if(is_inf(z[i])==1){
   //   if(z[i]<0){
    //  rz[i]=-1;
  //    }else{
    //  rz[i]=1;
   //   }
    rz[i]=z[i]<0?-1:1;
      }
    }
   r = 0.5*(1 + rz);
  
  return beta_lcdf(r|a,b);
}

//log CCDF
real jfst_lccdf (vector y, vector mu, real sigma, real alpha, real beta)
{
  int N=rows(y);
//  real nu;
//  real lam;
  real  a;
  real  b;
  vector[N] z;
  vector[N] r;
  vector[N] rz;
   if (sigma<=0)
    reject("sigma<=0; found sigma =", sigma);
  if(alpha<=0)
    reject ("alpha<=0, found alpha = ", alpha);
  if (beta<=0)
    reject ("beta<=0; found beta =", beta);
//     nu =2/beta;
//    lam=2*alpha/(beta*sqrt(2*beta+alpha*alpha));
//      a =(nu+lam)/2;
//      b = (nu-lam)/2;
    a=alpha;
    b=beta;
      z=(y-mu)/sigma;
  rz=z./(sqrt(a+b+(z.*z)));
      for (i in 1:N){
      if(is_inf(z[i])==1){
   //   if(z[i]<0){
    //  rz[i]=-1;
  //    }else{
    //  rz[i]=1;
   //   }
    rz[i]=z[i]<0?-1:1;
      }
    }
   r = 0.5*(1 + rz);
  return beta_lccdf(r|a,b);
}
'
  }else{
    dist<-'real jfst_lpdf(real y, real mu, real sigma, real alpha, real beta)
{
//  real nu;
//  real lam;
  real a;
  real b;
  real z;
  real rz;
   if (sigma<=0)
    reject("sigma<=0; found sigma =", sigma);
  if(alpha<=0)
    reject ("alpha<=0, found alpha = ", alpha);
  if (beta<=0)
    reject ("beta<=0; found beta =", beta);
//     nu =2/beta;
//    lam=2*alpha/(beta*sqrt(2*beta+alpha*alpha));
//      a =(nu+lam)/2;
//      b = (nu-lam)/2;
a=alpha;
b=beta;
  z=(y-mu)/sigma;
      rz=z/(sqrt(a+b+(z*z)));
  if(is_inf(z)==1){
  // if(z<0){
  //  rz=-1;
  //  }else{
  //  rz=1;
   // }
   rz=z<0?-1:1;
  }
return (a+0.5)*(log1p(rz))+(b+0.5)*(log1m(rz))-((a+b-1)*log(2))-(0.5*log(a+b))-lbeta(a,b)-log(sigma);
}

//CDF
real jfst_cdf(real y, real mu, real sigma, real alpha, real beta)
{
//  real nu;
//  real lam;
  real  a;
  real  b;  
  real z;
  real r;
    real rz;
   if (sigma<=0)
    reject("sigma<=0; found sigma =", sigma);
  if(alpha<=0)
    reject ("alpha<=0, found alpha = ", alpha);
  if (beta<=0)
    reject ("beta<=0; found beta =", beta);
    z=(y-mu)/sigma;
//     nu =2/beta;
//    lam=2*alpha/(beta*sqrt(2*beta+alpha*alpha));
//      a =(nu+lam)/2;
//      b = (nu-lam)/2;
a=alpha;
b=beta;
    rz=z/(sqrt(a+b+(z*z)));
 if(is_inf(z)==1){
  // if(z<0){
  //  rz=-1;
  //  }else{
  //  rz=1;
   // }
   rz=z<0?-1:1;
  }
     r = 0.5*(1 + rz);
  return exp(beta_lcdf(r|a,b));
}

//Log CDF
real jfst_lcdf(real y, real mu, real sigma, real alpha, real beta)
{
//  real nu;
//  real lam;
  real  a;
  real  b;
  real z;
  real r;
    real rz;
   if (sigma<=0)
    reject("sigma<=0; found sigma =", sigma);
  if(alpha<=0)
    reject ("alpha<=0, found alpha = ", alpha);
  if (beta<=0)
    reject ("beta<=0; found beta =", beta);
//     nu =2/beta;
//    lam=2*alpha/(beta*sqrt(2*beta+alpha*alpha));
//      a =(nu+lam)/2;
//      b = (nu-lam)/2;
a=alpha;
b=beta;
      z=(y-mu)/sigma;
       rz=z/(sqrt(a+b+(z*z)));
  if(is_inf(z)==1){
  // if(z<0){
  //  rz=-1;
  //  }else{
  //  rz=1;
   // }
   rz=z<0?-1:1;
  }
     r = 0.5*(1 + rz);
  return beta_lcdf(r|a,b);
}

//log CCDF
real jfst_lccdf (real y, real mu, real sigma, real alpha, real beta)
{
//  real nu;
//  real lam;
  real  a;
  real  b;
  real z;
  real r;
  real rz;
   if (sigma<=0)
    reject("sigma<=0; found sigma =", sigma);
  if(alpha<=0)
    reject ("alpha<=0, found alpha = ", alpha);
  if (beta<=0)
    reject ("beta<=0; found beta =", beta);
//     nu =2/beta;
//    lam=2*alpha/(beta*sqrt(2*beta+alpha*alpha));
//      a =(nu+lam)/2;
//      b = (nu-lam)/2;
a=alpha;
b=beta;
      z=(y-mu)/sigma;
  rz=z/(sqrt(a+b+(z*z)));
  if(is_inf(z)==1){
  // if(z<0){
  //  rz=-1;
  //  }else{
  //  rz=1;
   // }
   rz=z<0?-1:1;
  }
      r = 0.5*(1 + rz);
  return beta_lccdf(r|a,b);
}
'
  }
qr<-'
//quantile
real jfst_quantile(real p, real mu, real sigma, real alpha, real beta)
{
//  real nu;
//  real lam;
  real  a;
  real  b;
  real Balpha;
  real zalpha;
  if(sigma<=0)
    reject("sigma<=0, found sigma = ", sigma);
  if(alpha<=0)
    reject ("alpha<=0, found alpha = ", alpha);
  if(beta<=0)
    reject ("beta<=0, found beta = ", beta);
  if(p < 0||p > 1)
    reject("p<0 or p>1, found p = ", p);
//     nu =2/beta;
//    lam=2*alpha/(beta*sqrt(2*beta+alpha*alpha));
//      a =(nu+lam)/2;
//      b = (nu-lam)/2;
a=alpha;
b=beta;
  Balpha = inv_inc_beta(a,b,p);
  zalpha = (sqrt(a+b))*(2*Balpha-1)/(2*sqrt(Balpha*(1-Balpha)));
  return mu + sigma*zalpha;
  
}

// RNG --> Random Number Generator (RNG)
  real jfst_rng(real mu, real sigma, real alpha, real beta)
  {
  //real rb;
  //real t;
  //  real u;
   // real v;
  //  u=chi_square_rng(2*alpha); 
  //  v=chi_square_rng(2*beta);
  //return (sqrt(alpha+beta)*(u-v))/(2*sqrt(u*v));
  //    rb=beta_rng(alpha,beta);
  //  t=sqrt(alpha+beta)*(2*rb-1)/(2*sqrt(rb*(1-rb)));
  //  return  mu + sigma * t;
  //p = uniform_rng(0,1);
    return jfst_quantile(uniform_rng(0,1),mu,sigma, alpha, beta);
  }
  '
if(rng){
  paste0(dist,qr)
}else{
  dist 
}
}
