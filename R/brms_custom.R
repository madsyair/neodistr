#' Neonormal as Custom Distribution in brms 
#' @name brms_custom_family
#' @import brms 
#' @param family distribution neo-normal option: "msnburr", "msnburr2a", "gmsnburr", and "jfst"
#' @param vectorize logical; if TRUE,  Stan code of family distribution is vectorize 
#' The default value of this parameter is TRUE
#' @return custom_family is an object of class customfamily of brms and stanvars_family is stanvars object, the stan's code of function of neo-normal distributions (lpdf,cdf,lcdf,lccdf,quantile and rng) 
#' @author Achmad Syahrul Choir
#' @examples
#' \dontrun{
#'   library(brms)
#'   library(neodistr)
#'   x<-runif(100)
#'   e<-rmsnburr(100,0,1,0.8)
#'   y<-0.5+0.8*x+e
#'   data<-data.frame(y,x)
#'   msnburr<-brms_custom_family("msnburr")
#'   fit <- brm(
#'     y ~ x, data = data,
#'     family = msnburr$custom_family, stanvars = msnburr$stanvars_family,
#'     prior=c(set_prior("cauchy(0,5)",class="alpha"),set_prior("cauchy(0,1)",class="sigma"))
#'   )
#'   summary(fit)
#'   expose_functions(fit, vectorize = TRUE)
#'   pp <- posterior_predict(fit)
#'   ppe <- posterior_epred(fit)
#'   loo(fit)
#'   }
#' @export
brms_custom_family<- function(family="msnburr",vectorize=TRUE){
  .<-gmsnburr_lpdf<-NULL
  .<-posterior_predict_msnburr<-posterior_predict_msnburr2a<-posterior_predict_gmsnburr<-posterior_predict_jfst<-NULL
  .<-log_lik_msnburr<-log_lik_msnburr2a<-log_lik_gmsnburr<-log_lik_jfst<-NULL
  .<-posterior_epred_gmsnburr<-posterior_epred_msnburr<-posterior_epred_msnburr2a<-posterior_epred_jfst<-NULL
  if(family=="msnburr"){   # MSNBurr
    #create stan function code of neo-normal distribution 
       stan_funs_family<-stanf_msnburr(vectorize=vectorize)
       
    log_lik_msnburr <- function(i, prep) {
      .<-msnburr_lpdf<-NULL
      mu <- brms::get_dpar(prep, "mu", i = i)
      sigma <- brms::get_dpar(prep, "sigma")
      alpha <- brms::get_dpar(prep, "alpha")      
      y <- prep$data$Y[i]
      msnburr_lpdf(y, mu, sigma, alpha)
    }
    posterior_predict_msnburr <- function(i, prep, ...) {
      .<-msnburr_rng<-NULL
      mu <- brms::get_dpar(prep, "mu", i = i)
      sigma <- brms::get_dpar(prep, "sigma")
      alpha <- brms::get_dpar(prep, "alpha")   
      msnburr_rng(mu, sigma,alpha)
    }
   
    posterior_epred_msnburr <- function(prep) {
      mu <- brms::get_dpar(prep, "mu")
      alpha <- brms::get_dpar(prep, "alpha") 
      sigma <- brms::get_dpar(prep, "sigma") 
      summary_dist("msnburr",par=c(mu=mu,sigma=sigma,alpha=alpha))$Mean
    }
    
    neonormal_family <- function(vectorize=TRUE) {
      loop<-ifelse(vectorize,FALSE,TRUE)
      brms::custom_family(
        "msnburr",
        dpars = c("mu", "sigma","alpha"),
        links = c("identity","log","log"),
        type = "real",
        lb=c(NA,0,0),
        ub=c(NA,NA,NA),
        #          vars="vreal",
        loop=loop,
        log_lik = log_lik_msnburr,
        posterior_predict = posterior_predict_msnburr,
        posterior_epred = posterior_epred_msnburr
      )
    }
    custom_neonormal<-neonormal_family(vectorize=vectorize)
  }else if(family=="msnburr2a"){ # MSNBurr-IIa
    #create stan function code of neo-normal distribution 
    stan_funs_family<-stanf_msnburr2a(vectorize=vectorize)
    log_lik_msnburr2a <- function(i, prep) {
      .<-msnburr2a_lpdf<-NULL
      mu <- brms::get_dpar(prep, "mu", i = i)
      sigma <- brms::get_dpar(prep, "sigma")
      alpha <- brms::get_dpar(prep, "alpha")      
      y <- prep$data$Y[i]
      msnburr2a_lpdf(y, mu, sigma, alpha)
    }
    posterior_predict_msnburr2a <- function(i, prep, ...) {
      .<-msnburr2a_rng<-NULL
      mu <- brms::get_dpar(prep, "mu", i = i)
      sigma <- brms::get_dpar(prep, "sigma")
      alpha <- brms::get_dpar(prep, "alpha")   
      msnburr2a_rng(mu, sigma,alpha)
    }
    posterior_epred_msnburr2a <- function(prep)  {
      mu <- brms::get_dpar(prep, "mu")
      alpha <- brms::get_dpar(prep, "alpha") 
      sigma <- brms::get_dpar(prep, "sigma") 
      
      summary_dist("msnburr2a",par=c(mu=mu,sigma=sigma,alpha=alpha))$Mean
      
    }
    neonormal_family <- function(vectorize=TRUE) {
      loop<-ifelse(vectorize,FALSE,TRUE)
      custom_family(
        "msnburr2a",
        dpars = c("mu", "sigma","alpha"),
        links = c("identity","log","log"),
        lb=c(NA,0,0),
        ub=c(NA,NA,NA),
        type = "real",
        loop=loop,
        log_lik = log_lik_msnburr2a,
        posterior_predict = posterior_predict_msnburr2a,
        posterior_epred = posterior_epred_msnburr2a
      )
    }
    custom_neonormal<-neonormal_family(vectorize=vectorize)
  } else if(family=="gmsnburr"){ #GMSNBurr
     stan_funs_family<-stanf_gmsnburr(vectorize=vectorize)
    log_lik_gmsnburr <- function(i, prep){
      .<-gmsnburr_lpdf<-NULL
      mu <- brms::get_dpar(prep, "mu", i = i)
      sigma <- brms::get_dpar(prep, "sigma")
      alpha <- brms::get_dpar(prep, "alpha")     
      beta <- brms::get_dpar(prep, "beta")   
      y <- prep$data$Y[i]
      gmsnburr_lpdf(y, mu, sigma, alpha,beta)
    }
    posterior_predict_gmsnburr <- function(i, prep, ...) {
      .<-gmsnburr_rng<-NULL
      mu <- brms::get_dpar(prep, "mu", i = i)
      sigma <- brms::get_dpar(prep, "sigma")
      alpha <- brms::get_dpar(prep, "alpha")  
      beta <- brms::get_dpar(prep, "beta")  
      gmsnburr_rng(mu, sigma,alpha,beta)
    }
    posterior_epred_gmsnburr <- function(prep) { 
      mu <- brms::get_dpar(prep, "mu")
      sigma <- brms::get_dpar(prep, "sigma") 
      alpha <- brms::get_dpar(prep, "alpha") 
      beta <- brms::get_dpar(prep, "beta") 
      summary_dist("gmsnburr",par=c(mu=mu,sigma=sigma,alpha=alpha,beta=beta))$Mean
    }
    neonormal_family <- function(vectorize=TRUE) {
      loop<-ifelse(vectorize,FALSE,TRUE)
      custom_family(
        "gmsnburr",
        dpars = c("mu", "sigma","alpha","beta"),
        links = c("identity","log","log","log"),
        type = "real",
        lb=c(NA,0,0,0),
        ub=c(NA,NA,NA,NA),
        #        vars="vreal",
        loop=loop,
        log_lik = log_lik_gmsnburr,
        posterior_predict = posterior_predict_gmsnburr,
        posterior_epred = posterior_epred_gmsnburr
      )
    }
    custom_neonormal<-neonormal_family(vectorize=vectorize)
  } else if(family=="jfst"){ # Jones and Faddy Skew-t
    
    stan_funs_family<-stanf_jfst(vectorize=vectorize)
    log_lik_jfst <- function(i, prep) {
      .<-jfst_lpdf<-NULL
      mu <- brms::get_dpar(prep, "mu")
      sigma <- brms::get_dpar(prep, "sigma")
      alpha <- brms::get_dpar(prep, "alpha")     
      beta <- brms::get_dpar(prep, "beta")   
      y <- prep$data$Y[i]
      jfst_lpdf(y, mu, sigma, alpha,beta,log=TRUE)
    }
    posterior_predict_jfst <- function(i, prep, ...) {
      .<-jfst_rng<-NULL
      mu <- brms::get_dpar(prep, "mu", i = i)
      sigma <- brms::get_dpar(prep, "sigma")
      alpha <- brms::get_dpar(prep, "alpha")  
      beta <- brms::get_dpar(prep, "beta")  
      jfst_rng(mu, sigma,alpha,beta)
    }
    posterior_epred_jfst <- function(prep) {
      mu <- brms::get_dpar(prep, "mu")
      sigma <- brms::get_dpar(prep, "sigma") 
      alpha <- brms::get_dpar(prep, "alpha") 
      beta <- brms::get_dpar(prep, "beta") 
      summary_dist("jfst",par=c(mu=mu,sigma=sigma,alpha=alpha,beta=beta))$Mean
    }
    neonormal_family <- function(vectorize=TRUE) {
      loop<-ifelse(vectorize,FALSE,TRUE)
      custom_family(
        "jfst",
        dpars = c("mu", "sigma","alpha","beta"),
        links = c("identity","log","identity","log"),
        type = "real",
        lb=c(NA,0,NA,0),
        ub=c(NA,NA,NA,NA),
        loop=loop,
        log_lik = log_lik_jfst,
        posterior_predict = posterior_predict_jfst,
        posterior_epred = posterior_epred_jfst
      )
    }
    custom_neonormal<-neonormal_family(vectorize=vectorize)
  }
  
  stanvars_family <- stanvar(scode = stan_funs_family, block = "functions")
  
  
  return(list(custom_family=custom_neonormal,stanvars_family=stanvars_family))
  
  
}


