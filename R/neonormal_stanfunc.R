# Neo-normal Stan Function
#
# 
#  To call PDF, CDF,  CCDF, and RNG function for each distribution using Stan
#  
#  @param family To identify the type of distribution to be used (family = "gmsnburr", "msnburr", "msnburr2a", "jfst", "fossep")
#  @param func  Choose the function to be used (func = "PDF", "CDF", "CCDF", "RNG")
# 
#         @return \code{neonormal_stanfunc(family="gmsnburr", func="pdf")} gives the Probabillity Density Function  from GMSNBUrr distribution
#         \code{neonormal_stanfunc(family="gmsnburr", func="cdf")} gives the Cumulative Density Function from GMSNBurr distriburion
#        \code{neonormal_stanfunc(family="gmsnburr", func="ccdf")} gives the Complementary Cumulative Density Function from GMSNBurr distriburion
#         \code{neonormal_stanfunc(family="gmsnburr", func="rng")} gives the random number generator from GMSNBurr distriburion
#         \code{neonormal_stanfunc(family="msnburr", func="pdf")} gives the Probabillity Density Function from MSNBurr distriburion
#         \code{neonormal_stanfunc(family="msnburr", func="cdf")} gives the Cumulative Density Function from MSNBurr distriburion
#          \code{neonormal_stanfunc(family="msnburr", func="ccdf")} gives the Complementary Cumulative Density Function from MSNBurr distriburion
#          \code{neonormal_stanfunc(family="msnburr", func="rng")} gives the random number generator from MSNBurr distriburion
#          \code{neonormal_stanfunc(family="msnburr2a", func="pdf")} gives the Probabillity Density Function from MSNBurr-IIa distriburion
#          \code{neonormal_stanfunc(family="msnburr2a", func="cdf")} gives the Cumulative Density Function from MSNBurr-IIa distriburion
#          \code{neonormal_stanfunc(family="msnburr2a", func="ccdf")} gives the Complementary Cumulative Density Function from MSNBurr-IIa distriburion
#          \code{neonormal_stanfunc(family="msnburr2a", func="rng")} gives the random number generator from MSNBurr-IIa distriburion
#          \code{neonormal_stanfunc(family="jfst", func="pdf")} gives the Probabillity Density Function from Jones-Faddy's Skew-t distriburion
#          \code{neonormal_stanfunc(family="jfst", func="cdf")} gives the Cumulative Density Function from Jones-Faddy's Skew-t distriburion
#          \code{neonormal_stanfunc(family="jfst", func="ccdf")} gives the Complementary Cumulative Density Function from Jones-Faddy's Skew-t distriburion
#          \code{neonormal_stanfunc(family="jfst", func="rng")} gives the random number generator from Jones-Faddy's-Skew-t distriburion
#          \code{neonormal_stanfunc(family="fossep", func="pdf")} gives the Probabillity Density Function from Fernandez-Osiewalski-Steel Skew Exponential Power distriburion
#          \code{neonormal_stanfunc(family="fossep", func="cdf")} gives the Cumulative Density Function from Fernandez-Osiewalski-Steel Skew Exponential Power distriburion
#          \code{neonormal_stanfunc(family="fossep", func="ccdf")} gives the Complementary Cumulative Density Function from Fernandez-Osiewalski-Steel Skew Exponential Power distriburion
#          \code{neonormal_stanfunc(family="jsep", func="pdf")} gives the Probabillity Density Function from Jones Skew Exponential Power distribution
#          \code{neonormal_stanfunc(family="jsep", func="cdf")} gives the Cumulative Density Function from Jones Skew Exponential Power distribution
#          \code{neonormal_stanfunc(family="jsep", func="ccdf")} gives the Complementary Cumulative Density Function from Jones Skew Exponential Power distribution
#          
# 
# examples neonormal_stanfunc(family="gmsnburr", func="pdf")

#' @importFrom rstan expose_stan_functions stanc
neonormal_stanfunc<-function(family="gmsnburr",func="pdf",vectorize=TRUE){
  if (interactive() || identical(Sys.getenv("NOT_CRAN"), "true")) {

  
.<-msnburr_lpdf<-msnburr2a_lpdf<-jfst_lpdf<-gmsnburr_lpdf<-fossep_lpdf<-jsep_lpdf<-NULL
.<-gmsnburr_cdf<-msnburr_cdf<-msnburr2a_cdf<-jfst_cdf<-fossep_cdf<-jsep_cdf<-NULL
.<-gmsnburr_lcdf<-msnburr_lcdf<-msnburr2a_lcdf<-jfst_lcdf<-fossep_lcdf<-jsep_lcdf<-NULL
.<-gmsnburr_lccdf<-msnburr_lccdf<-msnburr2a_lccdf<-jfst_lccdf<-fossep_lccdf<-jsep_lccdf<-NULL
.<-gmsnburr_rng<-msnburr_rng<-msnburr2a_rng<-jfst_rng<-NULL
.<-gmsnburr_quantile<-msnburr_quantile<-msnburr2a_quantile<-jfst_quantile<-NULL
    
        fc<-NULL
    fc<-switch(family,
                "gmsnburr"  = stanf_gmsnburr(vectorize),
                "msnburr"   = stanf_msnburr(vectorize),
                "msnburr2a" = stanf_msnburr2a(vectorize),
                "jfst"      = stanf_jfst(vectorize),
                "fossep"    = stanf_fossep(vectorize),
                "jsep"      = stanf_jsep(vectorize)
   )
  
    func_code<-paste(c("functions{",fc,"}"),collapse="\n")
   #stanfile<-paste0("neonormal.stan")
   # write(func_code,stanfile)
 
    neonormal_function <- rstan::expose_stan_functions(stanc(model_code = func_code))
   # neonormal_function <- rstan::expose_stan_functions(func_code)
     #  if (file.exists(stanfile)) file.remove(stanfile)
    if(func == "pdf"){
      pdf<-switch(family,
        "gmsnburr" = function(y, mu, sigma,alpha,beta) {
          n<-length(y)
          pdf<-rep(0,n)
            for (i in 1:n){
              pdf[i]<-exp(sapply(y[i], FUN = gmsnburr_lpdf, mu=mu, sigma=sigma, alpha = alpha, beta=beta))
            }
          return(pdf)
        },
      
        "msnburr"=function(y,mu, sigma, alpha) {
          n<-length(y)
          pdf<-rep(0,n)
            for (i in 1:n){
              pdf[i]<-exp(sapply(y[i], FUN = msnburr_lpdf, mu=mu, sigma = sigma, alpha = alpha))
            }
          return(pdf)
        },
      
        "msnburr2a"=function(y, mu, sigma,alpha) {
          n<-length(y)
          pdf<-rep(0,n)
            for (i in 1:n){
              pdf[i]<-exp(sapply(y[i], FUN = msnburr2a_lpdf, mu=mu, sigma = sigma, alpha = alpha))
            }
          return(pdf)
          },
        "jfst"=function(y,mu,sigma,alpha,beta){
          n<-length(y)
          pdf<-rep(0,n)
            for(i in 1:n){
              pdf[i] <-exp(sapply(y[i], FUN=jfst_lpdf, mu=mu, sigma=sigma,alpha=alpha,beta=beta))
            }
          return(pdf)
        },
        
        "fossep"=function(y,mu,sigma,alpha,beta){
          n<-length(y)
          pdf<-rep(0,n)
          for(i in 1:n){
            pdf[i] <-exp(sapply(y[i], FUN=fossep_lpdf, mu=mu, sigma=sigma,alpha=alpha,beta=beta))
          }
          return(pdf)
        },
        "jsep"=function(y,mu,sigma,alpha,beta){
          n<-length(y)
          pdf<-rep(0,n)
          for(i in 1:n){
            pdf[i] <-exp(sapply(y[i], FUN=jsep_lpdf, mu=mu, sigma=sigma,alpha=alpha,beta=beta))
          }
          return(pdf)
        }
       )
      }else if(func=="cdf")
      
      {
      cdf<-switch(family,
       "gmsnburr"=function(y, mu, sigma,alpha,beta) {
          n<-length(y)
          cdf<-rep(0,n)
            for (i in 1:n){
              cdf[i]<-(sapply(y[i], FUN = gmsnburr_cdf, mu=mu, sigma = sigma,alpha = alpha, beta=beta))
            }
          return(cdf)
          },
       
        "msnburr"=function(y, mu, sigma,alpha) {
          n<-length(y)
          cdf<-rep(0,n)
            for (i in 1:n){
              cdf[i]<-(sapply(y[i], FUN = msnburr_cdf, mu=mu, sigma = sigma,alpha = alpha))
            }
          return(cdf)
        },
              
       "msnburr2a"=function(y, mu, sigma,alpha) {
          n<-length(y)
          cdf<-rep(0,n)
          for (i in 1:n){
            #cdf[i]<-exp(sapply(y[i], FUN = msnburr2a_lcdf, mu=mu, sigma = sigma,alpha = alpha))
            cdf[i]<-(sapply(y[i], FUN = msnburr2a_cdf, mu=mu, sigma = sigma,alpha = alpha))
            return(cdf)
            }
        },
              
       "jfst"=function(y, mu, sigma, alpha,beta){
          n<- length(y)
          cdf<-rep(0,n)
          for (i in 1:n){
           cdf[i]<- sapply(y[i], FUN= jfst_cdf, mu=mu, sigma=sigma, alpha=alpha,beta=beta)
          }
          return(cdf)
       },
       
       "fossep"=function(y, mu, sigma, alpha,beta){
         n<- length(y)
         cdf<-rep(0,n)
         for (i in 1:n){
           cdf[i]<- sapply(y[i], FUN= fossep_cdf, mu=mu, sigma=sigma, alpha=alpha,beta=beta)
         }
         return(cdf)
       },
       "jsep"=function(y, mu, sigma, alpha,beta){
         n<- length(y)
         cdf<-rep(0,n)
         for (i in 1:n){
           if (is.infinite(y[i]) && y[i] > 0) {
             cdf[i] <- 1  # CDF(Inf) = 1
           } else if (is.infinite(y[i]) && y[i] < 0) {
             cdf[i] <- 0  # CDF(-Inf) = 0
           }else
             cdf[i]<- sapply(y[i], FUN= jsep_cdf, mu=mu, sigma=sigma, alpha=alpha,beta=beta)
           }
         return(cdf)
       }
      
        )
    
      #CCDF
      }else if(func=="lcdf")
        
      {
        lcdf<-switch(family,
                    "gmsnburr"=function(y, mu, sigma,alpha,beta) {
                      n<-length(y)
                      lcdf<-rep(0,n)
                      for (i in 1:n){
                        lcdf[i]<-(sapply(y[i], FUN = gmsnburr_lcdf, mu=mu, sigma = sigma,alpha = alpha, beta=beta))
                      }
                      return(lcdf)
                    },
                    
                    "msnburr"=function(y, mu, sigma,alpha) {
                      n<-length(y)
                      lcdf<-rep(0,n)
                      for (i in 1:n){
                        lcdf[i]<-(sapply(y[i], FUN = msnburr_lcdf, mu=mu, sigma = sigma,alpha = alpha))
                      }
                      return(lcdf)
                    },
                    
                    "msnburr2a"=function(y, mu, sigma,alpha) {
                      n<-length(y)
                      lcdf<-rep(0,n)
                      for (i in 1:n){
                        #cdf[i]<-exp(sapply(y[i], FUN = msnburr2a_lcdf, mu=mu, sigma = sigma,alpha = alpha))
                        lcdf[i]<-(sapply(y[i], FUN = msnburr2a_lcdf, mu=mu, sigma = sigma,alpha = alpha))
                        return(lcdf)
                      }
                    },
                    
                    "jfst"=function(y, mu, sigma, alpha,beta){
                      n<- length(y)
                      lcdf<-rep(0,n)
                      for (i in 1:n){
                        lcdf[i]<- sapply(y[i], FUN= jfst_lcdf, mu=mu, sigma=sigma, alpha=alpha,beta=beta)
                      }
                      return(lcdf)
                    },
                    
                    "fossep"=function(y, mu, sigma, alpha,beta){
                      n<- length(y)
                      lcdf<-rep(0,n)
                      for (i in 1:n){
                        lcdf[i]<- sapply(y[i], FUN= fossep_lcdf, mu=mu, sigma=sigma, alpha=alpha,beta=beta)
                      }
                      return(lcdf)
                    },
                    "jsep"=function(y, mu, sigma, alpha,beta){
                      n<- length(y)
                      lcdf<-rep(0,n)
                      for (i in 1:n){
                        lcdf[i]<- sapply(y[i], FUN= jsep_lcdf, mu=mu, sigma=sigma, alpha=alpha,beta=beta)
                      }
                      return(lcdf)
                    }
        )
        
        #CCDF
      }else if(func=="ccdf")
      {
      ccdf<-switch(family,
       "gmsnburr"=function(y, mu, sigma, alpha,beta) {
            n<-length(y)
            ccdf<-rep(0,n)
            for (i in 1:n){
              ccdf[i]<-exp(sapply(y[i], FUN = gmsnburr_lccdf,mu=mu, sigma = sigma,  alpha = alpha, beta=beta))
            }
            return(ccdf)
          },
              
        "msnburr"=function(y, mu, sigma,alpha) {
            n<-length(y)
            ccdf<-rep(0,n)
            for (i in 1:n){
              ccdf[i]<-exp(sapply(y[i], FUN = msnburr_lccdf, mu=mu, sigma = sigma, alpha = alpha))
            }
            return(ccdf)
        },

              
        "msnburr2a"=function(y, mu, sigma,alpha) {
            n<-length(y)
            ccdf<-rep(0,n)
            for (i in 1:n){
              ccdf[i]<-exp(sapply(y[i], FUN = msnburr2a_lccdf, mu=mu, sigma = sigma,alpha = alpha))
            }
            return(ccdf)
        },
       
        "jfst" = function(y, mu, sigma, alpha, beta){
            n<- length(y)
            ccdf<-rep(0,n)
            for(i in 1:n){
                ccdf[i]<-exp(sapply(y[i], FUN =jfst_lccdf, mu=mu, sigma=sigma,  alpha=alpha,beta=beta ))
            }
            return(ccdf)
        },
       
        "fossep" = function(y, mu, sigma, alpha, beta){
          n<- length(y)
          ccdf<-rep(0,n)
          for(i in 1:n){
            ccdf[i]<-exp(sapply(y[i], FUN =fossep_lccdf, mu=mu, sigma=sigma,  alpha=alpha,beta=beta ))
          }
          return(ccdf)
        },
        "jsep" = function(y, mu, sigma, alpha, beta){
          n<- length(y)
          ccdf<-rep(0,n)
          for(i in 1:n){
            ccdf[i]<-exp(sapply(y[i], FUN =jsep_lccdf, mu=mu, sigma=sigma,  alpha=alpha,beta=beta ))
          }
          return(ccdf)
        }
  
         )
      
       }else if(func=="rng"){
        rng<-switch(family,
         "gmsnburr"=function(n,mu, sigma,alpha, beta) {
              rng<-rep(0,n)
              for (i in 1:n){
                rng[i]<-gmsnburr_rng(mu=mu, sigma = sigma, alpha=alpha, beta=beta)
              }
          return(rng)
          },
              
          "msnburr"=function(n, mu, sigma,alpha) {
              rng<-rep(0,n)
              for (i in 1:n){
                rng[i]<-msnburr_rng( mu=mu, sigma = sigma, alpha=alpha)
              }
            return(rng)
            },

          "msnburr2a"=function(n, mu, sigma,alpha) {
              rng<-rep(0,n)
              for (i in 1:n){
                rng[i]<-msnburr2a_rng(mu=mu, sigma = sigma,alpha=alpha)
              }
          return(rng)
          },
         
          "jfst"=function(n,mu,sigma,alpha,beta){
            rng<-rep(0,n)
            for(i in 1:n){
              rng[i]<- jfst_rng(mu=mu, sigma=sigma, alpha=alpha,beta=beta)
            }
            return(rng)
          }
         
        )
       }else if(func=="quantile")
       {
         quan<-switch(family,
                      "msnburr"=function(p,mu, sigma,alpha){
                        n<-length(p)
                        quan <-rep(0,n)
                        for (i in 1:n){
                          quan[i]<-(sapply(p[i], FUN = msnburr_quantile, mu=mu, sigma = sigma, alpha=alpha))
                        }
                        return(quan)
                      },
                      "msnburr2a" = function(p,mu,sigma,alpha){
                        n<-length(p)
                        quan <-rep(0,n)
                        for (i in 1:n){
                          quan[i]<-(sapply(p[i], FUN = msnburr2a_quantile, mu=mu, sigma = sigma, alpha=alpha))
                        }
                        return(quan)
                      },
                      "gmsnburr"=function(p,mu,sigma,alpha,beta){
                        n<-length(p)
                        quan <-rep(0,n)
                        for (i in 1:n){
                          quan[i]<-(sapply(p[i], FUN = gmsnburr_quantile, mu=mu, sigma = sigma, alpha=alpha, beta=beta))
                        }
                        return(quan)
                      },
                       "jfst"=function(p, mu, sigma,alpha,beta) {
                        n<-length(p)
                        quan <-rep(0,n)
                        for (i in 1:n){
                          quan[i]<-(sapply(p[i], FUN = jfst_quantile, mu=mu, sigma = sigma, alpha=alpha,beta = beta))
                        }
                        return(quan)
                      }
                    )
        }
  } else {
    message("neonormal_stanfunc() skipped: running on CRAN")
  }
  }
