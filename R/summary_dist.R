#'  Summaries of Neo-normal Distribution


#' @name summary_dist
#' @param family identify the type of Neo-normal distribution to be used. There are five categories of neo-normal distributions,
#' which encompass "msnburr" for MSNBurr , "msnburr2a" for MSNBurr-IIa, "gmsnburr" for GMSNBurr, "jfst" for Jones-Faddy's Skew-t Distribution, "fossep" for Fernandez-Osiewalski-Steel Skew Exponential Power Distribution, and "jsep" for Jones's Skew Exponential Power. 
#' The default value of this parameter is  "msnburr"
#'
#' @param par list values of each parameter, based on the chosen distribution. The default value is "par=c(alpha=1,mu=0,sigma=1)" for MSNBurr parameter
#' parameter of MSNBurr and MSNBurr-IIa are mu, sigma, alpha
#' parameter of GMSNBurr, JFST, FOSSEP, and JSEP are mu, sigma, alpha, beta
#'
#' @description
#' To display a summary of calculations for a specific neo-normal distribution, 
#' including the mean,median, mode, variance, skewness, and excess.kurtosis.
#'
#' @return media, mean, mode,  variance, skewness, and excess kurtosis of neo-normal distributions
#' @author Achmad Syahrul Choir
#' @references 
#' Choir, A. S. (2020). The New Neo-Normal Distributions and their Properties. Dissertation. Institut Teknologi Sepuluh Nopember.
#' Jones, M.C. and Faddy, M. J. (2003) A skew extension of the t distribution, with applications. Journal of the Royal Statistical Society, Series B, 65, pp 159-174
#' Rigby, R.A. and Stasinopoulos, M.D. and Heller, G.Z. and De Bastiani, F. 
#' (2020) Distributions for Modeling Location, Scale, and Shape: 
#' Using GAMLSS in R.CRC Press
#' Fernandez, C., Osiewalski, J., & Steel, M. F. (1995) Modeling and inference with v-spherical distributions. 
#' Journal of the American Statistical Association, 90(432), pp 1331-1340
#' 
#' @examples
#' summary_dist (family="msnburr2a", par=c(mu=0,sigma=1,alpha=4))
#' 

#' @export
summary_dist<-function(family="msnburr",par=c(mu=0,sigma=1,alpha=1)){
  if(family %in%c("msnburr","msnburr2a")){
      if(is.na(par["mu"])){
        par["mu"]<-1
       }
      if(is.na(par["sigma"])){
        par["sigma"]<-1
      }
      if(is.na(par["alpha"])){
        par["alpha"]<-1
      }
      alpha=par["alpha"]
      sigma=par["sigma"]
      mu=par["mu"]
  }else if(family=="gmsnburr"){
      if(is.na(par["mu"])){
        par["mu"]<-0
      }
      if(is.na(par["sigma"])){
        par["sigma"]<-1
      }
      if(is.na(par["alpha"])){
        par["alpha"]<-1
      }
      if(is.na(par["beta"])){
        par["beta"]<-1
      }
      alpha1=par["alpha"]
      alpha2=par["beta"]
      sigma=par["sigma"]
      mu=par["mu"]
  }else if(family=="jfst"){
    if(is.na(par["mu"])){
      par["mu"] <-0
    }
    if(is.na(par["sigma"])){
      par["sigma"] <-1
    }
    if(is.na(par["alpha"])){
      par["alpha"]<-2
    }
    if(is.na(par["beta"])){
      par["beta"] <-2
    }
    mu=par["mu"]
    sigma=par["sigma"]
    alpha=par["alpha"]
    beta=par["beta"]
  }else if(family=="fossep"){
    if(is.na(par["mu"])){
      par["mu"] <-0
    }
    if(is.na(par["sigma"])){
      par["sigma"] <-1
    }
    if(is.na(par["alpha"])){
      par["alpha"]<-2
    }
    if(is.na(par["beta"])){
      par["beta"] <-2
    }
    mu=par["mu"]
    sigma=par["sigma"]
    alpha=par["alpha"]
    beta=par["beta"]
  }else if(family=="jsep"){
    if(is.na(par["mu"])){
      par["mu"] <-0
    }
    if(is.na(par["sigma"])){
      par["sigma"] <-1
    }
    if(is.na(par["alpha"])){
      par["alpha"]<-2
    }
    if(is.na(par["beta"])){
      par["beta"] <-2
    }
    mu=par["mu"]
    sigma=par["sigma"]
    alpha=par["alpha"]
    beta=par["beta"]
  }

   if(family=="msnburr"){
    alpha1<-alpha
    alpha2<-1
  }else if(family=="msnburr2a"){
    alpha2<-alpha
    alpha1<-1
  }

if(family %in%c("msnburr","msnburr2a","gmsnburr")){ 
  
      if(family=="msnburr"){
      omega=((1+1/alpha1)^(1+alpha1))/(sqrt(2*pi))
      median<-qmsnburr(0.5,mu,sigma,alpha)
      }else if(family=="msnburr2a"){
        median<-qmsnburr2a(0.5,mu,sigma,alpha)
      omega=((1+1/alpha2)^(1+alpha2))/(sqrt(2*pi))
      }else if(family=="gmsnburr"){
        median<-qgmsnburr(0.5,mu,sigma,alpha1,alpha2)
      omega=(beta(alpha1,alpha2)*((alpha2/alpha1)^-alpha2)*((1+alpha2/alpha1)^(alpha1+alpha2)))/(sqrt(2*pi))
    }
    psi0a1<-psigamma(alpha1,deriv=0)
    psi1a1<-psigamma(alpha1,deriv=1)
    psi2a1<-psigamma(alpha1,deriv=2)
    psi3a1<-psigamma(alpha1,deriv=3)
    psi0a2<-psigamma(alpha2,deriv=0)
    psi1a2<-psigamma(alpha2,deriv=1)
    psi2a2<-psigamma(alpha2,deriv=2)
    psi3a2<-psigamma(alpha2,deriv=3)
    mean<-mu+(sigma/omega)*(psi0a1-psi0a2-log(alpha1)+log(alpha2))
    variance<-((sigma/omega)^2)*(psi1a1+psi1a2)
    skewness<-(psi2a1-psi2a2)/(psi1a1+psi1a2)^(3/2)
    excess.kurtosis<-(psi3a1+psi3a2)/(psi1a1+psi1a2)^2
    mode<-mu
    
  }else if(family == "jfst"){
  #   nu =2/beta
   # lam=2*alpha/(beta*sqrt(2*beta+alpha*alpha))
  #  a =(nu+lam)/2
   # b = (nu-lam)/2
    a<-alpha
    b<-beta
    if(a<=1|b<=1) {
      
      warning(paste0("a=",a, " b= ",b,". Variance, skewness, and kurtosis is calculated when a and b  greater than 1. a and b are calculated from alpha and beta"))
        if(a<=0.5|b<=0.5){
          warning(paste0("a=",a, " b= ",b," mean is calculated when a and b  greater than 0.5. a and b are calculated from alpha and beta"))
          
                         }
                      }
      
    ez = (sqrt(a+b)*(a-b)*gamma(a-0.5)*gamma(b-0.5))/(2*gamma(a)*gamma(b))
    varz<-((((a+b)*(((a-b)^2)+a+b-2))/(4*(a-1)*(b-1)))-ez^2)
    ez3<-((((a+b)^(3/2))*gamma(a-3/2)*gamma(b-3/2))/(8*gamma(a)*gamma(b)))*((a^3)+(3*a^2)-(7*a)-(b^3)-(3*b^2)+(7*b)+(3*a*b^2)-(3*(a^2)*b))
    ez4<-((a+b)^2/(16*(a-1)*(a-2)*(b-1)*(b-2)))*((a^4)-(2*a^3)-(a^2)+(2*a)+(b^4)-(2*b^3)-(b^2)+(2*b)+(2*(a-2)*(b-2)*((3*a*b)-(2*a^2)-(2*b^2)-a-b+3)))
    mean<-mu+sigma*ez
    variance <- varz*sigma^2
    m3z<-ez3-3*varz*ez-ez^3
    m3y<-m3z*sigma^3
    m4z<-ez4-4*ez3*ez+(6*varz*ez^2)+3*ez^4
    m4y<-m4z*sigma^4
    m4y<-m4z*sigma^4
    median<-qjfst(0.5,mu,sigma,alpha,beta)
    mode<-mu+(sigma*sqrt(a+b)*(a-b))/(sqrt(2*a+1)*sqrt(2*b+1))
    if(a<=1.5|b<=1.5){
      warning(paste0("a=",a, " b= ",b,". skewness is calculated when a and b  greater than 1.5. a and b are calculated from alpha and beta"))
      
    skewness=NA 
    }else{
    skewness <-m3y/(variance^1.5)
    }
 
    if(a<=2|b<=2){
      warning(paste0("a=",a, " b= ",b,".  kurtosis is calculated when a and b  greater than 2. a and b are calculated from alpha and beta"))
      
      excess.kurtosis<-NA
    }else{
    excess.kurtosis <- (m4y/variance^2)-3
    }
    
    if(a<=1|b<=1){
      warning(paste0("a=",a, " b= ",b,".  variance is calculated when a and b  greater than 1. a and b are calculated from alpha and beta"))
      variance<-NA
      skewness=NA
      excess.kurtosis=NA
    }else{
      variance <- variance
      skewness=skewness
      excess.kurtosis=excess.kurtosis
    }
    
    if(a<=0.5|b<=0.5){
      warning(paste0("a=",a, " b= ",b,". mean is calculated when a and b  greater than 0.5. a and b are calculated from alpha and beta"))
       mean=NA
      variance<-NA
      skewness=NA
      excess.kurtosis=NA
    }else{
      mean=mean
       variance <- variance
      skewness=skewness
      excess.kurtosis=excess.kurtosis
    }
  }else if(family=="fossep"){
    a<-alpha
    b<-beta
    
    ez= 2^(1/b)*gamma(2*1/b)*(a-(1/a))
    varz<-(2^(2/b)*gamma(3*1/b)*(a^2+(1/a^2)-1)/gamma(1/b))-(ez^2)
    ez3<-(2^(3/b)*gamma(4*1/b)*(a^4-(1/a^4)))/(gamma(1/b)*(a+(1/a)))
    ez4<-(2^(4/b)*gamma(5*1/b)*(a^5+1/a^5))/(gamma(1/b)*(a+(1/a)))
    
    mean<-mu+sigma*ez
    variance <- varz*sigma^2
    m3z<-ez3-(3*varz*ez)-(ez^3)
    m3y<-m3z*sigma^3
    m4z<-ez4-(4*ez3*ez)+(6*varz*ez^2)+(3*ez^4)
    m4y<-m4z*sigma^4
    median<-qfossep(0.5,mu,sigma,alpha,beta)
    skewness<-m3y/variance^(1.5)
    excess.kurtosis<-(m4y/variance^2)-3
    mode<-mu
  }else if(family == "jsep"){
    a<-alpha
    b<-beta
    
    c <- 1 / (gamma(1 + 1/b) + gamma(1 + 1/a)) 
    ez <- c * (1/b * gamma(2/b) - 1/a * gamma(2/a))
    varz<-c * (1/b * gamma(3/b) + 1/a * gamma(3/a)) - ez^2
    ez3<-c * (1/b * gamma(4/b) - 1/a * gamma(4/a))
    ez4<-c * (1/b * gamma(5/b) + 1/a * gamma(5/a))
    mean<-mu+sigma*ez
    variance <- varz*sigma^2
    m3z<-ez3-3*varz*ez-ez^3
    m3y<-m3z*sigma^3
    m4z<-ez4-4*ez3*ez+(6*varz*ez^2)+3*ez^4
    m4y<-m4z*sigma^4
    m4y<-m4z*sigma^4
    median<-qjsep(0.5,mu,sigma,alpha,beta)
    mode<-mu
    variance <- varz*sigma^2
    skewness <-m3y/(variance^1.5)
    excess.kurtosis<-(m4y/variance^2)-3
    
  }
  
 
  summary<-list(median=median,mean=mean,mode=mode, variance=variance,skewness=skewness,excess.kurtosis=excess.kurtosis)
  
  df<-as.data.frame(summary)
  names(df)=c("Median","Mean","Mode","Variance","Skewness","Excess-Kurtosis")
  rownames(df)<-""
  
  # Calculate the middle row
  # middle_row <- nrow(df) %/% 2 + 1
  
  # Insert the title in the middle row
  #df[df, ] <- "Title"
  
 
  
  return (df)
  
}
