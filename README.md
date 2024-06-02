# neodistr package
Neodistr package aims to provide density, distribution function, quantile function and random generation for neo-normal distributions. This package also provides a stan code for the distribution, so that we can estimate the parameter distribution using stan. In this package there is a function that can generate a custom_family for brms so that we can use it to modeling that uses brms with dependent variables having a neo-normal distribution   
```{r}
library(devtools)
install_github("madsyair/neodistr")
```

## References
Choir, A. S. (2020).The New Neo-Normal Distributions and their Properties. 
       Disertation. Institut Teknologi Sepuluh Nopember.
       
Iriawan, N. (2000).Computationally Intensive Approaches to Inference in Neo-Normal 
       Linear Models. Curtin University of Technology.
       
Jones, M.C. and Faddy, M. J. (2003) A skew extension of the t distribution,
       with applications. Journal of the Royal Statistical Society, 
       Series B, 65, pp 159-174.
       
Rigby, R.A. and Stasinopoulos, M.D. and Heller, G.Z. and De Bastiani, F. 
       (2019) Distributions for Modeling Location, Scale, 
       and Shape: Using GAMLSS in R.CRC Press
       

