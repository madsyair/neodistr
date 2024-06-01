# Neo-normal Distribution
This R package aims to provide density, distribution function, quantile function and random generation for neo-normal distributions. This package also provides a stan code for the distribution, so that we can estimate the parameter distribution using stan. In this package there is a function that can generate a custom_family for brms so that we can use it to modeling that uses brms with dependent variables having a neo-normal distribution   
```{r}
library(devtools)
install_github("madsyair/neodistr")
```
