library(neodistr)
library(testthat)

if (interactive() || identical(Sys.getenv("NOT_CRAN"), "true")) {
  
  # ----------------------------B. Test Stan Functions ------------------------------------------------
  # Vector
  pdf_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "pdf",vectorize = TRUE )
  cdf_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "cdf",vectorize = TRUE )
  ccdf_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "ccdf",vectorize = TRUE)
  lcdf_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "lcdf",vectorize = TRUE)
  
  # Non-vector
  pdf0_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "pdf",vectorize = FALSE )
  cdf0_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "cdf",vectorize = FALSE )
  ccdf0_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "ccdf",vectorize = FALSE )
  lcdf0_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "lcdf",vectorize = FALSE )
  quan_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "quantile" )
  rng_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "rng" )
  

# ----------------------------Test Inapproriate parameters-------------------------------------------


test_that("1. Wrong parameter values in Stan PDF and PMF functions", {
  skip_on_cran()
  
  expect_error(expect_true(is.nan(pdf_jfst(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(pdf_jfst(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(pdf_jfst(1, 1, 1, 1, -1))))
  
  expect_error(expect_true(is.nan(pdf0_jfst(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(pdf0_jfst(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(pdf0_jfst(1, 1, 1, 1, -1))))
  
})


test_that("2. Wrong parameter values in Stan CDF functions", {
  skip_on_cran() 
  
  # Vector
  expect_error(expect_true(is.nan(cdf_jfst(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(cdf_jfst(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(cdf_jfst(1, 1, 1, 1, -1))))
  # Non-vector
  expect_error(expect_true(is.nan(cdf0_jfst(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(cdf0_jfst(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(cdf0_jfst(1, 1, 1, 1, -1))))
  
})


test_that("3. Wrong parameter values in Stan quantile functions", {
  skip_on_cran()
  
  expect_error(expect_true(is.nan(quan_jfst(0.5, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(quan_jfst(0.5, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(quan_jfst(0.5, 1, 1, 1, -1))))
  
})


test_that("4. Wrong parameter values in RNG functions", {
  skip_on_cran()
  
  expect_error(expect_true(is.nan(rjfst(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(rjfst(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(rjfst(1, 1, 1, 1, -1))))
  
  expect_error(expect_true(is.nan(rng_jfst(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(rng_jfst(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(rng_jfst(1, 1, 1, 1, -1))))
  
})

test_that("5. Wrong parameter values in CCDF functions", {
  skip_on_cran()
  
  # 
  expect_error(expect_true(is.nan(ccdf_jfst(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(ccdf_jfst(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(ccdf_jfst(1, 1, 1, 1, -1))))
  # 
  expect_error(expect_true(is.nan(ccdf0_jfst(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(ccdf0_jfst(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(ccdf0_jfst(1, 1, 1, 1, -1))))
  
})

test_that("6. Wrong parameter values in LCDF functions", {
  skip_on_cran()
  
  #   
  expect_error(expect_true(is.nan(lcdf_jfst(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(lcdf_jfst(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(lcdf_jfst(1, 1, 1, 1, -1))))
  #   
  expect_error(expect_true(is.nan(lcdf0_jfst(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(lcdf0_jfst(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(lcdf0_jfst(1, 1, 1, 1, -1))))
  
})


# 
# ------------------------------------Infinity Test-----------------------------------------------
test_that("7. Testing PDFs  against infinite values", {
  skip_on_cran()
  
  expect_true(!is.nan(pdf_jfst(Inf, 0, 1, 1, 1)) && is.finite(pdf_jfst(Inf, 0, 1, 1, 1)))
  
})


test_that("8. Testing CDFs against infinite values", {
  skip_on_cran()
  
  expect_true(!is.nan(cdf_jfst(Inf, 0, 1, 1, 1)) && is.finite(cdf_jfst(Inf, 0, 1, 1, 1)))
  
})

test_that("9. Testing LCDFs against infinite values", {
  skip_on_cran()
  
  #   
  expect_true(!is.nan(lcdf_jfst(Inf, 0, 1, 1, 1)) && is.finite(lcdf_jfst(Inf, 0, 1, 1, 1)))
  
})


test_that("10. Testing PDFs  against negatively infinite values", {
  skip_on_cran()
  
  expect_true(!is.nan(pdf_jfst(-Inf, 0, 1, 1, 1)) && is.finite(pdf_jfst(-Inf, 0, 1, 1, 1)))
  
})


test_that("11. Testing CDFs against negatively infinite values", {
  skip_on_cran()
  
  expect_true(!is.nan(cdf_jfst(-Inf, 0, 1, 1, 1)) && is.finite(cdf_jfst(-Inf, 0, 1, 1, 1)))
  
})


test_that("12. Testing PDFs  against infinite values", {
  skip_on_cran()  
  
  expect_true(!is.nan(pdf0_jfst(Inf, 0, 1, 1, 1)) && is.finite(pdf0_jfst(Inf, 0, 1, 1, 1)))
  
})


test_that("13. Testing CDFs against infinite values", {
  skip_on_cran() 
  
  
  expect_true(!is.nan(cdf0_fossep(Inf, 0, 1, 2, 2)) && is.finite(cdf0_fossep(Inf, 0, 1, 2, 2)))
  
})

test_that("14. Testing LCDFs against infinite values", {
  skip_on_cran() 
  
  expect_true(!is.nan(lcdf0_jfst(Inf, 0, 1, 1, 1)) && is.finite(lcdf0_jfst(Inf, 0, 1, 1, 1)))
  
})


test_that("15. Testing PDFs  against negatively infinite values", {
  skip_on_cran()  
  expect_true(!is.nan(pdf0_msnburr(-Inf, 0, 1, 1)) && is.finite(pdf0_msnburr(-Inf, 0, 1, 1)))
  expect_true(!is.nan(pdf0_msnburr2a(-Inf, 0, 1, 1)) && is.finite(pdf0_msnburr2a(-Inf, 0, 1, 1)))
  
})


test_that("16. Testing CDFs against negatively infinite values", {
  
  skip_on_cran()
  
  expect_true(!is.nan(cdf0_jfst(-Inf, 0, 1, 1, 1)) && is.finite(cdf0_jfst(-Inf, 0, 1, 1, 1)))
  
})

# -------------------------------Test Log Probability--------------------------------------------------



test_that("17. Check if log-probabilities are logs of probabilities (CDF's)", {
  skip_on_cran()
  
  x <- c(-Inf, -15, -10, -5, -1, 0, 1, 5, 10, 15, Inf)
  
  expect_equal(suppressWarnings(lcdf_jfst(x, 0, 1, 1, 1)),
               log(suppressWarnings(cdf_jfst(x, 0, 1, 1, 1))))
  expect_equal(suppressWarnings(lcdf0_jfst(x, 0, 1, 1, 1)),
               log(suppressWarnings(cdf0_jfst(x, 0, 1, 1, 1))))
  
})

# -------------------------------------Test NAs-------------------------------------

test_that("18. Missing values in Stan PDF  functions", {
  skip_on_cran()
  
  expect_true(is.na(pdf_jfst(NA, 0, 1, 1, 1)))
  expect_true(is.na(pdf_jfst(1, NA, 1, 1, 1)))
  expect_true(is.na(pdf_jfst(1, 0, NA, 1, 1)))
  expect_true(is.na(pdf_jfst(1, 0, 1, NA, 1)))
  expect_true(is.na(pdf_jfst(1, 0, 1, 1, NA)))
  
  expect_true(is.na(pdf0_jfst(NA, 0, 1, 1, 1)))
  expect_true(is.na(pdf0_jfst(1, NA, 1, 1, 1)))
  expect_true(is.na(pdf0_jfst(1, 0, NA, 1, 1)))
  expect_true(is.na(pdf0_jfst(1, 0, 1, NA, 1)))
  expect_true(is.na(pdf0_jfst(1, 0, 1, 1, NA)))
  
})

test_that("18. Wrong parameter values in Stan CDF functions", {
  skip_on_cran()
  
  
  expect_error( expect_true(is.na(cdf_jfst(NA, 0, 1, 1, 1))))
  expect_error(  expect_true(is.na(cdf_jfst(1, NA, 1, 1, 1))))
  expect_error(expect_true(is.na(cdf_jfst(1, 0, NA, 1, 1))))
  expect_error(   expect_true(is.na(cdf_jfst(1, 0, 1, NA, 1))))
  expect_error( expect_true(is.na(cdf_jfst(1, 0, 1, 1, NA))))
  
  
  expect_error( expect_true(is.na(cdf0_jfst(NA, 0, 1, 1, 1))))
  expect_error(  expect_true(is.na(cdf0_jfst(1, NA, 1, 1, 1))))
  expect_error(expect_true(is.na(cdf0_jfst(1, 0, NA, 1, 1))))
  expect_error(   expect_true(is.na(cdf0_jfst(1, 0, 1, NA, 1))))
  expect_error( expect_true(is.na(cdf0_jfst(1, 0, 01, 1, NA))))
  
})

test_that("19. Wrong parameter values in inverse CDF functions", {
  skip_on_cran()  
  
  expect_error(expect_true(is.na(quan_jfst(NA, 0, 1, 1, 1))))
  expect_true(is.na(quan_jfst(1, NA, 1, 1, 1)))
  expect_true(is.na(quan_jfst(1, 0, NA, 1, 1)))
  expect_error(expect_true(is.na(quan_jfst(1, 0, 1, NA, 1))))
  expect_error(expect_true(is.na(quan_jfst(1, 0, 1, 1, NA))))
  
})

test_that("20 Wrong parameter values in RNG functions", {
  skip_on_cran()
  
  expect_true(is.na(rng_jfst(1, NA, 1, 1, 1)))
  expect_true(is.na(rng_jfst(1, 0, NA, 1, 1)))
  expect_error(expect_true(is.na(rng_jfst(1, 0, 1, NA, 1))))
  expect_error(expect_true(is.na(rng_jfst(1, 0, 1, 1, NA))))
  
})

# --------------------- Test Probabillity -------------------------------------------

test_that("21. All probabilities/densities >= 0", {
  skip_on_cran()
  
  y <- c( -100, -10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 100)
  
  expect_true(suppressWarnings(all(pdf_jfst(y, 0, 1, 1, 1) >= 0)))
  expect_true(suppressWarnings(all(pdf0_jfst(y, 0, 1, 1, 1) >= 0)))
  
})

test_that("22. All cumulative probabilities >= 0 and <= 1", {
  skip_on_cran()
  
  y <- c( -100, -10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 100)
  
  expect_true(all(cdf_jfst(y, 0, 1, 1, 1) >= 0 & cdf_jfst(y, 0, 1, 1, 1) <= 1))
  expect_true(all(cdf0_jfst(y, 0, 1, 1, 1) >= 0 & cdf_jfst(y, 0, 1, 1, 1) <= 1))
  
})


# -------------------------------------Test Quantile Function ------------------------------

test_that("23. Zeros in quantile functions", {
  skip_on_cran()
  
  expect_true(!is.nan(quan_jfst(0, 0, 1, 1, 1)))
  
})

test_that("24. Ones in quantile functions", {
  skip_on_cran()
  
  expect_true(!is.nan(quan_jfst(1, 0, 1, 1, 1)))
  
})

test_that("25. Checking p = F(F^-1(p))", {
  skip_on_cran()
  pp <- seq(0.00001, 1, by = 0.001)
  
  expect_equal(pp, cdf_jfst(quan_jfst(pp, 0, 1, 1, 1), 0, 1, 1, 1))
  expect_equal(pp, cdf_jfst(quan_jfst(pp, 0, 1, 2, 2), 0, 1, 2, 2))
  
  # for(i in 1:length(pp)){
  # expect_equal(pp[i], cdf_jfst(quan_jfst(pp[i], 0, 1, 1, 1.5), 0, 1, 1, 1.5))
  # }
  
  expect_equal(pp, cdf0_jfst(quan_jfst(pp, 0, 1, 1, 1), 0, 1, 1, 1))
  
})

# ----------------------------- Test RNG Converge -------------------------------------

probCoverage <- function(stub, ..., n = 5000L) {
  rfoo <- eval(as.name(paste0("r", stub)))
  pfoo <- eval(as.name(paste0("p", stub)))
  diff(pfoo(range(rfoo(n, ...)), ...))
}

# ----------------------------- PDF and CDF Test --------------------------------------
library(neodistr)
library (testthat)

test_that("integrate PDF from -inf to inf == 1", {
  
  expect_equal(integrate(pdf_jfst,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1,beta=1 )$value,1)
  expect_equal(integrate(pdf0_jfst,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1,beta=1 )$value,1)
  
})

test_that("integrate PDF from - Inf until x equal to cdf x", {
  
  expect_equal((integrate(djfst, lower= -Inf, upper=4, mu=0, sigma=1,alpha=1, beta=1)$value), (pjfst(4, mu=0, sigma=1, alpha=1, beta=1)))
  # expect_equal((integrate(pdf_jfst, lower= -Inf, upper=4, mu=0, sigma=1,alpha=1, beta=1)$value) , (cdf_jfst(4, mu=0, sigma=1, alpha=1, beta=1)))
  # expect_equal((integrate(pdf0_jfst, lower= -Inf, upper=4, mu=0, sigma=1,alpha=1, beta=1)$value) , (cdf0_jfst(4, mu=0, sigma=1, alpha=1, beta=1)))
  
})


# ------------------------------ Cumulative Test --------------------------------------

test_that("Checking ccdf = 1-cdf", {
  
  #   expect_equal(ccdf_jfst(0.5,0,1,1,1), (1- (pjfst(0.5, 0, 1, 1,1))))
  #   expect_equal(ccdf_jfst(0.5,0,1,1,1), (1- (cdf_jfst(0.5, 0, 1, 1,1))))
  #   expect_equal(ccdf0_jfst(0.5,0,1,1,1), (1- (cdf0_jfst(0.5, 0, 1, 1,1))))
  #   
  
})
} else {
  message("neonormal_stanfunc() skipped: running on CRAN")
}

