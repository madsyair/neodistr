library(neodistr)
library(testthat)


if (interactive() || identical(Sys.getenv("NOT_CRAN"), "true")) {
  
  # ----------------------------B. Test Stan Functions------------------------------------------------
  # Vector
  pdf_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "pdf",vectorize = TRUE )
  cdf_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "cdf",vectorize = TRUE )
  ccdf_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "ccdf",vectorize = TRUE)
  lcdf_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "lcdf",vectorize = TRUE)

  #Non-Vector
  pdf0_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "pdf",vectorize = FALSE )
  cdf0_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "cdf",vectorize = FALSE )
  ccdf0_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "ccdf",vectorize = FALSE )
  lcdf0_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "lcdf",vectorize = FALSE )
  
# ----------------------------Test Inapproriate parameters-------------------------------------------


test_that("1. Wrong parameter values in Stan PDF and PMF functions", {
  skip_on_cran()
  
  expect_error(expect_true(is.nan(pdf_jsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(pdf_jsep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(pdf_jsep(1, 1, 1, 2, -2))))
  
})


test_that("2. Wrong parameter values in Stan CDF functions", {
  skip_on_cran() 
  
  expect_error(expect_true(is.nan(cdf_jsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(cdf_jsep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(cdf_jsep(1, 1, 1, 2, -2))))
  
})


test_that("3. Wrong parameter values in Stan quantile functions", {
  skip_on_cran()
  
  #  expect_error(expect_true(is.nan(quan_jsep(1, 1, -1, 2, 2))))
  #  expect_error(expect_true(is.nan(quan_jsep(1, 1, 1, -2, 2))))
  #  expect_error(expect_true(is.nan(quan_jsep(1, 1, 1, 2, -2))))
  
})


test_that("4. Wrong parameter values in RNG functions", {
  skip_on_cran()
  
  #  expect_error(expect_true(is.nan(rng_jsep(1, 1, -1, 2, 2))))
  #  expect_error(expect_true(is.nan(rng_jsep(1, 1, 1, 2, -2))))
  
})

test_that("5. Wrong parameter values in CCDF functions", {
  skip_on_cran()
  #
  expect_error(expect_true(is.nan(ccdf_jsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(ccdf_jsep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(ccdf_jsep(1, 1, 1, 2, -2))))
  
})

test_that("6. Wrong parameter values in LCDF functions", {
  skip_on_cran()
  #   
  expect_error(expect_true(is.nan(lcdf_jsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(lcdf_jsep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(lcdf_jsep(1, 1, 1, 2, -2))))
  
})


# 
# ------------------------------------Infinity Test-----------------------------------------------
test_that("7. Testing PDFs  against infinite values", {
  skip_on_cran()
  
  expect_true(!is.nan(pdf_jsep(Inf, 0, 1, 2, 2)) && is.finite(pdf_jsep(Inf, 0, 1, 2, 2)))
  
})


test_that("8. Testing CDFs against infinite values", {
  skip_on_cran()
  
  expect_true(!is.nan(cdf_jsep(Inf, 0, 1, 2, 2)) && is.finite(cdf_jsep(Inf, 0, 1, 2, 2)))
  
})

test_that("9. Testing LCDFs against infinite values", {
  skip_on_cran()
  
  expect_true(!is.nan(lcdf_jsep(Inf, 0, 1, 2, 2)) && is.finite(lcdf_jsep(Inf, 0, 1, 2, 2)))
  
})


test_that("10. Testing PDFs  against negatively infinite values", {
  skip_on_cran()
  
  expect_true(!is.nan(pdf_jsep(-Inf, 0, 1, 2, 2)) && is.finite(pdf_jsep(-Inf, 0, 1, 2, 2)))
  
})


test_that("11. Testing CDFs against negatively infinite values", {
  skip_on_cran()
  
  expect_true(!is.nan(cdf_jsep(-Inf, 0, 1, 2, 2)) && is.finite(cdf_jsep(-Inf, 0, 1, 2, 2))) 
  
})


test_that("12. Testing PDFs  against infinite values", {
  skip_on_cran()  
  
  # expect_true(!is.nan(pdf0_jsep(Inf, 0, 1, 2, 2)) && is.finite(pdf0_jsep(Inf, 0, 1, 2, 2)))
  
})


test_that("13. Testing CDFs against infinite values", {
  skip_on_cran() 
  
  # expect_true(!is.nan(cdf0_jsep(Inf, 0, 1, 2, 2)) && is.finite(cdf0_jsep(Inf, 0, 1, 2, 2)))
  
})

test_that("14. Testing LCDFs against infinite values", {
  skip_on_cran() 
  #   
  
  # expect_true(!is.nan(lcdf0_jsep(Inf, 0, 1, 2, 2)) && is.finite(lcdf0_jsep(Inf, 0, 1, 2, 2)))
  
})


test_that("15. Testing PDFs  against negatively infinite values", {
  skip_on_cran()  
  
  # expect_true(!is.nan(pdf0_jsep(-Inf, 0, 1, 2, 2)) && is.finite(pdf_jsep(-Inf, 0, 1, 2, 2)))
  
})


test_that("16. Testing CDFs against negatively infinite values", {
  skip_on_cran()
  
  # expect_true(!is.nan(cdf0_jsep(-Inf, 0, 1, 2, 2)) && is.finite(cdf0_jsep(-Inf, 0, 1, 2, 2)))
  
})

# -------------------------------Test Log Probability--------------------------------------------------



test_that("17. Check if log-probabilities are logs of probabilities (CDF's)", {
  skip_on_cran()
  
  x <- c(-Inf, -15, -10, -5, -1, 0, 1, 5, 10, 15, Inf)
  
  expect_equal(suppressWarnings(lcdf_jsep(x, 0, 1, 2, 2)),
               log(suppressWarnings(cdf_jsep(x, 0, 1, 2, 2))))
  # expect_equal(suppressWarnings(lcdf0_jsep(x, 0, 1, 2, 2)),
  # log(suppressWarnings(cdf0_jsep(x, 0, 1, 2, 2))))
  
})

# -------------------------------------Test NAs-------------------------------------

test_that("18. Missing values in Stan PDF  functions", {
  skip_on_cran()
  
  expect_true(is.na(pdf_jsep(NA, 0, 1, 2, 2)))
  expect_true(is.na(pdf_jsep(1, NA, 1, 2, 2)))
  expect_true(is.na(pdf_jsep(1, 0, NA, 2, 2)))
  expect_true(is.na(pdf_jsep(1, 0, 1, NA, 2)))
  expect_true(is.na(pdf_jsep(1, 0, 1, 2, NA)))
  
  
  # expect_true(is.na(pdf0_jsep(NA, 0, 1, 2, 2)))
  # expect_true(is.na(pdf0_jsep(1, NA, 1, 2, 2)))
  # expect_true(is.na(pdf0_jsep(1, 0, NA, 2, 2)))
  # expect_true(is.na(pdf0_jsep(1, 0, 1, NA, 2)))
  # expect_true(is.na(pdf0_jsep(1, 0, 1, 2, NA)))
  
})

test_that("18. Wrong parameter values in Stan CDF functions", {
  skip_on_cran()
  
  expect_error(expect_true(is.na(cdf_jsep(NA, 0, 1, 0, 0.2))))
  expect_true(is.na(cdf_jsep(1, NA, 1, 2, 2)))
  expect_true(is.na(cdf_jsep(1, 0, NA, 2, 2)))
  expect_true(is.na(cdf_jsep(1, 0, 1, NA, 2)))
  expect_true(is.na(cdf_jsep(1, 0, 1, 2, NA))) 
  
  # expect_error(expect_true(is.na(cdf0_jsep(NA, 0, 1, 0, 0.2))))
  # expect_true(is.na(cdf0_jsep(1, NA, 1, 2, 2)))
  # expect_true(is.na(cdf0_jsep(1, 0, NA, 2, 2)))
  # expect_true(is.na(cdf0_jsep(1, 0, 1, NA, 2)))
  # expect_true(is.na(cdf0_jsep(1, 0, 1, 2, NA))) 
  
})

test_that("19. Wrong parameter values in inverse CDF functions", {
  skip_on_cran()  
  
  # expect_error(expect_true(is.na(quan_jsep(NA, 0, 1, 2, 2))))
  # expect_error(expect_true(is.na(quan_jsep(1, NA, 1, 2, 2))))
  # expect_error(expect_true(is.na(quan_jsep(1, 0, NA, 2, 2))))
  # expect_error(expect_true(is.na(quan_jsep(1, 0, 1, NA, 2))))
  # expect_error(expect_true(is.na(quan_jsep(1, 0, 1, 2, NA))))
  
})

test_that("20 Wrong parameter values in RNG functions", {
  skip_on_cran()
  
  expect_error(expect_true(is.na(rng_jsep(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(rng_jsep(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(rng_jsep(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(rng_jsep(1, 0, 1, 2, NA))))
  
})

# --------------------- Test Probabillity -------------------------------------------

test_that("21. All probabilities/densities >= 0", {
  skip_on_cran()
  y <- c( -100, -10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 100)
  
  expect_true(suppressWarnings(all(pdf_jsep(y, 0, 1, 2, 2) >= 0)))
  # expect_true(suppressWarnings(all(pdf0_jsep(y, 0, 1, 2, 2) >= 0)))
  
})


test_that("22. All cumulative probabilities >= 0 and <= 1", {
  skip_on_cran()
  y <- c( -100, -10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 100)
  
  expect_true(all(cdf_jsep(y, 0, 1, 2, 2) >= 0 & cdf_jsep(y, 0, 1, 2, 2) <= 1))
  # expect_true(all(cdf0_jsep(y, 0, 1, 2, 2) >= 0 & cdf_jsep(y, 0, 1, 2, 2) <= 1))
  
})


# -------------------------------------Test Quantile Function ------------------------------

test_that("23. Zeros in quantile functions", {
  skip_on_cran()
  # expect_true(!is.nan(quan_jsep(0, 0, 1, 2, 2)))
  
})

test_that("24. Ones in quantile functions", {
  skip_on_cran()
  # expect_error(expect_true(!is.nan(qjsep(1, 0, 1, 2, 2))))
  
})

test_that("25. Checking p = F(F^-1(p))", {
  skip_on_cran()
  pp <- seq(0.00001, 1, by = 0.001)
  
  #  expect_equal(pp, cdf_jsep(quan_jsep(pp, 0, 1, 2, 2), 0, 1, 2, 2))
  
  # for(i in 1:length(pp)){
  # expect_equal(pp[i], cdf_jsep(quan_jsep(pp[i], 0, 1, 2, 2), 0, 1, 2, 2))
  # }
  # expect_equal(pp, cdf0_jsep(quan_jsep(pp, 0, 1, 2, 2), 0, 1, 2, 2))
  
})

# ----------------------------- Test RNG Converge -------------------------------------

probCoverage <- function(stub, ..., n = 5000L) {
  rfoo <- eval(as.name(paste0("r", stub)))
  pfoo <- eval(as.name(paste0("p", stub)))
  diff(pfoo(range(rfoo(n, ...)), ...))
}


# ----------------------------- PDF and CDF Test ---------------------------------------
library(neodistr)
library (testthat)

test_that("integrate PDF from -inf to inf == 1", {
  
  expect_equal(integrate(pdf_jsep,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=2,beta= 2 )$value,1)
  #expect_equal(integrate(pdf0_jsep,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=2,beta= 2 )$value,1)
  
})

test_that("integrate PDF from - Inf until x equal to cdf x", {
  
  expect_equal((integrate(djsep, lower= -Inf, upper=4, mu=0, sigma=1,alpha=2, beta=2, rel.tol = 1e-10, abs.tol = 1e-12)$value), (pjsep(4, mu=0, sigma=1, alpha=2, beta=2)))
  expect_equal((integrate(pdf_jsep, lower= -Inf, upper=4, mu=0, sigma=1,alpha=2, beta=2, rel.tol = 1e-10, abs.tol = 1e-12)$value) , (cdf_jsep(4, mu=0, sigma=1, alpha=2, beta=2)))
  
})


# ----------------------------- Cumulative Test ---------------------------------------

test_that("Checking ccdf = 1-cdf", {
  #   
  expect_equal(ccdf_jsep(0.5,0,1,2,2), (1- (pjsep(0.5, 0, 1, 2,2))))
  expect_equal(ccdf_jsep(0.5,0,1,2,2), (1- (cdf_jsep(0.5, 0, 1, 2,2))))
  #   
  
})
} else {
  message("neonormal_stanfunc() skipped: running on CRAN")
}