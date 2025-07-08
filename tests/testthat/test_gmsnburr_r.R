library(neodistr)
library(testthat)

# ----------------------------A. Test R Functions------------------------------------------------
# ----------------------------Test Inapproriate parameters-------------------------------------------

test_that("1. Wrong parameter values in R PDF and PMF functions", {
  
  expect_error(expect_true(is.nan(dgmsnburr(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(dgmsnburr(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(dgmsnburr(1, 1, 1, 1, -1))))

})

test_that("2. Wrong parameter values in R CDF functions", {
 
   expect_error(expect_true(is.nan(pgmsnburr(1, 1, -1, 1, 1))))
   expect_error(expect_true(is.nan(pgmsnburr(1, 1, 1, -1, 1))))
   expect_error(expect_true(is.nan(pgmsnburr(1, 1, 1, 1, -1))))
 
})


test_that("3. Wrong parameter values in R quantile functions", {
  
  expect_error(expect_true(is.nan(qgmsnburr(0.5, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(qgmsnburr(0.5, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(qgmsnburr(0.5, 1, 1, 1, -1))))
  
})


test_that("4. Wrong parameter values in R RNG functions", {
  
  expect_error(expect_true(is.nan(rgmsnburr(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(rgmsnburr(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(rgmsnburr(1, 1, 1, 1, -1))))

  expect_error(expect_true(is.nan(rng_gmsnburr(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(rng_gmsnburr(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(rng_gmsnburr(1, 1, 1, 1, -1))))
  
})

# ------------------------------------Infinity Test-----------------------------------------------
test_that("5. Testing PDFs  against infinite values", {
 
  expect_true(!is.nan(dgmsnburr(Inf, 0, 1, 1, 1)) && is.finite(dgmsnburr(Inf, 0, 1, 1, 1)))

})

test_that("6. Testing CDFs against infinite values", {
  
  expect_true(!is.nan(pgmsnburr(Inf, 0, 1, 1, 1)) && is.finite(pgmsnburr(Inf, 0, 1, 1, 1)))
  
})

test_that("7. Testing PDFs  against negatively infinite values", {
 
  expect_true(!is.nan(dgmsnburr(-Inf, 0, 1, 1, 1)) && is.finite(dgmsnburr(-Inf, 0, 1, 1, 1)))
 
})

test_that("8. Testing CDFs against negatively infinite values", {
  
  expect_true(!is.nan(pgmsnburr(-Inf, 0, 1, 1, 1)) && is.finite(pgmsnburr(-Inf, 0, 1, 1, 1)))
  
})

# -------------------------------Test Log Probability--------------------------------------------------

test_that("9. Check if log-probabilities are logs of probabilities (PDF's)", {
  
  x <- c(-Inf, -15, -10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 15, Inf)
  
  expect_equal(suppressWarnings(dgmsnburr(x, 0, 1, 1, 1, log = TRUE)),
               log(suppressWarnings(dgmsnburr(x, 0, 1, 1, 1))))
 
})

test_that("10. Check if log-probabilities are logs of probabilities (CDF's)", {
  
  x <- c(-Inf, -15, -10, -5, -1, 0, 1, 5, 10, 15, Inf)
  
  expect_equal(suppressWarnings(pgmsnburr(x, 0, 1, 1, 1, log.p = TRUE)),
               log(suppressWarnings(pgmsnburr(x, 0, 1, 1, 1))))
  
})

# -------------------------------------Test NAs-------------------------------------

test_that("11. Missing values in PDF and PMF functions", {
  
  expect_true(is.na(dgmsnburr(NA, 0, 1, 1, 1)))
  expect_error(expect_true(is.na(dgmsnburr(1, NA, 1, 1, 1))))
  expect_error(expect_true(is.na(dgmsnburr(1, 0, NA, 1, 1))))
  expect_error(expect_true(is.na(dgmsnburr(1, 0, 1, NA, 1))))
  expect_error(expect_true(is.na(dgmsnburr(1, 0, 1, 1, NA))))
  
})

test_that("12. Missing values  in CDF functions", {
  
  expect_true(is.na(pgmsnburr(NA, 0, 1, 1, 1)))
  expect_error(expect_true(is.na(pgmsnburr(1, NA, 1, 1, 1))))
  expect_error(expect_true(is.na(pgmsnburr(1, 0, NA, 1, 1))))
  expect_error(expect_true(is.na(pgmsnburr(1, 0, 1, NA, 1))))
  expect_error(expect_true(is.na(pgmsnburr(1, 0, 1, 1, NA))))
  
})

test_that("13.  Missing values in inverse CDF functions", {
  
  expect_error(expect_true(is.na(qgmsnburr(NA, 0, 1, 1, 1))))
  expect_error(expect_true(is.na(qgmsnburr(1, NA, 1, 1, 1))))
  expect_error(expect_true(is.na(qgmsnburr(1, 0, NA, 1, 1))))
  expect_error(expect_true(is.na(qgmsnburr(1, 0, 1, NA, 1))))
  expect_error(expect_true(is.na(qgmsnburr(1, 0, 1, 1, NA))))
  
})

test_that("14. Missing values in RNG functions", {
  
  expect_error(expect_true(is.na(rgmsnburr(1, NA, 1, 1, 1))))
  expect_error(expect_true(is.na(rgmsnburr(1, 0, NA, 1, 1))))
  expect_error(expect_true(is.na(rgmsnburr(1, 0, 1, NA, 1))))
  expect_error(expect_true(is.na(rgmsnburr(1, 0, 1, 1, NA))))
  
})

# --------------------- Test Probabillity -------------------------------------------

test_that("15. All probabilities/densities >= 0", {
  
  y <- c( -100, -10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 100)
  
  expect_true(suppressWarnings(all(dgmsnburr(y, 0, 1, 1, 1) >= 0)))
 
})

test_that("16. All cumulative probabilities >= 0 and <= 1", {
  
  y <- c( -100, -10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 100)
  
  expect_true(all(pgmsnburr(y, 0, 1, 1, 1) >= 0 & pgmsnburr(y, 0, 1, 1, 1) <= 1))
 
})


# -------------------------------------Test Quantile Function ------------------------------

test_that("17. Zeros in quantile functions", {

  expect_error(expect_true(!is.nan(qgmsnburr(0, 0, 1, 1, 1))))
  
})

test_that("18. Ones in quantile functions", {
  
  expect_error(expect_true(!is.nan(qgmsnburr(1, 0, 1, 1, 1))))
  
})

test_that("19. Checking p = F(F^-1(p))", {
  
  pp <- seq(0.00001, 1, by = 0.001)
  
  expect_equal(pp, pgmsnburr(qgmsnburr(pp, 0, 1, 1, 1), 0, 1, 1, 1))
  
})

# ----------------------------- Test RNG Converge -------------------------------------

probCoverage <- function(stub, ..., n = 5000L) {
  rfoo <- eval(as.name(paste0("r", stub)))
  pfoo <- eval(as.name(paste0("p", stub)))
  diff(pfoo(range(rfoo(n, ...)), ...))
}



test_that("20. Coverage of RNG's", {
  
  skip_on_cran()
  
  expect_gte(probCoverage("gmsnburr", 0, 1, 1, 1), 0.99)
  
})


# -------------------------- PDF and CDF Test -----------------------------------------
library(neodistr)
library (testthat)

test_that("21. integrate PDF from -inf to inf == 1", {
 
  expect_equal(integrate(dgmsnburr,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1,beta=1 )$value,1)

})

test_that("22. integrate PDF from - Inf until x equal to cdf x", {

  expect_equal((integrate(dgmsnburr, lower= -Inf, upper=4, mu=0, sigma=1,alpha=1, beta=1)$value), (pgmsnburr(4, mu=0, sigma=1, alpha=1, beta=1)))

})
