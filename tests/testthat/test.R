library(neodistr)
library(testthat)

# ----------------------------A. Test R Functions------------------------------------------------
# ----------------------------Test Inapproriate parameters-------------------------------------------

test_that("1. Wrong parameter values in R PDF and PMF functions", {
  
  expect_error(expect_true(is.nan(dmsnburr(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(dmsnburr(1, 1, 1, -1))))
  
  expect_error(expect_true(is.nan(dmsnburr2a(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(dmsnburr2a(1, 1, 1, -1))))
  
  expect_error(expect_true(is.nan(dgmsnburr(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(dgmsnburr(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(dgmsnburr(1, 1, 1, 1, -1))))
  
  expect_error(expect_true(is.nan(djfst(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(djfst(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(djfst(1, 1, 1, 2, -2))))  

  
  expect_error(expect_true(is.nan(dfossep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(dfossep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(dfossep(1, 1, 1, 2, -2))))  

  expect_error(expect_true(is.nan(djsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(djsep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(djsep(1, 1, 1, 2, -2))))  

})

test_that("2. Wrong parameter values in R CDF functions", {
  
  expect_error(expect_true(is.nan(pmsnburr(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(pmsnburr(1, 1, 1, -1))))
   expect_error(expect_true(is.nan(pmsnburr2a(1, 1, -1, 1))))
   expect_error(expect_true(is.nan(pmsnburr2a(1, 1, 1, -1))))
  
   expect_error(expect_true(is.nan(pgmsnburr(1, 1, -1, 1, 1))))
   expect_error(expect_true(is.nan(pgmsnburr(1, 1, 1, -1, 1))))
   expect_error(expect_true(is.nan(pgmsnburr(1, 1, 1, 1, -1))))
  
   expect_error(expect_true(is.nan(pjfst(1, 1, -1, 2, 2))))
   expect_error(expect_true(is.nan(pjfst(1, 1, 1, -2, 2))))
   expect_error(expect_true(is.nan(pjfst(1, 1, 1, 2, -2))))

  expect_error(expect_true(is.nan(pfossep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(pfossep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(pfossep(1, 1, 1, 2, -2))))

  expect_error(expect_true(is.nan(pjsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(pjsep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(pjsep(1, 1, 1, 2, -2))))
})


test_that("3. Wrong parameter values in R quantile functions", {
  
  expect_error(expect_true(is.nan(qmsnburr(0.5, 1, -1, 1))))
  expect_error(expect_true(is.nan(qmsnburr(0.5, 1, 1, -1))))
  

  # 
  expect_error(expect_true(is.nan(qmsnburr2a(0.5, 1, -1, 1))))
  expect_error(expect_true(is.nan(qmsnburr2a(0.5, 1, 1, -1))))
  
  expect_error(expect_true(is.nan(qgmsnburr(0.5, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(qgmsnburr(0.5, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(qgmsnburr(0.5, 1, 1, 1, -1))))
  

   expect_error(expect_true(is.nan(qjfst(1, 1, -1, 2, 2))))
   expect_error(expect_true(is.nan(qjfst(1, 1, 1, -2, 2))))
   expect_error(expect_true(is.nan(qjfst(1, 1, 1, 2, -2)))) 

  expect_error(expect_true(is.nan(qfossep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(qfossep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(qfossep(1, 1, 1, 2, -2)))) 
  
  expect_error(expect_true(is.nan(qjsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(qjsep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(qjsep(1, 1, 1, 2, -2)))) 
})


test_that("4. Wrong parameter values in R RNG functions", {
  
  expect_error(expect_true(is.nan(rmsnburr(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(rmsnburr(1, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(rng_msnburr(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(rng_msnburr(1, 1, 1, -1))))
  
  
  
  # expect_error(expect_true(is.nan(rmsnburr2a(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(rmsnburr2a(1, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(rng_msnburr2a(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(rng_msnburr2a(1, 1, 1, -1))))
  
  
  # expect_error(expect_true(is.nan(rgmsnburr(1, 1, -1, 1, 1))))
  # expect_error(expect_true(is.nan(rgmsnburr(1, 1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(rgmsnburr(1, 1, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(rng_gmsnburr(1, 1, -1, 1, 1))))
  # expect_error(expect_true(is.nan(rng_gmsnburr(1, 1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(rng_gmsnburr(1, 1, 1, 1, -1))))
  # 
  
  # 
  # expect_error(expect_true(is.nan(rjfst(1, 1, -1, 2, 2))))
  # expect_error(expect_true(is.nan(rjfst(1, 1, 1, 2, -2))))
  # expect_error(expect_true(is.nan(quan_jfst(1, 1, 1, -2, 2))))
  # expect_error(expect_true(is.nan(rng_jfst(1, 1, -1, 2, 2))))
  # expect_error(expect_true(is.nan(rng_jfst(1, 1, 1, -2,2 ))))
  # expect_error(expect_true(is.nan(rng_jfst(1, 1, 1, 2, -2))))
  
  
  expect_error(expect_true(is.nan(rfossep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(rfossep(1, 1, 1, 2, -2))))



  expect_error(expect_true(is.nan(rjsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(rjsep(1, 1, 1, 2, -2)))) 
})

# ------------------------------------Infinity Test-----------------------------------------------
test_that("5. Testing PDFs  against infinite values", {
  
  expect_true(!is.nan(dmsnburr(Inf, 0, 1, 1)) && is.finite(dmsnburr(Inf, 0, 1, 1)))
  expect_true(!is.nan(dmsnburr2a(Inf, 0, 1, 1)) && is.finite(dmsnburr2a(Inf, 0, 1, 1)))
  expect_true(!is.nan(dgmsnburr(Inf, 0, 1, 1, 1)) && is.finite(dgmsnburr(Inf, 0, 1, 1, 1)))
  expect_true(!is.nan(djfst(Inf, 0, 1, 2, 2)) && is.finite(djfst(Inf, 0, 1, 2, 2)))
  expect_true(!is.nan(dfossep(Inf, 0, 1, 2, 2)) && is.finite(dfossep(Inf, 0, 1, 2, 2)))
  expect_true(!is.nan(djsep(Inf, 0, 1, 2, 2)) && is.finite(djsep(Inf, 0, 1, 2, 2)))

})

test_that("6. Testing CDFs against infinite values", {
  
  expect_true(!is.nan(pmsnburr(Inf, 0, 1, 1)) && is.finite(pmsnburr(Inf, 0, 1, 1)))
  expect_true(!is.nan(pmsnburr2a(Inf, 0, 1, 1)) && is.finite(pmsnburr2a(Inf, 0, 1, 1)))
  expect_true(!is.nan(pgmsnburr(Inf, 0, 1, 1, 1)) && is.finite(pgmsnburr(Inf, 0, 1, 1, 1)))
  expect_true(!is.nan(pjfst(Inf, 0, 1, 2, 2)) && is.finite(pjfst(Inf, 0, 1, 2, 2)))
  expect_true(!is.nan(pfossep(Inf, 0, 1, 2, 2)) && is.finite(pfossep(Inf, 0, 1, 2, 2)))
  expect_true(!is.nan(pjsep(Inf, 0, 1, 2, 2)) && is.finite(pjsep(Inf, 0, 1, 2, 2)))
})

test_that("7. Testing PDFs  against negatively infinite values", {
  
  expect_true(!is.nan(dmsnburr(-Inf, 0, 1, 1)) && is.finite(dmsnburr(-Inf, 0, 1, 1)))
  expect_true(!is.nan(dmsnburr2a(-Inf, 0, 1, 1)) && is.finite(dmsnburr2a(-Inf, 0, 1, 1)))
  expect_true(!is.nan(dgmsnburr(-Inf, 0, 1, 1, 1)) && is.finite(dgmsnburr(-Inf, 0, 1, 1, 1)))
  expect_true(!is.nan(djfst(-Inf, 0, 1, 2, 2)) && is.finite(djfst(-Inf, 0, 1, 2, 2)))
  expect_true(!is.nan(dfossep(-Inf, 0, 1, 2, 2)) && is.finite(dfossep(-Inf, 0, 1, 2, 2)))
  expect_true(!is.nan(djsep(-Inf, 0, 1, 2, 2)) && is.finite(djsep(-Inf, 0, 1, 2, 2)))
})

test_that("8. Testing CDFs against negatively infinite values", {
  
  expect_true(!is.nan(pmsnburr(-Inf, 0, 1, 1)) && is.finite(pmsnburr(-Inf, 0, 1, 1)))
  expect_true(!is.nan(pmsnburr2a(-Inf, 0, 1, 1)) && is.finite(pmsnburr2a(-Inf, 0, 1, 1)))
  expect_true(!is.nan(pgmsnburr(-Inf, 0, 1, 1, 1)) && is.finite(pgmsnburr(-Inf, 0, 1, 1, 1)))
  expect_true(!is.nan(pjfst(-Inf, 0, 1, 2, 2)) && is.finite(pjfst(-Inf, 0, 1, 2, 2)))
  expect_true(!is.nan(pfossep(-Inf, 0, 1, 2, 2)) && is.finite(pfossep(-Inf, 0, 1, 2, 2)))
  expect_true(!is.nan(pjsep(-Inf, 0, 1, 2, 2)) && is.finite(pjsep(-Inf, 0, 1, 2, 2)))
})

# -------------------------------Test Log Probability--------------------------------------------------

test_that("9. Check if log-probabilities are logs of probabilities (PDF's)", {
  
  x <- c(-Inf, -15, -10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 15, Inf)
  
  expect_equal(suppressWarnings(dmsnburr(x, 0, 1, 1, log = TRUE)),
               log(suppressWarnings(dmsnburr(x, 0, 1, 1))))
  expect_equal(suppressWarnings(dmsnburr2a(x, 0, 1, 1, log = TRUE)),
               log(suppressWarnings(dmsnburr2a(x, 0, 1, 1))))
  expect_equal(suppressWarnings(dgmsnburr(x, 0, 1, 1, 1, log = TRUE)),
               log(suppressWarnings(dgmsnburr(x, 0, 1, 1, 1))))
  expect_equal(suppressWarnings(djfst(x, 0, 1, 2, 2, log = TRUE)),
               log(suppressWarnings(djfst(x, 0, 1, 2, 2))))
  expect_equal(suppressWarnings(dfossep(x, 0, 1, 2, 2, log = TRUE)),
               log(suppressWarnings(dfossep(x, 0, 1, 2, 2))))    
  expect_equal(suppressWarnings(djsep(x, 0, 1, 2, 2, log = TRUE)),
               log(suppressWarnings(djsep(x, 0, 1, 2, 2))))
  
  
})

test_that("10. Check if log-probabilities are logs of probabilities (CDF's)", {
  
  x <- c(-Inf, -15, -10, -5, -1, 0, 1, 5, 10, 15, Inf)
  
  expect_equal(suppressWarnings(pmsnburr(x, 0, 1, 1, log.p = TRUE)),
               log(suppressWarnings(pmsnburr(x, 0, 1, 1))))
  expect_equal(suppressWarnings(pmsnburr2a(x, 0, 1, 1, log.p = TRUE)),
               log(suppressWarnings(pmsnburr2a(x, 0, 1, 1))))
  expect_equal(suppressWarnings(pgmsnburr(x, 0, 1, 1, 1, log.p = TRUE)),
               log(suppressWarnings(pgmsnburr(x, 0, 1, 1, 1))))
  expect_equal(suppressWarnings(pjfst(x, 0, 1, 2, 2, log.p = TRUE)),
               log(suppressWarnings(pjfst(x, 0, 1, 2, 2))))
  expect_equal(suppressWarnings(pfossep(x, 0, 1, 2, 2, log.p = TRUE)),
               log(suppressWarnings(pfossep(x, 0, 1, 2, 2))))
  expect_equal(suppressWarnings(pjsep(x, 0, 1, 2, 2, log.p = TRUE)),
               log(suppressWarnings(pjsep(x, 0, 1, 2, 2))))
  
})

# -------------------------------------Test NAs-------------------------------------

test_that("11. Missing values in PDF and PMF functions", {
  
  expect_true(is.na(dmsnburr(NA, 0, 1, 1)))
  expect_error(expect_true(is.na(dmsnburr(1, NA, 1, 1))))
  expect_error(expect_true(is.na(dmsnburr(1, 0, NA, 1))))
  expect_error(expect_true(is.na(dmsnburr(1, 0, 1, NA))))
  
  
  expect_true(is.na(dmsnburr2a(NA, 0, 1, 1)))
  expect_error(expect_true(is.na(dmsnburr2a(1, NA, 1, 1))))
  expect_error(expect_true(is.na(dmsnburr2a(1, 0, NA, 1))))
  expect_error(expect_true(is.na(dmsnburr2a(1, 0, 1, NA))))
  
  expect_true(is.na(dgmsnburr(NA, 0, 1, 1, 1)))
  expect_error(expect_true(is.na(dgmsnburr(1, NA, 1, 1, 1))))
  expect_error(expect_true(is.na(dgmsnburr(1, 0, NA, 1, 1))))
  expect_error(expect_true(is.na(dgmsnburr(1, 0, 1, NA, 1))))
  expect_error(expect_true(is.na(dgmsnburr(1, 0, 1, 1, NA))))
  
  
  expect_true(is.na(djfst(NA, 0, 1, 2, 2)))
  expect_error(expect_true(is.na(djfst(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(djfst(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(djfst(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(djfst(1, 0, 1, 2, NA))))
  
  
  expect_true(is.na(dfossep(NA, 0, 1, 2, 2)))
  expect_error(expect_true(is.na(dfossep(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(dfossep(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(dfossep(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(dfossep(1, 0, 1, 2, NA))))
  

  expect_true(is.na(djsep(NA, 0, 1, 2, 2)))
  expect_error(expect_true(is.na(djsep(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(djsep(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(djsep(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(djsep(1, 0, 1, 2, NA))))
  
})

test_that("12. Missing values  in CDF functions", {
  
  expect_true(is.na(pmsnburr(NA, 0, 1, 1)))
  expect_error(expect_true(is.na(pmsnburr(1, NA, 1, 1))))
  expect_error(expect_true(is.na(pmsnburr(1, 0, NA, 1))))
  expect_error(expect_true(is.na(pmsnburr(1, 0, 1, NA))))
  
  expect_true(is.na(pmsnburr2a(NA, 0, 1, 1)))
  expect_error(expect_true(is.na(pmsnburr2a(1, NA, 1, 1))))
  expect_error(expect_true(is.na(pmsnburr2a(1, 0, NA, 1))))
  expect_error(expect_true(is.na(pmsnburr2a(1, 0, 1, NA))))
  

  expect_true(is.na(pgmsnburr(NA, 0, 1, 1, 1)))
  expect_error(expect_true(is.na(pgmsnburr(1, NA, 1, 1, 1))))
  expect_error(expect_true(is.na(pgmsnburr(1, 0, NA, 1, 1))))
  expect_error(expect_true(is.na(pgmsnburr(1, 0, 1, NA, 1))))
  expect_error(expect_true(is.na(pgmsnburr(1, 0, 1, 1, NA))))
  

  expect_error(expect_true(is.na(pjfst(NA, 0, 1, 0, 0.2))))
  expect_error(expect_true(is.na(pjfst(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(pjfst(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(pjfst(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(pjfst(1, 0, 1, 2, NA))))
  
  expect_error(expect_true(is.na(pfossep(NA, 0, 1, 0, 0.2))))
  expect_error(expect_true(is.na(pfossep(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(pfossep(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(pfossep(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(pfossep(1, 0, 1, 2, NA)))) 


  expect_error(expect_true(is.na(pjsep(NA, 0, 1, 0, 0.2))))
  expect_error(expect_true(is.na(pjsep(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(pjsep(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(pjsep(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(pjsep(1, 0, 1, 2, NA)))) 
  
})

test_that("13.  Missing values in inverse CDF functions", {
  
  expect_error(expect_true(is.na(qmsnburr(NA, 0, 1, 1))))
  expect_error(expect_true(is.na(qmsnburr(1, NA, 1, 1))))
  expect_error(expect_true(is.na(qmsnburr(1, 0, NA, 1))))
  expect_error(expect_true(is.na(qmsnburr(1, 0, 1, NA))))
  
  expect_error(expect_true(is.na(qmsnburr2a(NA, 0, 1, 1))))
  expect_error(expect_true(is.na(qmsnburr2a(1, NA, 1, 1))))
  expect_error(expect_true(is.na(qmsnburr2a(1, 0, NA, 1))))
  expect_error(expect_true(is.na(qmsnburr2a(1, 0, 1, NA))))
  
  expect_error(expect_true(is.na(qgmsnburr(NA, 0, 1, 1, 1))))
  expect_error(expect_true(is.na(qgmsnburr(1, NA, 1, 1, 1))))
  expect_error(expect_true(is.na(qgmsnburr(1, 0, NA, 1, 1))))
  expect_error(expect_true(is.na(qgmsnburr(1, 0, 1, NA, 1))))
  expect_error(expect_true(is.na(qgmsnburr(1, 0, 1, 1, NA))))
  
  expect_error(expect_true(is.na(qjfst(NA, 0, 1, 2, 2))))
  expect_error(expect_true(is.na(qjfst(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(qjfst(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(qjfst(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(qjfst(1, 0, 1, 2, NA))))
  
  expect_error(expect_true(is.na(qfossep(NA, 0, 1, 2, 2))))
  expect_error(expect_true(is.na(qfossep(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(qfossep(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(qfossep(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(qfossep(1, 0, 1, 2, NA))))
  

  expect_error(expect_true(is.na(qjsep(NA, 0, 1, 2, 2))))
  expect_error(expect_true(is.na(qjsep(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(qjsep(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(qjsep(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(qjsep(1, 0, 1, 2, NA))))
})

test_that("14. Missing values in RNG functions", {
  
  expect_error(expect_true(is.na(rmsnburr(1, NA, 1, 1))))
  expect_error(expect_true(is.na(rmsnburr(1, 0, NA, 1))))
  expect_error(expect_true(is.na(rmsnburr(1, 0, 1, NA))))

  expect_error(expect_true(is.na(rmsnburr2a(1, NA, 1, 1))))
  expect_error(expect_true(is.na(rmsnburr2a(1, 0, NA, 1))))
  expect_error(expect_true(is.na(rmsnburr2a(1, 0, 1, NA))))
  
  expect_error(expect_true(is.na(rgmsnburr(1, NA, 1, 1, 1))))
  expect_error(expect_true(is.na(rgmsnburr(1, 0, NA, 1, 1))))
  expect_error(expect_true(is.na(rgmsnburr(1, 0, 1, NA, 1))))
  expect_error(expect_true(is.na(rgmsnburr(1, 0, 1, 1, NA))))
  
  expect_error(expect_true(is.na(rjfst(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(rjfst(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(rjfst(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(rjfst(1, 0, 1, 2, NA))))
  
  expect_error(expect_true(is.na(rfossep(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(rfossep(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(rfossep(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(rfossep(1, 0, 1, 2, NA))))

  expect_error(expect_true(is.na(rjsep(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(rjsep(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(rjsep(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(rjsep(1, 0, 1, 2, NA))))
  
})

# --------------------- Test Probabillity -------------------------------------------

test_that("15. All probabilities/densities >= 0", {
  
  y <- c( -100, -10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 100)
  
  expect_true(suppressWarnings(all(dmsnburr(y, 0, 1, 1) >= 0)))
  expect_true(suppressWarnings(all(dmsnburr2a(y, 0, 1, 1) >= 0)))
  expect_true(suppressWarnings(all(dgmsnburr(y, 0, 1, 1, 1) >= 0)))
  expect_true(suppressWarnings(all(djfst(y, 0, 1, 2, 2) >= 0)))
  expect_true(suppressWarnings(all(dfossep(y, 0, 1, 2, 2) >= 0)))
  expect_true(suppressWarnings(all(djsep(y, 0, 1, 2, 2) >= 0)))

})

test_that("16. All cumulative probabilities >= 0 and <= 1", {
  
  y <- c( -100, -10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 100)
  
  expect_true(all(pmsnburr(y, 0, 1, 1) >= 0 & pmsnburr(y, 0, 1, 1) <= 1))
  expect_true(all(pmsnburr2a(y, 0, 1, 1) >= 0 & pmsnburr2a(y, 0, 1, 1) <= 1))
  expect_true(all(pgmsnburr(y, 0, 1, 1, 1) >= 0 & pgmsnburr(y, 0, 1, 1, 1) <= 1))
  expect_true(all(pjfst(y, 0, 1, 2, 2) >= 0 & pjfst(y, 0, 1, 2,2) <= 1))
  expect_true(all(pfossep(y, 0, 1, 2, 2) >= 0 & pfossep(y, 0, 1, 2,2) <= 1))
  expect_true(all(pjsep(y, 0, 1, 2, 2) >= 0 & pjsep(y, 0, 1, 2,2) <= 1))

})


# -------------------------------------Test Quantile Function ------------------------------

test_that("17. Zeros in quantile functions", {

  expect_error(expect_true(!is.nan(qmsnburr(0, 0, 1, 1))))
  expect_error(expect_true(!is.nan(qmsnburr2a(0, 0, 1, 1))))
  expect_error(expect_true(!is.nan(qgmsnburr(0, 0, 1, 1, 1))))
  expect_error(expect_true(!is.nan(qjfst(0, 0, 1, 2, 2))))
  expect_error(expect_true(!is.nan(qfossep(0, 0, 1, 2, 2))))
  expect_error( expect_true(!is.nan(qjsep(0, 0, 1, 2, 2))))
  
})

test_that("18. Ones in quantile functions", {
  
  expect_error(expect_true(!is.nan(qmsnburr(1, 0, 1, 1))))
  expect_error(expect_true(!is.nan(qmsnburr2a(1, 0, 1, 1))))
  expect_error(expect_true(!is.nan(qgmsnburr(1, 0, 1, 1, 1))))
  expect_error(expect_true(!is.nan(qjfst(1, 0, 1, 2, 2))))
  expect_error(expect_true(!is.nan(qfossep(1, 0, 1, 2, 2))))
  expect_error(expect_true(!is.nan(qjsep(1, 0, 1, 2, 2))))

})

test_that("19. Checking p = F(F^-1(p))", {
  
  pp <- seq(0.00001, 1, by = 0.001)
  
  expect_equal(pp, pmsnburr(qmsnburr(pp, 0, 1, 1), 0, 1, 1))
  expect_equal(pp, pmsnburr2a(qmsnburr2a(pp, 0, 1, 1), 0, 1, 1))
  expect_equal(pp, pgmsnburr(qgmsnburr(pp, 0, 1, 1, 1), 0, 1, 1, 1))
  expect_equal(pp, pjfst(qjfst(pp, 0, 1, 2, 2), 0, 1, 2, 2))
  expect_equal(pp, pfossep(qfossep(pp, 0, 1, 2, 2), 0, 1, 2, 2))
  expect_equal(pp, pjsep(qjsep(pp, 0, 1, 2, 2), 0, 1, 2, 2))

  
})

# ----------------------------- Test RNG Converge -------------------------------------

probCoverage <- function(stub, ..., n = 5000L) {
  rfoo <- eval(as.name(paste0("r", stub)))
  pfoo <- eval(as.name(paste0("p", stub)))
  diff(pfoo(range(rfoo(n, ...)), ...))
}



test_that("20. Coverage of RNG's", {
  
  skip_on_cran()
  
  expect_gte(probCoverage("msnburr", 0, 1, 1), 0.99)
  
  expect_gte(probCoverage("msnburr2a", 0, 1, 1), 0.99)
  
  expect_gte(probCoverage("gmsnburr", 0, 1, 1, 1), 0.99)
  
  expect_gte(probCoverage("jfst", 0, 1, 2, 2), 0.99)
  
  expect_gte(probCoverage("fossep", 0, 1, 2, 2), 0.99)
  
  expect_gte(probCoverage("jsep", 0, 1, 2, 2), 0.99)
  
  
})


# -------------------------- PDF and CDF Test -----------------------------------------
library(neodistr)
library (testthat)

test_that("21. integrate PDF from -inf to inf == 1", {
  
  expect_equal(integrate(dmsnburr,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1 )$value,1)

  expect_equal(integrate(dmsnburr2a,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1 )$value,1)

  expect_equal(integrate(dgmsnburr,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1,beta=1 )$value,1)

  expect_equal(integrate(djfst,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=2,beta= 2 )$value,1)

  expect_equal(integrate(dfossep,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=2,beta= 2 )$value,1)

  expect_equal(integrate(djsep,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=2,beta= 2 )$value,1)

 })

test_that("22. integrate PDF from - Inf until x equal to cdf x", {
  
  expect_equal((integrate(dmsnburr, lower= -Inf, upper=4, mu=0, sigma=1,alpha=1)$value),(pmsnburr(4, mu=0, sigma=1, alpha=1)))

  expect_equal((integrate(dmsnburr2a, lower= -Inf, upper=4, mu=0, sigma=1,alpha=1)$value) , (pmsnburr2a(4, mu=0, sigma=1, alpha=1)))

  expect_equal((integrate(dgmsnburr, lower= -Inf, upper=4, mu=0, sigma=1,alpha=1, beta=1)$value), (pgmsnburr(4, mu=0, sigma=1, alpha=1, beta=1)))

  expect_equal((integrate(djfst, lower= -Inf, upper=4, mu=0, sigma=1,alpha=2, beta=2)$value) , (pjfst(4, mu=0, sigma=1, alpha=2, beta=2)))

  expect_equal((integrate(dfossep, lower= -Inf, upper=4, mu=0, sigma=1,alpha=2, beta=2, rel.tol = 1e-10, abs.tol = 1e-12)$value), (pfossep(4, mu=0, sigma=1, alpha=2, beta=2)))

  expect_equal((integrate(djsep, lower= -Inf, upper=4, mu=0, sigma=1,alpha=2, beta=2)$value) , (pjsep(4, mu=0, sigma=1, alpha=2, beta=2)))
})

if (interactive() || identical(Sys.getenv("NOT_CRAN"), "true")) {
# -------------------------------Test Stan Functions --------------------------------
# Vector
pdf_msnburr <- neodistr:::neonormal_stanfunc(family ="msnburr", func = "pdf",vectorize = TRUE )
cdf_msnburr <- neodistr:::neonormal_stanfunc(family ="msnburr", func = "cdf",vectorize = TRUE )
ccdf_msnburr <- neodistr:::neonormal_stanfunc(family ="msnburr", func = "ccdf",vectorize = TRUE)
lcdf_msnburr <- neodistr:::neonormal_stanfunc(family ="msnburr", func = "lcdf",vectorize = TRUE)

pdf_msnburr2a <- neodistr:::neonormal_stanfunc(family ="msnburr2a", func = "pdf",vectorize = TRUE )
cdf_msnburr2a <- neodistr:::neonormal_stanfunc(family ="msnburr2a", func = "cdf",vectorize = TRUE )
ccdf_msnburr2a <- neodistr:::neonormal_stanfunc(family ="msnburr2a", func = "ccdf",vectorize = TRUE)
lcdf_msnburr2a <- neodistr:::neonormal_stanfunc(family ="msnburr2a", func = "lcdf",vectorize = TRUE)

pdf_gmsnburr <- neodistr:::neonormal_stanfunc(family ="gmsnburr", func = "pdf",vectorize = TRUE )
cdf_gmsnburr <- neodistr:::neonormal_stanfunc(family ="gmsnburr", func = "cdf",vectorize = TRUE )
ccdf_gmsnburr <- neodistr:::neonormal_stanfunc(family ="gmsnburr", func = "ccdf",vectorize = TRUE)
lcdf_gmsnburr <- neodistr:::neonormal_stanfunc(family ="gmsnburr", func = "lcdf",vectorize = TRUE)

pdf_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "pdf",vectorize = TRUE )
cdf_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "cdf",vectorize = TRUE )
ccdf_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "ccdf",vectorize = TRUE)
lcdf_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "lcdf",vectorize = TRUE)

pdf_fossep <- neodistr:::neonormal_stanfunc(family ="fossep", func = "pdf",vectorize = TRUE )
cdf_fossep <- neodistr:::neonormal_stanfunc(family ="fossep", func = "cdf",vectorize = TRUE )
ccdf_fossep <- neodistr:::neonormal_stanfunc(family ="fossep", func = "ccdf",vectorize = TRUE)
lcdf_fossep <- neodistr:::neonormal_stanfunc(family ="fossep", func = "lcdf",vectorize = TRUE)
# 
pdf_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "pdf",vectorize = TRUE )
cdf_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "cdf",vectorize = TRUE )
ccdf_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "ccdf",vectorize = TRUE)
lcdf_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "lcdf",vectorize = TRUE)

# Non-vector
pdf0_msnburr <- neodistr:::neonormal_stanfunc(family ="msnburr", func = "pdf",vectorize = FALSE )
cdf0_msnburr <- neodistr:::neonormal_stanfunc(family ="msnburr", func = "cdf",vectorize = FALSE )
ccdf0_msnburr <- neodistr:::neonormal_stanfunc(family ="msnburr", func = "ccdf",vectorize = FALSE )
lcdf0_msnburr <- neodistr:::neonormal_stanfunc(family ="msnburr", func = "lcdf",vectorize = FALSE )
quan_msnburr <- neodistr:::neonormal_stanfunc(family ="msnburr", func = "quantile" )
rng_msnburr <- neodistr:::neonormal_stanfunc(family ="msnburr", func = "rng")
 
pdf0_msnburr2a <- neodistr:::neonormal_stanfunc(family ="msnburr2a", func = "pdf",vectorize = FALSE )
cdf0_msnburr2a <- neodistr:::neonormal_stanfunc(family ="msnburr2a", func = "cdf",vectorize = FALSE )
ccdf0_msnburr2a <- neodistr:::neonormal_stanfunc(family ="msnburr2a", func = "ccdf",vectorize = FALSE )
lcdf0_msnburr2a <- neodistr:::neonormal_stanfunc(family ="msnburr2a", func = "lcdf",vectorize = FALSE )
quan_msnburr2a <- neodistr:::neonormal_stanfunc(family ="msnburr2a", func = "quantile" )
rng_msnburr2a <- neodistr:::neonormal_stanfunc(family ="msnburr2a", func = "rng" )
# 
pdf0_gmsnburr <- neodistr:::neonormal_stanfunc(family ="gmsnburr", func = "pdf",vectorize = FALSE )
cdf0_gmsnburr <- neodistr:::neonormal_stanfunc(family ="gmsnburr", func = "cdf",vectorize = FALSE )
ccdf0_gmsnburr <- neodistr:::neonormal_stanfunc(family ="gmsnburr", func = "ccdf",vectorize = FALSE )
lcdf0_gmsnburr <- neodistr:::neonormal_stanfunc(family ="gmsnburr", func = "lcdf",vectorize = FALSE )
quan_gmsnburr <- neodistr:::neonormal_stanfunc(family ="gmsnburr", func = "quantile" )
rng_gmsnburr <- neodistr:::neonormal_stanfunc(family ="gmsnburr", func = "rng" )
# 
pdf0_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "pdf",vectorize = FALSE )
cdf0_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "cdf",vectorize = FALSE )
ccdf0_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "ccdf",vectorize = FALSE )
lcdf0_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "lcdf",vectorize = FALSE )
quan_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "quantile" )
rng_jfst <- neodistr:::neonormal_stanfunc(family ="jfst", func = "rng")

pdf0_fossep <- neodistr:::neonormal_stanfunc(family ="fossep", func = "pdf",vectorize = FALSE )
cdf0_fossep <- neodistr:::neonormal_stanfunc(family ="fossep", func = "cdf",vectorize = FALSE )
ccdf0_fossep <- neodistr:::neonormal_stanfunc(family ="fossep", func = "ccdf",vectorize = FALSE )
lcdf0_fossep <- neodistr:::neonormal_stanfunc(family ="fossep", func = "lcdf",vectorize = FALSE )
#quan_fossep <- neodistr:::neonormal_stanfunc(family ="fossep", func = "quantile")
#rng_fossep <- neodistr:::neonormal_stanfunc(family ="fossep", func = "rng")


pdf0_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "pdf",vectorize = FALSE )
cdf0_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "cdf",vectorize = FALSE )
ccdf0_jsep <- neodistr:::neonormal_stanfunc(family ="joep", func = "ccdf",vectorize = FALSE )
lcdf0_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "lcdf",vectorize = FALSE )
#quan_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "quantile")
#rng_jsep <- neodistr:::neonormal_stanfunc(family ="jsep", func = "rng")
} else {
  message("neonormal_stanfunc() skipped: running on CRAN")
}

# ----------------------------Test Inapproriate parameters-------------------------------------------


test_that("1. Wrong parameter values in Stan PDF and PMF functions", {
  skip_on_cran()
  
  # MSNBurr Distribution
  expect_error(expect_true(is.nan(pdf_msnburr(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(pdf_msnburr(1, 1, 1, -1))))
  
  expect_error(expect_true(is.nan(pdf0_msnburr(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(pdf0_msnburr(1, 1, 1, -1))))
  
  # MSNBurr-IIa  Distribution  
  expect_error(expect_true(is.nan(pdf_msnburr2a(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(pdf_msnburr2a(1, 1, 1, -1))))
  
  expect_error(expect_true(is.nan(pdf0_msnburr2a(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(pdf0_msnburr2a(1, 1, 1, -1))))
  
  # GMSNBurr Distribution

  expect_error(expect_true(is.nan(pdf_gmsnburr(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(pdf_gmsnburr(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(pdf_gmsnburr(1, 1, 1, 1, -1))))
  
  expect_error(expect_true(is.nan(pdf0_gmsnburr(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(pdf0_gmsnburr(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(pdf0_gmsnburr(1, 1, 1, 1, -1))))
  
  # JFST distribution
  expect_error(expect_true(is.nan(pdf_jfst(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(pdf_jfst(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(pdf_jfst(1, 1, 1, 2, -2)))) 

  expect_error(expect_true(is.nan(pdf0_jfst(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(pdf0_jfst(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(pdf0_jfst(1, 1, 1, 2, -2))))
  
  # FOSSEP distribution
  expect_error(expect_true(is.nan(pdf_fossep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(pdf_fossep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(pdf_fossep(1, 1, 1, 2, -2))))
  
  expect_error(expect_true(is.nan(pdf0_fossep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(pdf0_fossep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(pdf0_fossep(1, 1, 1, 2, -2))))
  
 # JSEP Distribution 
  expect_error(expect_true(is.nan(pdf_jsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(pdf_jsep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(pdf_jsep(1, 1, 1, 2, -2)))) 
  
  expect_error(expect_true(is.nan(pdf0_jsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(pdf0_jsep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(pdf0_jsep(1, 1, 1, 2, -2)))) 
  
})


test_that("2. Wrong parameter values in Stan CDF functions", {
  skip_on_cran() 
  # Vector

  # MSNBurr Distribution
  # Vector
  expect_error(expect_true(is.nan(cdf_msnburr(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(cdf_msnburr(1, 1, 1, -1))))
  # Non-vector
  expect_error(expect_true(is.nan(cdf0_msnburr(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(cdf0_msnburr(1, 1, 1, -1))))
  
  # MSNBurr-IIa  Distribution
  # Vector
  expect_error(expect_true(is.nan(cdf_msnburr2a(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(cdf_msnburr2a(1, 1, 1, -1))))
  # Non-vector
  expect_error(expect_true(is.nan(cdf0_msnburr2a(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(cdf0_msnburr2a(1, 1, 1, -1))))
  
# GMSNBurr Distribution
  # Vector
  expect_error(expect_true(is.nan(cdf_gmsnburr(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(cdf_gmsnburr(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(cdf_gmsnburr(1, 1, 1, 1, -1))))
  # Non-vector
  expect_error(expect_true(is.nan(cdf0_gmsnburr(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(cdf0_gmsnburr(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(cdf0_gmsnburr(1, 1, 1, 1, -1))))

# JFST distribution
  # Vector
  expect_error(expect_true(is.nan(cdf_jfst(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(cdf_jfst(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(cdf_jfst(1, 1, 1, 2, -2))))
  # Non-vector
  expect_error(expect_true(is.nan(cdf0_jfst(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(cdf0_jfst(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(cdf0_jfst(1, 1, 1, 2, -2)))) 
# FOSSEP distribution 
  # Vector
  expect_error(expect_true(is.nan(cdf_fossep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(cdf_fossep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(cdf_fossep(1, 1, 1, 2, -2))))
  # Non-vector
  expect_error(expect_true(is.nan(cdf0_fossep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(cdf0_fossep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(cdf0_fossep(1, 1, 1, 2, -2))))
# JSEP Distribution
  #Vector
  expect_error(expect_true(is.nan(cdf_jsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(cdf_jsep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(cdf_jsep(1, 1, 1, 2, -2))))
  # Non-vector
  expect_error(expect_true(is.nan(cdf0_jsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(cdf0_jsep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(cdf0_jsep(1, 1, 1, 2, -2))))
  
})


test_that("3. Wrong parameter values in Stan quantile functions", {
  skip_on_cran()

  # MSNBurr Distribution
   expect_error(expect_true(is.nan(quan_msnburr(0.5, 1, -1, 1))))
   expect_error(expect_true(is.nan(quan_msnburr(0.5, 1, 1, -1))))
  
  # MSNBurr-IIa  Distribution
   expect_error(expect_true(is.nan(quan_msnburr2a(0.5, 1, -1, 1))))
   expect_error(expect_true(is.nan(quan_msnburr2a(0.5, 1, 1, -1))))
 
  # GMSNBurr Distribution
  
  expect_error(expect_true(is.nan(quan_gmsnburr(0.5, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(quan_gmsnburr(0.5, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(quan_gmsnburr(0.5, 1, 1, 1, -1))))
  
  
  # JFST distribution
  
   expect_error(expect_true(is.nan(quan_jfst(1, 1, -1, 2, 2))))
   expect_error(expect_true(is.nan(quan_jfst(1, 1, 1, -2, 2))))
   expect_error(expect_true(is.nan(quan_jfst(1, 1, 1, 2, -2))))
  
  # FOSSEP distribution
   
   expect_error(expect_true(is.nan(quan_fossep(1, 1, -1, 2, 2))))
   expect_error(expect_true(is.nan(quan_fossep(1, 1, 1, -2, 2))))
   expect_error(expect_true(is.nan(quan_fossep(1, 1, 1, 2, -2))))
  # JSEP Distribution
   expect_error(expect_true(is.nan(quan_jsep(1, 1, -1, 2, 2))))
   expect_error(expect_true(is.nan(quan_jsep(1, 1, 1, -2, 2))))
   expect_error(expect_true(is.nan(quan_jsep(1, 1, 1, 2, -2))))
  
})


test_that("4. Wrong parameter values in RNG functions", {
  skip_on_cran()

  expect_error(expect_true(is.nan(rng_msnburr(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(rng_msnburr(1, 1, 1, -1))))
  
  expect_error(expect_true(is.nan(rng_msnburr2a(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(rng_msnburr2a(1, 1, 1, -1))))
  
  
   expect_error(expect_true(is.nan(rgmsnburr(1, 1, -1, 1, 1))))
   expect_error(expect_true(is.nan(rgmsnburr(1, 1, 1, -1, 1))))
   expect_error(expect_true(is.nan(rgmsnburr(1, 1, 1, 1, -1))))
  
   expect_error(expect_true(is.nan(rng_gmsnburr(1, 1, -1, 1, 1))))
   expect_error(expect_true(is.nan(rng_gmsnburr(1, 1, 1, -1, 1))))
   expect_error(expect_true(is.nan(rng_gmsnburr(1, 1, 1, 1, -1))))
  # 
  
  # 
   expect_error(expect_true(is.nan(rng_jfst(1, 1, -1, 2, 2))))
   expect_error(expect_true(is.nan(rng_jfst(1, 1, 1, -2,2 ))))
   expect_error(expect_true(is.nan(rng_jfst(1, 1, 1, 2, -2))))
  
  
  expect_error(expect_true(is.nan(rng_fossep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(rng_fossep(1, 1, 1, 2, -2))))
  
  expect_error(expect_true(is.nan(rng_jsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(rng_jsep(1, 1, 1, 2, -2)))) 
})

test_that("5. Wrong parameter values in CCDF functions", {
  skip_on_cran()
   expect_error(expect_true(is.nan(ccdf_msnburr(1, 1, -1, 1))))
   expect_error(expect_true(is.nan(ccdf_msnburr(1, 1, 1, -1))))
  # 
   expect_error(expect_true(is.nan(ccdf0_msnburr(1, 1, -1, 1))))
   expect_error(expect_true(is.nan(ccdf0_msnburr(1, 1, 1, -1))))
  # 
  # 
   expect_error(expect_true(is.nan(ccdf_msnburr2a(1, 1, -1, 1))))
   expect_error(expect_true(is.nan(ccdf_msnburr2a(1, 1, 1, -1))))
  # 
   expect_error(expect_true(is.nan(ccdf0_msnburr2a(1, 1, -1, 1))))
   expect_error(expect_true(is.nan(ccdf0_msnburr2a(1, 1, 1, -1))))
  # 
  # 
   expect_error(expect_true(is.nan(ccdf_gmsnburr(1, 1, -1, 1, 1))))
   expect_error(expect_true(is.nan(ccdf_gmsnburr(1, 1, 1, -1, 1))))
   expect_error(expect_true(is.nan(ccdf_gmsnburr(1, 1, 1, 1, -1))))
  # 
   expect_error(expect_true(is.nan(ccdf0_gmsnburr(1, 1, -1, 1, 1))))
   expect_error(expect_true(is.nan(ccdf0_gmsnburr(1, 1, 1, -1, 1))))
   expect_error(expect_true(is.nan(ccdf0_gmsnburr(1, 1, 1, 1, -1))))
  # 
  # 
   expect_error(expect_true(is.nan(ccdf_jfst(1, 1, -1, 2, 2))))
   expect_error(expect_true(is.nan(ccdf_jfst(1, 1, 1, -2, 2))))
   expect_error(expect_true(is.nan(rng_jfst(1, 1, 1, 2, -2))))
  # 
   expect_error(expect_true(is.nan(ccdf0_jfst(1, 1, -1, 2, 2))))
   expect_error(expect_true(is.nan(ccdf0_jfst(1, 1, -1, 2, 2))))
   expect_error(expect_true(is.nan(ccdf0_jfst(1, 1, 1, 2, -2))))
  # 
  # 
  expect_error(expect_true(is.nan(ccdf_fossep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(ccdf_fossep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(ccdf_fossep(1, 1, 1, 2, -2))))
  
  expect_error(expect_true(is.nan(ccdf0_fossep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(ccdf0_fossep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(ccdf0_fossep(1, 1, 1, 2, -2))))

  
  # 
  expect_error(expect_true(is.nan(ccdf_jsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(ccdf_jsep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(ccdf_jsep(1, 1, 1, 2, -2))))
  
  # 
  expect_error(expect_true(is.nan(ccdf0_jsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(ccdf0_jsep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(ccdf0_jsep(1, 1, 1, 2, -2))))
  
})

test_that("6. Wrong parameter values in LCDF functions", {
  skip_on_cran()
  expect_error(expect_true(is.nan(lcdf_msnburr(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(lcdf_msnburr(1, 1, 1, -1))))
  #   
  expect_error(expect_true(is.nan(lcdf0_msnburr(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(lcdf0_msnburr(1, 1, 1, -1))))
  #   
  #   
  expect_error(expect_true(is.nan(lcdf_msnburr2a(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(lcdf_msnburr2a(1, 1, 1, -1))))
  #   
   expect_error(expect_true(is.nan(lcdf0_msnburr2a(1, 1, -1, 1))))
   expect_error(expect_true(is.nan(lcdf0_msnburr2a(1, 1, 1, -1))))
  #   
  #   
   expect_error(expect_true(is.nan(lcdf_gmsnburr(1, 1, -1, 1, 1))))
   expect_error(expect_true(is.nan(lcdf_gmsnburr(1, 1, 1, -1, 1))))
   expect_error(expect_true(is.nan(lcdf_gmsnburr(1, 1, 1, 1, -1))))
  #   
   expect_error(expect_true(is.nan(lcdf0_gmsnburr(1, 1, -1, 1, 1))))
   expect_error(expect_true(is.nan(lcdf0_gmsnburr(1, 1, 1, -1, 1))))
   expect_error(expect_true(is.nan(lcdf0_gmsnburr(1, 1, 1, 1, -1))))
  #   
  #   
     expect_error(expect_true(is.nan(lcdf_jfst(1, 1, -1, 2, 2))))
     expect_error(expect_true(is.nan(lcdf_jfst(1, 1, 1, -2, 2))))
     expect_error(expect_true(is.nan(lcdf_jfst(1, 1, 1, 2, -2))))
  #   
     expect_error(expect_true(is.nan(lcdf0_jfst(1, 1, -1, 2, 2))))
     expect_error(expect_true(is.nan(lcdf0_jfst(1, 1, 1,-2,2))))
     expect_error(expect_true(is.nan(lcdf0_jfst(1, 1, 1,2,-2))))
  #   
  #   
  expect_error(expect_true(is.nan(lcdf_fossep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(lcdf_fossep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(lcdf_fossep(1, 1, 1, 2, -2))))
  
  expect_error(expect_true(is.nan(lcdf0_fossep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(lcdf0_fossep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(lcdf0_fossep(1, 1, 1, 2, -2))))
  
    #   
  #
  expect_error(expect_true(is.nan(lcdf_jsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(lcdf_jsep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(lcdf_jsep(1, 1, 1, 2, -2))))
  
  expect_error(expect_true(is.nan(lcdf0_jsep(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(lcdf0_jsep(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(lcdf0_jsep(1, 1, 1, 2, -2))))
  
})


# 
# ------------------------------------Infinity Test-----------------------------------------------
test_that("7. Testing PDFs  against infinite values", {
  skip_on_cran()
  expect_true(!is.nan(pdf_msnburr(Inf, 0, 1, 1)) && is.finite(pdf_msnburr(Inf, 0, 1, 1)))
  expect_true(!is.nan(pdf_msnburr2a(Inf, 0, 1, 1)) && is.finite(pdf_msnburr2a(Inf, 0, 1, 1)))
  expect_true(!is.nan(pdf_gmsnburr(Inf, 0, 1, 1, 1)) && is.finite(pdf_gmsnburr(Inf, 0, 1, 1, 1)))
  expect_true(!is.nan(pdf_jfst(Inf, 0, 1, 2, 2)) && is.finite(pdf_jfst(Inf, 0, 1, 2, 2)))
  expect_true(!is.nan(pdf_fossep(Inf, 0, 1, 2, 2)) && is.finite(pdf_fossep(Inf, 0, 1, 2, 2)))
  expect_true(!is.nan(pdf_jsep(Inf, 0, 1, 2, 2)) && is.finite(pdf_jsep(Inf, 0, 1, 2, 2)))
  
})


test_that("8. Testing CDFs against infinite values", {
  skip_on_cran()
  expect_true(!is.nan(cdf_msnburr(Inf, 0, 1, 1)) && is.finite(cdf_msnburr(Inf, 0, 1, 1)))
  expect_true(!is.nan(cdf_msnburr2a(Inf, 0, 1, 1)) && is.finite(cdf_msnburr2a(Inf, 0, 1, 1)))
  expect_true(!is.nan(cdf_gmsnburr(Inf, 0, 1, 1, 1)) && is.finite(cdf_gmsnburr(Inf, 0, 1, 1, 1)))
  expect_true(!is.nan(cdf_jfst(Inf, 0, 1, 2, 2)) && is.finite(cdf_jfst(Inf, 0, 1, 2, 2)))
  expect_true(!is.nan(cdf_fossep(Inf, 0, 1, 2, 2)) && is.finite(cdf_fossep(Inf, 0, 1, 2, 2)))
  expect_true(!is.nan(cdf_jsep(Inf, 0, 1, 2, 2)) && is.finite(cdf_jsep(Inf, 0, 1, 2, 2)))
})

test_that("9. Testing LCDFs against infinite values", {
  skip_on_cran()
  #   
#   
expect_true(!is.nan(lcdf_msnburr(Inf, 0, 1, 1)) && is.finite(lcdf_msnburr(Inf, 0, 1, 1)))
expect_true(!is.nan(lcdf_msnburr2a(Inf, 0, 1, 1)) && is.finite(lcdf_msnburr2a(Inf, 0, 1, 1)))
expect_true(!is.nan(lcdf_gmsnburr(Inf, 0, 1, 1, 1)) && is.finite(lcdf_gmsnburr(Inf, 0, 1, 1, 1)))
expect_true(!is.nan(lcdf_jfst(Inf, 0, 1, 2, 2)) && is.finite(lcdf_jfst(Inf, 0, 1, 2, 2)))
expect_true(!is.nan(lcdf_fossep(Inf, 0, 1, 2, 2)) && is.finite(lcdf_fossep(Inf, 0, 1, 2, 2)))
expect_true(!is.nan(lcdf_jsep(Inf, 0, 1, 2, 2)) && is.finite(lcdf_jsep(Inf, 0, 1, 2, 2)))
 })


test_that("10. Testing PDFs  against negatively infinite values", {
  skip_on_cran()
  expect_true(!is.nan(pdf_msnburr(-Inf, 0, 1, 1)) && is.finite(pdf_msnburr(-Inf, 0, 1, 1)))
  expect_true(!is.nan(pdf_msnburr2a(-Inf, 0, 1, 1)) && is.finite(pdf_msnburr2a(-Inf, 0, 1, 1)))
  expect_true(!is.nan(pdf_gmsnburr(-Inf, 0, 1, 1, 1)) && is.finite(pdf_gmsnburr(-Inf, 0, 1, 1, 1)))
  expect_true(!is.nan(pdf_jfst(-Inf, 0, 1, 2, 2)) && is.finite(pdf_jfst(-Inf, 0, 1, 2, 2)))
  expect_true(!is.nan(pdf_fossep(-Inf, 0, 1, 2, 2)) && is.finite(pdf_fossep(-Inf, 0, 1, 2, 2)))
  expect_true(!is.nan(pdf_jsep(-Inf, 0, 1, 2, 2)) && is.finite(pdf_jsep(-Inf, 0, 1, 2, 2)))
  
})


test_that("11. Testing CDFs against negatively infinite values", {
  skip_on_cran()
  expect_true(!is.nan(cdf_msnburr(-Inf, 0, 1, 1)) && is.finite(cdf_msnburr(-Inf, 0, 1, 1)))
  expect_true(!is.nan(cdf_msnburr2a(-Inf, 0, 1, 1)) && is.finite(cdf_msnburr2a(-Inf, 0, 1, 1)))
  expect_true(!is.nan(cdf_gmsnburr(-Inf, 0, 1, 1, 1)) && is.finite(cdf_gmsnburr(-Inf, 0, 1, 1, 1)))
  expect_true(!is.nan(cdf_jfst(-Inf, 0, 1, 2, 2)) && is.finite(cdf_jfst(-Inf, 0, 1, 2, 2))) 
  expect_true(!is.nan(cdf_fossep(-Inf, 0, 1, 2, 2)) && is.finite(cdf_fossep(-Inf, 0, 1, 2, 2))) 
  expect_true(!is.nan(cdf_jsep(-Inf, 0, 1, 2, 2)) && is.finite(cdf_jsep(-Inf, 0, 1, 2, 2))) 
})


test_that("12. Testing PDFs  against infinite values", {
 skip_on_cran()  
   expect_true(!is.nan(pdf0_msnburr(Inf, 0, 1, 1)) && is.finite(pdf0_msnburr(Inf, 0, 1, 1)))
   expect_true(!is.nan(pdf0_msnburr2a(Inf, 0, 1, 1)) && is.finite(pdf0_msnburr2a(Inf, 0, 1, 1)))
   expect_true(!is.nan(pdf0_gmsnburr(Inf, 0, 1, 1, 1)) && is.finite(pdf0_gmsnburr(Inf, 0, 1, 1, 1)))
   expect_true(!is.nan(pdf0_jfst(Inf, 0, 1, 2, 2)) && is.finite(pdf0_jfst(Inf, 0, 1, 2, 2)))
   expect_true(!is.nan(pdf0_jfst(Inf, 0, 1, 2, 2)) && is.finite(pdf0_jfst(Inf, 0, 1, 2, 2)))
   expect_true(!is.nan(pdf0_fossep(Inf, 0, 1, 2, 2)) && is.finite(pdf0_fossep(Inf, 0, 1, 2, 2)))
   expect_true(!is.nan(pdf0_jsep(Inf, 0, 1, 2, 2)) && is.finite(pdf0_jsep(Inf, 0, 1, 2, 2)))

})


test_that("13. Testing CDFs against infinite values", {
  skip_on_cran() 
   expect_true(!is.nan(cdf0_msnburr(Inf, 0, 1, 1)) && is.finite(cdf0_msnburr(Inf, 0, 1, 1)))
   expect_true(!is.nan(cdf0_msnburr2a(Inf, 0, 1, 1)) && is.finite(cdf0_msnburr2a(Inf, 0, 1, 1)))
   expect_true(!is.nan(cdf0_gmsnburr(Inf, 0, 1, 1, 1)) && is.finite(cdf0_gmsnburr(Inf, 0, 1, 1, 1)))
   expect_true(!is.nan(cdf0_jfst(Inf, 0, 1, 2, 2)) && is.finite(cdf0_jfst(Inf, 0, 1, 2, 2)))
   expect_true(!is.nan(cdf0_fossep(Inf, 0, 1, 2, 2)) && is.finite(cdf0_fossep(Inf, 0, 1, 2, 2)))
   expect_true(!is.nan(cdf0_jsep(Inf, 0, 1, 2, 2)) && is.finite(cdf0_jsep(Inf, 0, 1, 2, 2)))
   })

 test_that("14. Testing LCDFs against infinite values", {
  skip_on_cran() 
#   
   expect_true(!is.nan(lcdf0_msnburr(Inf, 0, 1, 1)) && is.finite(lcdf0_msnburr(Inf, 0, 1, 1)))
   expect_true(!is.nan(lcdf0_msnburr2a(Inf, 0, 1, 1)) && is.finite(lcdf0_msnburr2a(Inf, 0, 1, 1)))
   expect_true(!is.nan(lcdf0_gmsnburr(Inf, 0, 1, 1, 1)) && is.finite(lcdf0_gmsnburr(Inf, 0, 1, 1, 1)))
   expect_true(!is.nan(lcdf0_jfst(Inf, 0, 1, 2, 2)) && is.finite(lcdf0_jfst(Inf, 0, 1, 2, 2)))
   expect_true(!is.nan(lcdf0_fossep(Inf, 0, 1, 2, 2)) && is.finite(lcdf0_fossep(Inf, 0, 1, 2, 2)))
   expect_true(!is.nan(lcdf0_jsep(Inf, 0, 1, 2, 2)) && is.finite(lcdf0_jsep(Inf, 0, 1, 2, 2)))
 })


 test_that("15. Testing PDFs  against negatively infinite values", {
 skip_on_cran()  
   expect_true(!is.nan(pdf0_msnburr(-Inf, 0, 1, 1)) && is.finite(pdf0_msnburr(-Inf, 0, 1, 1)))
   expect_true(!is.nan(pdf0_msnburr2a(-Inf, 0, 1, 1)) && is.finite(pdf0_msnburr2a(-Inf, 0, 1, 1)))
   expect_true(!is.nan(pdf0_gmsnburr(-Inf, 0, 1, 1, 1)) && is.finite(pdf0_gmsnburr(-Inf, 0, 1, 1, 1)))
   expect_true(!is.nan(pdf0_jfst(-Inf, 0, 1, 2, 2)) && is.finite(pdf_jfst(-Inf, 0, 1, 2, 2)))
    expect_true(!is.nan(pdf0_fossep(-Inf, 0, 1, 2, 2)) && is.finite(pdf_fossep(-Inf, 0, 1, 2, 2)))
    expect_true(!is.nan(pdf0_jsep(-Inf, 0, 1, 2, 2)) && is.finite(pdf_jsep(-Inf, 0, 1, 2, 2)))   
   
#   
 })


 test_that("16. Testing CDFs against negatively infinite values", {
# 
   skip_on_cran()
   expect_true(!is.nan(cdf0_msnburr(-Inf, 0, 1, 1)) && is.finite(cdf0_msnburr(-Inf, 0, 1, 1)))
   expect_true(!is.nan(cdf0_msnburr2a(-Inf, 0, 1, 1)) && is.finite(cdf0_msnburr2a(-Inf, 0, 1, 1)))
   expect_true(!is.nan(cdf0_gmsnburr(-Inf, 0, 1, 1, 1)) && is.finite(cdf0_gmsnburr(-Inf, 0, 1, 1, 1)))
   expect_true(!is.nan(cdf0_jfst(-Inf, 0, 1, 2, 2)) && is.finite(cdf0_jfst(-Inf, 0, 1, 2, 2)))
   expect_true(!is.nan(cdf0_fossep(-Inf, 0, 1, 2, 2)) && is.finite(cdf0_fossep(-Inf, 0, 1, 2, 2)))
   expect_true(!is.nan(cdf0_jsep(-Inf, 0, 1, 2, 2)) && is.finite(cdf0_jsep(-Inf, 0, 1, 2, 2)))
   #   
 })

# -------------------------------Test Log Probability--------------------------------------------------



test_that("17. Check if log-probabilities are logs of probabilities (CDF's)", {
  skip_on_cran()
  
  x <- c(-Inf, -15, -10, -5, -1, 0, 1, 5, 10, 15, Inf)
  
   expect_equal(suppressWarnings(lcdf_msnburr(x, 0, 1, 2)),
                log(suppressWarnings(cdf_msnburr(x, 0, 1, 2))))
   expect_equal(suppressWarnings(lcdf0_msnburr(x, 0, 1, 2)),
                log(suppressWarnings(cdf0_msnburr(x, 0, 1, 2))))
   
   for ( i in 1 : length(x)){
     expect_equal(suppressWarnings(lcdf_msnburr2a(x[i], 0, 1, 2)),
                  log(suppressWarnings(cdf_msnburr2a(x[i], 0, 1, 2))))
     expect_equal(suppressWarnings(lcdf0_msnburr2a(x[i], 0, 1, 2)),
                  log(suppressWarnings(cdf0_msnburr2a(x[i], 0, 1, 2))))
   }
  
  
   expect_equal(suppressWarnings(lcdf_gmsnburr(x, 0, 1, 1, 1)),
                log(suppressWarnings(cdf_gmsnburr(x, 0, 1, 1, 1))))
   expect_equal(suppressWarnings(lcdf0_gmsnburr(x, 0, 1, 1, 1)),
                log(suppressWarnings(cdf0_gmsnburr(x, 0, 1, 1, 1))))
   
   expect_equal(suppressWarnings(lcdf_jfst(x, 0, 1, 2, 2)),
                log(suppressWarnings(cdf_jfst(x, 0, 1, 2, 2))))
   expect_equal(suppressWarnings(lcdf0_jfst(x, 0, 1, 2, 2)),
                log(suppressWarnings(cdf0_jfst(x, 0, 1, 2, 2))))
   
  expect_equal(suppressWarnings(lcdf_fossep(x, 0, 1, 2, 2)),
               log(suppressWarnings(cdf_fossep(x, 0, 1, 2, 2))))
  expect_equal(suppressWarnings(lcdf0_fossep(x, 0, 1, 2, 2)),
               log(suppressWarnings(cdf0_fossep(x, 0, 1, 2, 2))))
  
  expect_equal(suppressWarnings(lcdf_jsep(x, 0, 1, 2, 2)),
               log(suppressWarnings(cdf_jsep(x, 0, 1, 2, 2))))
  expect_equal(suppressWarnings(lcdf0_jsep(x, 0, 1, 2, 2)),
               log(suppressWarnings(cdf0_jsep(x, 0, 1, 2, 2))))
  
  
})

# -------------------------------------Test NAs-------------------------------------

test_that("18. Missing values in Stan PDF  functions", {
  skip_on_cran()
  expect_true(is.na(pdf_msnburr(NA, 0, 1, 1)))
  expect_true(is.na(pdf_msnburr(1, NA, 1, 1)))
  expect_true(is.na(pdf_msnburr(1, 0, NA, 1)))
  expect_true(is.na(pdf_msnburr(1, 0, 1, NA)))
  
  
  expect_true(is.na(pdf_msnburr2a(NA, 0, 1, 1)))
  expect_true(is.na(pdf_msnburr2a(1, NA, 1, 1)))
  expect_true(is.na(pdf_msnburr2a(1, 0, NA, 1)))
  expect_true(is.na(pdf_msnburr2a(1, 0, 1, NA)))
  
  expect_true(is.na(pdf_gmsnburr(NA, 0, 1, 1, 1)))
  expect_true(is.na(pdf_gmsnburr(1, NA, 1, 1, 1)))
  expect_true(is.na(pdf_gmsnburr(1, 0, NA, 1, 1)))
  expect_true(is.na(pdf_gmsnburr(1, 0, 1, NA, 1)))
  expect_true(is.na(pdf_gmsnburr(1, 0, 1, 1, NA)))
  
  
  expect_true(is.na(pdf_jfst(NA, 0, 1, 2, 2)))
  expect_true(is.na(pdf_jfst(1, NA, 1, 2, 2)))
  expect_true(is.na(pdf_jfst(1, 0, NA, 2, 2)))
  expect_true(is.na(pdf_jfst(1, 0, 1, NA, 2)))
  expect_true(is.na(pdf_jfst(1, 0, 1, 2, NA)))
  
  
  expect_true(is.na(pdf_fossep(NA, 0, 1, 2, 2)))
  expect_true(is.na(pdf_fossep(1, NA, 1, 2, 2)))
  expect_true(is.na(pdf_fossep(1, 0, NA, 2, 2)))
 expect_true(is.na(pdf_fossep(1, 0, 1, NA, 2)))
  expect_true(is.na(pdf_fossep(1, 0, 1, 2, NA)))
  
  
  expect_true(is.na(pdf_jsep(NA, 0, 1, 2, 2)))
  expect_true(is.na(pdf_jsep(1, NA, 1, 2, 2)))
  expect_true(is.na(pdf_jsep(1, 0, NA, 2, 2)))
  expect_true(is.na(pdf_jsep(1, 0, 1, NA, 2)))
  expect_true(is.na(pdf_jsep(1, 0, 1, 2, NA)))
  

  expect_true(is.na(pdf0_msnburr(NA, 0, 1, 1)))
  expect_true(is.na(pdf0_msnburr(1, NA, 1, 1)))
  expect_true(is.na(pdf0_msnburr(1, 0, NA, 1)))
  expect_true(is.na(pdf0_msnburr(1, 0, 1, NA)))
  
  
  expect_true(is.na(pdf0_msnburr2a(NA, 0, 1, 1)))
  expect_true(is.na(pdf0_msnburr2a(1, NA, 1, 1)))
  expect_true(is.na(pdf0_msnburr2a(1, 0, NA, 1)))
  expect_true(is.na(pdf0_msnburr2a(1, 0, 1, NA)))
  
  expect_true(is.na(pdf0_gmsnburr(NA, 0, 1, 1, 1)))
  expect_true(is.na(pdf0_gmsnburr(1, NA, 1, 1, 1)))
  expect_true(is.na(pdf0_gmsnburr(1, 0, NA, 1, 1)))
  expect_true(is.na(pdf0_gmsnburr(1, 0, 1, NA, 1)))
  expect_true(is.na(pdf0_gmsnburr(1, 0, 1, 1, NA)))
  
  
  expect_true(is.na(pdf0_jfst(NA, 0, 1, 2, 2)))
  expect_true(is.na(pdf0_jfst(1, NA, 1, 2, 2)))
  expect_true(is.na(pdf0_jfst(1, 0, NA, 2, 2)))
  expect_true(is.na(pdf0_jfst(1, 0, 1, NA, 2)))
  expect_true(is.na(pdf0_jfst(1, 0, 1, 2, NA)))
  
  
  expect_true(is.na(pdf0_fossep(NA, 0, 1, 2, 2)))
  expect_true(is.na(pdf0_fossep(1, NA, 1, 2, 2)))
  expect_true(is.na(pdf0_fossep(1, 0, NA, 2, 2)))
  expect_true(is.na(pdf0_fossep(1, 0, 1, NA, 2)))
  expect_true(is.na(pdf0_fossep(1, 0, 1, 2, NA)))
  
  
  expect_true(is.na(pdf0_jsep(NA, 0, 1, 2, 2)))
  expect_true(is.na(pdf0_jsep(1, NA, 1, 2, 2)))
  expect_true(is.na(pdf0_jsep(1, 0, NA, 2, 2)))
  expect_true(is.na(pdf0_jsep(1, 0, 1, NA, 2)))
  expect_true(is.na(pdf0_jsep(1, 0, 1, 2, NA)))
  
  })

test_that("18. Wrong parameter values in Stan CDF functions", {
  skip_on_cran()

    expect_true(is.na(cdf_msnburr(NA, 0, 1, 1)))
    expect_true(is.na(cdf_msnburr(1, NA, 1, 1)))
    expect_true(is.na(cdf_msnburr(1, 0, NA, 1)))
    expect_true(is.na(cdf_msnburr(1, 0, 1, NA)))
  
    expect_true(is.na(cdf_msnburr2a(NA, 0, 1, 1)))
    expect_true(is.na(cdf_msnburr2a(1, NA, 1, 1)))
    expect_true(is.na(cdf_msnburr2a(1, 0, NA, 1)))
    expect_true(is.na(cdf_msnburr2a(1, 0, 1, NA)))
  
   expect_error( expect_true(is.na(cdf_gmsnburr(NA, 0, 1, 1, 1))))
   expect_error(  expect_true(is.na(cdf_gmsnburr(1, NA, 1, 1, 1))))
  expect_error(expect_true(is.na(cdf_gmsnburr(1, 0, NA, 1, 1))))
  expect_error(   expect_true(is.na(cdf_gmsnburr(1, 0, 1, NA, 1))))
    expect_error( expect_true(is.na(cdf_gmsnburr(1, 0, 1, 1, NA))))
  
    expect_error( expect_true(is.na(cdf_jfst(NA, 0, 1, 1, 1))))
    expect_error(  expect_true(is.na(cdf_jfst(1, NA, 1, 1, 1))))
    expect_error(expect_true(is.na(cdf_jfst(1, 0, NA, 1, 1))))
    expect_error(   expect_true(is.na(cdf_jfst(1, 0, 1, NA, 1))))
    expect_error( expect_true(is.na(cdf_jfst(1, 0, 1, 1, NA))))
    
  expect_error(expect_true(is.na(cdf_fossep(NA, 0, 1, 0, 0.2))))
  expect_true(is.na(cdf_fossep(1, NA, 1, 2, 2)))
  expect_true(is.na(cdf_fossep(1, 0, NA, 2, 2)))
  expect_true(is.na(cdf_fossep(1, 0, 1, NA, 2)))
  expect_true(is.na(cdf_fossep(1, 0, 1, 2, NA))) 
  
  
  expect_error(expect_true(is.na(cdf_jsep(NA, 0, 1, 0, 0.2))))
  expect_true(is.na(cdf_jsep(1, NA, 1, 2, 2)))
  expect_true(is.na(cdf_jsep(1, 0, NA, 2, 2)))
  expect_true(is.na(cdf_jsep(1, 0, 1, NA, 2)))
  expect_true(is.na(cdf_jsep(1, 0, 1, 2, NA))) 
  
  
  
  expect_true(is.na(cdf0_msnburr(NA, 0, 1, 1)))
  expect_true(is.na(cdf0_msnburr(1, NA, 1, 1)))
  expect_true(is.na(cdf0_msnburr(1, 0, NA, 1)))
  expect_true(is.na(cdf0_msnburr(1, 0, 1, NA)))
  
  expect_true(is.na(cdf0_msnburr2a(NA, 0, 1, 1)))
  expect_true(is.na(cdf0_msnburr2a(1, NA, 1, 1)))
  expect_true(is.na(cdf0_msnburr2a(1, 0, NA, 1)))
  expect_true(is.na(cdf0_msnburr2a(1, 0, 1, NA)))
  
  expect_error( expect_true(is.na(cdf0_gmsnburr(NA, 0, 1, 1, 1))))
  expect_error(  expect_true(is.na(cdf0_gmsnburr(1, NA, 1, 1, 1))))
  expect_error(expect_true(is.na(cdf0_gmsnburr(1, 0, NA, 1, 1))))
  expect_error(   expect_true(is.na(cdf0_gmsnburr(1, 0, 1, NA, 1))))
  expect_error( expect_true(is.na(cdf0_gmsnburr(1, 0, 01, 1, NA))))
  
  expect_error( expect_true(is.na(cdf0_jfst(NA, 0, 1, 1, 1))))
  expect_error(  expect_true(is.na(cdf0_jfst(1, NA, 1, 1, 1))))
  expect_error(expect_true(is.na(cdf0_jfst(1, 0, NA, 1, 1))))
  expect_error(   expect_true(is.na(cdf0_jfst(1, 0, 1, NA, 1))))
  expect_error( expect_true(is.na(cdf0_jfst(1, 0, 1, 1, NA))))
  
  expect_error(expect_true(is.na(cdf0_fossep(NA, 0, 1, 0, 0.2))))
  expect_true(is.na(cdf0_fossep(1, NA, 1, 2, 2)))
  expect_true(is.na(cdf0_fossep(1, 0, NA, 2, 2)))
  expect_true(is.na(cdf0_fossep(1, 0, 1, NA, 2)))
  expect_true(is.na(cdf0_fossep(1, 0, 1, 2, NA))) 
  
  
  expect_error(expect_true(is.na(cdf_jsep(NA, 0, 1, 0, 0.2))))
  expect_true(is.na(cdf_jsep(1, NA, 1, 2, 2)))
  expect_true(is.na(cdf_jsep(1, 0, NA, 2, 2)))
  expect_true(is.na(cdf_jsep(1, 0, 1, NA, 2)))
  expect_true(is.na(cdf_jsep(1, 0, 1, 2, NA))) 
  
  
  
})

test_that("19. Wrong parameter values in inverse CDF functions", {
skip_on_cran()  
  expect_true(is.na(quan_msnburr(NA, 0, 1, 1)))
  expect_true(is.na(quan_msnburr(1, NA, 1, 1)))
  expect_true(is.na(quan_msnburr(1, 0, NA, 1)))
  expect_true(is.na(quan_msnburr(1, 0, 1, NA)))
  
  expect_true(is.na(quan_msnburr2a(NA, 0, 1, 1)))
  expect_true(is.na(quan_msnburr2a(1, NA, 1, 1)))
  expect_true(is.na(quan_msnburr2a(1, 0, NA, 1)))
  expect_true(is.na(quan_msnburr2a(1, 0, 1, NA)))
  
  expect_error(expect_true(is.na(quan_gmsnburr(NA, 0, 1, 1, 1))))
  expect_true(is.na(quan_gmsnburr(1, NA, 1, 1, 1)))
  expect_true(is.na(quan_gmsnburr(1, 0, NA, 1, 1)))
  expect_error(expect_true(is.na(quan_gmsnburr(1, 0, 1, NA, 1))))
  expect_error(expect_true(is.na(quan_gmsnburr(1, 0, 1, 1, NA))))
  
  expect_error(expect_true(is.na(quan_jfst(NA, 0, 1, 2, 2))))
  expect_true(is.na(quan_jfst(1, NA, 1, 2, 2)))
  expect_true(is.na(quan_jfst(1, 0, NA, 2, 2)))
  expect_error(expect_true(is.na(quan_jfst(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(quan_jfst(1, 0, 1, 2, NA))))
  
  expect_error(expect_true(is.na(quan_fossep(NA, 0, 1, 2, 2))))
  expect_error(expect_true(is.na(quan_fossep(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(quan_fossep(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(quan_fossep(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(quan_fossep(1, 0, 1, 2, NA))))
  
  
  expect_error(expect_true(is.na(quan_jsep(NA, 0, 1, 2, 2))))
  expect_error(expect_true(is.na(quan_jsep(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(quan_jsep(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(quan_jsep(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(quan_jsep(1, 0, 1, 2, NA))))
})

test_that("20 Wrong parameter values in RNG functions", {
  skip_on_cran()
  expect_true(is.na(rng_msnburr(1, NA, 1, 1)))
  expect_true(is.na(rng_msnburr(1, 0, NA, 1)))
  expect_true(is.na(rng_msnburr(1, 0, 1, NA)))
  

   expect_true(is.na(rng_msnburr2a(1, NA, 1, 1)))
   expect_true(is.na(rng_msnburr2a(1, 0, NA, 1)))
   expect_true(is.na(rng_msnburr2a(1, 0, 1, NA)))
  
  
    expect_true(is.na(rng_gmsnburr(1, NA, 1, 1, 1)))
    expect_true(is.na(rng_gmsnburr(1, 0, NA, 1, 1)))
    expect_error(expect_true(is.na(rng_gmsnburr(1, 0, 1, NA, 1))))
    expect_error(expect_true(is.na(rng_gmsnburr(1, 0, 1, 1, NA))))
  
  
  expect_true(is.na(rng_jfst(1, NA, 1, 2, 2)))
  expect_true(is.na(rng_jfst(1, 0, NA, 2, 2)))
  expect_error( expect_true(is.na(rng_jfst(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(rng_jfst(1, 0, 1, 2, NA))))
  

  expect_error(expect_true(is.na(rng_fossep(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(rng_fossep(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(rng_fossep(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(rng_fossep(1, 0, 1, 2, NA))))
  
  expect_error(expect_true(is.na(rng_jsep(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(rng_jsep(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(rng_jsep(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(rng_jsep(1, 0, 1, 2, NA))))
  
})

# --------------------- Test Probabillity -------------------------------------------

test_that("21. All probabilities/densities >= 0", {
  skip_on_cran()
  y <- c( -100, -10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 100)
  
   expect_true(suppressWarnings(all(pdf_msnburr(y, 0, 1, 1) >= 0)))
   expect_true(suppressWarnings(all(pdf_msnburr2a(y, 0, 1, 1) >= 0)))
   expect_true(suppressWarnings(all(pdf_gmsnburr(y, 0, 1, 1, 1) >= 0)))
   expect_true(suppressWarnings(all(pdf_jfst(y, 0, 1, 2, 2) >= 0)))
  expect_true(suppressWarnings(all(pdf_fossep(y, 0, 1, 2, 2) >= 0)))
  expect_true(suppressWarnings(all(pdf_jsep(y, 0, 1, 2, 2) >= 0)))
   expect_true(suppressWarnings(all(pdf0_msnburr(y, 0, 1, 1) >= 0)))
   expect_true(suppressWarnings(all(pdf0_msnburr2a(y, 0, 1, 1) >= 0)))
   expect_true(suppressWarnings(all(pdf0_gmsnburr(y, 0, 1, 1, 1) >= 0)))
   expect_true(suppressWarnings(all(pdf0_jfst(y, 0, 1, 2, 2) >= 0)))
   expect_true(suppressWarnings(all(pdf0_fossep(y, 0, 1, 2, 2) >= 0)))
   expect_true(suppressWarnings(all(pdf0_jsep(y, 0, 1, 2, 2) >= 0)))
   
})




test_that("22. All cumulative probabilities >= 0 and <= 1", {
  skip_on_cran()
  y <- c( -100, -10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 100)
  

  expect_true(all(cdf_msnburr(y, 0, 1, 1) >= 0 & cdf_msnburr(y, 0, 1, 1) <= 1))
   for(i in 1:length(y)){
     expect_true(all(cdf_msnburr2a(y[i], 0, 1, 1) >= 0 & cdf_msnburr2a(y, 0, 1, 1) <= 1))
   }
  # 
   expect_true(all(cdf_gmsnburr(y, 0, 1, 1, 1) >= 0 & cdf_gmsnburr(y, 0, 1, 1, 1) <= 1))
   expect_true(all(cdf_jfst(y, 0, 1, 2, 2) >= 0 & cdf_jfst(y, 0, 1, 2, 2) <= 1))
  expect_true(all(cdf_fossep(y, 0, 1, 2, 2) >= 0 & cdf_fossep(y, 0, 1, 2, 2) <= 1))
  expect_true(all(cdf_jsep(y, 0, 1, 2, 2) >= 0 & cdf_jsep(y, 0, 1, 2, 2) <= 1))
  # 
   expect_true(all(cdf0_msnburr(y, 0, 1, 1) >= 0 & cdf_msnburr(y, 0, 1, 1) <= 1))
   for (i in 1 : length(y)){
     expect_true(all(cdf0_msnburr2a(y[i], 0, 1, 1) >= 0 & cdf_msnburr2a(y, 0, 1, 1) <= 1))
   }
  # 
   expect_true(all(cdf0_gmsnburr(y, 0, 1, 1, 1) >= 0 & cdf_gmsnburr(y, 0, 1, 1, 1) <= 1))
   expect_true(all(cdf0_jfst(y, 0, 1, 2, 2) >= 0 & cdf_jfst(y, 0, 1, 2, 2) <= 1))
   expect_true(all(cdf0_fossep(y, 0, 1, 2, 2) >= 0 & cdf_fossep(y, 0, 1, 2, 2) <= 1))
   expect_true(all(cdf0_jsep(y, 0, 1, 2, 2) >= 0 & cdf_jsep(y, 0, 1, 2, 2) <= 1))
   
   })


# -------------------------------------Test Quantile Function ------------------------------

test_that("23. Zeros in quantile functions", {
  skip_on_cran()
  expect_true(!is.nan(quan_msnburr(0, 0, 1, 1)))
  expect_true(!is.nan(quan_msnburr2a(0, 0, 1, 1)))
  expect_true(!is.nan(quan_gmsnburr(0, 0, 1, 1, 1)))
  expect_true(!is.nan(quan_jfst(0, 0, 1, 2, 2)))
#  expect_true(!is.nan(quan_fossep(0, 0, 1, 2, 2)))
#   expect_true(!is.nan(quan_jsep(0, 0, 1, 2, 2)))
  
})

test_that("24. Ones in quantile functions", {
  skip_on_cran()
  expect_true(!is.nan(quan_msnburr(1, 0, 1, 1)))
  expect_true(!is.nan(quan_msnburr2a(1, 0, 1, 1)))
  expect_true(!is.nan(quan_gmsnburr(1, 0, 1, 1, 1)))
  expect_true(!is.nan(quan_jfst(1, 0, 1, 2, 2)))
# expect_error(expect_true(!is.nan(qfossep(1, 0, 1, 2, 2))))
# expect_error(expect_true(!is.nan(qjsep(1, 0, 1, 2, 2))))
  
})

test_that("25. Checking p = F(F^-1(p))", {
  skip_on_cran()
  pp <- seq(0.00001, 1, by = 0.001)
  
  expect_equal(pp, cdf_msnburr(quan_msnburr(pp, 0, 1, 1), 0, 1, 1))
  expect_equal(pp, cdf_msnburr2a(quan_msnburr2a(pp, 0, 1, 1), 0, 1, 1))
  expect_equal(pp, cdf_gmsnburr(quan_gmsnburr(pp, 0, 1, 1, 1), 0, 1, 1, 1))
  expect_equal(pp, cdf_jfst(quan_jfst(pp, 0, 1, 2, 2), 0, 1, 2, 2))
#  expect_equal(pp, cdf_fossep(quan_fossep(pp, 0, 1, 2, 2), 0, 1, 2, 2))
#  expect_equal(pp, cdf_jsep(quan_jsep(pp, 0, 1, 2, 2), 0, 1, 2, 2))
  # for(i in 1:length(pp)){
  # expect_equal(pp[i], cdf_msnburr(quan_msnburr(pp[i], 0, 1, 1), 0, 1, 1))
  # }
   for(i in 1:length(pp)){
     expect_equal(pp[i], cdf_msnburr2a(quan_msnburr2a(pp[i], 0, 1, 1), 0, 1, 1))
   }


  
    # for(i in 1:length(pp)){
  # expect_equal(pp[i], cdf_gmsnburr(quan_gmsnburr(pp[i], 0, 1, 1, 1.5), 0, 1, 1, 1.5))
  # expect_equal(pp[i], cdf_jfst(quan_jfst(pp[i], 0, 1, 2, 2), 0, 1, 2, 2))
  # expect_equal(pp[i], cdf_fossep(quan_fossep(pp[i], 0, 1, 2, 2), 0, 1, 2, 2))
  # }
   expect_equal(pp, cdf0_msnburr(quan_msnburr(pp, 0, 1, 1), 0, 1, 1))
   
   for(i in 1 : length(pp)){
     expect_equal(pp[i], cdf0_msnburr2a(quan_msnburr2a(pp[i], 0, 1, 1), 0, 1, 1))
   }
  # 
   expect_equal(pp, cdf0_msnburr2a(quan_msnburr2a(pp, 0, 1, 1), 0, 1, 1))
   expect_equal(pp, cdf0_gmsnburr(quan_gmsnburr(pp, 0, 1, 1, 1), 0, 1, 1, 1))
   expect_equal(pp, cdf0_jfst(quan_jfst(pp, 0, 1, 2, 2), 0, 1, 2, 2))
   expect_equal(pp, cdf0_fossep(quan_fossep(pp, 0, 1, 2, 2), 0, 1, 2, 2))
  # 
  
  
})

# ----------------------------- Test RNG Converge -------------------------------------

probCoverage <- function(stub, ..., n = 5000L) {
  rfoo <- eval(as.name(paste0("r", stub)))
  pfoo <- eval(as.name(paste0("p", stub)))
  diff(pfoo(range(rfoo(n, ...)), ...))
}







# -------------------------- PDF and CDF Test -----------------------------------------
library(neodistr)
library (testthat)

test_that("integrate PDF from -inf to inf == 1", {
  
   expect_equal(integrate(pdf_msnburr,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1 )$value,1)
   expect_equal(integrate(pdf0_msnburr,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1 )$value,1)
  
   expect_equal(integrate(pdf_msnburr2a,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1 )$value,1)
   expect_equal(integrate(pdf0_msnburr2a,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1 )$value,1)
  
   expect_equal(integrate(pdf_gmsnburr,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1,beta=1 )$value,1)
   expect_equal(integrate(pdf0_gmsnburr,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1,beta=1 )$value,1)
  
   expect_equal(integrate(pdf_jfst,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=2,beta= 2 )$value,1)
   expect_equal(integrate(pdf0_jfst,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=2,beta= 2)$value,1)
  
  expect_equal(integrate(pdf_fossep,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=2,beta= 2 )$value,1)
  expect_equal(integrate(pdf0_fossep,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=2,beta= 2 )$value,1)
  
  expect_equal(integrate(pdf_jsep,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=2,beta= 2 )$value,1)
  expect_equal(integrate(pdf0_jsep,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=2,beta= 2 )$value,1)
  
})

test_that("integrate PDF from - Inf until x equal to cdf x", {
  
  expect_equal((integrate(dmsnburr, lower= -Inf, upper=4, mu=0, sigma=1,alpha=1)$value),(pmsnburr(4, mu=0, sigma=1, alpha=1)))
  # expect_equal((integrate(pdf_msnburr, lower= -Inf, upper=4, mu=0, sigma=1,alpha=1)$value), (cdf_msnburr(4, mu=0, sigma=1, alpha=1)))
  # expect_equal((integrate(pdf0_msnburr, lower= -Inf, upper=4, mu=0, sigma=1,alpha=1)$value),(cdf0_msnburr(4, mu=0, sigma=1, alpha=1)))
  
  expect_equal((integrate(dmsnburr2a, lower= -Inf, upper=4, mu=0, sigma=1,alpha=1)$value) , (pmsnburr2a(4, mu=0, sigma=1, alpha=1)))
  # expect_equal((integrate(pdf_msnburr2a, lower= -Inf, upper=4, mu=0, sigma=1,alpha=1)$value), (cdf_msnburr2a(4, mu=0, sigma=1, alpha=1)))
  # expect_equal((integrate(pdf0_msnburr2a, lower= -Inf, upper=4, mu=0, sigma=1,alpha=1)$value) , (cdf0_msnburr2a(4, mu=0, sigma=1, alpha=1)))
  
  expect_equal((integrate(dgmsnburr, lower= -Inf, upper=4, mu=0, sigma=1,alpha=1, beta=1)$value), (pgmsnburr(4, mu=0, sigma=1, alpha=1, beta=1)))
  # expect_equal((integrate(pdf_gmsnburr, lower= -Inf, upper=4, mu=0, sigma=1,alpha=1, beta=1)$value) , (cdf_gmsnburr(4, mu=0, sigma=1, alpha=1, beta=1)))
  # expect_equal((integrate(pdf0_gmsnburr, lower= -Inf, upper=4, mu=0, sigma=1,alpha=1, beta=1)$value) , (cdf0_gmsnburr(4, mu=0, sigma=1, alpha=1, beta=1)))
  
  expect_equal((integrate(djfst, lower= -Inf, upper=4, mu=0, sigma=1,alpha=2, beta=2)$value) , (pjfst(4, mu=0, sigma=1, alpha=2, beta=2)))
  # expect_equal((integrate(pdf_jfst, lower= -Inf, upper=4, mu=0, sigma=1,alpha=2, beta=2)$value) , (cdf_jfst(4, mu=0, sigma=1, alpha=2, beta=2)))
  # expect_equal((integrate(pdf0_jfst, lower= -Inf, upper=4, mu=0, sigma=1,alpha=2, beta=2)$value) , (cdf0_jfst(4, mu=0, sigma=1, alpha=2, beta=2)))
  
  expect_equal((integrate(dfossep, lower= -Inf, upper=4, mu=0, sigma=1,alpha=2, beta=2, rel.tol = 1e-10, abs.tol = 1e-12)$value), (pfossep(4, mu=0, sigma=1, alpha=2, beta=2)))
  expect_equal((integrate(pdf_fossep, lower= -Inf, upper=4, mu=0, sigma=1,alpha=2, beta=2, rel.tol = 1e-10, abs.tol = 1e-12)$value) , (cdf_fossep(4, mu=0, sigma=1, alpha=2, beta=2)))
  
  expect_equal((integrate(djsep, lower= -Inf, upper=4, mu=0, sigma=1,alpha=2, beta=2)$value) , (pjsep(4, mu=0, sigma=1, alpha=2, beta=2)))
  expect_equal((integrate(pdf_jsep, lower= -Inf, upper=4, mu=0, sigma=1,alpha=2, beta=2)$value) , (cdf_jsep(4, mu=0, sigma=1, alpha=2, beta=2)))
})


# -----------------------------------Cumulative Test ----------------------------------------------------------------------------------------------------------------------------

test_that("Checking ccdf = 1-cdf", {
  #   
  #   
  #   
  #   expect_equal(ccdf_msnburr(0.5,0,1,1), (1-(pmsnburr(0.5, 0, 1, 1))))
  #   expect_equal(ccdf_msnburr(0.5,0,1,1), (1-(cdf_msnburr(0.5, 0, 1, 1))))
  #   expect_equal(ccdf0_msnburr(0.5,0,1,1), (1-(cdf0_msnburr(0.5, 0, 1, 1))))
  #   
  #   
  #   expect_equal((ccdf_msnburr2a(0.5,0,1,1)),(1-(pmsnburr2a(0.5, 0, 1, 1))))
  #   expect_equal(ccdf_msnburr2a(0.5,0,1,1),(1-(cdf_msnburr2a(0.5, 0, 1, 1))))
  #   expect_equal(ccdf0_msnburr2a(0.5,0,1,1),(1-(cdf0_msnburr2a(0.5, 0, 1, 1))))
  #   
  #   
  #   expect_equal(ccdf_gmsnburr(0.5,0,1,1,1), (1- (pgmsnburr(0.5, 0, 1, 1,1))))
  #   expect_equal(ccdf_gmsnburr(0.5,0,1,1,1), (1- (cdf_gmsnburr(0.5, 0, 1, 1,1))))
  #   expect_equal(ccdf0_gmsnburr(0.5,0,1,1,1), (1- (cdf0_gmsnburr(0.5, 0, 1, 1,1))))
  #   
  #   expect_equal(ccdf_jfst(0.5,0,1,2,2), (1- (pjfst(0.5, 0, 1, 2,2))))
  #   expect_equal(ccdf_jfst(0.5,0,1,2,2), (1- (cdf_jfst(0.5, 0, 1, 2,2))))
  #   expect_equal(ccdf0_jfst(0.5,0,1,2,2), (1- (cdf0_jfst(0.5, 0, 1, 2,2))))
  #   
  expect_equal(ccdf_fossep(0.5,0,1,2,2), (1- (pfossep(0.5, 0, 1, 2,2))))
  expect_equal(ccdf_fossep(0.5,0,1,2,2), (1- (cdf_fossep(0.5, 0, 1, 2,2))))
  #   
  #   
  expect_equal(ccdf_jsep(0.5,0,1,2,2), (1- (pjsep(0.5, 0, 1, 2,2))))
  expect_equal(ccdf_jsep(0.5,0,1,2,2), (1- (cdf_jsep(0.5, 0, 1, 2,2))))
})
