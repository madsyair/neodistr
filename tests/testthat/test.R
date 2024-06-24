library(neodistr)
library(testthat)
# Vector
#pdf_msnburr <- neonormal_stanfunc(family ="msnburr", func = "pdf",vectorize = TRUE )
#cdf_msnburr <- neonormal_stanfunc(family ="msnburr", func = "cdf",vectorize = TRUE )
#ccdf_msnburr <- neonormal_stanfunc(family ="msnburr", func = "ccdf",vectorize = TRUE)
#lcdf_msnburr <- neonormal_stanfunc(family ="msnburr", func = "lcdf",vectorize = TRUE)

#pdf_msnburr2a <- neonormal_stanfunc(family ="msnburr2a", func = "pdf",vectorize = TRUE )
#cdf_msnburr2a <- neonormal_stanfunc(family ="msnburr2a", func = "cdf",vectorize = TRUE )
#ccdf_msnburr2a <- neonormal_stanfunc(family ="msnburr2a", func = "ccdf",vectorize = TRUE)
#lcdf_msnburr2a <- neonormal_stanfunc(family ="msnburr2a", func = "lcdf",vectorize = TRUE)

#pdf_gmsnburr <- neonormal_stanfunc(family ="gmsnburr", func = "pdf",vectorize = TRUE )
#cdf_gmsnburr <- neonormal_stanfunc(family ="gmsnburr", func = "cdf",vectorize = TRUE )
#ccdf_gmsnburr <- neonormal_stanfunc(family ="gmsnburr", func = "ccdf",vectorize = TRUE)
#lcdf_gmsnburr <- neonormal_stanfunc(family ="gmsnburr", func = "lcdf",vectorize = TRUE)

# pdf_jfst <- neonormal_stanfunc(family ="jfst", func = "pdf",vectorize = TRUE )
# cdf_jfst <- neonormal_stanfunc(family ="jfst", func = "cdf",vectorize = TRUE )
# ccdf_jfst <- neonormal_stanfunc(family ="jfst", func = "ccdf",vectorize = TRUE)
# lcdf_jfst <- neonormal_stanfunc(family ="jfst", func = "lcdf",vectorize = TRUE)

# Non-vector
# pdf0_msnburr <- neonormal_stanfunc(family ="msnburr", func = "pdf",vectorize = FALSE )
# cdf0_msnburr <- neonormal_stanfunc(family ="msnburr", func = "cdf",vectorize = FALSE )
# ccdf0_msnburr <- neonormal_stanfunc(family ="msnburr", func = "ccdf",vectorize = FALSE )
# lcdf0_msnburr <- neonormal_stanfunc(family ="msnburr", func = "lcdf",vectorize = FALSE )
# quan_msnburr <- neonormal_stanfunc(family ="msnburr", func = "quantile" )
# rng_msnburr <- neonormal_stanfunc(family ="msnburr", func = "rng")
# 
# pdf0_msnburr2a <- neonormal_stanfunc(family ="msnburr2a", func = "pdf",vectorize = FALSE )
# cdf0_msnburr2a <- neonormal_stanfunc(family ="msnburr2a", func = "cdf",vectorize = FALSE )
# ccdf0_msnburr2a <- neonormal_stanfunc(family ="msnburr2a", func = "ccdf",vectorize = FALSE )
# lcdf0_msnburr2a <- neonormal_stanfunc(family ="msnburr2a", func = "lcdf",vectorize = FALSE )
# quan_msnburr2a <- neonormal_stanfunc(family ="msnburr2a", func = "quantile" )
# rng_msnburr2a <- neonormal_stanfunc(family ="msnburr2a", func = "rng" )
# 
# pdf0_gmsnburr <- neonormal_stanfunc(family ="gmsnburr", func = "pdf",vectorize = FALSE )
# cdf0_gmsnburr <- neonormal_stanfunc(family ="gmsnburr", func = "cdf",vectorize = FALSE )
# ccdf0_gmsnburr <- neonormal_stanfunc(family ="gmsnburr", func = "ccdf",vectorize = FALSE )
# lcdf0_gmsnburr <- neonormal_stanfunc(family ="gmsnburr", func = "lcdf",vectorize = FALSE )
# quan_gmsnburr <- neonormal_stanfunc(family ="gmsnburr", func = "quantile" )
# rng_gmsnburr <- neonormal_stanfunc(family ="gmsnburr", func = "rng" )
# 
# pdf0_jfst <- neonormal_stanfunc(family ="jfst", func = "pdf",vectorize = FALSE )
# cdf0_jfst <- neonormal_stanfunc(family ="jfst", func = "cdf",vectorize = FALSE )
# ccdf0_jfst <- neonormal_stanfunc(family ="jfst", func = "ccdf",vectorize = FALSE )
# lcdf0_jfst <- neonormal_stanfunc(family ="jfst", func = "lcdf",vectorize = FALSE )
# quan_jfst <- neonormal_stanfunc(family ="jfst", func = "quantile",vectorize = FALSE )
# rng_jfst <- neonormal_stanfunc(family ="jfst", func = "rng",vectorize = FALSE )

# ----------------------------Test Inapproriate parameters-------------------------------------------

test_that("Wrong parameter values in PDF and PMF functions", {
  
  expect_error(expect_true(is.nan(dmsnburr(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(dmsnburr(1, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(pdf_msnburr(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(pdf_msnburr(1, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(pdf0_msnburr(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(pdf0_dmsnburr(1, 1, 1, -1))))
  
  
  expect_error(expect_true(is.nan(dmsnburr2a(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(dmsnburr2a(1, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(pdf_msnburr2a(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(pdf_msnburr2a(1, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(pdf0_msnburr2a(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(pdf0_msnburr2a(1, 1, 1, -1))))
  
  
  expect_error(expect_true(is.nan(dgmsnburr(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(dgmsnburr(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(dgmsnburr(1, 1, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(pdf_gmsnburr(1, 1, -1, 1, 1))))
  # expect_error(expect_true(is.nan(pdf_gmsnburr(1, 1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(pdf_gmsnburr(1, 1, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(pdf0_gmsnburr(1, 1, -1, 1, 1))))
  # expect_error(expect_true(is.nan(pdf0_gmsnburr(1, 1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(pdf0_gmsnburr(1, 1, 1, 1, -1))))
  
  
  expect_error(expect_true(is.nan(djfst(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(djfst(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(djfst(1, 1, 1, 2, -2))))  
  # expect_error(expect_true(is.nan(pdf_jfst(1, 1, -1, 2, 2))))
  # expect_error(expect_true(is.nan(pdf_jfst(1, 1, 1, -2, 2))))
  # expect_error(expect_true(is.nan(pdf_jfst(1, 1, 1, 2, -2)))) 
  # expect_error(expect_true(is.nan(pdf0_jfst(1, 1, -1, 2, 2))))
  # expect_error(expect_true(is.nan(pdf0_jfst(1, 1, 1, -2, 2))))
  # expect_error(expect_true(is.nan(pdf0_jfst(1, 1, 1, 2, -2))))
})


test_that("Wrong parameter values in CDF functions", {
  
  expect_error(expect_true(is.nan(pmsnburr(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(pmsnburr(1, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(cdf_msnburr(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(cdf_msnburr(1, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(cdf0_msnburr(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(cdf0_msnburr(1, 1, 1, -1))))
  
  
  expect_error(expect_true(is.nan(pmsnburr2a(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(pmsnburr2a(1, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(cdf_msnburr2a(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(cdf_msnburr2a(1, 1, 1, -1))))
  # 
  # expect_error(expect_true(is.nan(cdf0_msnburr2a(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(cdf0_msnburr2a(1, 1, 1, -1))))
  
  
  expect_error(expect_true(is.nan(pgmsnburr(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(pgmsnburr(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(pgmsnburr(1, 1, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(cdf_gmsnburr(1, 1, -1, 1, 1))))
  # expect_error(expect_true(is.nan(cdf_gmsnburr(1, 1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(cdf_gmsnburr(1, 1, 1, 1, -1))))
  # 
  # expect_error(expect_true(is.nan(cdf0_gmsnburr(1, 1, -1, 1, 1))))
  # expect_error(expect_true(is.nan(cdf0_gmsnburr(1, 1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(cdf0_gmsnburr(1, 1, 1, 1, -1))))
  
  expect_error(expect_true(is.nan(pjfst(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(pjfst(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(pjfst(1, 1, 1, 2, -2))))
  # expect_error(expect_true(is.nan(cdf_jfst(1, 1, -1, 2, 2))))
  # expect_error(expect_true(is.nan(cdf_jfst(1, 1, 1, -2, 2))))
  # expect_error(expect_true(is.nan(cdf_jfst(1, 1, 1, 2, -2))))
  # 
  # expect_error(expect_true(is.nan(cdf0_jfst(1, 1, -1, 2, 2))))
  # expect_error(expect_true(is.nan(cdf0_jfst(1, 1, 1, -2, 2))))
  # expect_error(expect_true(is.nan(cdf0_jfst(1, 1, 1, 2, -2)))) 
})


test_that("Wrong parameter values in quantile functions", {
  
  expect_error(expect_true(is.nan(qmsnburr(0.5, 1, -1, 1))))
  expect_error(expect_true(is.nan(qmsnburr(0.5, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(quan_msnburr(0.5, 1, -1, 1))))
  # expect_error(expect_true(is.nan(quan_msnburr(0.5, 1, 1, -1))))
  
  
  expect_error(expect_true(is.nan(qmsnburr2a(0.5, 1, -1, 1))))
  expect_error(expect_true(is.nan(qmsnburr2a(0.5, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(quan_msnburr2a(0.5, 1, -1, 1))))
  # expect_error(expect_true(is.nan(quan_msnburr2a(0.5, 1, 1, -1))))
  
  
  expect_error(expect_true(is.nan(qgmsnburr(0.5, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(qgmsnburr(0.5, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(qgmsnburr(0.5, 1, 1, 1, -1))))
  
  
  # expect_error(expect_true(is.nan(quan_gmsnburr(0.5, 1, -1, 1, 1))))
  # expect_error(expect_true(is.nan(quan_gmsnburr(0.5, 1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(quan_gmsnburr(0.5, 1, 1, 1, -1))))
  
  
  
  expect_error(expect_true(is.nan(qjfst(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(qjfst(1, 1, 1, -2, 2))))
  expect_error(expect_true(is.nan(qjfst(1, 1, 1, 2, -2)))) 
  # expect_error(expect_true(is.nan(quan_jfst(1, 1, -1, 2, 2))))
  # expect_error(expect_true(is.nan(quan_jfst(1, 1, 1, -2, 2))))
  # expect_error(expect_true(is.nan(quan_jfst(1, 1, 1, 2, -2))))
  
  
})


test_that("Wrong parameter values in RNG functions", {
  
  expect_error(expect_true(is.nan(rmsnburr(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(rmsnburr(1, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(rng_msnburr(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(rng_msnburr(1, 1, 1, -1))))
  
  
  
  expect_error(expect_true(is.nan(rmsnburr2a(1, 1, -1, 1))))
  expect_error(expect_true(is.nan(rmsnburr2a(1, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(rng_msnburr2a(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(rng_msnburr2a(1, 1, 1, -1))))
  
  
  expect_error(expect_true(is.nan(rgmsnburr(1, 1, -1, 1, 1))))
  expect_error(expect_true(is.nan(rgmsnburr(1, 1, 1, -1, 1))))
  expect_error(expect_true(is.nan(rgmsnburr(1, 1, 1, 1, -1))))
  
  # expect_error(expect_true(is.nan(rng_gmsnburr(1, 1, -1, 1, 1))))
  # expect_error(expect_true(is.nan(rng_gmsnburr(1, 1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(rng_gmsnburr(1, 1, 1, 1, -1))))
  # 
  
  
  expect_error(expect_true(is.nan(rjfst(1, 1, -1, 2, 2))))
  expect_error(expect_true(is.nan(rjfst(1, 1, 1, 2, -2))))
  # expect_error(expect_true(is.nan(quan_jfst(1, 1, 1, -2, 2))))
  # expect_error(expect_true(is.nan(rng_jfst(1, 1, -1, 2, 2))))
  # expect_error(expect_true(is.nan(rng_jfst(1, 1, 1, -2,2 ))))
  # expect_error(expect_true(is.nan(rng_jfst(1, 1, 1, 2, -2))))
  
})

#test_that("Wrong parameter values in CCDF functions", {
  # expect_error(expect_true(is.nan(ccdf_msnburr(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(ccdf_msnburr(1, 1, 1, -1))))
  # 
  # expect_error(expect_true(is.nan(ccdf0_msnburr(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(ccdf0_msnburr(1, 1, 1, -1))))
  # 
  # 
  # expect_error(expect_true(is.nan(ccdf_msnburr2a(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(ccdf_msnburr2a(1, 1, 1, -1))))
  # 
  # expect_error(expect_true(is.nan(ccdf0_msnburr2a(1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(ccdf0_msnburr2a(1, 1, 1, -1))))
  # 
  # 
  # expect_error(expect_true(is.nan(ccdf_gmsnburr(1, 1, -1, 1, 1))))
  # expect_error(expect_true(is.nan(ccdf_gmsnburr(1, 1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(ccdf_gmsnburr(1, 1, 1, 1, -1))))
  # 
  # expect_error(expect_true(is.nan(ccdf0_gmsnburr(1, 1, -1, 1, 1))))
  # expect_error(expect_true(is.nan(ccdf0_gmsnburr(1, 1, 1, -1, 1))))
  # expect_error(expect_true(is.nan(ccdf0_gmsnburr(1, 1, 1, 1, -1))))
  # 
  # 
  # expect_error(expect_true(is.nan(ccdf_jfst(1, 1, -1, 2, 2))))
  # expect_error(expect_true(is.nan(ccdf_jfst(1, 1, 1, -2, 2))))
  # expect_error(expect_true(is.nan(rng_jfst(1, 1, 1, 2, -2))))
  # 
  # expect_error(expect_true(is.nan(ccdf0_jfst(1, 1, -1, 2, 2))))
  # expect_error(expect_true(is.nan(ccdf0_jfst(1, 1, -1, 2, 2))))
  # expect_error(expect_true(is.nan(ccdf0_jfst(1, 1, 1, 2, -2))))
  # 
#})

# test_that("Wrong parameter values in LCDF functions", {
#   expect_error(expect_true(is.nan(lcdf_msnburr(1, 1, -1, 1))))
#   expect_error(expect_true(is.nan(lcdf_msnburr(1, 1, 1, -1))))
#   
#   expect_error(expect_true(is.nan(lcdf0_msnburr(1, 1, -1, 1))))
#   expect_error(expect_true(is.nan(lcdf0_msnburr(1, 1, 1, -1))))
#   
#   
#   expect_error(expect_true(is.nan(lcdf_msnburr2a(1, 1, -1, 1))))
#   expect_error(expect_true(is.nan(lcdf_msnburr2a(1, 1, 1, -1))))
#   
#   expect_error(expect_true(is.nan(lcdf0_msnburr2a(1, 1, -1, 1))))
#   expect_error(expect_true(is.nan(lcdf0_msnburr2a(1, 1, 1, -1))))
#   
#   
#   expect_error(expect_true(is.nan(lcdf_gmsnburr(1, 1, -1, 1, 1))))
#   expect_error(expect_true(is.nan(lcdf_gmsnburr(1, 1, 1, -1, 1))))
#   expect_error(expect_true(is.nan(lcdf_gmsnburr(1, 1, 1, 1, -1))))
#   
#   expect_error(expect_true(is.nan(lcdf0_gmsnburr(1, 1, -1, 1, 1))))
#   expect_error(expect_true(is.nan(lcdf0_gmsnburr(1, 1, 1, -1, 1))))
#   expect_error(expect_true(is.nan(lcdf0_gmsnburr(1, 1, 1, 1, -1))))
#   
#   
#   expect_error(expect_true(is.nan(lcdf_jfst(1, 1, -1, 2, 2))))
#   expect_error(expect_true(is.nan(lcdf_jfst(1, 1, 1, -2, 2))))
#   expect_error(expect_true(is.nan(lcdf_jfst(1, 1, 1, 2, -2))))
#   
#   expect_error(expect_true(is.nan(lcdf0_jfst(1, 1, -1, 2, 2))))
#   expect_error(expect_true(is.nan(lcdf0_jfst(1, 1, 1,-2,2))))
#   expect_error(expect_true(is.nan(lcdf0_jfst(1, 1, 1,2,-2))))
# })
# 
# ------------------------------------Infinity Test-----------------------------------------------
test_that("Testing PDFs  against infinite values", {
  
  expect_true(!is.nan(dmsnburr(Inf, 0, 1, 1)) && is.finite(dmsnburr(Inf, 0, 1, 1)))
  # expect_true(!is.nan(pdf_msnburr(Inf, 0, 1, 1)) && is.finite(pdf_msnburr(Inf, 0, 1, 1)))
  expect_true(!is.nan(dmsnburr2a(Inf, 0, 1, 1)) && is.finite(dmsnburr2a(Inf, 0, 1, 1)))
  # expect_true(!is.nan(pdf_msnburr2a(Inf, 0, 1, 1)) && is.finite(pdf_msnburr2a(Inf, 0, 1, 1)))
  expect_true(!is.nan(dgmsnburr(Inf, 0, 1, 1, 1)) && is.finite(dgmsnburr(Inf, 0, 1, 1, 1)))
  # expect_true(!is.nan(pdf_gmsnburr(Inf, 0, 1, 1, 1)) && is.finite(pdf_gmsnburr(Inf, 0, 1, 1, 1)))
  expect_true(!is.nan(djfst(Inf, 0, 1, 2, 2)) && is.finite(djfst(Inf, 0, 1, 2, 2)))
  # expect_true(!is.nan(pdf_jfst(Inf, 0, 1, 2, 2)) && is.finite(pdf_jfst(Inf, 0, 1, 2, 2)))
  expect_true(!is.nan(djfst(Inf, 0, 1, 2, 2)) && is.finite(djfst(Inf, 0, 1, 2, 2)))
  # expect_true(!is.nan(pdf_jfst(Inf, 0, 1, 2, 2)) && is.finite(pdf_jfst(Inf, 0, 1, 2, 2)))
  
  
})


test_that("Testing CDFs against infinite values", {
  
  expect_true(!is.nan(pmsnburr(Inf, 0, 1, 1)) && is.finite(pmsnburr(Inf, 0, 1, 1)))
  # expect_true(!is.nan(cdf_msnburr(Inf, 0, 1, 1)) && is.finite(cdf_msnburr(Inf, 0, 1, 1)))
  expect_true(!is.nan(pmsnburr2a(Inf, 0, 1, 1)) && is.finite(pmsnburr2a(Inf, 0, 1, 1)))
  # expect_true(!is.nan(cdf_msnburr2a(Inf, 0, 1, 1)) && is.finite(cdf_msnburr2a(Inf, 0, 1, 1)))
  expect_true(!is.nan(pgmsnburr(Inf, 0, 1, 1, 1)) && is.finite(pgmsnburr(Inf, 0, 1, 1, 1)))
  # expect_true(!is.nan(cdf_gmsnburr(Inf, 0, 1, 1, 1)) && is.finite(cdf_gmsnburr(Inf, 0, 1, 1, 1)))
  expect_true(!is.nan(pjfst(Inf, 0, 1, 2, 2)) && is.finite(pjfst(Inf, 0, 1, 2, 2)))
  # expect_true(!is.nan(cdf_jfst(Inf, 0, 1, 2, 2)) && is.finite(cdf_jfst(Inf, 0, 1, 2, 2)))
})

# test_that("Testing LCDFs against infinite values", {
#   
#   
#   # expect_true(!is.nan(lcdf_msnburr(Inf, 0, 1, 1)) && is.finite(lcdf_msnburr(Inf, 0, 1, 1)))
#   # expect_true(!is.nan(lcdf_msnburr2a(Inf, 0, 1, 1)) && is.finite(lcdf_msnburr2a(Inf, 0, 1, 1)))
#   # expect_true(!is.nan(lcdf_gmsnburr(Inf, 0, 1, 1, 1)) && is.finite(lcdf_gmsnburr(Inf, 0, 1, 1, 1)))
#   # expect_true(!is.nan(lcdf_jfst(Inf, 0, 1, 2, 2)) && is.finite(lcdf_jfst(Inf, 0, 1, 2, 2)))
# })


test_that("Testing PDFs  against negatively infinite values", {
  
  expect_true(!is.nan(dmsnburr(-Inf, 0, 1, 1)) && is.finite(dmsnburr(-Inf, 0, 1, 1)))
  # expect_true(!is.nan(pdf_msnburr(-Inf, 0, 1, 1)) && is.finite(pdf_msnburr(-Inf, 0, 1, 1)))
  expect_true(!is.nan(dmsnburr2a(-Inf, 0, 1, 1)) && is.finite(dmsnburr2a(-Inf, 0, 1, 1)))
  # expect_true(!is.nan(pdf_msnburr2a(-Inf, 0, 1, 1)) && is.finite(pdf_msnburr2a(-Inf, 0, 1, 1)))
  expect_true(!is.nan(dgmsnburr(-Inf, 0, 1, 1, 1)) && is.finite(dgmsnburr(-Inf, 0, 1, 1, 1)))
  # expect_true(!is.nan(pdf_gmsnburr(-Inf, 0, 1, 1, 1)) && is.finite(pdf_gmsnburr(-Inf, 0, 1, 1, 1)))
  expect_true(!is.nan(djfst(-Inf, 0, 1, 2, 2)) && is.finite(djfst(-Inf, 0, 1, 2, 2)))
  # expect_true(!is.nan(pdf_jfst(-Inf, 0, 1, 2, 2)) && is.finite(pdf_jfst(-Inf, 0, 1, 2, 2)))
  
  
  
})


test_that("Testing CDFs against negatively infinite values", {
  
  expect_true(!is.nan(pmsnburr(-Inf, 0, 1, 1)) && is.finite(pmsnburr(-Inf, 0, 1, 1)))
  # expect_true(!is.nan(cdf_msnburr(-Inf, 0, 1, 1)) && is.finite(cdf_msnburr(-Inf, 0, 1, 1)))
  expect_true(!is.nan(pmsnburr2a(-Inf, 0, 1, 1)) && is.finite(pmsnburr2a(-Inf, 0, 1, 1)))
  # expect_true(!is.nan(cdf_msnburr2a(-Inf, 0, 1, 1)) && is.finite(cdf_msnburr2a(-Inf, 0, 1, 1)))
  expect_true(!is.nan(pgmsnburr(-Inf, 0, 1, 1, 1)) && is.finite(pgmsnburr(-Inf, 0, 1, 1, 1)))
  # expect_true(!is.nan(cdf_gmsnburr(-Inf, 0, 1, 1, 1)) && is.finite(cdf_gmsnburr(-Inf, 0, 1, 1, 1)))
  expect_true(!is.nan(pjfst(-Inf, 0, 1, 2, 2)) && is.finite(pjfst(-Inf, 0, 1, 2, 2)))
  # expect_true(!is.nan(cdf_jfst(-Inf, 0, 1, 2, 2)) && is.finite(cdf_jfst(-Inf, 0, 1, 2, 2)))  
  
})


# test_that("Testing PDFs  against infinite values", {
#   
#   expect_true(!is.nan(pdf0_msnburr(Inf, 0, 1, 1)) && is.finite(pdf0_msnburr(Inf, 0, 1, 1)))
#   expect_true(!is.nan(pdf0_msnburr2a(Inf, 0, 1, 1)) && is.finite(pdf0_msnburr2a(Inf, 0, 1, 1)))
#   expect_true(!is.nan(pdf0_gmsnburr(Inf, 0, 1, 1, 1)) && is.finite(pdf0_gmsnburr(Inf, 0, 1, 1, 1)))
#   #expect_true(!is.nan(djfst(Inf, 0, 1, 2, 2)) && is.finite(djfst(Inf, 0, 1, 2, 2)))
#   expect_true(!is.nan(pdf0_jfst(Inf, 0, 1, 2, 2)) && is.finite(pdf0_jfst(Inf, 0, 1, 2, 2)))
#   #expect_true(!is.nan(djfst2(Inf, 0, 1, 0, 0.2)) && is.finite(djfst2(Inf, 0, 1, 0, 0.2)))
#   expect_true(!is.nan(pdf0_jfst(Inf, 0, 1, 2, 2)) && is.finite(pdf0_jfst(Inf, 0, 1, 2, 2)))
#   
#   
# })


# test_that("Testing CDFs against infinite values", {
#   
#   expect_true(!is.nan(cdf0_msnburr(Inf, 0, 1, 1)) && is.finite(cdf0_msnburr(Inf, 0, 1, 1)))
#   expect_true(!is.nan(cdf0_msnburr2a(Inf, 0, 1, 1)) && is.finite(cdf0_msnburr2a(Inf, 0, 1, 1)))
#   expect_true(!is.nan(cdf0_gmsnburr(Inf, 0, 1, 1, 1)) && is.finite(cdf0_gmsnburr(Inf, 0, 1, 1, 1)))
#   #expect_true(!is.nan(pjfst(Inf, 0, 1, 2, 2)) && is.finite(pjfst(Inf, 0, 1, 2, 2)))
#   expect_true(!is.nan(cdf0_jfst(Inf, 0, 1, 2, 2)) && is.finite(cdf0_jfst(Inf, 0, 1, 2, 2)))
#   #expect_true(!is.nan(pjfst2(Inf, 0, 1, 0, 0.2)) && is.finite(pjfst2(Inf, 0, 1, 0, 0.2)))
#   #expect_true(!is.nan(cdf0_jfst2(Inf, 0, 1, 0, 0.2)) && is.finite(cdf0_jfst2(Inf, 0, 1, 0, 0.2)))
# })

# test_that("Testing LCDFs against infinite values", {
#   
#   
#   expect_true(!is.nan(lcdf0_msnburr(Inf, 0, 1, 1)) && is.finite(lcdf0_msnburr(Inf, 0, 1, 1)))
#   expect_true(!is.nan(lcdf0_msnburr2a(Inf, 0, 1, 1)) && is.finite(lcdf0_msnburr2a(Inf, 0, 1, 1)))
#   expect_true(!is.nan(lcdf0_gmsnburr(Inf, 0, 1, 1, 1)) && is.finite(lcdf0_gmsnburr(Inf, 0, 1, 1, 1)))
#   expect_true(!is.nan(lcdf0_jfst(Inf, 0, 1, 2, 2)) && is.finite(lcdf0_jfst(Inf, 0, 1, 2, 2)))
#   # expect_true(!is.nan(lcdf0_jfst2(Inf, 0, 1, 0, 0.2)) && is.finite(lcdf0_jfst2(Inf, 0, 1, 0, 0.2)))
# })


# test_that("Testing PDFs  against negatively infinite values", {
#   
#   expect_true(!is.nan(pdf0_msnburr(-Inf, 0, 1, 1)) && is.finite(pdf0_msnburr(-Inf, 0, 1, 1)))
#   expect_true(!is.nan(pdf0_msnburr2a(-Inf, 0, 1, 1)) && is.finite(pdf0_msnburr2a(-Inf, 0, 1, 1)))
#   expect_true(!is.nan(pdf0_gmsnburr(-Inf, 0, 1, 1, 1)) && is.finite(pdf0_gmsnburr(-Inf, 0, 1, 1, 1)))
#   expect_true(!is.nan(pdf_jfst(-Inf, 0, 1, 2, 2)) && is.finite(pdf_jfst(-Inf, 0, 1, 2, 2)))
#   
#   
#   
# })


# test_that("Testing CDFs against negatively infinite values", {
#   
#   expect_true(!is.nan(cdf0_msnburr(-Inf, 0, 1, 1)) && is.finite(cdf0_msnburr(-Inf, 0, 1, 1)))
#   expect_true(!is.nan(cdf0_msnburr2a(-Inf, 0, 1, 1)) && is.finite(cdf0_msnburr2a(-Inf, 0, 1, 1)))
#   expect_true(!is.nan(cdf0_gmsnburr(-Inf, 0, 1, 1, 1)) && is.finite(cdf0_gmsnburr(-Inf, 0, 1, 1, 1)))
#   expect_true(!is.nan(cdf0_jfst(-Inf, 0, 1, 2, 2)) && is.finite(cdf0_jfst(-Inf, 0, 1, 2, 2)))
#   
# })

# -------------------------------Test Log Probability--------------------------------------------------

test_that("Check if log-probabilities are logs of probabilities (PDF's)", {
  
  x <- c(-Inf, -100, -10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 100, Inf)
  
  expect_equal(suppressWarnings(dmsnburr(x, 0, 1, 1, log = TRUE)),
               log(suppressWarnings(dmsnburr(x, 0, 1, 1))))
  expect_equal(suppressWarnings(dmsnburr2a(x, 0, 1, 1, log = TRUE)),
               log(suppressWarnings(dmsnburr2a(x, 0, 1, 1))))
  expect_equal(suppressWarnings(dgmsnburr(x, 0, 1, 1, 1, log = TRUE)),
               log(suppressWarnings(dgmsnburr(x, 0, 1, 1, 1))))
  expect_equal(suppressWarnings(djfst(x, 0, 1, 2, 2, log = TRUE)),
               log(suppressWarnings(djfst(x, 0, 1, 2, 2))))
  
  
})


test_that("Check if log-probabilities are logs of probabilities (CDF's)", {
  
  x <- c(-Inf, -100, -10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 100, Inf)
  
  expect_equal(suppressWarnings(pmsnburr(x, 0, 1, 1, log.p = TRUE)),
               log(suppressWarnings(pmsnburr(x, 0, 1, 1))))
  expect_equal(suppressWarnings(pmsnburr2a(x, 0, 1, 1, log.p = TRUE)),
               log(suppressWarnings(pmsnburr2a(x, 0, 1, 1))))
  expect_equal(suppressWarnings(pgmsnburr(x, 0, 1, 1, 1, log.p = TRUE)),
               log(suppressWarnings(pgmsnburr(x, 0, 1, 1, 1))))
  expect_equal(suppressWarnings(pjfst(x, 0, 1, 2, 2, log.p = TRUE)),
               log(suppressWarnings(pjfst(x, 0, 1, 2, 2))))
  
  # expect_equal(suppressWarnings(lcdf_msnburr(x, 0, 1, 2)),
  #              log(suppressWarnings(cdf_msnburr(x, 0, 1, 2))))
  # expect_equal(suppressWarnings(lcdf0_msnburr(x, 0, 1, 2)),
  #              log(suppressWarnings(cdf0_msnburr(x, 0, 1, 2))))
  # 
  # for ( i in 1 : length(x)){
  #   expect_equal(suppressWarnings(lcdf_msnburr2a(x[i], 0, 1, 2)),
  #                log(suppressWarnings(cdf_msnburr2a(x[i], 0, 1, 2))))
  #   expect_equal(suppressWarnings(lcdf0_msnburr2a(x[i], 0, 1, 2)),
  #                log(suppressWarnings(cdf0_msnburr2a(x[i], 0, 1, 2))))
  # }
  
  
  # expect_equal(suppressWarnings(lcdf_gmsnburr(x, 0, 1, 1, 1)),
  #              log(suppressWarnings(cdf_gmsnburr(x, 0, 1, 1, 1))))
  # expect_equal(suppressWarnings(lcdf0_gmsnburr(x, 0, 1, 1, 1)),
  #              log(suppressWarnings(cdf0_gmsnburr(x, 0, 1, 1, 1))))
  # 
  # expect_equal(suppressWarnings(lcdf_jfst(x, 0, 1, 2, 2)),
  #              log(suppressWarnings(cdf_jfst(x, 0, 1, 2, 2))))
  # expect_equal(suppressWarnings(lcdf0_jfst(x, 0, 1, 2, 2)),
  #              log(suppressWarnings(cdf0_jfst(x, 0, 1, 2, 2))))
  # 
})

# -------------------------------------Test NAs-------------------------------------

test_that("Missing values in PDF and PMF functions", {
  
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
  
  
})





test_that("Wrong parameter values in CDF functions", {
  
  expect_true(is.na(pmsnburr(NA, 0, 1, 1)))
  expect_error(expect_true(is.na(pmsnburr(1, NA, 1, 1))))
  expect_error(expect_true(is.na(pmsnburr(1, 0, NA, 1))))
  expect_error(expect_true(is.na(pmsnburr(1, 0, 1, NA))))
  
  #  expect_true(is.na(cdf_msnburr(NA, 0, 1, 1)))
  #  expect_true(is.na(cdf_msnburr(1, NA, 1, 1)))
  #  expect_true(is.na(cdf_msnburr(1, 0, NA, 1)))
  #  expect_true(is.na(cdf_msnburr(1, 0, 1, NA)))
  
  expect_true(is.na(pmsnburr2a(NA, 0, 1, 1)))
  expect_error(expect_true(is.na(pmsnburr2a(1, NA, 1, 1))))
  expect_error(expect_true(is.na(pmsnburr2a(1, 0, NA, 1))))
  expect_error(expect_true(is.na(pmsnburr2a(1, 0, 1, NA))))
  
  #  expect_true(is.na(cdf_msnburr2a(NA, 0, 1, 1)))
  #  expect_true(is.na(cdf_msnburr2a(1, NA, 1, 1)))
  #  expect_true(is.na(cdf_msnburr2a(1, 0, NA, 1)))
  #  expect_true(is.na(cdf_msnburr2a(1, 0, 1, NA)))
  
  expect_true(is.na(pgmsnburr(NA, 0, 1, 1, 1)))
  expect_error(expect_true(is.na(pgmsnburr(1, NA, 1, 1, 1))))
  expect_error(expect_true(is.na(pgmsnburr(1, 0, NA, 1, 1))))
  expect_error(expect_true(is.na(pgmsnburr(1, 0, 1, NA, 1))))
  expect_error(expect_true(is.na(pgmsnburr(1, 0, 1, 1, NA))))
  
  #  expect_true(is.na(cdf_gmsnburr(NA, 0, 1, 1, 1)))
  #  expect_true(is.na(cdf_gmsnburr(1, NA, 1, 1, 1)))
  #  expect_true(is.na(cdf_gmsnburr(1, 0, NA, 1, 1)))
  #  expect_true(is.na(cdf_gmsnburr(1, 0, 1, NA, 1)))
  #  expect_true(is.na(cdf_gmsnburr(1, 0, 1, 1, NA)))
  
  expect_error(expect_true(is.na(pjfst(NA, 0, 1, 0, 0.2))))
  expect_error(expect_true(is.na(pjfst(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(pjfst(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(pjfst(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(pjfst(1, 0, 1, 2, NA)))) 
  
})




test_that("Wrong parameter values in inverse CDF functions", {
  
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
  
})




test_that("Wrong parameter values in RNG functions", {
  
  expect_error(expect_true(is.na(rmsnburr(1, NA, 1, 1))))
  expect_error(expect_true(is.na(rmsnburr(1, 0, NA, 1))))
  expect_error(expect_true(is.na(rmsnburr(1, 0, 1, NA))))
  
  
  
  expect_error(expect_true(is.na(rmsnburr2a(1, NA, 1, 1))))
  expect_error(expect_true(is.na(rmsnburr2a(1, 0, NA, 1))))
  expect_error(expect_true(is.na(rmsnburr2a(1, 0, 1, NA))))
  
  #  expect_warning(expect_true(is.na(rng_msnburr2a(1, NA, 1, 1))))
  #  expect_warning(expect_true(is.na(rng_msnburr2a(1, 0, NA, 1))))
  #  expect_warning(expect_true(is.na(rng_msnburr2a(1, 0, 1, NA))))
  
  expect_error(expect_true(is.na(rgmsnburr(1, NA, 1, 1, 1))))
  expect_error(expect_true(is.na(rgmsnburr(1, 0, NA, 1, 1))))
  expect_error(expect_true(is.na(rgmsnburr(1, 0, 1, NA, 1))))
  expect_error(expect_true(is.na(rgmsnburr(1, 0, 1, 1, NA))))
  
  #  expect_warning(expect_true(is.na(rng_gmsnburr(1, NA, 1, 1, 1))))
  #  expect_warning(expect_true(is.na(rng_gmsnburr(1, 0, NA, 1, 1))))
  #  expect_warning(expect_true(is.na(rng_gmsnburr(1, 0, 1, NA, 1))))
  #  expect_warning(expect_true(is.na(rng_gmsnburr(1, 0, 1, 1, NA))))
  
  
  expect_error(expect_true(is.na(rjfst(1, NA, 1, 2, 2))))
  expect_error(expect_true(is.na(rjfst(1, 0, NA, 2, 2))))
  expect_error(expect_true(is.na(rjfst(1, 0, 1, NA, 2))))
  expect_error(expect_true(is.na(rjfst(1, 0, 1, 2, NA))))
  
  #  expect_warning(expect_true(is.na(rng_jfst(1, NA, 1, 0.2, 0))))
  #  expect_warning(expect_true(is.na(rng_jfst(1, 0, NA, 0.2, 0))))
  # expect_warning(expect_true(is.na(rng_jfst(1, 0, 1, NA, 0))))
  #  expect_warning(expect_true(is.na(rng_jfst(1, 0, 1, 0.2, NA))))
  
  
})

# --------------------- Test Probabillity -------------------------------------------

test_that("All probabilities/densities >= 0", {
  
  y <- c( -100, -10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 100)
  
  expect_true(suppressWarnings(all(dmsnburr(y, 0, 1, 1) >= 0)))
  expect_true(suppressWarnings(all(dmsnburr2a(y, 0, 1, 1) >= 0)))
  expect_true(suppressWarnings(all(dgmsnburr(y, 0, 1, 1, 1) >= 0)))
  expect_true(suppressWarnings(all(djfst(y, 0, 1, 2, 2) >= 0)))
  
  # expect_true(suppressWarnings(all(pdf_msnburr(y, 0, 1, 1) >= 0)))
  # expect_true(suppressWarnings(all(pdf_msnburr2a(y, 0, 1, 1) >= 0)))
  # expect_true(suppressWarnings(all(pdf_gmsnburr(y, 0, 1, 1, 1) >= 0)))
  # expect_true(suppressWarnings(all(pdf_jfst(y, 0, 1, 2, 2) >= 0)))
  # 
  # expect_true(suppressWarnings(all(pdf0_msnburr(y, 0, 1, 1) >= 0)))
  # expect_true(suppressWarnings(all(pdf0_msnburr2a(y, 0, 1, 1) >= 0)))
  # expect_true(suppressWarnings(all(pdf0_gmsnburr(y, 0, 1, 1, 1) >= 0)))
  # expect_true(suppressWarnings(all(pdf0_jfst(y, 0, 1, 2, 2) >= 0)))
  
  
})




test_that("All cumulative probabilities >= 0 and <= 1", {
  
  y <- c( -100, -10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 100)
  
  expect_true(all(pmsnburr(y, 0, 1, 1) >= 0 & pmsnburr(y, 0, 1, 1) <= 1))
  expect_true(all(pmsnburr2a(y, 0, 1, 1) >= 0 & pmsnburr2a(y, 0, 1, 1) <= 1))
  expect_true(all(pgmsnburr(y, 0, 1, 1, 1) >= 0 & pgmsnburr(y, 0, 1, 1, 1) <= 1))
  expect_true(all(pjfst(y, 0, 1, 2, 2) >= 0 & pjfst(y, 0, 1, 2,2) <= 1))
  
  # expect_true(all(cdf_msnburr(y, 0, 1, 1) >= 0 & cdf_msnburr(y, 0, 1, 1) <= 1))
  # for(i in 1:length(y)){
  #   expect_true(all(cdf_msnburr2a(y[i], 0, 1, 1) >= 0 & cdf_msnburr2a(y, 0, 1, 1) <= 1))
  # }
  # 
  # expect_true(all(cdf_gmsnburr(y, 0, 1, 1, 1) >= 0 & cdf_gmsnburr(y, 0, 1, 1, 1) <= 1))
  # expect_true(all(cdf_jfst(y, 0, 1, 2, 2) >= 0 & cdf_jfst(y, 0, 1, 2, 2) <= 1))
  # 
  # expect_true(all(cdf0_msnburr(y, 0, 1, 1) >= 0 & cdf_msnburr(y, 0, 1, 1) <= 1))
  # for (i in 1 : length(y)){
  #   expect_true(all(cdf0_msnburr2a(y[i], 0, 1, 1) >= 0 & cdf_msnburr2a(y, 0, 1, 1) <= 1))
  # }
  # 
  # expect_true(all(cdf0_gmsnburr(y, 0, 1, 1, 1) >= 0 & cdf_gmsnburr(y, 0, 1, 1, 1) <= 1))
  # expect_true(all(cdf0_jfst(y, 0, 1, 2, 2) >= 0 & cdf_jfst(y, 0, 1, 2, 2) <= 1))
})


# -------------------------------------Test Quantile Function ------------------------------

test_that("Zeros in quantile functions", {
  
  expect_error(  expect_true(!is.nan(qmsnburr(0, 0, 1, 1))))
  expect_error(expect_true(!is.nan(qmsnburr2a(0, 0, 1, 1))))
  expect_error(expect_true(!is.nan(qgmsnburr(0, 0, 1, 1, 1))))
  expect_error(expect_true(!is.nan(qjfst(0, 0, 1, 2, 2))))
  
})

test_that("Ones in quantile functions", {
  
  expect_error(expect_true(!is.nan(qmsnburr(1, 0, 1, 1))))
  expect_error(expect_true(!is.nan(qmsnburr2a(1, 0, 1, 1))))
  expect_error(expect_true(!is.nan(qgmsnburr(1, 0, 1, 1, 1))))
  expect_error(expect_true(!is.nan(qjfst(1, 0, 1, 2, 2))))

})

test_that("Checking p = F(F^-1(p))", {
  
  pp <- seq(0.00001, 1, by = 0.001)
  
  expect_equal(pp, pmsnburr(qmsnburr(pp, 0, 1, 1), 0, 1, 1))
  expect_equal(pp, pmsnburr2a(qmsnburr2a(pp, 0, 1, 1), 0, 1, 1))
  expect_equal(pp, pgmsnburr(qgmsnburr(pp, 0, 1, 1, 1), 0, 1, 1, 1))
  expect_equal(pp, pjfst(qjfst(pp, 0, 1, 2, 2), 0, 1, 2, 2))
  # for(i in 1:length(pp)){
  # expect_equal(pp[i], cdf_msnburr(quan_msnburr(pp[i], 0, 1, 1), 0, 1, 1))
  # }
  # for(i in 1:length(pp)){
  #   expect_equal(pp[i], cdf_msnburr2a(quan_msnburr2a(pp[i], 0, 1, 1), 0, 1, 1))
  # }
  # for(i in 1:length(pp)){
  # expect_equal(pp[i], cdf_gmsnburr(quan_gmsnburr(pp[i], 0, 1, 1, 1.5), 0, 1, 1, 1.5))
  # expect_equal(pp[i], cdf_jfst(quan_jfst(pp[i], 0, 1, 2, 2), 0, 1, 2, 2))
  # }
  # expect_equal(pp, cdf0_msnburr(quan_msnburr(pp, 0, 1, 1), 0, 1, 1))
  # 
  # for(i in 1 : length(pp)){
  #   expect_equal(pp[i], cdf0_msnburr2a(quan_msnburr2a(pp[i], 0, 1, 1), 0, 1, 1))
  # }
  # 
  # expect_equal(pp, cdf0_gmsnburr(quan_gmsnburr(pp, 0, 1, 1, 1), 0, 1, 1, 1))
  # expect_equal(pp, cdf0_jfst(quan_jfst(pp, 0, 1, 2, 2), 0, 1, 2, 2))
  # 
  
  
})

# ----------------------------- Test RNG Converge -------------------------------------

probCoverage <- function(stub, ..., n = 5000L) {
  rfoo <- eval(as.name(paste0("r", stub)))
  pfoo <- eval(as.name(paste0("p", stub)))
  diff(pfoo(range(rfoo(n, ...)), ...))
}



test_that("Coverage of RNG's", {
  
  skip_on_cran()
  
  expect_gte(probCoverage("msnburr", 0, 1, 1), 0.99)
  
  expect_gte(probCoverage("msnburr2a", 0, 1, 1), 0.99)
  
  expect_gte(probCoverage("gmsnburr", 0, 1, 1, 1), 0.99)
  
  expect_gte(probCoverage("jfst", 0, 1, 2, 2), 0.99)
  
  
  
  
})


# -------------------------- PDF and CDF Test -----------------------------------------
library(neodistr)
library (testthat)
# Vector
# pdf_msnburr <- neonormal_stanfunc(family ="msnburr", func = "pdf",vectorize = TRUE )
# cdf_msnburr <- neonormal_stanfunc(family ="msnburr", func = "cdf",vectorize = TRUE )
# 
# pdf_msnburr2a <- neonormal_stanfunc(family ="msnburr2a", func = "pdf",vectorize = TRUE )
# cdf_msnburr2a <- neonormal_stanfunc(family ="msnburr2a", func = "cdf",vectorize = TRUE )
# 
# pdf_gmsnburr <- neonormal_stanfunc(family ="gmsnburr", func = "pdf",vectorize = TRUE )
# cdf_gmsnburr <- neonormal_stanfunc(family ="gmsnburr", func = "cdf",vectorize = TRUE )
# 
# pdf_jfst <- neonormal_stanfunc(family ="jfst", func = "pdf",vectorize = TRUE )
# cdf_jfst <- neonormal_stanfunc(family ="jfst", func = "cdf",vectorize = TRUE )
# 
# # Non-vector
# pdf0_msnburr <- neonormal_stanfunc(family ="msnburr", func = "pdf",vectorize = FALSE )
# cdf0_msnburr <- neonormal_stanfunc(family ="msnburr", func = "cdf",vectorize = FALSE )
# 
# pdf0_msnburr2a <- neonormal_stanfunc(family ="msnburr2a", func = "pdf",vectorize = FALSE )
# cdf0_msnburr2a <- neonormal_stanfunc(family ="msnburr2a", func = "cdf",vectorize = FALSE )
# 
# pdf0_gmsnburr <- neonormal_stanfunc(family ="gmsnburr", func = "pdf",vectorize = FALSE )
# cdf0_gmsnburr <- neonormal_stanfunc(family ="gmsnburr", func = "cdf",vectorize = FALSE )
# 
# pdf0_jfst <- neonormal_stanfunc(family ="jfst", func = "pdf",vectorize = FALSE )
# cdf0_jfst <- neonormal_stanfunc(family ="jfst", func = "cdf",vectorize = FALSE )


test_that("integrate PDF from -inf to inf == 1", {
  
  expect_equal(integrate(dmsnburr,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1 )$value,1)
  # expect_equal(integrate(pdf_msnburr,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1 )$value,1)
  # expect_equal(integrate(pdf0_msnburr,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1 )$value,1)
  
  expect_equal(integrate(dmsnburr2a,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1 )$value,1)
  # expect_equal(integrate(pdf_msnburr2a,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1 )$value,1)
  # expect_equal(integrate(pdf0_msnburr2a,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1 )$value,1)
  
  expect_equal(integrate(dgmsnburr,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1,beta=1 )$value,1)
  # expect_equal(integrate(pdf_gmsnburr,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1,beta=1 )$value,1)
  # expect_equal(integrate(pdf0_gmsnburr,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=1,beta=1 )$value,1)
  
  expect_equal(integrate(djfst,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=2,beta= 2 )$value,1)
  # expect_equal(integrate(pdf_jfst,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=2,beta= 2 )$value,1)
  # expect_equal(integrate(pdf0_jfst,lower=-Inf, upper = Inf,mu=0,sigma=1,alpha=2,beta= 2)$value,1)
  
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
  
})


# -----------------------------------Cumulative Test ----------------------------------------------------------------------------------------------------------------------------

# test_that("Checking ccdf = 1-cdf", {
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
#   
#   
# })
