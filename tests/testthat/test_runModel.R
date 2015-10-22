# Testing code for the RCMIP5 'runModel.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("R/runModel.R")
#   library(testthat)
#   test_file("tests/testthat/test_runModel.R")

context("runModel")

test_that("runModel correctly calculates step", {
  par.ls <- publishedParameters()[['TECO']]
  par <- c(rep(0.1, length(par.ls$tau)-1), par.ls$tau, par.ls$trans$val)
  names(par) <- c(sprintf('label1.a%d', 1:(length(par.ls$tau)-1)), sprintf('tau%d', 1:length(par.ls$tau)), as.character(par.ls$trans$name))
  
  test<- runModel(par, timeArr=1:10, transStr='A')
  y0 <- test[1,1+1:length(par.ls$tau)]
  K <- makeDecompMatrix(par=par, transStr='A')
  
  expect_true(all(abs((y0+K %*% t(y0))-test[2,1+1:length(par.ls$tau)])/test[2, 1+1:length(par.ls$tau)] < 1e-3))
})

test_that("runModel correctly runs a one pool model",{
  modelOut <- runModel(par=unlist(list(tau1=180)), timeArr=2^seq(0, 10, length=50))
  expect_true(all(abs(modelOut$C1-exp(-1/180*modelOut$time) ) < 1e-5))
})