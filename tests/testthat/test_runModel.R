# Testing code for the RCMIP5 'runModel.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("R/runModel.R")
#   library(testthat)
#   test_file("tests/testthat/test_runModel.R")

context("runModel")

test_that('runModel executes',{
  par.ls <- publishedParameters()[['TECO']]
  par <- c(rep(0.1, length(par.ls$tau)-1), par.ls$tau, par.ls$trans$val)
  names(par) <- c(sprintf('label1.a%d', 1:(length(par.ls$tau)-1)), sprintf('tau%d', 1:length(par.ls$tau)), as.character(par.ls$trans$name))
  
  expect_error(runModel())
  expect_error(runModel(par=par))
  expect_error(runModel(timeArr=1:10))
  expect_true(is.data.frame(runModel(par=par, timeArr=1:10, transStr='A')))
})

test_that("runModel correctly calculates step", {
  par.ls <- publishedParameters()[['TECO']]
  par <- c(rep(0.1, length(par.ls$tau)-1), par.ls$tau, par.ls$trans$val)
  names(par) <- c(sprintf('label1.a%d', 1:(length(par.ls$tau)-1)), sprintf('tau%d', 1:length(par.ls$tau)), as.character(par.ls$trans$name))
  
  expect_silent(test <- runModel(par, timeArr=1:10, transStr='A'))
  y0 <- test[1,1+1:length(par.ls$tau)]
  expect_silent(K <- makeDecompMatrix(par=par, transStr='A'))
  
  expect_true(all(abs((y0+K %*% t(y0))-test[2,1+1:length(par.ls$tau)])/test[2, 1+1:length(par.ls$tau)] < 1e-3))
})

test_that("runModel correctly runs a one pool model",{
  expect_silent(modelOut <- runModel(par=unlist(list(tau1=180)), timeArr=2^seq(0, 10, length=50)))
  expect_true(all(abs(modelOut$C1-exp(-1/180*modelOut$time) ) < 1e-5))
})

test_that("runModel runs a nonlinear model",{
  par <- unlist(list('v_enz'=0.2, 'km_enz'=10,
              'v_up'=1, 'km_up'=2,
              'cue'=0.5, 'basal' = 0.01,
              'turnover_b'=0.5, 'turnover_e'=0.1,
              a1=0.9, a2=0.01))
  test <- runModel(par=par, timeArr=2^seq(0, 10, length=50), cModel=dC.SCBModel)
})