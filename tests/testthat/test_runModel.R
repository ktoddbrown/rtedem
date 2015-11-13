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
  modelDef <- publishedParameters()[[1]]
  
  expect_error(runModel())
  expect_error(runModel(par=par))
  expect_error(runModel(timeArr=1:10))
  expect_true(is.data.frame(runModel(par=modelDef$par, timeArr=1:10, y0=c(C1=1, C2=1, C3=1, C4=1, C5=1), reactionNetwork=modelDef$reactionNetwork)))
})


test_that("runModel correctly runs a one pool model",{
  expect_silent(modelOut <- runModel(par=unlist(list(tau1=180)), timeArr=2^seq(0, 10, length=50), y0=c(C1=1), reactionNetwork=data.frame(from=c('C1'), to=c(NA), reaction=c('C1*1/tau1')), verbose=FALSE))
  expect_true(all(abs(modelOut$C1-modelOut$C1[1]*exp(-1/180*modelOut$time) ) < 1e-5))
})
