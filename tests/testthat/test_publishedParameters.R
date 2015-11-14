# Testing code for the RCMIP5 'publishedParameters.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("R/publishedParameters.R")
#   library(testthat)
#   test_file("tests/testthat/test_publishedParameters.R")

context("publishedParameters")

test_that("publishedParameters handles bad inputs",{
  expect_error(publishedParameters(nameArr=c(1,2,3)))
})


test_that('publishedParmaeters models all follow conservation of mass',{
  allModels <- publishedParameters()
  counter <- 0
  TradModels <- c('TECO', 'CLM', 'CENTURY', 'YASSO07')
  for(model in allModels[names(allModels) %in% TradModels]){
    counter <- counter + 1
    y0 <- c(C1=1, C2=1, C3=1, C4=1, C5=1, C6=1, C7=1)
    y0 <- c(y0[1:length(model$type)], 'CO2'=0)
    expect_equal(0,
          sum(unlist(dCModel(t=1, y=y0, parms=model$par, 
                             reactionNetwork=model$reactionNetwork))), 
          info=sprintf("model: %s", names(allModels)[names(allModels) %in% TradModels][counter]))
  }
  for(model in allModels[!names(allModels) %in% TradModels]){
    counter <- counter + 1
    y0 <- model$y0
    expect_equal(0,
                 sum(unlist(dCModel(t=1, y=y0, parms=model$par, 
                                    reactionNetwork=model$reactionNetwork))), 
                 info=sprintf("model: %s", names(allModels)[!names(allModels) %in% TradModels][counter]))
    y0[TRUE] <- 1
    expect_equal(0,
                 sum(unlist(dCModel(t=1, y=y0, parms=model$par, 
                                    reactionNetwork=model$reactionNetwork))), 
                 info=sprintf("model: %s", names(allModels)[!names(allModels) %in% TradModels][counter]))
  }
})