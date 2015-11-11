# Testing code for the RCMIP5 'dC.guildModel.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("R/dC.guildModel.R")
#   library(testthat)
#   test_file("tests/testthat/test_dC.guildModel.R")

context("dC.guildModel")

test_that('dC.guildModel produces expected errors',{})

test_that('dC.guildModel reproduces a simple first order model', {
  reactionNetwork <- data.frame(from=c('C1', 'C1', 'C2', 'C2'), 
                                to=c(NA, 'C2', NA, 'C1'), 
                                reaction=c('1/tau1*(1-trans2)*C1', '1/tau1*trans2*C1', 
                                           '1/tau2*(1-trans3)*C2', '1/tau2*trans3*C2'),
                                type=c('decay', 'transfer', 'decay', 'transfer'),
                                stringsAsFactors=FALSE)
  y <- c(C1=1, C2=3)
  parms <- unlist(list(tau1=10, tau2=100, trans2=0.5, trans3=0.1))
  
  expect_equal(dC.firstOrderModel(t=0, y=y, parms=parms, transStr='trans'),
                  dC.guildModel(t=0, y=y, parms=parms, reactionNetwork=reactionNetwork))
})