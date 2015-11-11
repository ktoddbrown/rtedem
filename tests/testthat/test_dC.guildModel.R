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

test_that('dC.guildModel', {
  reactionNetwork <- data.frame(from=c('C1', 'C1', 'C2', 'C2'), 
                                to=c(NA, 'C2', NA, 'C1'), 
                                reaction=c('k1*(1-trans2)*C1', 'k1*trans2*C1', 'k2*(1-trans3)*C2', 'k2*trans3*C2'),
                                type=c('decay', 'transfer', 'decay', 'transfer'),
                                stringsAsFactors=FALSE)
  y <- c(C1=1, C2=3)
  dy <- y
  parms <- list(k1=0.1, k2=0.01, trans2=0.5, trans3=0.1)
  attach(c(as.list(y), parms))
  
  temp <- ddply(reactionNetwork, c('from', 'to', 'reaction'), summarize, value = eval(parse(text = as.character(reaction))))
  detach(c(as.list(y), parms))
  dC <- data.frame()
  for(idStr in unique(c(temp$to, temp$from))){
    dC <- rbind.fill(dC, data.frame(id=idStr, 
                                    value=sum(temp$value[temp$to %in% idStr]) -
                                          sum(temp$value[temp$from %in% idStr])))
  }
  dy <- dC$value[!is.na(dC$id)]
  names(dy) <- dC$id[!is.na(dC$id)]
  dy <- dy[names(y)]
})