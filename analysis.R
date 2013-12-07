
# source('C:/Users/Roy/r_projects/analysis2/analysis.R')
setwd('C:/Users/Roy/r_projects/analysis2')
load('samsungData.rda')


# Your task is to build a function that predicts what activity a subject
# is performing based on the quantitative measurements from the Samsung
# phone. For this analysis your training set must include the data from
# subjects 1, 3, 5, and 6.  But you may use more subjects data to train
# if you wish. Your test set is the data from subjects 27, 28, 29, and
# 30, but you may use more data to test. Be careful that your
# training/test sets do not overlap.

# You should perform all of the steps in building a predictive model and
# describe your analysis in a report as explained below.


# Dimension reduction
  #  There are 561 substantive variables!
  #  SVD or PCA--see 'clustering example' lecture from week 4, which uses this data.
  # combination of SVD and cluster to come up with linear combos of specific vars that maximize discriminability of the activities w/the least amount of data.
  #  function of that linear combo that predicts activity?
  #  Role for CART here?


s = names(samsungData)
s = gsub( '[()]'          ,''  ,s ) # replace parens
s = gsub( '[^0-9a-zA-Z]+' ,'_' ,s ) # replace nonalpha chars with underscore
s = make.unique(s)
names(samsungData) = s

trainers  <- samsungData[samsungData$subject %in% c(1, 3, 5, 6) , ]
testers   <- samsungData[samsungData$subject %in% 27:30         , ]

tst <- trainers[, c(10:15, 562, 563)]

findvars <- function(x,dv,id){
  ivs <- setdiff(names(x),c(dv,id))
  #initialize result list of the appropriate length
  result <- setNames(vector("list",length(ivs)),ivs)
  for (i in seq_along(ivs)){
    # print(paste(dv,ivs[i],sep = "~"))
    result[[i]] <- abs(coef(lm(paste(ivs[i],dv,sep = "~"),data = x)))
  }
  as.data.frame(t(as.data.frame(result)))
}

# x <- findvars(x = tst, dv = 'activity', id = 'subject')
x <- findvars(x = trainers, dv = 'activity', id = 'subject')
nvars <- 4
# This returns the names of the 10-best predictors for activity standing
best_standing <- row.names(x[order(-x['activitystanding']), 0:2][1:nvars, ])
best_sitting  <- row.names(x[order(-x['activitysitting' ]), 0:2][1:nvars, ])
best_walk     <- row.names(x[order(-x['activitywalk'    ]), 0:2][1:nvars, ])
best_walkdown <- row.names(x[order(-x['activitywalkdown']), 0:2][1:nvars, ])
best_walkup   <- row.names(x[order(-x['activitywalkup'  ]), 0:2][1:nvars, ])

vars_to_use <- unique(c(best_standing, best_sitting, best_walk, best_walkdown, best_walkup))
library(lattice)
pairs(trainers[, vars_to_use], col = as.factor(trainers[, 'activity']))
