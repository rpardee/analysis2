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


trainers  <- samsungData[samsungData$subject %in% c(1, 3, 5, 6) , ]
testers   <- samsungData[samsungData$subject %in% 27:30         , ]

# Dimension reduction
  #  There are 561 substantive variables!
  #  SVD or PCA--see 'clustering example' lecture from week 4, which uses this data.
  # combination of SVD and cluster to come up with linear combos of specific vars that maximize discriminability of the activities w/the least amount of data.
  #  function of that linear combo that predicts activity?
  #  Role for CART here?


s = names(samsungData)
s = gsub( '[()]'           ,''  ,s ) # replace parens
s = gsub( '[^0-9a-zA-Z]+' ,'_' ,s ) # replace nonalpha chars with underscore
s = make.unique(s)
names(samsungData) = s

# activity ~ 'tBodyAcc-max()-X' + 'tBodyAcc-max()-Y' + 'tBodyAcc-max()-Z' + 'tBodyAcc-min()-X' + 'tBodyAcc-min()-Y' + 'tBodyAcc-min()-Z'

findvars <- function() {
  # Loops through the possible predictor vars, does an lm() predicting activity
  # from each, and returns a data.frame of coefficients.
  r <- data.frame()
  ivs <- setdiff(names(samsungData), c('subject', 'activity'))[1:20]
  for (iv in ivs) {
    print(paste("trying", c(iv,'activity')))
    sub <- complete.cases(samsungData[, c(iv, 'activity')])
    m <- lm(activity ~ iv, data = sub, na.rm = TRUE)
    c <- t(as.data.frame(sapply(m$coefficients, abs)))
    row.names(c) <- iv
    r <- c(r, c)
  }
  return(r)
}
findvars()

m <- lm(formula = tBodyAcc_max_X ~ activity, data = samsungData)
sapply(m$coefficients, abs)


