
# rf.R

library(caret)
library(randomForest)
library(Standard)
library(magrittr)

load("data/train.RData")
load("data/test.RData")

# set up training ctrls
tc_fit <- trainControl(method = "none", number = 1, verboseIter = TRUE, returnData = FALSE)
tc_boot <- trainControl(method = "boot", number = 1, verboseIter = TRUE, returnData = FALSE)

# tuning grids
tg0 <- expand.grid(mtry = seq(10, 111, by = 5))

# rf0
rf0 <- train(y = train$crime_count,
             x = subset(train, select = -crime_count),
             method = "rf",
             trControl = tc_boot,
             metric = "RMSE",
             nthreads = 4,
             tuneGrid = tg0)