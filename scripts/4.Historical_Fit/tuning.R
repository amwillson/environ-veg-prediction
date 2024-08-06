## Using MLR package for multi-hyperparameter tuning
## Tuning mtry = number of variables selected at each node - this is the standard tuned parameter in caret
##        nodesize = the number of observations 
rm(list = ls())

load('data/processed/training/point_train.RData')
load('data/processed/testing/point_test.RData')

# Remove columns we don't want in random forest
point_train <- point_train |>
  dplyr::select(-x, -y, -grid_id, -ecosystem_id, -state, -tree) |>
  dplyr::mutate(species = as.factor(species),
                Floodplain = as.factor(Floodplain))
point_test <- point_test |>
  dplyr::select(-x, -y, -grid_id, -ecosystem_id, -state, -tree) |>
  dplyr::mutate(species = as.factor(species),
                Floodplain = as.factor(Floodplain))

# Create a task
traintask <- mlr3::as_task_classif(x = point_train, target = 'species')
#traintask <- mlr::makeClassifTask(data = point_train, target = 'species')
#testtask <- mlr::makeClassifTask(data = point_test, target = 'species')

# Make randomForest learner
rf.lrn <- mlr3::lrn('classif.randomForest')
#rf.lrn <- mlr::makeLearner('classif.randomForest')
rf.lrn$par.vals <- list(importance = TRUE, do.trace = TRUE)

ParamHelpers::getParamSet(rf.lrn)

# Set parameter space 
#https://arxiv.org/pdf/1804.03515.pdf
params <- ParamHelpers::makeParamSet(ParamHelpers::makeIntegerParam('mtry', lower = 2, upper = 3), # 2 to 16
                                     ParamHelpers::makeIntegerParam('nodesize', lower = 1, upper = 2), # 1 to 5
                                     ParamHelpers::makeDiscreteParam('ntree', values = c(100, 200))) # 100 to 1000

# Set validation strategy
rdesc <- mlr::makeResampleDesc('CV', iters = 5L)

# Set optimization technique
ctrl <- mlr::makeTuneControlRandom(maxit = 5L)

# Start tuning
tune <- mlr::tuneParams(learner = rf.lrn,
                   task = traintask,
                   resampling = rdesc,
                   measures = list(mlr::acc),
                   par.set = params,
                   control = ctrl,
                   show.info = TRUE)
