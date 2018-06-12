c_grf <- list(label = "Generalized Random Forest",
                  library = c("grf", "dplyr"),
                  loop = NULL,
                  type = c( "Regression"),
                  parameters = data.frame(parameter = c("sample.fraction","mtry", "num.trees", "min.node.size",
                                                        "alpha","honesty", "imbalance.penalty"),
                                          class = c("numeric","numeric","numeric","numeric",
                                                    "numeric","logical", "numeric"),
                                          label = c("Fraction of data used to build each tree",
                                                    "Number of variables tried for each split",
                                                    "Number of trees grown in the forest",
                                                    "Target for the minimum number of observations in each tree leaf",
                                                    "Tuning parameter that controls the maximum imbalance of a split",
                                                    "Whether or not honest splitting",
                                                    "Controls how harshly imbalance is penalized"
                                                    )),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      param_grf <- grf::tune_regression_forest(X = x, Y = y,  num.fit.trees = 50)
                      param_grf <- as.data.frame(t(param_grf$params))
                      out <- expand.grid(
                        sample.fraction = 0.5,
                        mtry = param_grf$mtry,
                        num.trees = 2000,
                        min.node.size = param_grf$min.node.size,
                        alpha  = param_grf$alpha,
                        honesty = FALSE,
                        imbalance.penalty = param_grf$imbalance.penalty
                      )
                    } else {
                      param_grf <- grf::tune_regression_forest(X = x, Y = y,  num.fit.trees = 50)
                      param_grf <- as.data.frame(t(param_grf$params))
                      out <- data.frame(
                        sample.fraction = runif(len,0.2,0.5),
                        mtry = sample(1:ncol(x), size = len, replace = TRUE),
                        num.trees = floor(runif(len,1500,2500)),
                        min.node.size = sample(c(1:4,param_grf$min.node.size), size = len, replace = TRUE),
                        alpha  = runif(len,param_grf$alpha*0.9,param_grf$alpha*1.1),
                        honesty = sample( c(TRUE,FALSE), len, replace = TRUE),
                        imbalance.penalty = runif(len,param_grf$imbalance.penalty*0.9,param_grf$imbalance.penalty*1.1)
                      )
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...)
                    grf::regression_forest(X = x, Y = y, sample.fraction = param$sample.fraction, mtry = param$mtry, num.trees = param$num.trees, min.node.size = param$min.node.size,
                                           alpha = param$alpha, honesty = param$honesty, imbalance.penalty = param$imbalance.penalty, ...),
                  predict = function(modelFit, newdata, submodels = NULL)
                    if(!is.null(newdata)) predict(modelFit, newdata, estimate.variance = TRUE)$predictions else predict(modelFit,  estimate.variance = TRUE)$predictions,
                  prob = NULL,
                  predictors = function(x, ...) {
                    unique(as.vector(variable.names(x)))
                  },
                  varImp = function(object, ...){
                    varImp <- grf::variable_importance(object, ...)
                    if(object$problemType == "Regression"){
                      rownames(varImp) <- (grf_object$xNames)
                      varImp <- data.frame(Overall = varImp)
                    }
                    else {
                      varImp <- "not implemented"
                    }
                  },
                  levels = NULL,
                  tags = c("Generalized Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection")
              )
