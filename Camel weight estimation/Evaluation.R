# Regression evaluation functions
MAE <- function(actual, predicted){
    MLmetrics::MAE(predicted, actual)
}
R2 <- function(actual, predicted){
    MLmetrics::R2_Score(predicted, actual)
}
RMSE <- function(actual, predicted){
    MLmetrics::RMSE(predicted, actual)
}
MAPE <- function(actual, predicted){
    MLmetrics::MAPE(predicted, actual)
}
MSE <- function(actual, predicted){
    MLmetrics::MSE(predicted, actual)
}

NewSummary <- function (data,
                            lev = NULL,
                            model = NULL) {
    c(MAE  = MAE((data$obs), (data$pred)),
      RMSE = RMSE((data$obs), (data$pred)),
      MSE  = MSE((data$obs), (data$pred)),
      R2   = R2((data$obs), (data$pred)),
      MAPE = MAPE((data$obs), (data$pred)))
}
