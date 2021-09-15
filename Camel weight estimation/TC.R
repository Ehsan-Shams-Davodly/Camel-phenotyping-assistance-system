# Test time complexity
TC <- function(Model, Data){
    library(tictoc)
    start_time <- Sys.time()
    temp <- predict(Model, Data)
    end_time <- Sys.time()
    return(end_time - start_time)
}