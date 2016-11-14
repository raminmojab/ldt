
#----------------------------------------
#' @title Required data to simulate a forecast
#'
#' @description It provides the required data to forecast and calculate the forecast errors and forecast standard errors in a simulation practice.
#'
#' @field TrainingSampleTarget A \code{ts} for the target variable that will be used in estimation.
#' @field ValidationSample A \code{matrix} for the target variables that will be used in validation.
#' @field TrainingSampleOther A \code{matrix} that contains other information for estimation of the model.
#' @field NewXRegValidation A \code{matrix} of exogenous variables in the validation sample.
#'
#' @export
#----------------------------------------
simulationdata <- setRefClass("simulationdata",
                                 fields = list(TrainingSampleTarget = 'ts',
                                               ValidationSample = 'matrix',
                                               TrainingSampleOther = 'matrix',
                                               NewXRegValidation = 'matrix'),
                                 methods = list(
                                     initialize = function(trainingsampletarget,validationsample,trainingsampleother,newxregvalidation)
                                     {
                                         if (missing(trainingsampletarget) == FALSE)
                                         {
                                             TrainingSampleTarget <<- trainingsampletarget
                                             ValidationSample <<- as.matrix(validationsample)
                                             if (missing(trainingsampleother) == FALSE)
                                             {
                                                 TrainingSampleOther <<- trainingsampleother
                                                 NewXRegValidation <<- as.matrix(newxregvalidation)
                                             }
                                         }
                                     }))
