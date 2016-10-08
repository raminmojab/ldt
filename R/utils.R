

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
