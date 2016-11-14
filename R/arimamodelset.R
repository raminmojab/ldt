
#----------------------------------------
#' @title A \code{modelset} for ARIMA models
#'
#' @description This is a reference class and defines a set of ARIMA (autoregressive integrated moving average) models based on stats package. It contains \code{modelset}.
#'
#' @field TargetData A \code{ts} that contains the target variable's data. It is set using \code{ldt$EndoData}'s first column.
#' @field IsSeasonal Determines whether the frequency of \code{TargetData} is higher than 1.
#' @field XReg Exogenous variables' data. It is set from \code{ldt$ExoData}. Note that ARIMA models are univariate and therefore in this setting the other endogenous variables in \code{ldt$EndoData} are ignored. If possible, you can use their lagged values as exogenous.
#' @field NewXReg Data for exogenous variables (i.e., \code{XReg}) in the forecast period. Of course, these data are not used in the out-of-sample simulations and therefore model verification; However, currently we restrict our attention to the practical models (i.e., those with which we can forecast the unknown future).
#' @field SimulationData A list of \code{simulationdata} objects. The length of the list is determined by \code{ldt$SimulationCount}.
#'
#' @include ldt.R scoringrule.R utils.R modelset.R
#' @export
#----------------------------------------
arimamodelset <- setRefClass("arimamodelset",
                            fields = list(
                                TargetData = 'ts',
                                IsSeasonal = 'logical',
                                XReg = 'matrix',
                                NewXReg = 'matrix',
                                SimulationData = 'list'
                            ),
                            contains = "modelset")


#----------------------------------------
#' @title The constructor of \code{arimamodelset} class
#' @name arimamodelset_initialize
#' @description It will generate the required fields in this \code{arimamodelset} and its parent \code{modelset}.
#' @field parentldt The corresponding ldt class.
#----------------------------------------
NULL
arimamodelset$methods(initialize = function(parentldt)
{
    if (missing(parentldt) == FALSE)
    {
        ParentLDT <<- parentldt
        ID <<- "ARIMA"
        Description <<- "ARIMA models from stats package"
        setSupportedScoringRules(list("MAE","MSE","LSR","LnSR","QSR","HSR","CRPSR"))

        TargetData <<- parentldt$EndoData[,1]
        star = start(TargetData)
        freq = frequency(TargetData)
        if (freq > 1)
            IsSeasonal <<- TRUE
        else
            IsSeasonal <<- FALSE

        if (length(parentldt$ExoData) != 0)
        {
            # deal with exogenous data
            n = dim(as.matrix(TargetData))[1]
            n0 = dim(parentldt$ExoData)[1]
            XReg <<- parentldt$ExoData[1:n,]
            NewXReg <<- parentldt$ExoData[(n + 1):n0,]

        }
        else
        {
            XReg <<- matrix(numeric(0), 0,0)
            NewXReg <<-  matrix(numeric(0), 0,0)
        }

        ## generate the SimulationData
        evalcount = ParentLDT$SimulationCount
        SimulationData <<- list()
        targetmat = as.matrix(TargetData)

        for (i in (1:evalcount))
        {
            t0 = ts(data = targetmat[1:(nrow(targetmat) - i),], frequency = freq, start = star)
            v0 = targetmat[(nrow(targetmat) - i + 1):nrow(targetmat),,drop = FALSE]
            if (length(XReg) == 0)
            {
                SimulationData[[i]] <<- simulationdata$new(t0,v0)
            }
            else
            {
                x0 = XReg[1:(nrow(XReg) - i),,drop = FALSE]
                nx0 = XReg[(nrow(XReg) - i + 1):nrow(XReg),,drop = FALSE]
                SimulationData[[i]] <<- simulationdata$new(t0,v0,x0,nx0)
            }
        }

        ## generate the Subs
        Subs <<- list()
        for (p in (1:ParentLDT$MaxLag))
        {
            for (q in (1:ParentLDT$MaxLag))
            {
                if (IsSeasonal)
                {
                    for (P in (1:ParentLDT$MaxLag))
                    {
                        for (Q in (1:ParentLDT$MaxLag))
                        {
                            Subs[[length(Subs) + 1]] <<- arimamodelsetsub$new(.self,p,0,q,FALSE,P,0,Q)
                            Subs[[length(Subs) + 1]] <<- arimamodelsetsub$new(.self,p,0,q,TRUE,P,0,Q)

                            Subs[[length(Subs) + 1]] <<- arimamodelsetsub$new(.self,p,1,q,FALSE,P,0,Q)
                            #Subs[[length(Subs) + 1]] <<- arimamodelsetsub$new(.self,p,1,q,TRUE,P,0,Q)    # ignores intercept when differencing

                            Subs[[length(Subs) + 1]] <<- arimamodelsetsub$new(.self,p,2,q,FALSE,P,0,Q)
                            #Subs[[length(Subs) + 1]] <<- arimamodelsetsub$new(.self,p,2,q,TRUE,P,0,Q)

                            # ..........................D=1
                            Subs[[length(Subs) + 1]] <<-  arimamodelsetsub$new(.self,p,0,q,FALSE,P,1,Q)
                            #Subs[[length(Subs) + 1]] <<-  arimamodelsetsub$new(.self,p,0,q,TRUE,P,1,Q)  # ignores intercept when differencing

                            Subs[[length(Subs) + 1]] <<-  arimamodelsetsub$new(.self,p,1,q,FALSE,P,1,Q)
                            #Subs[[length(Subs) + 1]] <<-  arimamodelsetsub$new(.self,p,1,q,TRUE,P,1,Q)

                            Subs[[length(Subs) + 1]] <<-  arimamodelsetsub$new(.self,p,2,q,FALSE,P,1,Q)
                            #Subs[[length(Subs) + 1]] <<-  arimamodelsetsub$new(.self,p,2,q,TRUE,P,1,Q)

                            # ..........................D=2
                            Subs[[length(Subs) + 1]] <<-  arimamodelsetsub$new(.self,p,0,q,FALSE,P,2,Q)
                            #Subs[[length(Subs) + 1]] <<-  arimamodelsetsub$new(.self,p,0,q,TRUE,P,2,Q)

                            Subs[[length(Subs) + 1]] <<-  arimamodelsetsub$new(.self,p,1,q,FALSE,P,2,Q)
                            #Subs[[length(Subs) + 1]] <<-  arimamodelsetsub$new(.self,p,1,q,TRUE,P,2,Q)

                            Subs[[length(Subs) + 1]] <<-  arimamodelsetsub$new(.self,p,2,q,FALSE,P,2,Q)
                            #Subs[[length(Subs) + 1]] <<-  arimamodelsetsub$new(.self,p,2,q,TRUE,P,2,Q)
                        }
                    }
                }
                else
                {
                    Subs[[length(Subs) + 1]] <<- arimamodelsetsub$new(.self,p,0,q,FALSE) # for d=0 and without intercept
                    Subs[[length(Subs) + 1]] <<-   arimamodelsetsub$new(.self,p,0,q,TRUE) # for d=0 and intercept

                    Subs[[length(Subs) + 1]] <<-  arimamodelsetsub$new(.self,p,1,q,FALSE) # for d=1
                    Subs[[length(Subs) + 1]] <<-  arimamodelsetsub$new(.self,p,1,q,TRUE) # for d=1           ?????

                    Subs[[length(Subs) + 1]] <<- arimamodelsetsub$new(.self,p,2,q,FALSE) # for d=2
                    Subs[[length(Subs) + 1]] <<-   arimamodelsetsub$new(.self,p,2,q,TRUE) # for d=2                  ????
                }
            }
        }

    }
    callSuper()
})


arimamodelset$methods(show = function(){
    count = 0
    if (is.null(XReg) == FALSE)
        count = ncol(XReg)
    return(paste("LDTPACK","\n\t ",ID," (",Description,")",
                 "\n\t SUB count=",length(Subs),
                 "\n\t potential exo count=",count,
                 "\n\t required model count = ", .self$GetCounts()[[1]], sep = ""))
})

