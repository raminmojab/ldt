evaluatedata <- setRefClass("evaluatedata",
                            fields = list(TrainingSampleTarget = 'ts',
                                          ValidationSample = 'matrix',
                                          TrainingSampleExo = 'matrix',
                                          NewXRegValidation = 'matrix'),
                            methods = list(
                                initialize = function(trainingsampletarget,validationsample,trainingsampleexo,newxregvalidation)
                                {
                                    if (missing(trainingsampletarget) == FALSE)
                                    {
                                        TrainingSampleTarget <<- trainingsampletarget
                                        ValidationSample <<- as.matrix(validationsample)
                                        if (missing(trainingsampleexo) == FALSE)
                                        {
                                            TrainingSampleExo <<- trainingsampleexo
                                            NewXRegValidation <<- as.matrix(newxregvalidation)
                                        }
                                    }
                                }))

ldtpackarima <- setRefClass("ldtpackarima",
                            fields = list(
                                TargetData = 'ts',
                                IsSeasonal = 'logical',
                                XReg = 'matrix',
                                NewXReg = 'matrix',
                                EvaluationData = 'list' # a list of evaluatedata to be used in out of sample simulations
                            ),
                            contains = "ldtpack")

ldtpackarima$methods(show = function(){
    count = 0
    if (is.null(XReg) == FALSE)
        count = ncol(XReg)
    return(paste("LDTPACK","\n\t ",ID," (",Description,")",
                 "\n\t SUB count=",length(Processes),
                 "\n\t potential exo count=",count,
                 "\n\t required model count = ", .self$GetCounts()[[1]], sep = ""))
})

ldtpackarima$methods(initialize = function(parentldt)
{
    if (missing(parentldt) == FALSE)
    {
        ParentLDT <<- parentldt
        ID <<- "ARIMA FORECAST"
        Description <<- "Different ARIMA models from stats package"
        setSupportedEvaluations(list("MAE","MSE","LSR","LnSR","QSR","HSR","CRPSR"))

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

        ## generate the EvaluationData
        evalcount = ParentLDT$SimulationCount
        EvaluationData <<- list()
        targetmat = as.matrix(TargetData)

        for (i in (1:evalcount))
        {
            t0 = ts(data=targetmat[1:(nrow(targetmat) - i),], frequency = freq, start = star)
            v0 = targetmat[(nrow(targetmat) - i + 1):nrow(targetmat),,drop = FALSE]
            if (length(XReg) == 0)
            {
                EvaluationData[[i]] <<- evaluatedata$new(t0,v0)
            }
            else
            {
                x0 = XReg[1:(nrow(XReg) - i),,drop = FALSE]
                nx0 = XReg[(nrow(XReg) - i + 1):nrow(XReg),,drop = FALSE]
                EvaluationData[[i]] <<- evaluatedata$new(t0,v0,x0,nx0)
            }
        }

        ## generate the processes
        Processes <<- list()
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
                            Processes[[length(Processes) + 1]] <<- ldtpackarimasub$new(.self,p,0,q,FALSE,P,0,Q)
                            Processes[[length(Processes) + 1]] <<- ldtpackarimasub$new(.self,p,0,q,TRUE,P,0,Q)

                            Processes[[length(Processes) + 1]] <<- ldtpackarimasub$new(.self,p,1,q,FALSE,P,0,Q)
                            #Processes[[length(Processes) + 1]] <<- ldtpackarimasub$new(.self,p,1,q,TRUE,P,0,Q)    # ignores intercept when differencing

                            Processes[[length(Processes) + 1]] <<- ldtpackarimasub$new(.self,p,2,q,FALSE,P,0,Q)
                            #Processes[[length(Processes) + 1]] <<- ldtpackarimasub$new(.self,p,2,q,TRUE,P,0,Q)

                            # ..........................D=1
                            Processes[[length(Processes) + 1]] <<-  ldtpackarimasub$new(.self,p,0,q,FALSE,P,1,Q)
                            #Processes[[length(Processes) + 1]] <<-  ldtpackarimasub$new(.self,p,0,q,TRUE,P,1,Q)  # ignores intercept when differencing

                            Processes[[length(Processes) + 1]] <<-  ldtpackarimasub$new(.self,p,1,q,FALSE,P,1,Q)
                            #Processes[[length(Processes) + 1]] <<-  ldtpackarimasub$new(.self,p,1,q,TRUE,P,1,Q)

                            Processes[[length(Processes) + 1]] <<-  ldtpackarimasub$new(.self,p,2,q,FALSE,P,1,Q)
                            #Processes[[length(Processes) + 1]] <<-  ldtpackarimasub$new(.self,p,2,q,TRUE,P,1,Q)

                            # ..........................D=2
                            Processes[[length(Processes) + 1]] <<-  ldtpackarimasub$new(.self,p,0,q,FALSE,P,2,Q)
                            #Processes[[length(Processes) + 1]] <<-  ldtpackarimasub$new(.self,p,0,q,TRUE,P,2,Q)

                            Processes[[length(Processes) + 1]] <<-  ldtpackarimasub$new(.self,p,1,q,FALSE,P,2,Q)
                            #Processes[[length(Processes) + 1]] <<-  ldtpackarimasub$new(.self,p,1,q,TRUE,P,2,Q)

                            Processes[[length(Processes) + 1]] <<-  ldtpackarimasub$new(.self,p,2,q,FALSE,P,2,Q)
                            #Processes[[length(Processes) + 1]] <<-  ldtpackarimasub$new(.self,p,2,q,TRUE,P,2,Q)
                        }
                    }
                }
                else
                {
                    Processes[[length(Processes) + 1]] <<- ldtpackarimasub$new(.self,p,0,q,FALSE) # for d=0 and without intercept
                    Processes[[length(Processes) + 1]] <<-   ldtpackarimasub$new(.self,p,0,q,TRUE) # for d=0 and intercept

                    Processes[[length(Processes) + 1]] <<-  ldtpackarimasub$new(.self,p,1,q,FALSE) # for d=1
                    Processes[[length(Processes) + 1]] <<-  ldtpackarimasub$new(.self,p,1,q,TRUE) # for d=1           ?????

                    Processes[[length(Processes) + 1]] <<- ldtpackarimasub$new(.self,p,2,q,FALSE) # for d=2
                    Processes[[length(Processes) + 1]] <<-   ldtpackarimasub$new(.self,p,2,q,TRUE) # for d=2                  ????
                }
            }
        }

    }
})




