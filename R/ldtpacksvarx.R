
# stationary var with stationary exogenous variables (using MTS package)
ldtpacksvarx <- setRefClass("ldtpacksvarx",
                            fields = list(
                                TargetData = 'ts',
                                EndoData = 'matrix',
                                XReg = 'matrix',
                                NewXReg = 'matrix',
                                EvaluationData = 'list' # a list of evaluatedata to be used in out of sample simulations
                            ),
                            contains = "ldtpack")

ldtpackarima$methods(initialize = function(parentldt)
{
    if (missing(parentldt) == FALSE)
    {
        ParentLDT <<- parentldt
        ID <<- "VARX"
        Description <<- "Stationary VAR with stationary sxogenous variables using MTS package"
        setSupportedScoringRules(list("MAE","MSE","LSR","LnSR","QSR","HSR","CRPSR"))

        TargetData <<- parentldt$EndoData[,1]
        if (NCOL(parentldt$EndoData) > 1)
            EndoData <<- as.matrix(parentldt$EndoData[,2:NCOL(parentldt$EndoData)])
        else
            EndoData <<- matrix(numeric(0), 0,0)

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

        ## generate the processes
        Processes <<- list()
        for (p in 1:parentldt$MaxLag)
        {
            for (q in 1:parentldt$MaxLag)
            {
                for (size in 1:parentldt$MaxSize)
                {
                    Processes[[length(Processes) + 1]] <<- ldtpacksvarxsub$new(.self,p,q,size,TRUE)
                    Processes[[length(Processes) + 1]] <<- ldtpacksvarxsub$new(.self,p,q,size,FALSE)
                }
            }
        }



    }
    callSuper()
})





