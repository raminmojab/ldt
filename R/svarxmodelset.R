
#----------------------------------------
#' @title A \code{modelset} for Stationary VARX models
#'
#' @description This is a reference class and defines a set of Stationary VAR (vector autoregressive) models with stationary exogenous variables, based on \code{stats} package. It contains \code{modelset}.
#'
#' @field TargetData A \code{ts} that contains the target variable's data. It is set using \code{ldt$EndoData}'s first column.
#' @field EndoExoData A \code{matrix} that contains other types of data; i.e., other endogenous and exogenous ones. Endogenous data comes first.
#' @field ExoStartIndex The index of the column in \code{EndoExoData} which contains the first exogenous variable.
#' @field NewExoData A \code{matrix} that contains the future values of the exogenous variables.
#' @field SimulationData A list of \code{simulationdata} objects. The length of the list is determined by \code{ldt$SimulationCount}.
#'
#' @include ldt.R scoringrule.R utils.R modelset.R
#' @export
#----------------------------------------
svarxmodelset <- setRefClass("svarxmodelset",
                            fields = list(
                                TargetData = 'ts',
                                EndoExoData = 'matrix',
                                ExoStartIndex = 'numeric',
                                NewExoData = 'matrix',
                                SimulationData = 'list'
                            ),
                            contains = "modelset")

#----------------------------------------
#' @title The constructor of \code{svarxmodelset} class
#' @name svarxmodelset_initialize
#' @description It will generate the required fields in this \code{svarxmodelset} and its parent \code{modelset}.
#' @field parentldt The corresponding ldt class.
#----------------------------------------
NULL
svarxmodelset$methods(initialize = function(parentldt)
{
    if (missing(parentldt) == FALSE)
    {
        ParentLDT <<- parentldt
        ID <<- "St. VARX"
        Description <<- "Stationary VAR model with stationary exogenous variables using stats package"
        setSupportedScoringRules(list("MAE","MSE","LSR","LnSR","QSR","HSR","CRPSR"))

        TargetData <<- parentldt$EndoData[,1]
        star = start(TargetData)
        freq = frequency(TargetData)

        endodata = matrix(numeric(0), 0,0)
        exodata = matrix(numeric(0), 0,0)
        newexodata = matrix(numeric(0), 0,0)


        if (NCOL(parentldt$EndoData) > 1)
            endodata = as.matrix(parentldt$EndoData[,2:NCOL(parentldt$EndoData)])
        else
            stop("This is not potentially multivariate.")

        if (length(parentldt$ExoData) != 0)
        {
            # deal with exogenous data
            n = dim(as.matrix(TargetData))[1]
            n0 = dim(parentldt$ExoData)[1]
            exodata = parentldt$ExoData[1:n,]
            NewExoData <<- parentldt$ExoData[(n + 1):n0,]
        }

        if (length(endodata) != 0 && length(exodata) != 0)
        {
            EndoExoData <<- cbind2(endodata, exodata)
            ExoStartIndex <<- NCOL(endodata) + 1
        }
        else if (length(endodata) != 0)
        {
            EndoExoData <<- endodata
            ExoStartIndex <<- .Machine$integer.max
        }
        else if (length(exodata) != 0)
        {
            EndoExoData <<- exodata
            ExoStartIndex <<- 1
        }
        else
        {
            EndoExoData <<- matrix(numeric(0), 0,0)
            ExoStartIndex <<- -1
        }


        ## generate the SimulationData
        evalcount = ParentLDT$SimulationCount
        SimulationData <<- list()
        targetmat = as.matrix(TargetData)

        for (i in (1:evalcount))
        {
            t0 = ts(data = targetmat[1:(nrow(targetmat) - i),], frequency = freq, start = star)
            v0 = targetmat[(nrow(targetmat) - i + 1):nrow(targetmat),,drop = FALSE]
            if (length(EndoExoData) == 0)
            {
                SimulationData[[i]] <<- simulationdata$new(t0,v0)
            }
            else
            {
                x0 = EndoExoData[1:(nrow(EndoExoData) - i),,drop = FALSE]
                nx0 = EndoExoData[(nrow(EndoExoData) - i + 1):nrow(EndoExoData),,drop = FALSE]
                SimulationData[[i]] <<- simulationdata$new(t0,v0,x0,nx0)
            }
        }

        ## generate the Subs
        Subs <<- list()
        for (p in 1:parentldt$MaxLag)
        {
            for (size in 2:parentldt$MaxSize) # just multivariates
            {
                Subs[[length(Subs) + 1]] <<- svarxmodelsetsub$new(.self,p,size,TRUE)
                Subs[[length(Subs) + 1]] <<- svarxmodelsetsub$new(.self,p,size,FALSE)
            }
        }

    }
    callSuper()
})



svarxmodelset$methods(show = function(){
    count = 0
    if (is.null(EndoExoData) == FALSE)
        count = ncol(EndoExoData)
    return(paste("LDTPACK","\n\t ",ID," (",Description,")",
                 "\n\t SUB count=",length(Subs),
                 "\n\t potential endo-exo count=",count,
                 "\n\t required model count = ", .self$GetCounts()[[1]], sep = ""))
})

