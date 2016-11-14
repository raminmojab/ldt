
#----------------------------------------
#' @title A subset of a \code{arimamodelset}
#'
#' @description A set of ARIMA models defined in \code{arimamodelset} class. It contains \code{modelsetsub}.
#'
#' @field p p in ARIMA(p,d,q)x(P,D,Q) model.
#' @field d d in ARIMA(p,d,q)x(P,D,Q) model.
#' @field q q in ARIMA(p,d,q)x(P,D,Q) model.
#' @field P P in ARIMA(p,d,q)x(P,D,Q) model.
#' @field D D in ARIMA(p,d,q)x(P,D,Q) model.
#' @field Q Q in ARIMA(p,d,q)x(P,D,Q) model.
#' @field Intercept Determines whether the ARIMA models in this subset contains intercept.
#' @field ExoIndexes Determines different exogenous variables based on an indexing approach. A vector of (for example) [2 3] means that the second and third variables in \code{arimamodelset$XReg} are the exogenous variables. This vector will grow in \code{arimamodelsetsub$GetNextModel}.
#'
#' @include ldt.R scoringrule.R utils.R modelsetsub.R
#' @export
#----------------------------------------
arimamodelsetsub <- setRefClass("arimamodelsetsub",
                               fields = list(
                                   p = 'numeric',
                                   d = 'numeric',
                                   q = 'numeric',
                                   P = 'numeric',
                                   D = 'numeric',
                                   Q = 'numeric',
                                   Intercept = 'logical',
                                   ExoIndexes = 'vector'
                               ),
                               contains = "modelsetsub")


#----------------------------------------
#' @title The constructor of \code{arimamodelsetsub} class
#' @name arimamodelsetsub_initialize
#' @description It will generate the required fields in this \code{arimamodelsetsub} and its parent \code{modelsetsub}.
#' @field parentarima The corresponding \code{arimamodelset} class.
#' @field p sets \code{p} field of the class.
#' @field d sets \code{d} field of the class.
#' @field q sets \code{q} field of the class.
#' @field intercept sets \code{Intercept} field of the class.
#' @field P sets \code{P} field of the class.
#' @field D sets \code{D} field of the class.
#' @field Q sets \code{Q} field of the class.
#----------------------------------------
NULL
arimamodelsetsub$methods(initialize = function(parentarima,p,d,q,intercept,P,D,Q){
    if (missing(parentarima) == FALSE)
    {
        ParentSet <<- parentarima
        CountValid <<- 0
        CountFailed <<- 0

        p <<- p
        d <<- d
        q <<- q
        Intercept <<- intercept

        if (missing(P) && parentarima$IsSeasonal)
            stop("missing value for seasonal data")

        if (missing(P) == FALSE)
        {
            P <<- P
            D <<- D
            Q <<- Q
        }
        else
        {
            P <<- 0
            D <<- 0
            Q <<- 0
        }

        if (length(parentarima$XReg) == 0)
        {
            CountRequired <<- 1
        }
        else
        {
            rc = 1
            if (parentarima$ParentLDT$MaxSize - 1 > 0)
            {
                for (i in (1:(parentarima$ParentLDT$MaxSize - 1)))
                    rc = rc + choose(ncol(parentarima$XReg), i)
            }
            CountRequired <<- rc
        }


    }
})


#----------------------------------------
#' @title see \code{modelsetsub$GetNextModel} description.
#' @name arimamodelsetsub_GetNextModel
#' @description see \code{modelsetsub$GetNextModel} description.
#' @field isfirst see \code{modelsetsub$GetNextModel} description.
#' @return see \code{modelsetsub$GetNextModel} description.
#----------------------------------------
NULL
arimamodelsetsub$methods(GetNextModel = function(isfirst){

    if (isfirst)
    {
        message("\t\t",show())
        message("\t\t Exogenous = NULL")
    }
    else
    {
        move = .self$movetonext()
        if (move == FALSE)
            return(NULL)
        message("\t\t Exogenous=", paste(ExoIndexes ,collapse = ","))
    }


    res = list()
    h = 0
    tryCatch(
        {
            for (W in ParentSet$SimulationData)
            {
                h = h + 1
                x = NULL
                xf = NULL
                if (is.null(ExoIndexes) == FALSE)
                {
                    x = W$TrainingSampleOther[,ExoIndexes, drop = FALSE]
                    xf = W$NewXRegValidation[,ExoIndexes, drop = FALSE]
                }
                model = arima(W$TrainingSampleTarget, order = c(p, d, q),
                              seasonal = list(order = c(P, D, Q), period = NA),
                              xreg = x, include.mean = Intercept)

                #hor = min(h, ParentSet$ParentLDT$MaxHorizon) # it is not efficient, but I'm not sure other options can be more efficient

                pre = predict(model, n.ahead = h , newxreg = xf)
                err = pre$pred - W$ValidationSample
                se = pre$se
                # if (anyNA(se) || anyNA(err))
                #     stop("NA found")
                res[[length(res) + 1]] = list(model,err,se)

            }
            return(res)
        },
        error = function(err)
        {
            ## any error in any evaluation will result in discarding the model
            print(err)
            return(list())
        }
    )


})

#----------------------------------------
#' @title Changes \code{ExoIndexes} field to reach the next ARIMA model
#' @name arimamodelsetsub_movetonext
#' @return FALSE if no more move is possible. True, if it moved to the next model.
#----------------------------------------
NULL
arimamodelsetsub$methods(movetonext = function(){

    if (length(ParentSet$XReg) == 0 || ParentSet$ParentLDT$MaxSize == 1)
        return(FALSE)

    if (is.null(ExoIndexes))
    {
        ExoIndexes <<- 1
        return(TRUE)
    }

    maxValue = ncol(ParentSet$XReg)
    maxSize = length(ExoIndexes)
    indexOfFreeMove = 0;
    counter = 0;
    for (indexOfFreeMove in (maxSize:0))
    {
        if (indexOfFreeMove == 0)
            break

        treshold = maxValue - counter
        if (ExoIndexes[[indexOfFreeMove]] < treshold)
            break
        counter = counter + 1
    }

    if (indexOfFreeMove == 0)
    {
        if (maxSize == (ParentSet$ParentLDT$MaxSize - 1))
            return(FALSE)
        else
        {
            ExoIndexes <<- (1:(maxSize + 1))
            return(TRUE)
        }
    }
    else
    {
        ExoIndexes[[indexOfFreeMove]] <<- ExoIndexes[[indexOfFreeMove]] + 1
        if (indexOfFreeMove < maxSize)
        {
            for (i in ((indexOfFreeMove + 1):maxSize))
                ExoIndexes[[i]] <<- ExoIndexes[[i - 1]] + 1
        }
        return(TRUE)
    }
})


arimamodelsetsub$methods(show = function(){
    return(paste("ARIMA (",p,",", d,",", q,")x(", P,",", D,",", Q,") required model count=", CountRequired, sep = ""))
})
