
ldtpackarimasub <- setRefClass("ldtpackarimasub",
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
                               contains = "ldtpacksub")

ldtpackarimasub$methods(show = function(){
    return(paste("ARIMA (",p,",", d,",", q,")x(", P,",", D,",", Q,") required model count=", CountRequired, sep = ""))
})


ldtpackarimasub$methods(initialize = function(parentarima,p,d,q,intercept,P,D,Q){
    if (missing(parentarima) == FALSE)
    {
        ParentPack <<- parentarima
        CountValid <<- 0
        CountFailed <<- 0
        CountWarning <<- 0

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

# returns NULL if there is no more move
# returns an empty list if any error occured
# returns a list of 1.Model 2. Forecast error and 3. forecast standard error
ldtpackarimasub$methods(GetNextModel = function(isfirst){

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
            for (W in ParentPack$EvaluationData)
            {
                h = h + 1
                x = NULL
                xf = NULL
                if (is.null(ExoIndexes) == FALSE)
                {
                    x = W$TrainingSampleExo[,ExoIndexes, drop = FALSE]
                    xf = W$NewXRegValidation[,ExoIndexes, drop = FALSE]
                }
                model = arima(W$TrainingSampleTarget, order = c(p, d, q),
                              seasonal = list(order = c(P, D, Q), period = NA),
                              xreg = x, include.mean = Intercept)

                #hor = min(h, ParentPack$ParentLDT$MaxHorizon) # it is not efficient, but I'm not sure other options can be more efficient

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


ldtpackarimasub$methods(movetonext = function(){

    if (length(ParentPack$XReg) == 0 || ParentPack$ParentLDT$MaxSize == 1)
        return(FALSE)

    if (is.null(ExoIndexes))
    {
        ExoIndexes <<- 1
        return(TRUE)
    }

    maxValue = ncol(ParentPack$XReg)
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
        if (maxSize == (ParentPack$ParentLDT$MaxSize - 1))
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
