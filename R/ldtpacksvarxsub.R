

ldtpacksvarxsub <- setRefClass("ldtpacksvarxsub",
                               fields = list(
                                   P = 'numeric',
                                   Size = 'numeric',
                                   Intercept = 'logical',
                                   Indexes = 'vector'
                               ),
                               contains = "ldtpacksub")

ldtpacksvarxsub$methods(initialize = function(parentsvarx,p,size,intercept){
    if (missing(parentsvarx) == FALSE)
    {
        ParentPack <<- parentsvarx
        CountValid <<- 0
        CountFailed <<- 0
        CountWarning <<- 0

        P <<- p
        Size <<- size
        Intercept <<- intercept


        if (length(parentsvarx$EndoExoData) == 0)
        {
            CountRequired <<- 1
        }
        else
        {
            CountRequired <<- choose(ncol(parentsvarx$EndoExoData), size)
        }


    }
})

ldtpacksvarxsub$methods(show = function(){
    return(paste("VARX (", P, ") potential endo-exo count=", NCOL(ParentPack$EndoExoData),
                 ") required model count=", CountRequired, sep = ""))
})


ldtpacksvarxsub$methods(GetNextModel = function(isfirst){

    if (isfirst && Size == 1)
    {
        message("\t\t",show())
        message("\t\t Endogenous-Exogenous=NULL")
    }
    else
    {
        if (isfirst)
        {
            Indexes <<- as.vector(1:Size)
        }
        else
        {
            move = .self$movetonext()
            if (move == FALSE)
                return(NULL)
        }
        message("\t\t Endogenous-Exogenous=", paste(Indexes ,collapse = ","))
    }

    res = list()
    h = 0
    tryCatch(
        {
            for (W in ParentPack$SimulationData)
            {
                h = h + 1
                z = NULL
                x = NULL
                xf = NULL
                if (is.null(Indexes) == FALSE)
                {
                    indEndo = Indexes[Indexes < ParentPack$ExoStartIndex]
                    indExo = Indexes[Indexes >= ParentPack$ExoStartIndex]
                    z = cbind2(W$TrainingSampleTarget, W$TrainingSampleOther[,indEndo, drop = FALSE])
                    x = W$TrainingSampleOther[,indExo, drop = FALSE]
                    xf = W$NewXRegValidation[,indExo, drop = FALSE]
                }
                model = MTS::VARX(zt = z, p = P, xt = x, include.mean = Intercept)
                pre = MTS::VARXpred(model, hstep = h, newxt = xf)

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


ldtpacksvarxsub$methods(movetonext = function(){

    if (length(ParentPack$EndoExoData) == 0 || ParentPack$ParentLDT$MaxSize == 1)
        return(FALSE)

    maxValue = ncol(ParentPack$EndoExoData)
    maxSize = length(Indexes)
    indexOfFreeMove = 0;
    counter = 0;
    for (indexOfFreeMove in (maxSize:0))
    {
        if (indexOfFreeMove == 0)
            break

        treshold = maxValue - counter
        if (Indexes[[indexOfFreeMove]] < treshold)
            break
        counter = counter + 1
    }

    if (indexOfFreeMove == 0)
    {
        return(FALSE)
    }
    else
    {
        Indexes[[indexOfFreeMove]] <<- Indexes[[indexOfFreeMove]] + 1
        if (indexOfFreeMove < maxSize)
        {
            for (i in ((indexOfFreeMove + 1):maxSize))
                Indexes[[i]] <<- Indexes[[i - 1]] + 1
        }
        return(TRUE)
    }
})
