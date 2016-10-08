

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

        CountRequired <<- choose(ncol(parentsvarx$EndoExoData), size - 1)


    }
})

ldtpacksvarxsub$methods(show = function(){
    return(paste("VARX(", P, ") size=", Size,
                 "; required model count=", CountRequired, sep = ""))
})


ldtpacksvarxsub$methods(GetNextModel = function(isfirst){
    if (isfirst)
    {
        message("\t\t",show())
        message("\t\t Endogenous-Exogenous=NULL")
        Indexes <<- as.vector(1:(Size - 1))
        indEndo = Indexes[Indexes < ParentPack$ExoStartIndex]
    }
    else
    {
        indEndo = numeric(0)
        while (length(indEndo) == 0)
        {
            move = .self$movetonext()
            if (move == FALSE)
                return(NULL)
            indEndo = Indexes[Indexes < ParentPack$ExoStartIndex]
        }
    }
    message("\t\t Endogenous-Exogenous=", paste(Indexes ,collapse = ","))

    res = list()
    h = 0
   # tryCatch(
   #     {
            for (W in ParentPack$SimulationData)
            {
                h = h + 1
                z = cbind2(W$TrainingSampleTarget, W$TrainingSampleOther[,indEndo, drop = FALSE])

                print("*****")
                print(ParentPack$ExoStartIndex)
                print(paste(Indexes))
                indExo = Indexes[Indexes >= ParentPack$ExoStartIndex]
                print(paste(indEndo))
                print("*****")
                if (length(indExo) > 0)
                {
                    print("with exo")
                    x = W$TrainingSampleOther[,indExo, drop = FALSE]
                    xf = W$NewXRegValidation[,indExo, drop = FALSE]
                    print("with exo")
                    print(paste("cols endo=", NCOL(z)))
                    print(paste("cols exo=", NCOL(x)))
                    model = MTS::VARX(zt = z, p = P, xt = x, include.mean = Intercept)
                    pre = MTS::VARXpred(model, hstep = h, newxt = xf)
                }
                else
                {
                    print("without exo")
                    model = MTS::VAR(x = z, p = P, include.mean = Intercept)
                    pre = MTS::VARpred(model, h = h)
                }

                err = pre$pred - W$ValidationSample
                se = pre$se
                # if (anyNA(se) || anyNA(err))
                #     stop("NA found")
                res[[length(res) + 1]] = list(model,err,se)

            }
            return(res)
 #       },
 #       error = function(err)
 #       {
 #           ## any error in any evaluation will result in discarding the model
 #           print(err)
 #           return(list())
 #       }
 #   )


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
