

#----------------------------------------
#' @title A subset of a \code{svarxmodelset}
#'
#' @description A set of Stationary VARX models defined in \code{svarxmodelset} class. It contains \code{modelsetsub}.
#'
#' @field P The number of lags of the model.
#' @field Size The number of different endogenous and exogenous variables in the model.
#' @field Intercept Determines whether this model has an intercept.
#' @field Indexes P Determines different combinations of endogenous and exogenous variables. A vector of (for example) [2 3] means that the second and third variables in \code{svarxmodelset$EndoExoData} are in this model. This vector will change in \code{svarxmodelsetsub$GetNextModel}.
#'
#' @include ldt.R scoringrule.R utils.R modelsetsub.R
#' @export
#----------------------------------------
svarxmodelsetsub <- setRefClass("svarxmodelsetsub",
                               fields = list(
                                   P = 'numeric',
                                   Size = 'numeric',
                                   Intercept = 'logical',
                                   Indexes = 'vector'
                               ),
                               contains = "modelsetsub")

#----------------------------------------
#' @title The constructor of \code{svarxmodelsetsub} class
#' @name svarxmodelsetsub_initialize
#' @details It will generate the required fields in this \code{svarxmodelsetsub} and its parent \code{modelsetsub}.
#' @field parentsvarx The corresponding \code{arimamodelset} class.
#' @field p sets \code{p} field of the class.
#' @field size sets \code{size} field of the class.
#' @field intercept sets \code{Intercept} field of the class.
#----------------------------------------
NULL
svarxmodelsetsub$methods(initialize = function(parentsvarx,p,size,intercept){
    if (missing(parentsvarx) == FALSE)
    {
        ParentSet <<- parentsvarx
        CountValid <<- 0
        CountFailed <<- 0

        P <<- p
        Size <<- size
        Intercept <<- intercept

        CountRequired <<- choose(ncol(parentsvarx$EndoExoData), size - 1)


    }
})

#----------------------------------------
#' @title see \code{modelsetsub_GetNextModel} description.
#' @name svarxmodelsetsub_GetNextModel
#' @description see \code{modelsetsub_GetNextModel} description.
#' @field isfirst see \code{modelsetsub_GetNextModel} description.
#' @return see \code{modelsetsub_GetNextModel} description.
#----------------------------------------
NULL
svarxmodelsetsub$methods(GetNextModel = function(isfirst){
    if (isfirst)
    {
        message("\t\t",show())
        message("\t\t Endogenous-Exogenous=NULL")
        Indexes <<- as.vector(1:(Size - 1))
        indEndo = Indexes[Indexes < ParentSet$ExoStartIndex]
    }
    else
    {
        indEndo = numeric(0)
        while (length(indEndo) == 0)
        {
            move = .self$movetonext()
            if (move == FALSE)
                return(NULL)
            indEndo = Indexes[Indexes < ParentSet$ExoStartIndex]
        }
    }
    message("\t\t Endogenous-Exogenous=", paste(Indexes ,collapse = ","))

    res = list()
    h = 0
   # tryCatch(
   #     {
            for (W in ParentSet$SimulationData)
            {
                h = h + 1
                z = cbind2(W$TrainingSampleTarget, W$TrainingSampleOther[,indEndo, drop = FALSE])

                if (length(indExo) > 0)
                {
                    x = W$TrainingSampleOther[,indExo, drop = FALSE]
                    xf = W$NewXRegValidation[,indExo, drop = FALSE]
                }

                model = ar(z, order.max = P, xt = x, include.mean = Intercept, aic = FALSE, method = "mle")
                pre = predict(model, hstep = h, newxt = xf)

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

#----------------------------------------
#' @title Changes \code{Indexes} field to reach the next St. VARX model
#' @name svarxmodelsetsub_movetonext
#' @return FALSE if no more move is possible. True, if it moved to the next model.
#----------------------------------------
NULL
svarxmodelsetsub$methods(movetonext = function(){

    if (length(ParentSet$EndoExoData) == 0 || ParentSet$ParentLDT$MaxSize == 1)
        return(FALSE)

    maxValue = ncol(ParentSet$EndoExoData)
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


svarxmodelsetsub$methods(show = function(){
    return(paste("VARX(", P, ") size=", Size,
                 "; required model count=", CountRequired, sep = ""))
})
