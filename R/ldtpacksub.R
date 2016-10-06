ldtpacksub <- setRefClass("ldtpacksub",
                          fields = list(
                              ParentPack = 'ldtpack',
                              CountRequired = 'numeric',
                              CountValid = 'numeric',
                              CountFailed = 'numeric',
                              CountWarning = 'numeric'
                          ))

ldtpacksub$methods(show = function(){
    stop('ldtpacksub: override this function.')
})


ldtpacksub$methods(GetNextModel = function(isfirst){
    stop('ldtpacksub: override this function.')
})

ldtpacksub$methods(consider = function(modelerrorse)
{
    if (length(modelerrorse) == 0)
    {
        CountFailed <<- CountFailed + 1
        return(FALSE)
    }
    ParentPack$AllModels[[length(ParentPack$AllModels) + 1 ]] <<- modelerrorse[[1]]

    CountValid <<- CountValid + 1
    H = ParentPack$ParentLDT$MaxHorizon
   i = 0
     for (e in ParentPack$SupportedScoringRules)
    {
         i = i + 1
        count = 0
        v = vector(mode = "numeric", length = length(H))
        for (l in modelerrorse)
        {
            v = v + e$Score(l[[2]], l[[3]])
        }
        v = v / length(modelerrorse)

        ParentPack$considernew(modelerrorse[[1]][[1]],i,v)
    }
    return(TRUE)
})

ldtpacksub$methods(Run = function()
{
    message("\t...LDTPACKSUB:")
    a = GetNextModel(TRUE)
    consider(a)

    while (TRUE)
    {
        b = GetNextModel(FALSE)
        if (is.null(b))
            break
        consider(b)
    }

    if (CountRequired != (CountValid + CountFailed))
        warning("CountRequired != CountValid + CountFailed.")

    message("\t\tFailed Estimations/Forecasts (%): ", CountFailed/CountRequired * 100)
    message("")
})




