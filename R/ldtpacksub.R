ldtpacksub <- setRefClass("ldtpacksub", 
                          fields = list(  
                              ParentPack = 'ldtpack',
                              CountRequired = 'numeric',
                              CountValid = 'numeric',
                              CountFailed = 'numeric'
                          ))

ldtpacksub$methods(show = function(){
    print(toString())
})

ldtpacksub$methods(toString = function(){
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
    for (e in ParentPack$SupportedEvaluations)
    { 
        count = 0
        v = vector(mode = "numeric", length = length(H))
        for (l in modelerrorse) 
        {  
            v = v + e$Score(l[[2]], l[[3]])
        }
        v = v / length(modelerrorse)
        
        ParentPack$ParentLDT$considernew(modelerrorse[[1]][[1]],e,v)
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




