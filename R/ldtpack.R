

#----------------------------------------
#' 
#' 
#' @field the main ldt class
#' @field a unique string that represents the package
#' @field a list of 'evaluation' objects that this package can provide. Not all packages can report (e.g.) AIC
#' @field the size of models set
#' @field number of estimated and evaluated models
#' @field number of failed estimation/evaluations (at the end, CountValid + CountFailed must be equal to RequiredCount)
#' @field a list of ldtpacksub
#' @field just in case you wanted to check all the models
#' 
#' 
#----------------------------------------
ldtpack <- setRefClass("ldtpack", 
                       fields = list(
                           ParentLDT = 'ldt',
                           ID = 'character',
                           Description = 'character',
                           SupportedEvaluations = 'list',
                           Processes = 'list',
                           AllModels = 'list' 
                       ))

ldtpack$methods(show = function(){
    print(toString())
})

ldtpack$methods(toString = function(){
    stop('ldtpack: override this function.')
})

# (CountRrequired,CountValid,CountFailed)
ldtpack$methods(GetCounts = function(){
    cr = 0
    cv = 0
    cf = 0
    for (p in Processes)
    {
        cr = cr + p$CountRequired
        cv = cv + p$CountValid
        cf = cf + p$CountFailed
    }
    return(c(cr,cv,cf))
}) 

ldtpack$methods(setSupportedEvaluations = function(namelist)
{
    se = list()
    i = 0
    for (s in namelist)
    {
        exist = FALSE
        for (e in ParentLDT$ScoringRules)
        {
            if (e$Name == s)
            {       
                i = i + 1
                se[[i]] = e
                exist = TRUE
                break
            } 
        }
        if (exist == FALSE)
            stop(paste("the evaluation is not supported. name = ", s))
    }
    SupportedEvaluations <<- se  
}) 

 






