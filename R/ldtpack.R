

#----------------------------------------
#' @title Represents a specific forecast package
#'
#' @field ParentLDT The main ldt class
#' @field ID A unique string that represents the related package
#' @field Description A description for the related package
#' @field SupportedScoringRules A list of 'evaluation' objects that this package can provide. Not all packages can report (e.g.) AIC
#' @field Processes A list of ldtpacksub
#' @field Results A list that contains the results for each member of SupportedScoringRules.
#' @field AllModels Just in case you wanted to check all the models
#'
#----------------------------------------
ldtpack <- setRefClass("ldtpack",
                       fields = list(
                           ParentLDT = 'ldt',
                           ID = 'character',
                           Description = 'character',
                           SupportedScoringRules = 'list',
                           Processes = 'list',
                           Results = 'list',
                           AllModels = 'list'
                       ))

ldtpack$methods(initialize = function()
{
    if (length(SupportedScoringRules) > 0)
    {
        Results <<- list()
        for (i in (1:length(SupportedScoringRules)))
        {
            f = list()
            for (h in (1:ParentLDT$MaxHorizon))
            {
                f[[h]] = list(NA, NULL)
            }
            Results[[i]] <<- f
        }
    }
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

ldtpack$methods(setSupportedScoringRules = function(namelist)
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
    SupportedScoringRules <<- se
})

#----------------------------------------
#' @title consider a new score
#'
#' @name ldtpack_considernew
#'
#' @param model the model based on which the scores are generated
#' @param i the index of evalation in SupportedScoringRules based on which the scores are generated
#' @param scores the generated scores
#'
#'
#----------------------------------------
ldtpack$methods(considernew = function(model, i, scores)
{
    eval = SupportedScoringRules[[i]]
    for (h in (1:ParentLDT$MaxHorizon))
    {
        cs = Results[[i]][[h]][[1]]
        s = scores[[h]]
        if (is.na(cs) ||
            (eval$IsPositivelyOriented && s > cs) ||
            (eval$IsPositivelyOriented == FALSE && s < cs))
        {
            #print(paste("Comparing: ", cs , " (current) and ", s,sep = "" ))
            Results[[i]][[h]] <<- list(s, model)
        }
    }
})






