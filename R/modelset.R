

#----------------------------------------
#' @title Represents a specific model set
#'
#' @description A model set is a collection of different statistical models that share one or more properties. For example, {AR(p) for p=1...5} can be considered as a set of autoregressive models.
#'
#' @field ParentLDT The main ldt class
#' @field ID A unique string that represents the related package
#' @field Description A description for the related package
#' @field SupportedScoringRules A list of \code{scoringrule}s that the models in this \code{modelset} support. E.g., not all models can report AIC.
#' @field Subs A list of \code{modelsetsub}.
#' @field Results A list that contains the results for each member of SupportedScoringRules. \code{Results[[i]][[h]]} represents the best model with respect to the i-th rule in \code{SupportedScoringRules} and for horizon h.
#' @field AllModels In some cases you might want to save all the models.
#'
#' @include ldt.R scoringrule.R utils.R
#' @export
#----------------------------------------
modelset <- setRefClass("modelset",
                       fields = list(
                           ParentLDT = 'ldt',
                           ID = 'character',
                           Description = 'character',
                           SupportedScoringRules = 'list',
                           Subs = 'list',
                           Results = 'list',
                           AllModels = 'list'
                       ))

#----------------------------------------
#' @title The constructor of \code{modelset} class
#' @name modelset_initialize
#' @details The \code{Results} field is prepared in this constructor. Set the \code{SupportedScoringRules} field (and other fields) before executing \code{callSuper()} command in the inherited classes.
#'
#----------------------------------------
NULL
modelset$methods(initialize = function()
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


#----------------------------------------
#' @title Reports the current state of the process
#' @name modelset_GetCounts
#' @description It reports the number of required, valid and invalid estimations based on information provided in the \code{Subs} field.
#'
#' @return an array with three elements: [[1]] CountRrequired, [[2]] CountValid, [[3]] CountFailed
#'
#----------------------------------------
NULL
modelset$methods(GetCounts = function(){
    cr = 0
    cv = 0
    cf = 0
    for (p in Subs)
    {
        cr = cr + p$CountRequired
        cv = cv + p$CountValid
        cf = cf + p$CountFailed
    }
    return(c(cr,cv,cf))
})

modelset$methods(summerize = function(){
    cs = paste(c("Required","Valid","Failed"),GetCounts(),sep = "=",collapse = ",")
    return(paste(ID, "[", Description, "], ", toString(cs), sep = ""))
})

#----------------------------------------
#' @title Sets \code{SupportedScoringRules} field
#' @name modelset_setSupportedScoringRules
#' @description Sets \code{SupportedScoringRules} field based on a given list of IDs (strings). Note that a \code{scoringrule} with such an \code{ID} must be presented in \code{ParentLDT$ScoringRules}, or an error will be raised.
#'
#' @return None
#'
#' @field namelist A list of strings that each represent an \code{ID} of a \code{scoringrule} in \code{ParentLDT$ScoringRules}.
#'
#----------------------------------------
NULL
modelset$methods(setSupportedScoringRules = function(namelist)
{
    se = list()
    i = 0
    for (s in namelist)
    {
        exist = FALSE
        for (e in ParentLDT$ScoringRules)
        {
            if (e$ID == s)
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
#' @title Considers a new estimated model
#' @description Each model must generate a score for each forecast horizons. This method will compare the generated scores with the best ones and determines whether to discard this model or keep it.
#' @name modelset_considernew
#'
#' @param model The model based on which the scores are generated.
#' @param i The index of \code{scoringrule} in \code{SupportedScoringRules} field, based on which the scores are generated.
#' @param scores The generated scores as a vector. i-th element is for the i-th horizon.
#'
#----------------------------------------
NULL
modelset$methods(considernew = function(model, i, scores)
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






