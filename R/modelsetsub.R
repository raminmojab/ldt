

#----------------------------------------
#' @title A subset of a \code{modelset}
#'
#' @description It is sometimes practical to partition a \code{modelset} into some other subsets.
#'
#' @field ParentSet The corresponding \code{modelset}.
#' @field CountRequired A integer that represents the size of this subset.
#' @field CountValid A integer that represents the current number of successful estimations/forecasts. By successful I mean no singular matrix inversion or no other types of errors.
#' @field CountFailed A integer that represents the current number of failed estimations/forecasts. Some of these failures are due to the provided data (user) and some are due to the implemented package.
#'
#' @include ldt.R scoringrule.R utils.R modelset.R
#' @export
#----------------------------------------
modelsetsub <- setRefClass("modelsetsub",
                          fields = list(
                              ParentSet = 'modelset',
                              CountRequired = 'numeric',
                              CountValid = 'numeric',
                              CountFailed = 'numeric'
                          ))



#----------------------------------------
#' @title Handles moving from one model to the next
#' @name modelset_GetNextModel
#' @description (You should overwrite this method) Models will be estimated and evaluated one after the other. This method should provide the required algorithm.
#' @field isfirst Determines whether it is the first model in this \code{modelsetsub}.
#' @return  NULL if there is no more move, An empty list if any error occured, A list of 1.Model 2. Forecast error and 3. forecast standard error otherwise.
#----------------------------------------
NULL
modelsetsub$methods(GetNextModel = function(isfirst){
    stop('modelsetsub: override this function.')
})

#----------------------------------------
#' @title Considers a new estimated model
#' @name modelset_consider
#' @details This method converts the forecast errors and the forecast standard errors to a score, based on \code{scoringrule$Score} field. Then, it sends the provided scores to \code{modelset$considernew} for comparison purposes.
#' @field modelerrorse A vector with three elements: [[1]] The estimated model, [[2]] The forecast error, [[3]] The forecast standard error. If model has failed to forecast, it should be an empty vector (i.e., length(modelerrorse) == 0).
#' @return FALSE if \code{modelerrorse} is empty (a failure), TRUE, otherwise.
#----------------------------------------
NULL
modelsetsub$methods(consider = function(modelerrorse)
{
    if (length(modelerrorse) == 0)
    {
        CountFailed <<- CountFailed + 1
        return(FALSE)
    }
    ParentSet$AllModels[[length(ParentSet$AllModels) + 1 ]] <<- modelerrorse[[1]]

    CountValid <<- CountValid + 1
    H = ParentSet$ParentLDT$MaxHorizon
   i = 0
     for (e in ParentSet$SupportedScoringRules)
    {
         i = i + 1
        count = 0
        v = vector(mode = "numeric", length = length(H))
        for (l in modelerrorse)
        {
            v = v + e$Score(l[[2]], l[[3]])
        }
        v = v / length(modelerrorse)

        ParentSet$considernew(modelerrorse[[1]][[1]],i,v)
    }
    return(TRUE)
})


#----------------------------------------
#' @title Starts the forecasting process in this \code{modelsetsub}
#' @description This method provides a loop and estimates one model after the other.
#' @name modelsetsub_Run
#' @return None
#'
#----------------------------------------
NULL
modelsetsub$methods(Run = function()
{
    message("\t...SUB:")
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




