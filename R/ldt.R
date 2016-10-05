#----------------------------------------
#'
#' @field a ts that contains the data for the endogenous variable. The target variable (the one we are trying to forecast) is the first variable in this series.
#' @field a matrix that contains the data for the exogenous variable. note that the frequency and start date will be fetched from EndoData
#' @field a positive integer that indicates the maximum of the forecast horizons. Forecasts will be provided up to this horizon. Default is 1
#' @field a positive integer that indicates the maximum lag of models (including AR and MA parts). Default is 1
#' @field a positive integer that indicates the maximum size of multiple/multivariate models. Default is 1. For example, in arima case, a value of 2 means that models might have zero or one exogenous variables.
#' @field a list of evaluation objects. Default is a list of AIC, BIC, MAE, MSE, LSR, LnSR, QSR, HSR and CRPSR (for all score rules, normality is assumed)
#' @field a positive integer that indicates the number of Out-of-sample evaluations. i.e., how many times we should seperate the data into training and evaluation samples and test the forecast accuracy of different models. The default is 1
#' @field a list of ldtpack objects
#'
#' @example
#' load("data/endodata_rand.rda")
#' load("data/exodata_rand.rda")
#' a=ldt$new(endodata,exodata, maxsize = 3)
#' a$Run()
#'
#' @export
#----------------------------------------
ldt <- setRefClass("ldt",
                   fields = list(
                       EndoData = 'ts',
                       ExoData = 'matrix',
                       MaxHorizon = "numeric",
                       MaxLag = 'numeric',
                       MaxSize = 'numeric',
                       ScoringRules = 'list',
                       SimulationCount = 'numeric',
                       Packs = 'list'))


ldt$methods(initialize = function(endodata = ts(), exodata = matrix(numeric(0), 0,0) , maxhorizon = 1,
                                  maxlag = 1, maxsize = 1, simulationcount = 1, ...)
{
    MaxHorizon <<- maxhorizon
    MaxLag <<- maxlag
    MaxSize <<- maxsize
    SimulationCount <<- simulationcount
    EndoData <<- endodata
    ExoData <<- exodata

    if (length(endodata) == 1)
        return() # this is probably a call while building the package. An error will be raised in Run method.


    #set evaluations:
    evallist = list()

    evallist[[1]] = evaluation$new("MAE", "Mean Absolute Error", FALSE,
                                   function(err,se)
                                   {return(abs(err))})

    evallist[[2]] = evaluation$new("MSE", "Mean Square Error", FALSE,
                                   function(err,se)
                                   {return(err ^ 2)})

    evallist[[3]] = evaluation$new("LSR", "Linear Score Rule", TRUE,
                                   function(err,se) # see dnorm help for using vector
                                   {return(dnorm(err, mean = 0, sd = se))})

    evallist[[4]] = evaluation$new("LnSR", "Logarithmic Score Rule", TRUE,
                                   function(err,se)
                                   {return(dnorm(err, mean = 0, sd = se, log = TRUE))})

    evallist[[5]] = evaluation$new("QSR", "Quadratic Score Rule", TRUE,
                                   function(err,se)
                                   {return(2 * dnorm(err, mean = 0, sd = se) - 0.5 / (se * 1.77245385090552))})

    evallist[[6]] = evaluation$new("HSR", "Hyvarinen Score Rule", TRUE,
                                   function(err,se)
                                   {return((1 / (se ^ 2)) * (2 - (err / se) ^ 2))})

    evallist[[7]] = evaluation$new("CRPSR", "Continuous Ranked Probability Score Rule", TRUE,
                                   function(err,se) {
                                       z = err / se;
                                       phi = dnorm(z, mean = 0, sd = 1)
                                       PHI = pnorm(z, mean = 0, sd = 1)
                                       return(se * (0.564189583547756 + z - 2 * phi - 2 * z * PHI))
                                   })
    ScoringRules <<- evallist





    # set packs
    Packs[[1]] <<- ldtpackarima$new(.self)


})


# I separate the run block from initialize, because it is reference class and it is important to give
#  the user a chance to change the fields. Thats why any validation must be provided here.
ldt$methods(Run = function()
{
    if (length(EndoData) == 1)
        stop("ldt: a valid 'EndoData' must be provided.")

    if (length(ExoData) == 0)
    {
        # make sure the forecast horizon and exogenous variables are consistent
        n = NROW(EndoData)
        n0 = NROW(ExoData)
        if (n0 - n < MaxHorizon)
            stop("ldt: there are not enough (out-of-sample) observations in the exogenous data. provide the data or lower the maximum horizon.")

    }

    # summerize?


    count = 0
    for (p in Packs)
    {
        message(p$show())
        for (r in p$Processes)
        {
            r$Run()
        }
        cs = p$GetCounts()
        if (cs[[1]] != (cs[[2]] + cs[[3]]))
            warning("CountRequired != CountValid + CountFailed.")

        message("\tFailed Estimations/Forecasts (%): ", cs[[3]]/cs[[1]] * 100)
    }
})









