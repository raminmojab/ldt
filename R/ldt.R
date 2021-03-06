#----------------------------------------
#----------------------------------------
#' ldt: A package for probabilistic forecasting
#'
#' ldt tries to summerize and compare the results of all other packages that provides a means
#' of time-series forecasting, in a probabilistic approach
#'
#'
#' @author
#' Ramin Mojab \email{ron3403272@gmail.com}
#'
#' @name ldt
#' @docType package
NULL
#----------------------------------------
#----------------------------------------


#----------------------------------------
#' @title Main Class of the Package
#' @docType class
#' @description This is a reference class. Generate an instance with \code{$new} method, change values if needed, and execute \code{\link{ldt_Run}} method to start the estimation, forecast, and evaluation process.
#'
#' @examples
#' ##load("data/endodata_rand.rda")
#' ##load("data/exodata_rand.rda")
#' ##a=ldt$new(endodata,exodata, maxsize = 3)
#' ##a$Run()
#'
#' @field EndoData A 'ts' that contains the data for the endogenous variables. The target variable (the one we are trying to forecast) is the first variable in this series.
#' @field ExoData A 'matrix' that contains the data for the exogenous variable. note that the frequency and start date will be fetched from EndoData.
#' @field MaxHorizon A 'positive integer'. Forecasts will be provided up to this number.  Default is 1.
#' @field MaxLag A 'positive integer' that indicates the maximum lag of models (including AR and MA parts). Default is 1.
#' @field MaxSize A 'positive integer' that indicates the maximum size of multiple/multivariate models. Default is 1. For example, in arima case, a value of 2 means that models might have zero or one exogenous variables.
#' @field ScoringRules A list of 'scoringrule' objects. Default is a list of AIC, BIC, MAE, MSE, LSR, LnSR, QSR, HSR and CRPSR (for all score rules, normality is assumed).
#' @field SimulationCount A 'positive integer' that indicates the number of out-of-sample simulations; i.e., how many times we should seperate the data into training and evaluation samples and test the forecast accuracy. Default is 1.
#' @field ModelSets A list of 'modelset' objects
#' @include scoringrule.R utils.R
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
                       ModelSets = 'list'))

#----------------------------------------
#' @title Constructor for \code{ldt} Class
#' @name ldt_initialize
#' @description initialize ldt reference class by using ldt$new(...).
#' @param endodata sets \code{EndoData} field of the class.
#' @param exodata sets \code{ExoData} field of the class.
#' @param maxhorizon sets \code{MaxHorizon} field of the class.
#' @param maxlag sets \code{MaxLag} field of the class.
#' @param maxsize sets \code{MaxSize} field of the class.
#' @param simulationcount sets \code{SimulationCount} field of the class.
#'
#----------------------------------------
NULL
ldt$methods(initialize = function(endodata = ts(), exodata = matrix(numeric(0), 0,0) , maxhorizon = 1,
                                  maxlag = 1, maxsize = 1, simulationcount = 1)
{
    MaxHorizon <<- maxhorizon
    MaxLag <<- maxlag
    MaxSize <<- maxsize
    SimulationCount <<- simulationcount
    EndoData <<- endodata
    ExoData <<- exodata

    if (length(endodata) == 1)
        return() # this is probably a call while building the package. An error will be raised in Run method.


    #set ScoringRules:
    evallist = list()
    evallist[[1]] = scoringrule$new("MAE", "Mean Absolute Error", FALSE,
                                   function(err,se)
                                   {return(abs(err))})

    evallist[[2]] = scoringrule$new("MSE", "Mean Square Error", FALSE,
                                   function(err,se)
                                   {return(err ^ 2)})

    evallist[[3]] = scoringrule$new("LSR", "Linear Score Rule", TRUE,
                                   function(err,se) # see dnorm help for using vector
                                   {return(dnorm(err, mean = 0, sd = se))})

    evallist[[4]] = scoringrule$new("LnSR", "Logarithmic Score Rule", TRUE,
                                   function(err,se)
                                   {return(dnorm(err, mean = 0, sd = se, log = TRUE))})

    evallist[[5]] = scoringrule$new("QSR", "Quadratic Score Rule", TRUE,
                                   function(err,se)
                                   {return(2 * dnorm(err, mean = 0, sd = se) - 0.5 / (se * 1.77245385090552))})

    evallist[[6]] = scoringrule$new("HSR", "Hyvarinen Score Rule", TRUE,
                                   function(err,se)
                                   {return((1 / (se ^ 2)) * (2 - (err / se) ^ 2))})

    evallist[[7]] = scoringrule$new("CRPSR", "Continuous Ranked Probability Score Rule", TRUE,
                                   function(err,se) {
                                       z = err / se;
                                       phi = dnorm(z, mean = 0, sd = 1)
                                       PHI = pnorm(z, mean = 0, sd = 1)
                                       return(se * (0.564189583547756 + z - 2 * phi - 2 * z * PHI))
                                   })
    ScoringRules <<- evallist





    # set ModelSets
    #ModelSets[[1]] <<- arimamodelset$new(.self)
    ModelSets[[1]] <<- svarxmodelset$new(.self)

})




#----------------------------------------
#' @title Starts the Forecasting Process
#' @name ldt_Run
#' @description Use this function to start estimation/forecast/evaluation process.
#'
#' @note I separate the run block from initialize, because it is reference class and it is important to give the user a chance to change the fields. Thats why any validation must be provided here.
#----------------------------------------
NULL
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
            stop("ldt: there are not enough (out-of-sample) observations in the exogenous data. provide the data or lower MaxHorizon")

    }

    show()

    count = 0
    for (p in ModelSets)
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





#----------------------------------------
#' @title Summerizes this class
#' @name ldt_show
#'
#----------------------------------------
NULL
ldt$methods(show = function(){
    print("This is a LDT main class: ", quote=FALSE)
    strt = start(EndoData)
    print(paste("EndoData: ", " (", NROW(EndoData), "x" , NCOL(EndoData) ,")" , ", start=", strt[[1]],":", strt[[2]], ", frequency=", frequency(EndoData) , sep = ""), quote=FALSE)
    print(paste("ExoData:  ", " (", NROW(ExoData), "x" , NCOL(ExoData) ,")", sep = ""), quote=FALSE)
    print(paste("MaxHorizon: ", MaxHorizon), quote=FALSE)
    print(paste("MaxLag: ", MaxLag), quote=FALSE)
    print(paste("MaxSize: ", MaxSize), quote=FALSE)
    print(paste("SimulationCount: ", SimulationCount), quote=FALSE)

    scr = list()
    for (i in ScoringRules)
    {
        scr[[length(scr) + 1]] = i$summerize()
    }
    print("ScoringRules:", quote = FALSE)
    print(paste("    ",scr), quote = FALSE)

    pks = list()
    for (i in ModelSets)
    {
        pks[[length(pks) + 1]] = i$summerize()
    }
    print("ModelSets:", quote = FALSE)
    print(paste("    ",pks), quote = FALSE)


})




