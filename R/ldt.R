#----------------------------------------
#'
#' @field a ts object (time series) that contains the data for the endogenous variable. The target variable (the one we are trying to forecast) is the first variable in this series.
#' @field a ts object (time series) that contains the data for the exogenous variable
#' @field a positive integer that indicates the maximum of the forecast horizons. Forecasts will be provided up to this horizon. Default is 1
#' @field a positive integer that indicates the maximum lag of models (including AR and MA parts). Default is 1
#' @field a positive integer that indicates the maximum size of multiple/multivariate models. Default is 1. For example, in arima case, a value of 2 means that models might have zero or one exogenous variables.
#' @field a list of evaluation objects. Default is a list of AIC, BIC, MAE, MSE, LSR, LnSR, QSR, HSR and CRPSR (for all score rules, normality is assumed)
#' @field a positive integer that indicates the number of Out-of-sample evaluations. i.e., how many times we should seperate the data into training and evaluation samples and test the forecast accuracy of different models. The default is 1
#' @field a list of ldtpack objects
#' @field a list that contains the results for each member of ScoringRules. The i-th element is the results corresponding the i-th member of ScoringRules.
#' 
#' 
#' @example 
#' 
#' 
#' @export
#----------------------------------------
ldt <- setRefClass("ldt", 
                   fields = list
                   (
                   EndoData = 'ts',
                   ExoData = 'ts',
                   MaxHorizon = "numeric",
                   MaxLag = 'numeric',
                   MaxSize = 'numeric', 
                   ScoringRules = 'list',
                   SimulationCount = 'numeric',
                   Packs = 'list',
                   Results = 'list'
                   ),
                   methods = list( 
                       initialize = function() 
                       {  
                           MaxHorizon <<- 1
                           MaxLag <<- 1
                           MaxSize <<- 1
                           SimulationCount <<- 1
                           
                           # set default evaluations
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

                           Results <<- list()
                           for (i in (1:length(ScoringRules)))
                           {
                               f = list()
                               for (h in (1:MaxHorizon))
                               { 
                                    f[[h]] = list(NA, NULL)
                               }
                               Results[[i]] <<- f
                           }
                           
                       })) 

ldt$methods(check = function()
{
    
    ## make sure Endo and Exo start at the same time:
    
    
    #n = dim(as.matrix(EndoData))[1]
    #n0 = dim(ExoData)[1] 
    #if (n0-n < MaxHorizon)
    #    stop("Not enough observation is provided in ExoData.")
    
    
    ## Adjust MaxSize to be the minimum of the given value and the number of Endo and Exo Data
    # note that the check is different for Uni variate models such as ARIMA and VAR
    #      the difference is related to the number of endogenous variables (they are useless in univariate models)
    
    
    ## if there is exo and maximum size is 1, there is no place for other exogenous variables. give a warning
})

#----------------------------------------
#'
#' @name ldt_setData 
#' 
#' @param determines the type of data (Endogenous or Exogenous)
#' @param an object of class 'ts' to be used as the data.
#' @param a character representation of the data. If data_ts is not supplied and this is, then data are read from this text via read.table fuction. Not that the string must have a specific structure; The first row must contain the names of the variables (headers) and the cell at [2,1] (first column of the second row) must contain a date string (such as "1390m3" or "1400q1" or "3000". see dates.R file)
#' @param a relative path to the data file. If data_ts and data_string are not supplied and this is, then data are read from this text via read.table fuction
#' @param (of type data.frame) if data_ts, data_string or data_file are not supplied, you should provide this and the other two arguments (i.e., data_matrix, first_date_chars)
#' @param (of type data.matrix) the values of different variables
#' @param (of type characters) the first date representation. Use a string representation of an integer for yearly data, e.g., "2000". Use "q" and "m" to distringuish frequency, e.g., 2000q1 is the first quarter of year 2000 and/or 2000m8 is the 8th month of year 2000 
#' @param set argument for read.table function
#' @param encoding for read.table function 
#' @param numerals for read.table function
#'  
#----------------------------------------
ldt$methods(setData = function(is_endo=TRUE, data_ts, data_string, data_file, headers, data_matrix, first_date, sep = "\t", encoding = "unknown", numerals = c("allow.loss", "warn.loss", "no.loss"))
{
    if (missing(data_ts))
    { 
        if (missing(headers) || missing(data_matrix) || missing(first_date))
        {  
            headers = read.table(file = data_file, text = data_string, sep = sep, header = FALSE, nrows = 1, row.names = 1, stringsAsFactors = FALSE, encoding = encoding, numerals = numerals)
            secondrow = read.table(file = data_file, text = data_string, sep = sep, header = FALSE, skip = 1, nrows = 1, row.names = NULL, stringsAsFactors = FALSE, encoding = encoding, numerals = numerals)
            first_date = toString(secondrow[1, 1])
            datamats = data.matrix(read.table(file = data_file, text = data_string, sep = sep, header = FALSE, skip = 1, row.names = 1, stringsAsFactors = FALSE, encoding = encoding, numerals = numerals), FALSE)
        }
        else
        {  
            if (missing(headers) == FALSE || missing(data_matrix) == FALSE || missing(first_date) == FALSE)
                stop("One of required arguments are missing. Please provide headers, data_matrix and first_date")
        }
        firstdateinfo = Convert2RoNDate(first_date)
        data_ts = ts(datamats, frequency = firstdateinfo@Frequency, start = c(firstdateinfo@First, firstdateinfo@Second), names = headers)
    } 
    
    if (is_endo)
        EndoData <<- data_ts
    else
        ExoData <<- data_ts
    

    #check()
})

#----------------------------------------
#'
#' @name ldt_setFromXML 
#' 
#' @param The XML file with data
#----------------------------------------
ldt$methods(setFromXML = function(xmlfile)
{
    stop("Not implemented. To implement, see Project data in 2016.18.9 version.")
    if (missing(xmlfile))
        stop("no input argument is given.")
 
})



#----------------------------------------
#'
#' @name ldt_setPacks 
#' 
#' 
#' 
#----------------------------------------
ldt$methods(setPacks = function()
{
    Packs[[1]] <<- ldtpackarima$new(.self)
    
})

#----------------------------------------
#' 
#'  
#'  
#----------------------------------------
ldt$methods(Run = function()
{
    count = 0
    for (p in Packs)
    {
        message(p$toString())
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
#'
#' @name ldt_considernew 
#' 
#' @field the model based on which the scores are generated
#' @field the evalation in ScoringRules based on which the scores are generated
#' @field the generated scores
#' 
#' 
#----------------------------------------
ldt$methods(considernew = function(model, eval, scores)
{ 
    
    i = 0
    for (e in ScoringRules)
    { 
        i = i + 1
        if (identical(e,eval))
        { 
            for (h in (1:MaxHorizon))
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
            return()
        }
    }
    stop("Could not find the given scoring rule.") 
})





