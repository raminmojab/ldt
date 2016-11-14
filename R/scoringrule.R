
#----------------------------------------
#' @title Represents a scoring rule
#'
#' @description A \code{scoringrule} measures the accuracy of probabilistic forecasts in an out-of-sample simulation practice.
#'
#' @field ID A short string that describes this \code{scoringrule}.
#' @field Description A string that provides other information about this \code{scoringrule}.
#' @field IsPositivelyOriented A boolean indicates wether higher values are better.
#' @field Score The scoring function. It should get two arguments: vector of forecast errors and vector of forecast standard errors and it should return a vector of the same length
#'
#'
#' @export
#----------------------------------------
scoringrule <- setRefClass("scoringrule",
                              fields = list(ID = 'character',
                                            Description = 'character',
                                            IsPositivelyOriented = 'logical',
                                            Score = 'function'))

#----------------------------------------
#' @title The constructor of \code{scoringrule} class
#'
#' @name scoringrule_initialize
#'
#' @param id sets ID field of the class
#' @param description sets Description field of the class
#' @param ispositiveoriented sets Ispositiveoriented field of the class
#' @param score sets Score field of the class
#'
#----------------------------------------
NULL
scoringrule$methods(initialize = function(id, description,ispositiveoriented,score)
{
    if (missing(id) == FALSE)
    {
        ID <<- id
        Description <<- description
        IsPositivelyOriented <<- ispositiveoriented
        Score <<- score
    }
})

scoringrule$methods(show = function(){
    print("This is a scoringrule class: ", quote = FALSE)
    print(summerize())
})

scoringrule$methods(summerize = function(){
    pos = "positive oriented"
    if (IsPositivelyOriented == FALSE)
        pos="negative oriented"
    return(paste(ID, ":", Description, " (" ,pos, ")",sep = ""))
    })

