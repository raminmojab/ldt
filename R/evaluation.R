
#----------------------------------------
#' @title Represents a scoring rule
#'
#' @field Name Abbreviation of the rule
#' @field Description Full name and any other description about the rule
#' @field IsPositivelyOriented A boolean indicates wether higher is better
#' @field Score The scoring function. It should get two arguments: vector of forecast errors and vector of forecast standard errors and it should return a vector of the same length
#'
#'
#' @export
#'
#----------------------------------------
evaluation <- setRefClass("evaluation",
                              fields = list(Name = 'character',
                                            Description = 'character',
                                            IsPositivelyOriented = 'logical',
                                            Score = 'function'),
                              methods = list(
                                  initialize = function(name, description,ispositiveoriented,score)
                                  {
                                      if (missing(name) == FALSE)
                                      {
                                          Name <<- name
                                          Description <<- description
                                          IsPositivelyOriented <<- ispositiveoriented
                                          Score <<- score
                                      }
                                          },
                                  show = function()
                                  {
                                      pos = "positive oriented"
                                      if (IsPositivelyOriented == FALSE)
                                          pos="negative oriented"
                                      paste("evaluation: ", Name, ":", Description, " (" ,pos,", ", ")","\n",sep="")
                                  }))



