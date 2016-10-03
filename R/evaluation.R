
#----------------------------------------
#'
#' @field abbreviation of the rule
#' @field full name and any other description about the rule
#' @field a boolean indicates wether higher is better
#' @fiels the scoring function. It should get two arguments: vector of forecast errors and vector of forecast standard errors and it should return a vector of the same length
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



