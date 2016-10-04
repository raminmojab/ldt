#require(XML)
#require(timeSeries)
#require(vars)
#require(forecast)

source("utls.R")
source("evaluation.R")
source("ldt.R")
source("ldtpack.R")
source("ldtpacksub.R")
source("ldtpackarimasub.R")
source("ldtpackarima.R")


a = ldt$new()
a$setData(data_file = "endodata.txt")
a$setData(is_endo = FALSE, data_file = "exodata.txt")

a$SimulationCount = 3
a$setPacks()
a$Run()

