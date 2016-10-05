#require(XML)
#require(timeSeries)
#require(vars)
#require(forecast)

source("R/utls.R")
source("R/evaluation.R")
source("R/ldt.R")
source("R/ldtpack.R")
source("R/ldtpacksub.R")
source("R/ldtpackarimasub.R")
source("R/ldtpackarima.R")


a = ldt$new()
a$setData(data_file = "endodata.txt")
a$setData(is_endo = FALSE, data_file = "exodata.txt")
a$MaxSize = 3
a$SimulationCount = 3
a$setPacks()
a$Run()

