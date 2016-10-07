

ldtpacksvarxsub <- setRefClass("ldtpacksvarxsub",
                               fields = list(
                                   P = 'numeric',
                                   Q = 'numeric',
                                   Size = 'numeric',
                                   Intercept = 'logical',
                                   Indexes = 'vector'
                               ),
                               contains = "ldtpacksub")

ldtpackarimasub$methods(initialize = function(parentsvarx,p,q,size,intercept){
    if (missing(parentarima) == FALSE)
    {
        ParentPack <<- parentsvarx
        CountValid <<- 0
        CountFailed <<- 0
        CountWarning <<- 0

        P <<- p
        Q <<- q
        Size <<- size
        Intercept <<- intercept


        if (length(parentarima$XReg) == 0)
        {
            CountRequired <<- 1
        }
        else
        {

        }

        if (length(parentarima$XReg) == 0)
        {
            CountRequired <<- 1
        }
        else
        {

        }

    }
})
