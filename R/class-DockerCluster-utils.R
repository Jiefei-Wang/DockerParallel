DockerCluster.finalizer<- function(e){
    if(e$stopClusterOnExit){
        e$cluster$stopCluster()
    }
}


createTempFunction <- function(name, func){
    funcFormals <- formals(func)
    funcFormals <- funcFormals[names(funcFormals)!="cluster"]
    parameters <- ""
    if(length(funcFormals)!=0){
        parameters <- paste0(
            paste0(names(funcFormals), "=", names(funcFormals)),collapse = ",")
        parameters <- paste0(",", parameters)
    }
    functionTemplate <- "
                              function(){
                        functionName(cluster = x parameters)
                    }
                              "
    functionTemplate <- sub("functionName",name, functionTemplate)
    functionTemplate <- sub("parameters",parameters, functionTemplate)
    newFuncExpression <-
        parse(text = functionTemplate
        )[[1]]

    newFunc <- eval(newFuncExpression)
    formals(newFunc) <- funcFormals
    newFunc
}

configNATStatus <- function(cluster){
    cloudRuntime <- cluster@cloudRuntime
    cloudConfig <- cluster@cloudConfig

    publicIpNULL <- is.null(cloudRuntime$serverPublicIp)
    privateIpNULL <- is.null(cloudRuntime$serverPrivateIp)
    if(!all(publicIpNULL,privateIpNULL)){
        cluster@serverContainer <- NULL
    }
    if(publicIpNULL&&!privateIpNULL){
        cloudConfig$serverClientSameNAT <- FALSE
        cloudConfig$serverWorkerSameNAT <- FALSE
    }
    if(!publicIpNULL&&privateIpNULL){
        cloudConfig$serverClientSameNAT <- TRUE
        cloudConfig$serverWorkerSameNAT <- TRUE
    }
    cluster
}
