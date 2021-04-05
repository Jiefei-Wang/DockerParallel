DockerCluster.finalizer<- function(e){
    if(e$stopClusterOnExit){
        e$cluster$stopCluster()
    }
}


## Change the formals of the function so
## it can implicitly use the variable `cluster`
createTempFunction <- function(func, cluster){
    funcEnv <- new.env(parent = environment(func))
    funcEnv$cluster <- cluster
    funcFormals <- formals(func)
    funcFormals$cluster<-NULL
    formals(func) <- funcFormals
    environment(func) <- funcEnv
    func
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
