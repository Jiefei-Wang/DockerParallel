DockerCluster.finalizer<- function(e){
    if(e$stopClusterOnExit){
        settings <- .getClusterSettings(e$cluster)
        if(settings$cloudProviderInitialized){
            workerNumbers <- e$cluster$getWorkerNumbers()
            liveWorkers <- workerNumbers$running + workerNumbers$initializing
            if(e$cluster$isServerRunning()|| liveWorkers > 0){
                e$cluster$stopCluster(ignoreError = TRUE, cleanup = TRUE)
            }
        }
    }
}

initializeCloudProviderInternal <- function(cluster){
    settings <- .getClusterSettings(cluster)
    if(!settings$cloudProviderInitialized){
        verbose <- cluster$verbose
        provider <- .getCloudProvider(cluster)
        initializeCloudProvider(provider = provider, cluster=cluster, verbose = verbose)
        settings$cloudProviderInitialized <- TRUE
    }
}



## Check if the cluster exists on the cloud
## and ask the user if reuse the same cluster
## return `TRUE` to indicate the caller should proceed
## `FALSE` means the caller should directly return
checkIfClusterExistAndAsk <- function(cluster){
    verbose <- cluster$verbose
    provider <- .getCloudProvider(cluster)
    verbosePrint(verbose>0, "Checking if the cluster exist")

    exist <- dockerClusterExists(provider = provider,
                                 cluster = cluster,
                                 verbose = verbose)
    if(exist){
        jobName <- .getJobQueueName(cluster)
        msg <- paste0(
            "The cluster with the job queue name <",
            jobName,
            "> exists on the cloud, do you want to reuse the same cluster?",
            " Answering \"no\" will create a new cluster with the same job queue name"
        )
        answer <- menu(c("Yes", "No", "Cancel"), title=msg)
        if(answer == 1){
            reconnectClusterInternal(cluster = cluster)
            return(FALSE)
        }
        if(answer == 2){
            return(TRUE)
        }
        if(answer == 0||answer== 3){
            return(FALSE)
        }
    }
    TRUE
}

## Reconnect the cluster, the cluster must exist!
reconnectClusterInternal <- function(cluster, ...){
    verbose <- cluster$verbose
    provider <- .getCloudProvider(cluster)
    reconnectDockerCluster(provider = provider,
                           cluster = cluster,
                           verbose = verbose)
    if(cluster$stopClusterOnExit){
        verbosePrint(verbose>0, "<stopClusterOnExit> will be set to FALSE")
        cluster$stopClusterOnExit <- FALSE
    }
    updateServerIp(cluster)
    workerNumbers <- cluster$getWorkerNumbers()
    .setExpectedWorkerNumber(cluster, workerNumbers$initializing + workerNumbers$running)
    cluster$registerBackend(...)
}


updateWorkerNumber <- function(cluster){
    verbose <- cluster$verbose
    provider <- .getCloudProvider(cluster)
    workerNumbers <-
        tryCatch(
            getDockerWorkerNumbers(
                provider = provider,
                cluster = cluster,
                verbose = verbose),
            error = function(e) warning(e$message))
    if(length(workerNumbers)!=2){
        stop("The worker numbers returned by <getDockerWorkerNumbers> must be of length 2")
    }
    if(!is.list(workerNumbers)){
        stop("The worker numbers returned by <getDockerWorkerNumbers> must be a list")
    }
    if(!setequal(names(workerNumbers), c("initializing", "running"))){
        stop("The name of the worker numbers returned by getDockerWorkerNumbers must be 'initializing' and 'running'")
    }
    .setInitializingWorkerNumber(cluster, workerNumbers$initializing)
    .setRunningWorkerNumber(cluster, workerNumbers$running)
}



## Change the formals of the function so
## it can implicitly use the variable `cluster`
createTempFunction <- function(func, cluster){
    funcEnv <- new.env(parent = environment(func))
    funcEnv$cluster <- cluster
    funcFormals <- formals(func)
    funcFormals[["cluster"]]<-NULL
    formals(func) <- funcFormals
    environment(func) <- funcEnv
    func
}

configNATStatus <- function(cluster){
    cloudConfig <- .getCloudConfig(cluster)
    cloudRuntime <- .getCloudRuntime(cluster)

    publicIpNULL <- is.null(cloudRuntime$serverPublicIp)
    privateIpNULL <- is.null(cloudRuntime$serverPrivateIp)
    # if(!all(publicIpNULL,privateIpNULL)){
    #     cluster@serverContainer <- NULL
    # }
    if(publicIpNULL&&!privateIpNULL){
        cloudConfig$serverWorkerSameLAN <- FALSE
        cloudConfig$serverWorkerSameNAT <- FALSE
    }
    if(!publicIpNULL&&privateIpNULL){
        cloudConfig$serverWorkerSameLAN <- TRUE
        cloudConfig$serverWorkerSameNAT <- TRUE
    }
    cluster
}

existsProviderMethod <- function(provider, name){
    existsMethod(name, class(provider)[1])
}

resetServerRuntime <- function(cloudRuntime){
    cloudRuntime$serverPublicIp <- character(0)
    cloudRuntime$serverPrivateIp <- character(0)
    cloudRuntime$serverPublicPort <- integer(0)
    cloudRuntime$serverPrivatePort <- integer(0)
}

handleError <- function(expr, errorToWarning = FALSE){
    if(errorToWarning){
        tryCatch(expr, error = function(e) warning(e$message))
    }else{
        expr
    }
}

updateServerStatus <- function(cluster){
    verbose <- cluster$verbose
    provider <- .getCloudProvider(cluster)
    cloudRuntime <- .getCloudRuntime(cluster)
    serverFromOtherSource <- .getServerFromOtherSource(cluster)
    if(!serverFromOtherSource){
        status <- getServerStatus(provider = provider,
                                  cluster = cluster,
                                  verbose = verbose)
        if(status == "stopped"){
            resetServerRuntime(cloudRuntime)
        }
        if(status == "running"){
            updateServerIp(cluster)
        }
        status
    }else{
        "running"
    }
}

updateServerIp <- function(cluster){
    verbose <- cluster$verbose
    provider <- .getCloudProvider(cluster)
    serverFromOtherSource <- .getServerFromOtherSource(cluster)
    if(!serverFromOtherSource){
        serverIp <- getDockerServerIp(
            provider = provider,
            cluster = cluster,
            verbose = verbose
        )

        stopifnot(length(serverIp$publicIp)<=1)
        stopifnot(length(serverIp$publicPort)<=1)
        stopifnot(length(serverIp$privateIp)<=1)
        stopifnot(length(serverIp$privatePort)<=1)

        ## Handle the null case
        if(is.null(serverIp$publicIp)) serverIp$publicIp <- character(0)
        if(is.null(serverIp$publicPort)) serverIp$publicPort <- integer(0)
        if(is.null(serverIp$privateIp)) serverIp$privateIp <- character(0)
        if(is.null(serverIp$privatePort)) serverIp$privatePort <- integer(0)

        .setServerPrivateIp(cluster, serverIp$privateIp)
        .setServerPrivatePort(cluster, serverIp$privatePort)
        .setServerPublicIp(cluster, serverIp$publicIp)
        .setServerPublicPort(cluster, serverIp$publicPort)
    }
}



waitServerRunning <- function(cluster){
    provider <- .getCloudProvider(cluster)
    verbose <- cluster$verbose
    while(TRUE){
        serverStatus <- getServerStatus(
            provider = provider,
            cluster = cluster,
            verbose = verbose)
        if(serverStatus == "initializing"){
            verbosePrint(verbose > 1, "The server is still initializing, check again after 1 second")
        }
        if(serverStatus == "running"){
            return(TRUE)
        }
        if(serverStatus == "stopped"){
            return(FALSE)
        }
        Sys.sleep(1)
    }
}
