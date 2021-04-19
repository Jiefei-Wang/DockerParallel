ECSfilterList <- list(`tag:docker-parallel-tag`="docker-parallel-tag")
ECSTagTemplate <- list(
    list(ResourceType=NULL,
         Tag = list(
             list(Key= "docker-parallel-tag", Value = "docker-parallel-tag")
         )
    )
)

cleanupProvider <- function(x, verbose = TRUE){
    if(x$clusterNameVerified && x$clusterName=="R-worker-cluster"){
        verbosePrint(verbose, "Deleting worker cluster")
        tryCatch({
            deleteCluster(x$clusterName)
            x$clusterNameVerified <- FALSE
        },
        error = function(e) message(e))
    }

    if(x$vpcVerified){
        verbosePrint(verbose, "Deleting vpc")
        tryCatch({
            deleteVpc(x$vpcId)
            x$vpcVerified <- FALSE
            x$internetGatewayVerified <- FALSE
            x$securityGroupVerified <- FALSE
            x$subnetVerified <- FALSE
            x$routeTableVerified <- FALSE
        },
        error = function(e) message(e))
    }
    if(x$internetGatewayVerified){
        verbosePrint(verbose, "Deleting internet gateway")
        tryCatch({
            deleteInternetGateway(x$internetGatewayId)
            x$internetGatewayVerified <- FALSE
        },
        error = function(e) message(e))
    }
    invisible()
}


## Collect the server's public and private IP
packServerIp <- function(cluster){
    paste0(.getServerPublicIp(cluster),"-", .getServerPrivateIp(cluster))
}
## base64 encode the cloud config setting
encodeCloudConfig <- function(cloudConfig){
    fields <- names(cloudConfig$getRefClass()$fields())
    info <- lapply(fields, function(i) cloudConfig$field(i))
    names(info) <- fields
    jsonlite::base64_enc(serialize(info, NULL))
}
## base64 decode the cloud config setting
decodeCloudConfig <- function(value){
    unserialize(jsonlite::base64_dec(value))
}


## Find the cloud config values from the environment variable
getCloudConfigInfo <- function(taskDescription){
    env <- taskDescription$overrides$containerOverrides[[1]]$environment
    env <- ArrayToList(env)
    value <- env[["ECSFargateCloudConfigInfo"]]
    if(!is.null(value)){
        cloudConfigValue <- decodeCloudConfig(value)
        cloudConfigValue
    }else{
        NULL
    }
}

findServerInfo <- function(cluster, serverHandles){
    provider <- .getCloudProvider(cluster)
    jobQueue <- .getJobQueueName(cluster)
    clusterName <- provider$clusterName
    if(!is.null(serverHandles)){
        info <- ecs_describe_tasks(cluster = clusterName, tasks = serverHandles)
        arns <- c()
        idx <- c()
        for(i in seq_along(info$tasks)){
            cloudConfigValue <- getCloudConfigInfo(info$tasks[[i]])
            if(identical(cloudConfigValue$jobQueueName,jobQueue)){
                arns <- c(arns, info$tasks[[i]]$taskArn)
                idx <- c(idx, i)
            }
        }
        if(length(arns) >= 1){
            if(length(arns) > 1){
                answer <- menu(arns,
                               title="Multiple servers exist on the cloud, please select:")
                if(answer == 0)
                    answer <- 1
                arns <- arns[answer]
                idx <- idx[answer]
            }
            cloudConfigValue <- getCloudConfigInfo(info$tasks[[idx]])
            return(
                list(handle = info$tasks[[idx]]$taskArn,
                     cloudConfigValue=cloudConfigValue)
            )
        }


    }
    NULL
}

findWorkerHandles <- function(cluster, jobQueueName){
    provider <- .getCloudProvider(cluster)
    clusterName <- provider$clusterName
    workerHandles <- listRunningWorkers(cluster)
    serverIp <- packServerIp(cluster)
    result <- c()
    if(!is.null(workerHandles)){
        info <- ecs_describe_tasks(cluster = clusterName, tasks = workerHandles)
        for(i in info$tasks){
            env <- i$overrides$containerOverrides[[1]]$environment
            env <- ArrayToList(env)
            if(identical(env[["ECSFargateCloudJobQueueName"]], jobQueueName)&&
               identical(env[["ECSFargateCloudServerIP"]], serverIp)){
                workerNum <- as.numeric(env[["ECSFargateCloudWorkerNumber"]])
                result <- c(result, rep(i$taskArn,workerNum))
            }
        }
    }
    result
}

listRunningServer <- function(cluster){
    provider <- .getCloudProvider(cluster)
    serverHandle <- listTasks(provider$clusterName, taskFamily = provider$serverTaskDefName)
    serverHandle
}

listRunningWorkers <- function(cluster){
    provider <- .getCloudProvider(cluster)
    workerHandles <- listTasks(provider$clusterName, taskFamily = provider$workerTaskDefName)
    workerHandles
}

## Repeat each x[i] element n[i] times
repeatVector <- function(x, n){
    unlist(lapply(seq_along(x), function(i) rep(x[[i]],n[i])))
}

