#################################
#  task management
#################################
runTask <- function(clusterName, taskDefName, taskCount,
                    container,
                    cpu, memory,
                    securityGroupId,
                    subnetId,
                    enablePublicIp = TRUE
){
    stopifnot(taskCount<=10)
    envJson <- environmentToJSON(container$environment)

    assignPublicIp <- ifelse(enablePublicIp, "ENABLED", "DISABLED")

    networkConfiguration <-
        list(
            awsvpcConfiguration = list(subnets = list(subnetId),
                                       securityGroups = list(securityGroupId),
                                       assignPublicIp = assignPublicIp)
        )
    overrides <- list(
        containerOverrides = list(list(
            name = container$name, environment = envJson)),
        cpu = as.character(cpu), memory = as.character(memory)
    )
    if(!is.null(container$command)){
        overrides$containerOverrides[[1]]$command <- list(container$command)
    }
    ## handling the network error and prevent
    ## the container from duplicating.
    tryNum <- 5
    startedBy <- paste0(sample(letters, 30, TRUE),collapse = "")
    response <- NULL
    ids <- NULL
    for(i in seq_along(tryNum)){
        response <-
            tryCatch(
                ecs_run_task(cluster = clusterName,
                             taskDefinition = taskDefName,
                             count = taskCount,
                             enableECSManagedTags = TRUE,
                             launchType = "FARGATE",
                             networkConfiguration=networkConfiguration,
                             overrides=overrides,
                             startedBy = startedBy,
                             retry_time = 0,
                             network_timeout = 2
                ),
                error = function(e) {message(e);NULL}
            )
        if(is.null(response)){
            ids <- listTasks(clusterName = clusterName, startedBy = startedBy)
            if(length(ids)!=0){
                break
            }
        }else{
            ids <- vapply(response$tasks,function(x)x$taskArn, character(1))
            break
        }
    }
    ids
}


listTasks<-function(clusterName,
                    status = c("RUNNING", "PENDING","STOPPED"),
                    taskFamily = NULL,
                    startedBy = NULL){
    if(!is.null(status)){
        status <- match.arg(status)
    }
    response <- ecs_list_tasks(cluster=clusterName, desiredStatus = status,
                               family = taskFamily, startedBy=startedBy)
    response
}

stopTasks <- function(clusterName, taskIds){
    for(id in taskIds){
        ecs_stop_task(cluster = clusterName, task = id)
    }
}


getTaskDetails<-function(clusterName, taskIds, getIP = FALSE){
    response <- ecs_describe_tasks(cluster = clusterName,
                                   tasks=taskIds)

    taskIds <- vapply(response$tasks,function(x)x$taskArn, character(1))
    status <- vapply(response$tasks,function(x)x$lastStatus, character(1))
    privateIPs <- vapply(response$tasks,function(x){
        networkInterface <- x$containers[[1]]$networkInterfaces
        if(length(networkInterface)!=0){
            x$containers[[1]]$networkInterfaces[[1]]$privateIpv4Address
        }else{
            ""
        }
    }
    , character(1))

    if(getIP){
        ENIs <- vapply(response$tasks,getInstanceENI,character(1))
        idx <- which(ENIs!="")
        publicIPs <- rep("", length(taskIds))
        if(length(idx)!=0){
            publicIPs[idx] <- getInstanceIP(ENIs[idx])
        }
        data.frame(taskId = taskIds, status = status, privateIp = privateIPs, publicIp = publicIPs)
    }else{
        data.frame(taskId = taskIds, status = status, privateIp = privateIPs)
    }
}

getInstanceENI<-function(x){
    eni<-""
    for(i in x$attachments){
        if(i$type=="ElasticNetworkInterface"&&i$status=="ATTACHED"){
            eni<- getTagValue(i$details,"name","value","networkInterfaceId")
        }
    }
    eni
}

getInstanceIP <- function(ENIs){
    response <- ec2_describe_network_interfaces(NetworkInterfaceId = ENIs)
    IPs<- vapply(response, function(x)x$association$publicIp[[1]], character(1))
    IPs
}

