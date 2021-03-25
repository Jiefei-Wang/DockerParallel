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
    envJson <- environmentToJSON(container@environment)

    assignPublicIp <- ifelse(enablePublicIp, "ENABLED", "DISABLED")

    networkConfiguration <-
        list(
            awsvpcConfiguration = list(subnets = list(subnetId),
                                       securityGroups = list(securityGroupId),
                                       assignPublicIp = assignPublicIp)
        )
    request$overrides$containerOverrides[[1]]$environment <- envJson
    request$overrides$cpu<- as.character(CPU)
    request$overrides$memory<- as.character(memory)

    overrides <- list(
        containerOverrides = list(list(
            name = "worker",environment = envJson)),
        cpu = 0, memory = 0
    )
    if(!is.null(command)){
        overrides$containerOverrides[[1]]$command <- list(command)
    }
    response <- ecs_run_task(cluster = clusterName,
                             taskDefinition = taskDefName,
                             count = taskCount,
                             enableECSManagedTags = TRUE,
                             launchType = "FARGATE",
                             networkConfiguration=networkConfiguration,
                             overrides=overrides
    )
    ids <- vapply(response,function(x)x$taskArn, character(1))
    ids
}


listTasks<-function(clusterName,
                    status = c("RUNNING", "PENDING","STOPPED"),
                    taskFamily = NULL){
    status <- match.arg(status)
    response <- ecs_list_tasks(cluster=cluster, desiredStatus = status,
                               family = taskFamily)
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
        publicIPs[idx] <- getInstanceIP(ENIs[idx])
        data.frame(taskId = taskIds, status = status, privateIP = privateIPs, publicIP = publicIPs)
    }else{
        data.frame(taskId = taskIds, status = status, privateIP = privateIPs)
    }
}

getInstanceENI<-function(x){
    eni<-""
    for(i in x$attachments){
        if(i$type=="ElasticNetworkInterface"&&i$status=="ATTACHED"){
            eni<- get_tag_value(i$details,"name","value","networkInterfaceId")
        }
    }
    eni
}

getInstanceIP <- function(ENIs){
    response <- ec2_describe_network_interfaces(NetworkInterfaceId = ENIs)
    IPs<- vapply(response, function(x)x$association$publicIp[[1]], character(1))
    IPs
}

