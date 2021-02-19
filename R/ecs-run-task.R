#################################
#  task management
#################################



runTask <- function(clusterName, taskDefName, taskCount,
                    cpu, memory,
                    securityGroupId,
                    subnetId,
                    env = list()
){
    stopifnot(taskCount<=10)
    envJson <- getEnvJson(env)

    request <- ecs_get_json("run-task.json")
    request$cluster <- clusterName
    request$overrides$cpu<- as.character(cpu)
    request$overrides$memory<- as.character(memory)
    request$taskDefinition <- taskDefName
    request$count <- taskCount

    request$overrides$containerOverrides[[1]]$environment <- envJson
    request$networkConfiguration$awsvpcConfiguration$securityGroups[[1]]<- securityGroupId
    request$networkConfiguration$awsvpcConfiguration$subnets[[1]]<- subnetId
    #existing_rule <- ecs_list_security_rule()
    response <- ecs_run_task(request)
    ids <- vapply(response$tasks,function(x)x$taskArn, character(1))
    ids
}


listTasks<-function(clusterName,
                         status = c("RUNNING", "PENDING","STOPPED"),
                         taskFamily = NULL){
    status <- match.arg(status)
    request <- list()
    request$cluster <- clusterName
    request$desiredStatus <- status
    if(!is.null(taskFamily)){
        request$family <- taskFamily
    }
    response <- ecs_list_tasks(request)
    response
}

stopTasks <- function(clusterName, taskIds){
    request <- list()
    request$cluster <- clusterName
    for(id in taskIds){
        request$task <- id
        ecs_stop_task(request)
    }
}


getTaskDetails <- function(clusterName, taskIds, getIP = FALSE){
    result <- c()
    describedTaskNumber <- 0
    while(describedTaskNumber!=length(taskIds)){
        currentTaskNumber <- min(100,length(taskIds)-describedTaskNumber)
        currentTaskIds <- taskIds[(describedTaskNumber+1):(describedTaskNumber+currentTaskNumber)]
        result<-rbind(result,
                      geTaskDetailsInternal(clusterName, currentTaskIds, getIP=getIP))
        describedTaskNumber <- describedTaskNumber + currentTaskNumber
    }
    result
}

geTaskDetailsInternal<-function(clusterName, taskIds, getIP = FALSE){
    target <- "DescribeTasks"
    request <- list(
        cluster = clusterName,
        tasks=as.list(taskIds)
    )
    response <- ecs_describe_tasks(request)

    taskIds <- vapply(response$tasks,function(x)x$taskArn, character(1))
    status <- vapply(response$tasks,function(x)x$lastStatus, character(1))
    if(getIP){
        ENIs <- vapply(response$tasks,getInstanceENI,character(1))
        idx <- which(ENIs!="")
        IPs <- rep("", length(taskIds))
        IPs[idx] <- getInstanceIP(ENIs[idx])
        data.frame(taskId = taskIds, status = status, IP = IPs)
    }else{
        data.frame(taskId = taskIds, status = status)
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
    query <- list()
    for(i in seq_along(ENIs)){
        query[[paste0("NetworkInterfaceId.",i)]] <- ENIs[i]
    }
    response <- ec2_describe_network_interfaces(query)
    IPs<- vapply(response, function(x)x$association$publicIp[[1]], character(1))
    IPs
}

