## x <- ECSFargateConfig();verbose <- TRUE
##
## wait_tasks_to_run(a$task_id, progress_bar = TRUE)

runDockers <- function(x, verbose = FALSE){
    initConfig(x, verbose = verbose)
    runServer(x, verbose = verbose)
    runWorkers(x, workerNum = x@workerNum, verbose = TRUE)
    x
}
stopDockers <- function(x){
    if(!is.empty(x$serverId))
        stopTasks(x$clusterName,x$serverId)
    if(!is.empty(x$workerIds))
        stopTasks(x$clusterName,x$workerIds)
}


runServer <- function(x, verbose = TRUE){
    server <- x@server
    ## configure hardware
    cpu <- getECSCPU(server)
    memory <- getECSMem(server)
    hardware <- getValidFargateCpuMemory(cpu, memory)
    if(hardware$cpu!=cpu||
       hardware$memory!= memory){
        message("The required Fargate hardware does not exist,\n",
                "we have upgrate it to cpu ",
                hardware$cpu
                ,"units and memory ",
                hardware$cpu
                ,"MB")
    }
    env <- server@environment
    verbosePrint(verbose, "Configuring server ssh public key")
    if(!"sshPubKey"%in%names(env)){
        if(!is.empty(getSSHPubKeyValue())){
            env[["sshPubKey"]] <- getSSHPubKeyValue()
        }else{
            verbosePrint(verbose, "No ssh public key has been found")
        }
    }
    verbosePrint(verbose, "Configuring server redis password")
    if(!"redisPassword"%in%names(env)){
        env[["redisPassword"]] <- paste0(letters[sample(26,26, replace = TRUE)],collapse = "")
    }
    setECSData(x, "serverPassword", env[["redisPassword"]])

    if(!"redisPort"%in%names(env)){
        env[["redisPort"]] <- 6379
    }
    setECSData(x, "serverPort", env[["redisPort"]])

    if(!"redisQueue"%in%names(env)){
        queue <- paste0(Sys.info()[["nodename"]], Sys.time())
        queue <- gsub(" ","_", queue, fixed = TRUE)
        env[["redisQueue"]] <- queue
    }
    setECSData(x, "serverQueue", env[["redisQueue"]])

    verbosePrint(verbose, "Deploying server container")
    id <- runTask(x$clusterName, x$serverTaskDefName, 1,
                  cpu=hardware$cpu, memory=hardware$memory,
                  securityGroupId = x$securityGroupId,
                  subnetId=x$subnetId,
                  env = env
    )
    setECSData(x, "serverId", id)

    success <- waitTasksToRun(x$clusterName, id)
    if(success){
        serverInfo <- getTaskDetails(x$clusterName, id, getIP = TRUE)
        if(nrow(serverInfo)!=0){
            setECSData(x, "serverIP", serverInfo$IP)
        }else{
            stop("Fail to start the server")
        }
    }else{
        stop("Fail to start the server")
    }

}


runWorkers <- function(x, workerNum = 1, verbose= TRUE){
    worker <- x@worker
    ## configure hardware
    cpu <- getECSCPU(worker)
    memory <- getECSMem(worker)
    workersPerContainer <- getNWorkerPerContainer(cpu,memory)
    # workersPerContainer <- 2
    containerNum <- ceiling(workerNum/workersPerContainer)
    workerLastContainer <- workerNum%%workersPerContainer
    regularContainerHardware <-
        getValidFargateCpuMemory(cpu*workersPerContainer, memory*workersPerContainer)
    lastContainderHardware <-
        getValidFargateCpuMemory(cpu*workerLastContainer, memory*workerLastContainer)

    if(regularContainerHardware$cpu!=cpu*workersPerContainer||
       regularContainerHardware$memory!= memory*workersPerContainer){
        message("The required Fargate hardware does not exist,\n",
                "we have upgrate it to cpu ",
                round(regularContainerHardware$cpu/workersPerContainer,3)
                ,"units and memory ",
                round(regularContainerHardware$memory/workersPerContainer,3)
                ,"MB")
    }

    startedNumber <- 0
    taskIds <- c()
    while(startedNumber<containerNum){
        startTime <- Sys.time()
        requiredNumber <- min(10, containerNum-startedNumber)
        verbosePrint(verbose, "starting containers:",startedNumber,"/",containerNum)
        if(requiredNumber + startedNumber== containerNum&&workerLastContainer!=0){
            regularContainerNumber <- requiredNumber-1
            last_container_indicator <- TRUE
        }else{
            regularContainerNumber <- requiredNumber
            last_container_indicator <- FALSE
        }
        if(regularContainerNumber>0){
            taskIds<-c(taskIds,
                       runWorkersInternal(x = x,
                                            cpu = regularContainerHardware$cpu,
                                            memory = regularContainerHardware$memory,
                                            taskCount = regularContainerNumber,
                                            verbose=verbose))
        }
        if(last_container_indicator){
            taskIds<-c(taskIds,
                       runWorkersInternal(x = x,
                                            cpu = lastContainderHardware$cpu,
                                            memory = lastContainderHardware$memory,
                                            taskCount=1,
                                            verbose=verbose))
        }
        end_time <- Sys.time()
        startedNumber <- startedNumber+requiredNumber
        if(startedNumber!=containerNum){
            sleep_time <- 11-(end_time-startTime)
            if(sleep_time>0)
                Sys.sleep(sleep_time)
        }
    }
    workersInContainer <- rep(workersPerContainer, containerNum)
    setECSData(x, "workerIds", c(x$workerIds, taskIds))
    if(workerLastContainer!=0)
        workersInContainer[containerNum] <- workerLastContainer
    data.frame(task_id = taskIds,
               workerNum = workersInContainer)
}

## CPU and memory must be valid
runWorkersInternal <- function(x,
                             cpu, memory, taskCount = 1, verbose= TRUE){
    stopifnot(taskCount<=10)
    serverIP <- getECSData(x, "serverIP")
    workerEnv <- x@worker@environment
    sshPubKey <- getSSHPubKeyValue()
    if(!is.null(sshPubKey)&&!"sshPubKey"%in%names(workerEnv)){
        workerEnv[["sshPubKey"]] <- sshPubKey
    }
    serverIP <- getECSData(x, "serverIP")
    workerEnv[["redisServer"]] <- serverIP
    serverPassword <- getECSData(x, "serverPassword")
    workerEnv[["redisPassword"]] <- serverPassword
    serverPort <- getECSData(x, "serverPort")
    workerEnv[["redisPort"]] <- serverPort
    serverQueue <- getECSData(x, "serverQueue")
    workerEnv[["redisQueue"]] <- serverQueue

    ids <- runTask(x$clusterName,x$workerTaskDefName,taskCount,
                   cpu=cpu,memory=memory,
                   securityGroupId = x$securityGroupId,
                   subnetId= x$subnetId,
                   env = workerEnv)

}

# taskIds <- task_list$task_id
waitTasksToRun<-function(clusterName, taskIds, progressBar = FALSE){
    instanceNum <- length(taskIds)
    instanceIndicator <- rep(FALSE, instanceNum)
    if(progressBar){
        pb <- txtProgressBar(min=0,max = length(taskIds), style=3)
    }
    while(sum(instanceIndicator)<instanceNum){
        pendingIdx <- which(!instanceIndicator)
        taskDetails <- getTaskDetails(clusterName, taskIds[pendingIdx])
        if(nrow(taskDetails)!=length(pendingIdx)){
            warning("Requestion task number and result does not match")
            break
        }
        if(sum(taskDetails$status=="STOPPED")>0){
            warning("The task has been stopped")
            break
        }
        runningIdx <- pendingIdx[taskDetails$status=="RUNNING"]
        instanceIndicator[runningIdx] <- TRUE

        if(progressBar){
            setTxtProgressBar(pb, sum(instanceIndicator))
        }
    }
    if(progressBar){
        close(pb)
    }
    sum(instanceIndicator)== instanceNum
}




