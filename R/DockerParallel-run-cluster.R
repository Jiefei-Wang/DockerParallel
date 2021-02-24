## x <- ECSFargateConfig();verbose <- TRUE
## runCloudCluster(x, TRUE)
## registerDoECSRedis(x)
##

runCloudCluster <- function(x, verbose = FALSE){
    initConfig(x, verbose = verbose)
    runServer(x, verbose = verbose)
    runWorkers(x, workerNum = x@workerNum, verbose = TRUE)

    ## Wait for the server start, then we get its public IP
    serverInstanceId <- getECSClusterData(x, "serverInstanceId")
    clusterName <- getECSCloudData(x, "clusterName")
    if(!is.null(serverInstanceId)){
        verbosePrint(verbose, "Waiting for the server start")
        waitTasksToRun(clusterName, serverInstanceId)
        taskInfo <-
            getTaskDetails(
                clusterName = clusterName,
                taskIds = serverInstanceId,
                getIP = TRUE)
        setECSClusterData(x, "serverClientIP", taskInfo$publicIP)
    }
    x
}

stopCloudCluster <- function(x){
    clusterName <- getECSCloudData(x, "clusterName")
    serverInstanceId <- getECSClusterData(x, "serverInstanceId")
    workerInstanceIds <- getECSClusterData(x, "workerInstanceIds")
    if(!is.empty(serverInstanceId))
        stopTasks(clusterName, serverInstanceId)
    if(!is.empty(workerInstanceIds))
        stopTasks(clusterName,workerInstanceIds)
}


runServer <- function(x, verbose = TRUE){
    server <- x@server
    if(is(server, "Container")){
        hardware <- x@serverHardware
        ## configure hardware
        CPU <- hardware$CPU
        memory <- hardware$memory
        hardware <- getValidFargateCpuMemory(CPU, memory)
        if(hardware$CPU!=CPU||
           hardware$memory!= memory){
            message("The required Fargate hardware does not exist,\n",
                    "we have upgrate it to CPU ",
                    hardware$CPU
                    ,"units and memory ",
                    hardware$CPU
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
        if(!"redisPassword"%in%names(env)){
            env[["redisPassword"]] <- paste0(letters[sample(26,26, replace = TRUE)],collapse = "")
        }
        if(!"redisPort"%in%names(env)){
            env[["redisPort"]] <- 6379
        }

        verbosePrint(verbose, "Deploying server container")
        id <- runTask(x$clusterName, x$serverTaskDefName, 1,
                      CPU=hardware$CPU, memory=hardware$memory,
                      securityGroupId = x$securityGroupId,
                      subnetId=x$subnetId,
                      command = server@command,
                      env = env
        )

        ## If `getTaskDetails` runs too fast
        ## it may not be able to get the server's IP
        ## as the IP does not exist
        tryNum <- 0
        serverInfoValid <- FALSE
        clusterName <- getECSCloudData(x, "clusterName")
        while(TRUE){
            serverInfo <- getTaskDetails(clusterName, id)
            serverInfoValid <- nrow(serverInfo)!=0&&serverInfo$privateIP!=""
            tryNum <- tryNum + 1
            if(tryNum>6){
                stop("Unable to find the server's IP")
            }
            if(serverInfoValid){
                break
            }
            Sys.sleep(0.5)
        }

        setECSClusterData(x, "serverWorkerIP", serverInfo$privateIP)
        setECSClusterData(x, "serverInstanceId", id)
        setECSClusterData(x, "serverPort", env[["redisPort"]])
        setECSClusterData(x, "serverPassword", env[["redisPassword"]])
    }else{
        setECSClusterData(x, "serverWorkerIP", server@IP)
        setECSClusterData(x, "serverPort", server@port)
        setECSClusterData(x, "serverPassword", server@password)
    }
}


runWorkers <- function(x, workerNum = 1, verbose= TRUE){
    worker <- x@worker
    hardware <- x@workerHardware

    ## initialize the queue info
    if(is.empty(getECSClusterData(x, "serverQueue"))){
        if(is.empty(worker@environment$redisQueue)){
            queue <- paste0(Sys.info()[["nodename"]], Sys.time())
            queue <- gsub(" ","_", queue, fixed = TRUE)
            setECSClusterData(x, "serverQueue", queue)
        }else{
            setECSClusterData(x, "serverQueue", worker@environment$redisQueue)
        }
    }

    ## Compute the maximum worker per container based on
    ## the hardware requirement per worker
    CPU <- hardware$CPU
    memory <- hardware$memory
    workersPerContainer <- getMaxWorkerPerContainer(CPU,memory)
    containerNum <- ceiling(workerNum/workersPerContainer)
    workerLastContainer <- workerNum%%workersPerContainer
    regularContainerHardware <-
        getValidFargateCpuMemory(CPU*workersPerContainer, memory*workersPerContainer)
    lastContainderHardware <-
        getValidFargateCpuMemory(CPU*workerLastContainer, memory*workerLastContainer)

    if(regularContainerHardware$CPU!=CPU*workersPerContainer||
       regularContainerHardware$memory!= memory*workersPerContainer){
        message("The required Fargate hardware does not exist,\n",
                "we have upgrate it to CPU ",
                round(regularContainerHardware$CPU/workersPerContainer,3)
                ," units and memory ",
                round(regularContainerHardware$memory/workersPerContainer,3)
                ," MB")
    }

    ## Try to deploy the worker containers
    ## The speed is limited to 10 containers per 11 seconds
    startedNumber <- 0
    workerInstanceIds <- c()
    while(startedNumber<containerNum){
        startTime <- Sys.time()
        requiredNumber <- min(10, containerNum-startedNumber)
        verbosePrint(verbose, "starting worker containers:",startedNumber,"/",containerNum)
        if(requiredNumber + startedNumber== containerNum&&workerLastContainer!=0){
            regularContainerNumber <- requiredNumber-1
            last_container_indicator <- TRUE
        }else{
            regularContainerNumber <- requiredNumber
            last_container_indicator <- FALSE
        }
        if(regularContainerNumber>0){
            workerInstanceIds<-c(workerInstanceIds,
                                 runWorkersInternal(x = x,
                                                    CPU = regularContainerHardware$CPU,
                                                    memory = regularContainerHardware$memory,
                                                    workerNum = workersPerContainer,
                                                    taskCount = regularContainerNumber,
                                                    verbose=verbose))
        }
        if(last_container_indicator){
            workerInstanceIds<-c(workerInstanceIds,
                                 runWorkersInternal(x = x,
                                                    CPU = lastContainderHardware$CPU,
                                                    memory = lastContainderHardware$memory,
                                                    workerNum = workerLastContainer,
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

    ## Record the container instance ids
    setECSClusterData(x, "workerInstanceIds",
                      c(getECSClusterData(x, "workerInstanceIds"),
                        workerInstanceIds))

    ## Record the number of workers per container
    workersInContainer <- rep(workersPerContainer, containerNum)
    if(workerLastContainer!=0)
        workersInContainer[containerNum] <- workerLastContainer
    setECSClusterData(x, "workerNums",
                      c(getECSClusterData(x, "workerNums"),
                        workersInContainer))

    data.frame(task_id = workerInstanceIds,
               workerNum = workersInContainer)
}

## Configure the worker environment and
## run the same container `taskCount` times
## CPU and memory must be valid fargate settings
runWorkersInternal <- function(x,
                               CPU, memory, workerNum, taskCount = 1, verbose= TRUE){
    stopifnot(taskCount<=10)
    workerEnv <- x@worker@environment
    sshPubKey <- getSSHPubKeyValue()
    if(!is.null(sshPubKey)&&!"sshPubKey"%in%names(workerEnv)){
        workerEnv[["sshPubKey"]] <- sshPubKey
    }
    serverWorkerIP <- getECSClusterData(x, "serverWorkerIP")
    workerEnv[["redisServer"]] <- serverWorkerIP
    serverPassword <- getECSClusterData(x, "serverPassword")
    workerEnv[["redisPassword"]] <- serverPassword
    serverPort <- getECSClusterData(x, "serverPort")
    workerEnv[["redisPort"]] <- serverPort
    serverQueue <- getECSClusterData(x, "serverQueue")
    workerEnv[["redisQueue"]] <- serverQueue
    workerEnv[["workerNum"]] <- workerNum

    ids <- runTask(x$clusterName,x$workerTaskDefName,taskCount,
                   CPU=CPU,memory=memory,
                   securityGroupId = x$securityGroupId,
                   subnetId= x$subnetId,
                   command = x@worker@command,
                   env = workerEnv)
    ids
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




