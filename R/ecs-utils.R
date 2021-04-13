ecsLaunchHistory <- new.env()
validFargateSettings <- list()
validFargateSettings$"256" <- c(512,1024,2048)
validFargateSettings$"512" <- 1024*(1:4)
validFargateSettings$"1024" <- 1024*(2:8)
validFargateSettings$"2048" <- 1024*(4:16)
validFargateSettings$"4096" <- 1024*(8:30)


ecsRunWorkers <- function(provider, cluster, container, hardware,
                          containerNumber, workerPerContainer, verbose){
  container <- container$copy()
  verbosePrint(verbose>1,
               "Deploying ", containerNumber," container with ",
               workerPerContainer," workers per container.")

  taskDefName <- provider$workerTaskDefName
  ## Set the worker container environment
  workerContainer <- configWorkerContainerEnv(
    container = container,
    cluster = cluster,
    workerNumber = workerPerContainer,
    verbose = verbose
  )

  ## Config the hardware for the container
  requiredHardware <- hardware
  requiredHardware@cpu <- hardware@cpu*workerPerContainer
  requiredHardware@memory <- hardware@memory*workerPerContainer
  fargateHardware <- getValidFargateHardware(requiredHardware)
  if(verbose){
    informUpgradedHardware(fargateHardware, requiredHardware, workerPerContainer)
  }
  ## save the cluster info to the container environment
  serverIp <- packServerIp(cluster)
  workerContainer$environment[["ECSFargateCloudJobQueueName"]] <- .getJobQueueName(cluster)
  workerContainer$environment[["ECSFargateCloudServerIP"]] <- serverIp
  workerContainer$environment[["ECSFargateCloudWorkerNumber"]] <- workerPerContainer

  instances <- ecsTaskScheduler(provider=provider,
                                taskDefName=taskDefName,
                                container=workerContainer,
                                hardware=fargateHardware,
                                containerNum=containerNumber,
                                publicIpEnable=TRUE)
  instances
}


ecsLaunchThrottle <- list(
  interval = 11,
  launchNumber = 10
)
addLaunchHistory <- function(number=1){
  launchTime <- Sys.time()
  for(i in seq_len(number)){
    ecsLaunchHistory[[paste0(as.character(launchTime), "+" ,number)]] <- launchTime

  }
}

removeUnusedLaunchHistory <- function(){
  idx <- vapply(ecsLaunchHistory, function(x){
    difftime(Sys.time(),x, units = "secs") > ecsLaunchThrottle$interval
  }, logical(1))
  remove(list = names(ecsLaunchHistory)[idx], envir = ecsLaunchHistory)
}
getAvailableLaunchNumber <- function(){
  removeUnusedLaunchHistory()
  ecsLaunchThrottle$launchNumber - length(ecsLaunchHistory)
}

ecsTaskScheduler <- function(provider, taskDefName , container, hardware,
                             containerNum, publicIpEnable){
  instanceIds <- c()
  while(length(instanceIds) < containerNum){
    requiredNum <- containerNum - length(instanceIds)
    availableNum <- min(getAvailableLaunchNumber(), requiredNum)
    if(availableNum<=0){
      next
    }
    instances <- runTask(clusterName=provider$clusterName,
                         taskDefName=taskDefName,
                         taskCount=availableNum,
                         container=container,
                         cpu=hardware@cpu,
                         memory=hardware@memory,
                         securityGroupId=provider$securityGroupId,
                         subnetId=provider$subnetId,
                         enablePublicIp=publicIpEnable
    )
    if(is.null(instances)){
      break
    }
    addLaunchHistory(availableNum)
    instanceIds <- c(instanceIds, instances)
  }
  instanceIds
}

getValidFargateHardware<-function(hardware){
  cpu <- hardware@cpu
  memory <- hardware@memory

  result <- list()
  validCpuNumbers <- as.numeric(names(validFargateSettings))
  validCpus <- validCpuNumbers[validCpuNumbers>=cpu]
  for(i in validCpus){
    idx <- which(validFargateSettings[[as.character(i)]]>=memory)
    if(length(idx)!=0){
      hardware@cpu <- i
      hardware@memory <- validFargateSettings[[as.character(i)]][idx[1]]
      break
    }
  }
  hardware
}

## get resource name from ARN
ECSGetResourceNames <- function(ARNs){
  unname(vapply(ARNs, ECSGetResourceName, character(1)))
}

ECSGetResourceName <- function(ARN){
  match_index <- gregexpr(":",ARN,fixed = TRUE)[[1]]
  x <- substring(ARN, match_index[5]+1)
  separator <-regexpr("/", x, fixed = TRUE)
  if(separator==-1){
    x
  }else{
    sub(".+?/","",x)
  }
}

getMaxWorkerPerContainer <- function(hardware){
  cpu <- hardware@cpu
  memory <- hardware@memory
  maxWorker <- min(floor(4096/cpu),floor(1024*30/memory))
  if(maxWorker==0){
    stop("Cannot find a fargate hardware to accommodate the CPU and memory requirement for the worker")
  }
  maxWorker
}

informUpgradedHardware<- function(fargateHardware, requiredHardware, workerNumber){
  if(fargateHardware@cpu!=requiredHardware@cpu||
     fargateHardware@memory!= requiredHardware@memory){
    message("The required Fargate hardware does not exist,\n",
            "we have upgrate it to CPU ",
            round(fargateHardware@cpu/workerNumber,3)
            ," units and memory ",
            round(fargateHardware@memory/workerNumber,3)
            ," MB")
  }
}


getTagValue <-function(tagList, tagName, tagValue, target){
  for(i in tagList){
    if(i[[tagName]]==target){
      return(i[[tagValue]])
    }
  }
  return(NULL)
}

is.empty <- function(x){
  is.null(x) || length(x)==0
}

environmentToJSON <- function(x){
  x <- x[!vapply(x, is.null, logical(1))]
  result <- list()
  for(i in seq_along(x)){
    result[[i]] <- list(name = names(x)[i], value = as.character(x[[i]]))
  }
  result
}

ListToArray <- function(x, name = "name", value = "value"){
  result <- list()
  for(i in seq_along(x)){
    result[[i]] <- list()
    result[[i]][[name]] <- names(x)[i]
    result[[i]][[value]] <- x[[i]]
  }
  result
}
ArrayToList <- function(x, name = "name", value = "value"){
  resultName <- c()
  result <- list()
  for(i in seq_along(x)){
    result[[i]] <- x[[i]][["value"]]
    resultName[i] <- x[[i]][["name"]]
  }
  names(result) <- resultName
  result
}
verbosePrint<-function(verbose, ...){
  if(verbose)
    message(...)
}

generateRandomPassword <- function(len = 26){
  paste0(letters[sample(26, len, replace = TRUE)],collapse = "")
}
