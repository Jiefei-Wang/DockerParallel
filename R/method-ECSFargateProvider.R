#' @export
setMethod("initializeProvider", "ECSFargateProvider", function(provider, cluster, verbose){
    .initializeECS(provider, cluster, verbose)
})

.initializeECS <- function(provider, cluster, verbose){
    if(!provider$initialized){
        if(!cluster$isServerRunning()){
            .setServerWorkerSameLAN(cluster, TRUE)
        }
        verbosePrint(verbose, "Initializing the ECS provider")
        ## Cluster name
        verbosePrint(verbose>1, "\tSetting up cluster")
        clusterName <- configClusterName(provider)
        verbosePrint(verbose>1, "\tCluster name: \t", clusterName)
        ## VPC
        verbosePrint(verbose>1, "\tSetting up VPC")
        VPCId <- configVpcId(provider)
        verbosePrint(verbose>1, "\tVPC: \t", VPCId)
        ## subnet
        verbosePrint(verbose>1, "\tSetting up subnet")
        subnetId <- configSubnetId(provider)
        verbosePrint(verbose>1, "\tSubnet id: \t", subnetId)
        ## gateway
        verbosePrint(verbose>1, "\tSetting up gateway")
        gatewayId <- configInternetGateway(provider)
        verbosePrint(verbose>1, "\tGateway: \t", gatewayId)
        ## route table
        verbosePrint(verbose>1, "\tSetting up route table")
        routeTableId <- configRouteTable(provider)
        verbosePrint(verbose>1, "\tRoute table: \t", routeTableId)
        ## route
        verbosePrint(verbose>1, "\tSetting up default route")
        configDefaultRoute(provider)
        verbosePrint(verbose>1, "\tDefault route finished")
        ## security group
        verbosePrint(verbose>1, "\tSetting up security group")
        securityGroupId <- configSecurityGroup(provider)
        verbosePrint(verbose>1, "\tSecurity group: ",securityGroupId)
        # Inbound permission
        verbosePrint(verbose>1, "\tSetting up SSH and server-worker inbound permission")
        port <- c(22, cluster@cloudConfig$serverPort)
        ConfigInboundPermissions(provider, port)
        verbosePrint(verbose>1, "\tInbound permission finished")
        # Task definition
        verbosePrint(verbose>1, "\tSetting up task defintion")
        configTaskDefinition(provider, cluster)
        verbosePrint(verbose>1, "\tTask defintion finished")
        provider$initialized <- TRUE
    }
}


#' @export
setMethod("runDockerServer", "ECSFargateProvider",
          function(provider, cluster, container, hardware, verbose = 0L){
              verbosePrint(verbose>0, "Deploying server container")

              ## save the cloud config to the running instance
              encodedCloudConfig <- encodeCloudConfig(.getCloudConfig(cluster))
              container <- container$copy()
              container$environment[["ECSFargateCloudConfigInfo"]] <- encodedCloudConfig

              fargateHardware <- getValidFargateHardware(hardware)
              if(verbose){
                  informUpgradedHardware(fargateHardware, hardware, 1)
              }
              taskDefName <- provider$serverTaskDefName
              instanceId <- ecsTaskScheduler(
                  provider = provider,
                  taskDefName=taskDefName,
                  container=container,
                  hardware= fargateHardware,
                  containerNum=1,
                  publicIpEnable=TRUE
              )

              if(is.null(instanceId)){
                  stop("Fail to deploy the ECS container, something is wrong")
              }
              instanceId
          })


## The worker container will set the environment variable:
## ECSFargateCloudJobQueueName
## ECSFargateCloudServerIP
## ECSFargateCloudWorkerNumber
##

#' @export
setMethod("runDockerWorkers", "ECSFargateProvider",
          function(provider, cluster, container, hardware, workerNumber, verbose = 0L){
              verbosePrint(verbose>0, "Deploying worker container")
              instanceIds <- c()
              maxWorkers <- getMaxWorkerPerContainer(hardware)
              maxWorkers <- min(container$maxWorkerNum, maxWorkers)
              ## run the containers which have the maximum worker number
              containerWithMaxWorker <- floor(workerNumber/maxWorkers)
              if(containerWithMaxWorker>0){
                  instances <- ecsRunWorkers(
                      provider=provider,
                      cluster=cluster,
                      container=container,
                      hardware=hardware,
                      containerNumber=containerWithMaxWorker,
                      workerPerContainer=maxWorkers,
                      verbose=verbose
                  )
                  if(length(instances)!= containerWithMaxWorker){
                      stopTasks(provider$clusterName, instances)
                      stop("Fail to deploy the ECS worker container, something is wrong")
                  }
                  instanceIds<-c(instanceIds, instances)
              }
              ## Run the container which does not have the maximum worker number
              lastContainerWorkerNum <- workerNumber - maxWorkers*containerWithMaxWorker
              if(lastContainerWorkerNum!=0){
                  instance <- ecsRunWorkers(
                      provider=provider,
                      cluster=cluster,
                      container=container,
                      hardware=hardware,
                      containerNumber=1,
                      workerPerContainer=lastContainerWorkerNum,
                      verbose=verbose)
                  instanceIds <-c (instanceIds, instance)
                  if(length(instance)!=1){
                      stopTasks(provider$clusterName, instance)
                      stop("Fail to deploy the ECS worker container, something is wrong")
                  }
              }

              ## Repeat the instance id worker number times.
              workerNumberPerContainer <- rep(maxWorkers, length(instanceIds))
              if(lastContainerWorkerNum!=0){
                  workerNumberPerContainer[length(instanceIds)] <- lastContainerWorkerNum
              }
              repeatVector(instanceIds, workerNumberPerContainer)
          }
)
#' @export
setMethod("getDockerInstanceIps", "ECSFargateProvider",
          function(provider, instanceHandles, verbose = 0L){
              while(TRUE){
                  taskInfo <- getTaskDetails(provider$clusterName,
                                             taskIds = instanceHandles,
                                             getIP = TRUE)
                  if(taskInfo$publicIp!=""&&taskInfo$privateIp!=""){
                      break
                  }
                  if(taskInfo$status=="STOPPED"){
                      stop("The server has been stopped")
                  }
              }
              taskInfo
          }
)

#' @export
setMethod("getDockerInstanceStatus", "ECSFargateProvider",
          function(provider, instanceHandles, verbose = 0L){
              uniqueHandles <- unique(instanceHandles)
              taskInfo <- getTaskDetails(provider$clusterName, taskIds = uniqueHandles)
              instanceStatus <- rep("initializing", length(uniqueHandles))
              instanceStatus[taskInfo$status == "RUNNING"] <- "running"
              instanceStatus[taskInfo$status == "STOPPED"] <- "stopped"
              result <- rep("", length(instanceHandles))
              for(i in seq_along(uniqueHandles)){
                  idx <- vapply(instanceHandles, function(x) identical(x, uniqueHandles[[i]]),logical(1))
                  result[idx] <- instanceStatus[i]
              }
              result
          }
)

#' @export
setMethod("killDockerInstances", "ECSFargateProvider",
          function(provider, instanceHandles, verbose = 0L){
              stopTasks(provider$clusterName, taskIds = unique(instanceHandles))
              rep(TRUE, length(instanceHandles))
          }
)


#' @export
setMethod("dockerClusterExists", "ECSFargateProvider",
          function(provider, cluster, verbose = 0L){
              serverHandles <- listRunningServer(cluster)
              serverInfo <- findServerInfo(cluster, serverHandles)
              !is.null(serverInfo)
          }
)


#' @export
setMethod("reconnectDockerCluster", "ECSFargateProvider",
          function(provider, cluster, verbose = 0L){
              cloudConfig <- .getCloudConfig(cluster)
              serverHandles <- listRunningServer(cluster)
              serverInfo <- findServerInfo(cluster, serverHandles)
              if(!is.null(serverInfo)){
                  ## Set the cloud config
                  serverHandle <- serverInfo$handle
                  cloudConfigValue <- serverInfo$cloudConfigValue
                  lapply(names(cloudConfigValue), function(i)
                      cloudConfig$field(i, cloudConfigValue[[i]]))

                  ## Set the server runtime
                  serverDetails <-
                      getTaskDetails(provider$clusterName,taskIds = serverHandle, getIP = TRUE)
                  .setServerHandle(cluster, serverHandle)
                  .setServerPublicIp(cluster, serverDetails$publicIp)
                  .setServerPrivateIp(cluster, serverDetails$privateIp)

                  ## Set the worker runtime
                  workerHandles <- findWorkerHandles(cluster, cloudConfigValue$jobQueueName)
                  .addWorkerHandles(cluster, workerHandles)
                  .setWorkerNumber(cluster, length(workerHandles))
              }
          }
)


