setMethod("initialProvider", "ECSProvider", function(provider, cluster, verbose, ...){
    ## Cluster name
    verbosePrint(verbose, "Setting up cluster")
    clusterName <- configClusterName(provider)
    verbosePrint(verbose, "Cluster name: \t", clusterName)
    ## VPC
    verbosePrint(verbose, "Setting up VPC")
    VPCId <- configVpcId(provider)
    verbosePrint(verbose, "VPC: \t", VPCId)
    ## subnet
    verbosePrint(verbose, "Setting up subnet")
    subnetId <- configSubnetId(provider)
    verbosePrint(verbose, "Subnet id: \t", subnetId)
    ## gateway
    verbosePrint(verbose, "Setting up gateway")
    gatewayId <- configInternetGateway(provider)
    verbosePrint(verbose, "Gateway: \t", gatewayId)
    ## route table
    verbosePrint(verbose, "Setting up route table")
    routeTableId <- configRouteTable(provider)
    verbosePrint(verbose, "Route table: \t", routeTableId)
    ## route
    verbosePrint(verbose, "Setting up default route")
    configDefaultRoute(provider)
    verbosePrint(verbose, "Default route finished")
    ## security group
    verbosePrint(verbose, "Setting up security group")
    securityGroupId <- configSecurityGroup(provider)
    verbosePrint(verbose, "Security group: ",securityGroupId)
    # Inbound permission
    verbosePrint(verbose, "Setting up SSH and server-worker inbound permission")
    port <- c(22, cluster@cloudConfig$serverPort)
    ConfigInboundPermissions(provider, port)
    verbosePrint(verbose, "Inbound permission finished")
    # Task definition
    verbosePrint(verbose, "Setting up task defintion")
    configTaskDefinition(provider)
    verbosePrint(verbose, "Task defintion finished")
})




setMethod("runServer", "ECSProvider",
          function(provider, cluster, container, hardware, verbose = FALSE, ...){
              verbosePrint(verbose>0, "Deploying server container")
              fargateHardware <- getValidFargateHardware(hardware)
              if(verbose){
                  informUpgradedHardware(fargateHardware, hardware, 1)
              }
              instanceId <- ecsTaskScheduler(provider, container, fargateHardware, 1, TRUE)
              if(is.null(instanceId)){
                  stop("Fail to deploy the ECS container, something is wrong")
              }
              instanceId
          })
setMethod("runWorkers", "ECSProvider",
          function(provider, cluster, container, hardware, workerNumber, verbose = FALSE, ...){
              verbosePrint(verbose>0, "Deploying server container")

              instanceIds <- c()
              maxWorkers <- getMaxWorkerPerContainer(hardware)
              maxWorkers <- min(container@maxWorkers, maxWorkers)
              ## run the containers which have the maximum worker number
              containerWithMaxWorker <- floor(workerNumber/maxWorkers)
              if(containerWithMaxWorker>0){

                  instances <- ecsRunWorkers(
                      provider=provider,
                      container=container,
                      hardware=hardware,
                      containerNumber=containerWithMaxWorker,
                      workerPerContainer=maxWorkers,
                      verbose=verbose
                  )
                  if(length(instances)!= containerWithMaxWorker){
                      stopTasks(provider$clusterName, instances)
                      stop("Fail to deploy the ECS container, something is wrong")
                  }
                  instanceIds<-c(instanceIds, instances)
              }
              ## Run the container which does not have the maximum worker number
              lastContainerWorkerNum <- workerNumber - maxWorkers*containerWithMaxWorker
              if(lastContainerWorkerNum!=0){
                  instance <- ecsRunWorkers(
                      provider=provider,
                      container=container,
                      hardware=hardware,
                      containerNumber=1,
                      workerPerContainer=lastContainerWorkerNum,
                      verbose=verbose)
                  instanceIds <-c (instanceIds, instance)
                  if(length(instance)!=1){
                      stopTasks(provider$clusterName, instance)
                      stop("Fail to deploy the ECS container, something is wrong")
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

setMethod("getClusterIp", "ECSProvider",
          function(provider, serverHandle, verbose = FALSE, ...){
              while(TRUE){
                  taskInfo <- getTaskDetails(provider$clusterName,
                                             taskIds = serverHandle,
                                             getIP = TRUE)
                  if(taskInfo$status=="STOPPED"){
                      stop("The server has been stopped")
                  }
                  if(taskInfo$publicIP!=""){
                      break
                  }
              }
              taskInfo$publicIP
          }
)


setMethod("instanceAlive", "ECSProvider", function(provider, instanceHandles, verbose = FALSE, ...){
    uniqueHandles <- unique(instanceHandles)
    taskInfo <- getTaskDetails(provider$clusterName, taskIds = uniqueHandles)
    instanceStatus <- taskInfo$status != "STOPPED"
    result <- rep(FALSE, length(instanceHandles))
    for(i in uniqueHandles){
        result[instanceHandles==i] <- instanceStatus[i]
    }
    result
})


setMethod("killInstances", "ECSProvider", function(provider, instanceHandles, verbose = FALSE, ...){
    stopTasks(provider$clusterName, taskIds = unique(instanceHandles))
    rep(TRUE, length(instanceHandles))
})




