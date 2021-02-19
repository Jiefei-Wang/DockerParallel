ECSDefault <- list(
  clusterName = "R-worker-cluster",
  serverTaskDefName = "R-server-task-definition",
  workerTaskDefName = "R-worker-task-definition",
  securityGroupName = "R-worker-security-group"
)

ECSFargateConfig <- function(workerNum = 1,
                             workerCPU = 0.25,
                             workerMem = 512,
                             serverCPU = 0.25,
                             serverMem = 2048,
                             serverContainer = redisServerContainer(),
                             workerContainer = redisWorkerContainer()
){
  serverContainer@cpu <- serverCPU
  serverContainer@memory <- serverMem
  workerContainer@cpu <- workerCPU
  workerContainer@memory <- workerMem
  fargate <- .ECSHardware(type = "fargate")
  serverData <- new.env(parent = emptyenv())
  serverData$clusterName <- NULL
  serverData$serverTaskDefName <- NULL
  serverData$workerTaskDefName <- NULL
  serverData$securityGroupName <- NULL
  serverData$VPCId <- NULL
  serverData$subnetId <- NULL
  serverData$securityGroupId <- NULL
  serverData$internetGatewayId <- NULL
  serverData$routeTableId <- NULL

  .ECSConfig(server = serverContainer,
             worker = workerContainer,
             workerNum = workerNum,
             workerHardware = fargate,
             serverHardware = fargate,
             data = new.env(parent = emptyenv()))
}

getECSData <- function(x, name){
  x@data[[name]]
}

setECSData <- function(x, name, value){
  x@data[[name]] <- value
}


# cloud and hardware is binded

# cloud <- ECSFargateConfig()
# addWorker(cloud, worker, hardware)
# deployContainers(cloud)
# getRunningContainer(cloud)
# getCluster(cloud)


resetCloudConfig <- function(x){
  rm(list=names(x@data),envir = x@data)
}


#' @export
setMethod(f = "show",signature = "ECSConfig",
          definition = function(object){
            cat("ECS config object\n")
            cat("Server config:\n")
            show(object@server)
            cat("Worker config\n")
            cat("  workerNum:", object@workerNum,"\n")
            show(object@worker)
            invisible(NULL)
          })

#' @export
setMethod(f = "names",signature = "ECSConfig",
          definition = function(x){
            nms <- slotNames(x)
            nms[nms != "data"]
          })

#' @export
setMethod(f = "$",signature = "ECSConfig",
          definition = function(x, name){
            x@data[[name, exact = FALSE]]
          })
#' @export
setMethod(f = "$<-",signature = "ECSConfig",
          definition = function(x, name, value){
            x@data[[name]] <- value
            x
          })
#' @export
setMethod(f = "[[",signature = "ECSConfig",
          definition = function(x, i, j, ...){
            result <- x@data[[name,...]]
            if(is.null(result)){
              result <- `@`(x, name)
            }
            result
          })
#' @export
setMethod(f = "[[<-",signature = "ECSConfig",
          definition = function(x, i, j, ...,value){
            `@`(x, i) <- value
            x@data[[i]] <- NULL
            x
          })


initConfig<-function(x, verbose= TRUE){
  ## Cluster name
  verbosePrint(verbose, "Setting up cluster")
  clusterName <- configClusterName(x)
  verbosePrint(verbose, "Cluster name: \t", clusterName)
  ## VPC
  verbosePrint(verbose, "Setting up VPC")
  VPCId <- configVPCId(x)
  verbosePrint(verbose, "VPC: \t", VPCId)
  ## subnet
  verbosePrint(verbose, "Setting up subnet")
  subnetId <- configSubnetId(x)
  verbosePrint(verbose, "Subnet id: \t", subnetId)
  ## gateway
  verbosePrint(verbose, "Setting up gateway")
  gatewayId <- configInternetGateway(x)
  verbosePrint(verbose, "Gateway: \t", gatewayId)
  ## route table
  verbosePrint(verbose, "Setting up route table")
  routeTableId <- configRouteTable(x)
  verbosePrint(verbose, "Route table: \t", routeTableId)
  ## route
  verbosePrint(verbose, "Setting up default route")
  configDefaultRoute(x)
  verbosePrint(verbose, "Default route finished")
  ## security group
  verbosePrint(verbose, "Setting up security group")
  securityGroupId <- configSecurityGroupId(x)
  verbosePrint(verbose, "Security group: ",securityGroupId)
  ## Inbound permission
  verbosePrint(verbose, "Setting up inbound permission")
  if(is.null(x@server@environment$redisPort)){
    redisPort <- 6379
  }else{
    redisPort <- as.numeric(x@server@environment$redisPort)
  }
  ports <- c(22,redisPort)
  ConfigInboundPermissions(x, ports)
  verbosePrint(verbose, "Inbound permission finished")
  ## Task definition
  verbosePrint(verbose, "Setting up task defintion")
  configTaskDefinition(x)
  verbosePrint(verbose, "Task defintion finished")
}

cleanupConfig <- function(x, verbose = TRUE){
  if(is.empty(x@clusterName) &&
     !is.empty(getECSData(x, "clusterName"))){
    verbosePrint(verbose, "Deleting worker cluster")
    tryCatch({
      deleteCluster(getECSData(x, "clusterName"))
      setECSData(x, "clusterName", NULL)
    },
    error = function(e) message(e))
  }
  if(is.empty(x@VPCId) &&
     !is.empty(getECSData(x, "VPCId"))){
    verbosePrint(verbose, "Deleting vpc")
    tryCatch({
      deleteVPC(getECSData(x, "VPCId"))
      setECSData(x, "VPCId", NULL)
    },
    error = function(e) message(e))
  }
  verbosePrint(verbose, "Deleting internet gateway")
  if(is.empty(x@internetGatewayId) &&
     !is.empty(getECSData(x, "internetGatewayId"))){
    tryCatch({
      deleteInternetGateway(getECSData(x, "internetGatewayId"))
      setECSData(x, "internetGatewayId", NULL)
    },
    error = function(e) message(e))
  }
  invisible()
}


