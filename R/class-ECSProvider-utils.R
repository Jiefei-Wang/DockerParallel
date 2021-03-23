ECSfilterList <- list(`tag:docker-parallel-tag`="docker-parallel-tag")
ECSTagTemplate <- list(
    list(ResourceType=NULL,
         Tag = list(
             list(Key= "docker-parallel-tag", Value = "docker-parallel-tag")
         )
    )
)

initCluster <- function(x, cluster, verbose = FALSE){
    # Inbound permission
    verbosePrint(verbose, "Setting up server-worker inbound permission")
    port <- cluster@cloudRuntime$serverPort
    ConfigInboundPermissions(x, port)
    verbosePrint(verbose, "Inbound permission finished")

    # Task definition
    verbosePrint(verbose, "Setting up task defintion")
    workerImage <- cluster@cloudConfig$workerContainer@image
    if(!is.null(cluster@cloudConfig$serverContainer)){
        serverImage <-cluster@cloudConfig$serverContainer@image
    }else{
        serverImage <- NULL
    }
    configTaskDefinition(x, workerImage, serverImage)
    verbosePrint(verbose, "Task defintion finished")
}






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
    verbosePrint(verbose, "Deleting internet gateway")
    if(x$internetGatewayVerified){
        tryCatch({
            deleteInternetGateway(x$internetGatewayId)
            x$internetGatewayVerified <- FALSE
        },
        error = function(e) message(e))
    }
    invisible()
}

