ECSfilterList <- list(`tag:docker-parallel-tag`="docker-parallel-tag")
ECSTagTemplate <- list(
    list(ResourceType=NULL,
         Tag = list(
             list(Key= "docker-parallel-tag", Value = "docker-parallel-tag")
         )
    )
)

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

