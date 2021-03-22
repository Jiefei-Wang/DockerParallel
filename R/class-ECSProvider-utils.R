
initProvider<-function(x, verbose= TRUE){
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
    if(is(x@server, "Container")){
        ports <- c(22, getRedisServerPort(x))
    }else{
        ports <- 22
    }
    ConfigInboundPermissions(x, ports)
    verbosePrint(verbose, "Inbound permission finished")
    ## Task definition
    verbosePrint(verbose, "Setting up task defintion")
    configTaskDefinition(x)
    verbosePrint(verbose, "Task defintion finished")
}

cleanupConfig <- function(x, verbose = TRUE){
    if(is.empty(x@clusterName) &&
       !is.empty(getECSCloudData(x, "clusterName"))){
        verbosePrint(verbose, "Deleting worker cluster")
        tryCatch({
            deleteCluster(getECSCloudData(x, "clusterName"))
            setECSCloudData(x, "clusterName", NULL)
        },
        error = function(e) message(e))
    }
    if(is.empty(x@VPCId) &&
       !is.empty(getECSCloudData(x, "VPCId"))){
        verbosePrint(verbose, "Deleting vpc")
        tryCatch({
            deleteVPC(getECSCloudData(x, "VPCId"))
            setECSCloudData(x, "VPCId", NULL)
        },
        error = function(e) message(e))
    }
    verbosePrint(verbose, "Deleting internet gateway")
    if(is.empty(x@internetGatewayId) &&
       !is.empty(getECSCloudData(x, "internetGatewayId"))){
        tryCatch({
            deleteInternetGateway(getECSCloudData(x, "internetGatewayId"))
            setECSCloudData(x, "internetGatewayId", NULL)
        },
        error = function(e) message(e))
    }
    invisible()
}


