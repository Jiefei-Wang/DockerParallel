setMethod("initialProvider", "ECSProvider", function(provider, verbose, ...){
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
    verbosePrint(verbose, "Setting up SSH inbound permission")
    port <- 22
    ConfigInboundPermissions(provider, port)
    verbosePrint(verbose, "Inbound permission finished")
})


setMethod("initialCluster", "ANY", function(provider, cluster, verbose,...){
    # Inbound permission
    verbosePrint(verbose, "Setting up server-worker inbound permission")
    port <- cluster@cloudRuntime$serverPort
    ConfigInboundPermissions(provider, port)
    verbosePrint(verbose, "Inbound permission finished")

    # Task definition
    verbosePrint(verbose, "Setting up task defintion")
    workerImage <- cluster@cloudConfig$workerContainer@image
    if(!is.null(cluster@cloudConfig$serverContainer)){
        serverImage <-cluster@cloudConfig$serverContainer@image
    }else{
        serverImage <- NULL
    }
    configTaskDefinition(provider, workerImage, serverImage)
    verbosePrint(verbose, "Task defintion finished")
})


setMethod("runContainers", "ECSProvider",
          function(provider, container, hardware, containerNumber, verbose = FALSE, ...){

})
