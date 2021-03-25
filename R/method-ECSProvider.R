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


setMethod("runServerContainers", "ECSProvider",
          function(provider, container, hardware, verbose = FALSE, ...){
              hardware <- getValidFargateHardware(hardware)


})
setMethod("runWorkerContainers", "ECSProvider",
          function(provider, container, hardware, containerNumber, verbose = FALSE, ...){


})









