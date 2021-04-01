ECSCloudProvider <- function(clusterName = "R-worker-cluster",
                        serverTaskDefName = "R-server-task-definition",
                        workerTaskDefName = "R-worker-task-definition",
                        securityGroupName = "R-parallel-security-group",
                        vpcId = NULL,
                        subnetId = NULL,
                        securityGroupId = NULL,
                        internetGatewayId = NULL,
                        routeTableId = NULL,
                        workerPublicIpEnable = TRUE){
    .ECSCloudProvider$new(
        clusterName = clusterName,
        serverTaskDefName = serverTaskDefName,
        workerTaskDefName = workerTaskDefName,
        securityGroupName = securityGroupName,
        vpcId = vpcId,
        subnetId = subnetId,
        securityGroupId = securityGroupId,
        internetGatewayId = internetGatewayId,
        routeTableId = routeTableId,
        workerPublicIpEnable=workerPublicIpEnable,
        clusterNameVerified = FALSE,
        serverTaskDefNameVerified = FALSE,
        workerTaskDefNameVerified = FALSE,
        securityGroupVerified = FALSE,
        vpcVerified = FALSE,
        subnetVerified = FALSE,
        internetGatewayVerified = FALSE,
        routeTableVerified = FALSE,
        routeVerified = FALSE,
        inboundPermissionVerified = FALSE,
        initialized = FALSE
    )
}

.ECSCloudProvider$methods(
    show = function(){
        cat("Cluster name:        ", .self$clusterName, "\n")
        cat("Server task definition:     ", .self$serverTaskDefName, "\n")
        cat("Worker task definition:     ", .self$workerTaskDefName, "\n")
        cat("Security group name: ", .self$securityGroupName, "\n")

        if(!is.null(.self$vpcId)){
            cat("VPC ID:              ", .self$vpcId, "\n")
        }
        if(!is.null(.self$subnetId)){
            cat("Subnet ID:           ", .self$subnetId, "\n")
        }
        if(!is.null(.self$securityGroupId)){
            cat("Security group ID:   ", .self$securityGroupId, "\n")
        }
        if(!is.null(.self$internetGatewayId)){
            cat("Internet gateway ID: ", .self$internetGatewayId, "\n")
        }
        if(!is.null(.self$routeTableId)){
            cat("Route table ID:      ", .self$routeTableId, "\n")
        }
        invisible(NULL)
    }
)

