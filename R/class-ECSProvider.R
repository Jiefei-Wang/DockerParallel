ECSProvider <- function(clusterName = "R-worker-cluster",
                        taskDefName = "R-DockerParallel-task-definition",
                        securityGroupName = "R-parallel-security-group",
                        vpcId = NULL,
                        subnetId = NULL,
                        securityGroupId = NULL,
                        internetGatewayId = NULL,
                        routeTableId = NULL,
                        workerPublicIpEnable = TRUE){
    .ECSProvider$new(
        clusterName = clusterName,
        taskDefName = taskDefName,
        securityGroupName = securityGroupName,
        vpcId = vpcId,
        subnetId = subnetId,
        securityGroupId = securityGroupId,
        internetGatewayId = internetGatewayId,
        routeTableId = routeTableId,
        workerPublicIpEnable=workerPublicIpEnable,
        clusterNameVerified = FALSE,
        taskDefNameVerified = FALSE,
        securityGroupVerified = FALSE,
        vpcVerified = FALSE,
        subnetVerified = FALSE,
        internetGatewayVerified = FALSE,
        routeTableVerified = FALSE,
        routeVerified = FALSE,
        inboundPermissionVerified = FALSE
    )
}

.ECSProvider$methods(
    show = function(){
        cat("Cluster name:        ", .self$clusterName, "\n")
        cat("Task definition:     ", .self$taskDefName, "\n")
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

