setClassUnion("CharOrNULL",c("NULL","character"))

.Container <- setClass(
    "Container",
    representation(
        environment = "list",
        image = "character",
        command = "CharOrNULL",
        maxWorkers = "numeric"
    )
)

setClassUnion("ContainerOrNULL",c("NULL","Container"))

#' The instance hardware for running the docker
#'
#' The instance hardware for running the docker
#'
#' @slot cpu The CPU limitation for the docker. 1024 CPU unit
#' corresponds to 1 core.
#' @slot memory The memory limitation for the docker, the unit
#' is MB
#' @slot id The id of the hardware, the meaning of `id` depends on
#' the cloud provider.
#' @export
.CloudHardware <- setClass(
    "CloudHardware",
    representation(
        cpu = "numeric",
        memory = "numeric",
        id = "CharOrNULL"
    )
)

#' The cloud configuration
#'
#' The cloud configuration
#'
#' @slot clusterName The name of the cluster
#' @slot workerNum The required number of workers that should be
#' run on the cloud
#' @slot serverContainer The container definition for the server.
#' If the value is NULL, the server must be provided from the other source.
#' @slot workerContainer The container definition for the worker
#' @slot serverHardware The server hardware
#' @slot workerHardware The worker hardware
#'
#' @export
.CloudConfig <- setRefClass(
    "CloudConfig",
    fields = list(
        clusterName = "character",
        workerNum = "integer",
        serverContainer = "ContainerOrNULL",
        workerContainer = "Container",
        serverHardware = "CloudHardware",
        workerHardware = "CloudHardware"
    )
)

.CloudRuntime <- setRefClass(
    "CloudRuntime",
    fields = list(
        serverPublicIp = "CharOrNULL",
        serverPrivateIp = "CharOrNULL",
        serverPort = "integer",
        serverPassword = "CharOrNULL",
        workerPublicIps = "CharOrNULL",
        workerPrivateIps = "CharOrNULL",
        serverHandle = "ANY",
        workerHandles = "list"
    )
)


.DockerCluster <- setClass(
    "DockerCluster",
    representation(
        cloudProvider = "ANY",
        cloudConfig = "CloudConfig",
        cloudRuntime = "CloudRuntime"
    )
)


###########################
## ECS provider
###########################
.ECSProvider <- setRefClass(
    "ECSProvider",
    fields = list(
        clusterName = "CharOrNULL",
        serverTaskDefName = "CharOrNULL",
        workerTaskDefName = "CharOrNULL",
        securityGroupName = "CharOrNULL",
        vpcId = "CharOrNULL",
        subnetId = "CharOrNULL",
        securityGroupId = "CharOrNULL",
        internetGatewayId = "CharOrNULL",
        routeTableId = "CharOrNULL"
    )
)
