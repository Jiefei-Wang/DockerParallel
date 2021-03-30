setClassUnion("CharOrNULL",c("NULL","character"))
setClassUnion("IntOrNULL",c("NULL","integer"))


.Container <- setClass(
    "Container",
    slots = list(
        name = "CharOrNULL",
        environment = "list",
        image = "character",
        command = "CharOrNULL",
        maxWorkers = "integer"
    )
)

setClassUnion("ContainerOrNULL",c("NULL","Container"))

#' The root class of the cloud provider
#'
#' The root class of the cloud provider
#'
#' @export
.CloudProvider <- setRefClass("CloudProvider")

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
    slots = list(
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
#' @slot workerNumber The required number of workers that should be
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
        workerNumber = "integer",
        serverContainer = "ContainerOrNULL",
        workerContainer = "Container",
        serverHardware = "CloudHardware",
        workerHardware = "CloudHardware",
        serverPort = "IntOrNULL",
        serverPassword = "CharOrNULL",
        serverWorkerSameNAT = "logical",
        serverClientSameNAT = "logical"
    )
)

.CloudRuntime <- setRefClass(
    "CloudRuntime",
    fields = list(
        serverHandle = "ANY",
        workerHandles = "list",
        workerPerHandle = "integer",
        serverPublicIp = "CharOrNULL",
        serverPrivateIp = "CharOrNULL"
    )
)


.DockerCluster <- setClass(
    "DockerCluster",
    slots = list(
        cloudProvider = "CloudProvider",
        cloudConfig = "CloudConfig",
        cloudRuntime = "CloudRuntime",
        settings = "environment"
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
        routeTableId = "CharOrNULL",
        workerPublicIpEnable = "logical",
        clusterNameVerified = "logical",
        serverTaskDefNameVerified = "logical",
        workerTaskDefNameVerified = "logical",
        securityGroupVerified = "logical",
        vpcVerified = "logical",
        subnetVerified = "logical",
        internetGatewayVerified = "logical",
        routeTableVerified = "logical",
        routeVerified = "logical",
        inboundPermissionVerified = "logical",
        initialized = "logical"
    ),
    contains = "CloudProvider"
)
