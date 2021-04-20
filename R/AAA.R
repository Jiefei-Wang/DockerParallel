setClassUnion("CharOrNULL",c("NULL","character"))
setClassUnion("IntOrNULL",c("NULL","integer"))

#' The root class of the container
#'
#' @field name Character, the optional name of a container
#' @field maxWorkerNum Integer, the maximum worker number in a container
#' @field environment List, the environment variables in the container
#' @field image Character, the container image
#' @exportClass DockerContainer
.DockerContainer <- setRefClass(
    "DockerContainer",
    fields = list(
        name = "CharOrNULL",
        maxWorkerNum = "integer",
        environment = "list",
        image = "character"
    )
)

setClassUnion("DockerContainerOrNULL",c("NULL","DockerContainer"))

#' The root class of the cloud provider
#'
#' The root class of the cloud provider
#'
#' @exportClass CloudProvider
.CloudProvider <- setRefClass("CloudProvider")

#' The hardware for running the docker
#'
#' The hardware for running the docker
#'
#' @slot cpu Numeric, the CPU limitation for the docker. 1024 CPU unit
#' corresponds to 1 core.
#' @slot memory Numeric, the memory limitation for the docker, the unit
#' is MB
#' @slot id Character or NULL, the id of the hardware, the meaning of `id` depends on
#' the cloud provider.
#' @exportClass DockerHardware
.DockerHardware <- setClass(
    "DockerHardware",
    slots = list(
        cpu = "numeric",
        memory = "numeric",
        id = "CharOrNULL"
    )
)


#' The cloud configuration
#'
#' The cloud configuration. It is a class purely for storing the information
#' for the cloud.
#' The values in `CloudConfig` in a cluster can be accessed by the getter function
#' which starts with the prefix `.get`(e.g. `.getJobQueueName(cluster)`).
#'
#' @field jobQueueName Character, the name of the job queue
#' @field workerNumber Integer, the required number of workers that should be
#' run on the cloud
#' @field serverHardware DockerHardware, the server hardware
#' @field workerHardware DockerHardware, the worker hardware
#' @field serverPort Integer or NULL, the port that will be used by the worker
#' to connect with the server
#' @field serverPassword Character or NULL, the server password
#' @field serverWorkerSameLAN Logical, whether the server and workers are behind
#' the same router
#' @field serverClientSameLAN Logical, whether the server and client are behind
#' the same router
.CloudConfig <- setRefClass(
    "CloudConfig",
    fields = list(
        jobQueueName = "character",
        workerNumber = "integer",
        serverHardware = "DockerHardware",
        workerHardware = "DockerHardware",
        serverPort = "IntOrNULL",
        serverPassword = "CharOrNULL",
        serverWorkerSameLAN = "logical",
        serverClientSameLAN = "logical"
    )
)

#' The cloud runtime
#'
#' The cloud runtime. It is a class purely for storing the runtime information
#' for the cloud.
#' The values in `CloudRuntime` in a cluster can be accessed by the getter function
#' which starts with the prefix `.get`(e.g. `.getServerHandle(cluster)`).
#'
#' @field serverHandle Any data type, the server handle that can be recognized by the
#' cloud provider.
#' @field workerHandles A list object for internal use only.
#' Please call `.getWorkerHandles(cluster)` to access the worker handles
#' @field workerPerHandle An internal counter. Please call `.getWorkerHandles(cluster)`
#' to access the worker handles
#' @field serverPublicIp Character or NULL, the server public IP
#' @field serverPrivateIp Character or NULL, the server private IP
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

#' The docker cluster class
#'
#' The docker cluster class.
#' The values in the cluster can be accessed by the getter or setter function
#' which starts with the prefix `.get` or `.set`(e.g. `.getJobQueueName(cluster)`).
#'
#' @slot cloudProvider CloudProvider
#' @slot cloudConfig CloudConfig
#' @slot serverContainer The container definition for the server.
#' If the value is NULL, the server must be provided from the other source.
#' @slot workerContainer The container definition for the worker
#' @slot cloudRuntime CloudRuntime
#' @slot settings Environment, the cluster settings
.DockerCluster <- setClass(
    "DockerCluster",
    slots = list(
        cloudProvider = "CloudProvider",
        cloudConfig = "CloudConfig",
        serverContainer = "DockerContainerOrNULL",
        workerContainer = "DockerContainer",
        cloudRuntime = "CloudRuntime",
        settings = "environment"
    )
)



##################################
##          ClusterMethodGetter
##################################
#' An utility class
#'
#' An utility class for exporting the APIs from the cloud provider and
#' container.
.ClusterMethodGetter <- setClass("ClusterMethodGetter",
                                 slots = list(
                                     cluster = "DockerCluster",
                                     object = "ANY"
                                 )
)
