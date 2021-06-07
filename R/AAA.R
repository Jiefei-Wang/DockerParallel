#' The root class of the container
#'
#' @field name Character(1) or character(0), the optional name of a container.
#' @field backend Character(1), the backend used by the parallel package
#' @field maxWorkerNum Integer(1), the maximum worker number in a container.
#' @field environment List, the environment variables in the container.
#' @field image Character(1), the container image.
#' @exportClass DockerContainer
.DockerContainer <- setRefClass(
    "DockerContainer",
    fields = list(
        name = "character",
        backend = "character",
        maxWorkerNum = "integer",
        environment = "list",
        image = "character"
    )
)

# setClassUnion("DockerContainerOrNULL",c("NULL","DockerContainer"))

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
#' @slot cpu Numeric(1), the CPU limitation for the docker. 1024 CPU unit
#' corresponds to 1 core.
#' @slot memory Numeric(1), the memory limitation for the docker, the unit
#' is MB.
#' @slot id character(1) or character(0), the id of the hardware, the meaning of
#' `id` depends on the cloud provider.
#' @exportClass DockerHardware
.DockerHardware <- setClass(
    "DockerHardware",
    slots = list(
        cpu = "numeric",
        memory = "numeric",
        id = "character"
    )
)


#' The cloud configuration
#'
#' The cloud configuration. It is a class purely for storing the information
#' for the cloud.
#' The values in `CloudConfig` in a cluster can be accessed by the getter function
#' which starts with the prefix `.get`(e.g. `.getJobQueueName(cluster)`).
#'
#' @field jobQueueName Character(1), the name of the job queue.
#' @field expectedWorkerNumber Integer(1), the expected number of workers that should be
#' run on the cloud.
#' @field serverHardware DockerHardware, the server hardware.
#' @field workerHardware DockerHardware, the worker hardware.
#' @field serverPort Integer(1) or integer(0), the port that will be used by the worker
#' to connect with the server.
#' @field serverPassword Character(1) or character(0), the server password.
#' @field serverWorkerSameLAN Logical(1), whether the server and workers are behind
#' the same router.
#' @field serverClientSameLAN Logical(1), whether the server and client are behind
#' the same router.
.CloudConfig <- setRefClass(
    "CloudConfig",
    fields = list(
        jobQueueName = "character",
        expectedWorkerNumber = "integer",
        serverHardware = "DockerHardware",
        workerHardware = "DockerHardware",
        serverPort = "integer",
        serverPassword = "character",
        serverWorkerSameLAN = "logical",
        serverClientSameLAN = "logical"
    )
)

#' The cloud runtime
#'
#' The cloud runtime. It is a class purely for storing the runtime information
#' for the cloud.
#' The values in `CloudRuntime` in a cluster can be accessed by the getter function
#' which starts with the prefix `.get`(e.g. `.getServerPublicIp(cluster)`).
#' @field serverFromOtherSource Logical(1), whether the server is provided outside of
#' cluster. If `TRUE`, the cluster will not try to stop the server when it is stopped.
#' @field serverPublicIp Character(1) or character(0), the server public IP.
#' @field serverPublicPort Integer(1) or integer(0), the server public port.
#' @field serverPrivateIp Character(1) or character(0), the server private IP.
#' @field serverPrivatePort Integer(1) or integer(0), the server private port.
#' @field runningWorkerNumber Integer(1), the current initializing workers.
#' @field runningWorkerNumber Integer(1), the current running workers.
.CloudRuntime <- setRefClass(
    "CloudRuntime",
    fields = list(
        serverFromOtherSource = "logical",
        serverPublicIp = "character",
        serverPublicPort = "integer",
        serverPrivateIp = "character",
        serverPrivatePort = "integer",
        initializingWorkerNumber = "integer",
        runningWorkerNumber = "integer"
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
#' @slot workerContainer The container definition for the worker
#' @slot cloudRuntime CloudRuntime
#' @slot settings Environment, the cluster settings
.DockerCluster <- setClass(
    "DockerCluster",
    slots = list(
        cloudProvider = "CloudProvider",
        cloudConfig = "CloudConfig",
        serverContainer = "DockerContainer",
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
