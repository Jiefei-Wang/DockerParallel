setClassUnion("NumOrChar",c("numeric","character"))
setClassUnion("CharOrNULL",c("NULL","character"))

.Container <- setClass(
    "Container",
    representation(
        environment = "list",
        image = "character",
        command = "CharOrNULL"
    )
)

## dedicated redis server
.DedicatedServer <- setClass(
    "DedicatedServer",
    representation(
        IP = "character",
        port = "integer",
        password = "CharOrNULL"
    )
)

setClassUnion("ContainerOrDedicatedServer",c("Container","DedicatedServer"))


.ECSHardware <- setClass(
    "ECSHardware",
    representation(
        CPU = "numeric",
        memory = "numeric",
        type = "character",
        id = "CharOrNULL"
    )
)

.ECSConfig <- setClass(
    "ECSConfig",
    representation(
        server = "ContainerOrDedicatedServer",
        worker = "Container",
        workerNum = "numeric",
        workerHardware = "ECSHardware",
        serverHardware = "ECSHardware",
        clusterName = "character",
        serverTaskDefName = "character",
        workerTaskDefName = "character",
        securityGroupName = "character",
        VPCId = "character",
        subnetId = "character",
        securityGroupId = "character",
        internetGatewayId = "character",
        routeTableId = "character",
        cloudData = "environment",
        clusterData = "environment"
    )
)






