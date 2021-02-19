setClassUnion("NumOrChar",c("numeric","character"))
setClassUnion("CharOrNULL",c("NULL","character"))

.Container <- setClass(
  "Container",
  representation(
    cpu = "NumOrChar",
    memory = "NumOrChar",
    environment = "list",
    image = "character",
    exec = "CharOrNULL"
  )
)

.ECSHardware <- setClass(
  "ECSHardware",
  representation(
   type = "character",
   id = "character"
  )
)

.ECSConfig <- setClass(
  "ECSConfig",
  representation(
    server = "Container",
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
    data = "environment"
  )
)






