#'
#'
#'
#'
#' ECSFargateConfig <- function(workerNum = 1,
#'                              workerCPU = 0.25,
#'                              workerMem = 512,
#'                              serverCPU = 0.25,
#'                              serverMem = 2048,
#'                              server = Container("dockerparallel/parallel-redis-server"),
#'                              worker = Container("dockerparallel/parallel-redis-worker")
#' ){
#'     serverHardware <- ECSHardware(type = "fargate",
#'                                    CPU = serverCPU,
#'                                    memory = serverMem)
#'     workerHardware <- ECSHardware(type = "fargate",
#'                                    CPU = workerCPU,
#'                                    memory = workerMem)
#'     cloudData <- new.env(parent = emptyenv())
#'     ## contains:
#'     ## serverWorkerIP, serverClientIP, serverPort,
#'     ## serverPassword, serverInstanceId
#'     ## queue, workerIPs, workerInstanceIds, workerNums
#'     clusterData <- new.env(parent = emptyenv())
#'
#'     .ECSConfig(server = server,
#'                worker = worker,
#'                workerNum = workerNum,
#'                workerHardware = workerHardware,
#'                serverHardware = serverHardware,
#'                cloudData = cloudData,
#'                clusterData = clusterData)
#' }
#'
#'
#'
#'
#' #' @export
#' setMethod(f = "show",signature = "ECSConfig",
#'           definition = function(object){
#'               cat("ECS config object\n")
#'               cat("Server config:\n")
#'               cat("  CPU:       ", object@serverHardware@CPU, "cores\n")
#'               cat("  Memory:    ", object@serverHardware@memory, "MB\n")
#'               cat("  Image:     ", object@server@image, "\n")
#'               cat("Worker config:\n")
#'               cat("  Worker num:", object@workerNum,"\n")
#'               cat("  CPU:       ", object@workerHardware@CPU, "cores\n")
#'               cat("  Memory:    ", object@workerHardware@memory, "MB\n")
#'               cat("  Image:     ", object@worker@image, "\n")
#'               cat("Use <$more()> to see the cloud configuration")
#'               invisible(NULL)
#'           })
#'
#'
#' #' @export
#' setMethod(f = "names",signature = "ECSConfig",
#'           definition = function(x){
#'               nms <- slotNames(x)
#'               c(nms[!nms %in% c("cloudData", "clusterData")], "more")
#'           })
#'
#' #' @export
#' setMethod(f = "$",signature = "ECSConfig",
#'           definition = function(x, name){
#'               if(name != "more"){
#'                   x[[name, exact = FALSE]]
#'               }else{
#'                   function()showDetails(x)
#'               }
#'           })
#' #' @export
#' setMethod(f = "$<-",signature = "ECSConfig",
#'           definition = function(x, name, value){
#'               if(name != "more"){
#'                   x[[name, exact = FALSE]] <- value
#'                   x
#'               }else{
#'                   stop("`@more` is not assignable")
#'               }
#'           })
#' #' @export
#' setMethod(f = "[[",signature = "ECSConfig",
#'           definition = function(x, i, j, ...){
#'               ## Do the partial match first
#'               args <- list(...)
#'               if("exact"%in%names(args)&& !args$exact){
#'                   idx <- pmatch(i, slotNames(x))
#'                   if(!is.na(idx)){
#'                       i <- slotNames(x)[[idx]]
#'                   }
#'               }
#'               result <- getECSCloudData(x, i)
#'               if(is.null(result)){
#'                   result <- do.call("@",list(x,i))
#'               }
#'               if(is.empty(result)){
#'                   if(i%in%names(ECSDefault)){
#'                       result <- ECSDefault[[i]]
#'                   }
#'               }
#'               result
#'           })
#' #' @export
#' setMethod(f = "[[<-",signature = "ECSConfig",
#'           definition = function(x, i, j, ...,value){
#'               x <- do.call("@<-",list(x,i,value))
#'               setECSCloudData(x, i, NULL)
#'               if(i=="routeTableId"){
#'                   setECSCloudData(x, "defaultRouteInitialized", NULL)
#'               }
#'               if(i%in%c("securityGroupId", "securityGroupName")){
#'                   setECSCloudData(x, "securityGroupId", NULL)
#'                   setECSCloudData(x, "securityGroupName", NULL)
#'                   setECSCloudData(x, "inboundPermissionInitialized", NULL)
#'               }
#'               if(i=="server"){
#'                   setECSCloudData(x, "inboundPermissionInitialized", NULL)
#'               }
#'               x
#'           })
#'
#'
