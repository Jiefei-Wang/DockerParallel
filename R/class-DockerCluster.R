dockerCluster <- function(cloudProvider = ECSProvider(),
                          cloudConfig = CloudConfig(),
                          cloudRuntime = CloudRuntime(),
                          verbose = FALSE){
    if(!is.null(cloudRuntime$serverPrivateIp)){
        cloudConfig$serverContainer = NULL
    }
    .DockerCluster(cloudProvider=cloudProvider,
                   cloudConfig=cloudConfig,
                   cloudRuntime=cloudRuntime,
                   verbose= verbose)
}
