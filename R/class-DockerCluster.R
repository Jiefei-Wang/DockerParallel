dockerCluster <- function(cloudProvider = ECSProvider(),
                          cloudConfig = CloudConfig(),
                          cloudRuntime = CloudRuntime()){
    if(!is.null(cloudRuntime$serverPrivateIp)){
        cloudConfig$serverContainer = NULL
    }
    .DockerCluster(cloudProvider=cloudProvider,
                   cloudConfig=cloudConfig,
                   cloudRuntime=cloudRuntime)
}
