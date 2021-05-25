


#' Set the default cloud provider and container
#'
#' Set the default cloud provider and container. You must install the provider and container
#' packages before using them.
#'
#' @param cloudProvider The default cloud provider name, can be abbreviated
#' @param container The default container name, can be abbreviated
#' @examples
#' \dontrun{
#' clusterPreset(cloudProvider = "ECSFargateProvider", container = "rbaseDoRedis")
#' cluster <- makeDockerCluster()
#' cluster
#' }
#' @return No return value
#' @export
clusterPreset<- function(
    cloudProvider = c("","ECSFargateProvider"),
    container = c("",
                  "rbaseDoRedis",
                  "rbaseRedisParam",
                  "biocDoRedis",
                  "biocRedisParam"
    )
){
    cloudProvider <- match.arg(cloudProvider)
    container <- match.arg(container)

    provider <- NULL
    workerContainer <- NULL
    if(cloudProvider == "ECSFargateProvider"){
        loadPackage("ECSFargateProvider")
        eval(parse(text = "provider <- ECSFargateProvider::ECSFargateProvider()"))
    }


    if(container == "rbaseDoRedis"){
        loadPackage("doRedisContainer")
        eval(parse(text = "workerContainer <- doRedisContainer::doRedisWorkerContainer(image = \"r-base\")"))
    }

    if(container == "rbaseRedisParam"){
        loadPackage("RedisParamContainer")
        eval(parse(text = "workerContainer <- RedisParamContainer::RedisParamWorkerContainer(image = \"r-base\")"))
    }
    if(container == "biocDoRedis"){
        loadPackage("doRedisContainer")
        eval(parse(text = "workerContainer <- doRedisContainer::doRedisWorkerContainer(image = \"bioconductor\")"))
    }
    if(container == "biocRedisParam"){
        loadPackage("RedisParamContainer")
        eval(parse(text = "workerContainer <- RedisParamContainer::RedisParamWorkerContainer(image = \"bioconductor\")"))
        }
    if(container!=""&&is.null(workerContainer)){
        stop("Somethine is wrong")
    }

    packageSetting$cloudProvider <- provider
    packageSetting$workerContainer <- workerContainer
    invisible(NULL)
}

#' Serialize docker cluster static data
#'
#' Serialize docker cluster static data. This function is designed for the
#' generic `reconnectDockerCluster`. The serialized data can be used by the cloud
#' provider to recover the `DockerCluster` object.
#'
#' @param cluster The `DockerCluster` object
#' @param serializedData The serialized data returned by `serializeDockerClusterStaticData`
#' @return
#' serializeDockerClusterStaticData: The binary serialized data
#' unserializeDockerClusterStaticData: No return value should be expected, the cluster
#' object will be updated.
#' @rdname serializeStaticData
#' @export
serializeDockerClusterStaticData <- function(cluster){
    cloudConfig <- .getCloudConfig(cluster)
    serverContainer <- .getServerContainer(cluster)
    workerContainer <- .getWorkerContainer(cluster)
    settings <- .getClusterSettings(cluster)

    clusterData <- list(cloudConfig = getObjectData(cloudConfig),
                        serverContainer = getObjectData(serverContainer),
                        workerContainer = getObjectData(workerContainer),
                        settings = as.list(settings))
    clusterData$settings$parallelBackendRegistered <- NULL
    clusterData$settings$cluster <- NULL
    serialize(clusterData, NULL)
}

#' @rdname serializeStaticData
#' @export
unserializeDockerClusterStaticData <- function(cluster, serializedData){
    clusterData <- unserialize(serializedData)
    cloudConfig <- .getCloudConfig(cluster)
    serverContainer <- .getServerContainer(cluster)
    workerContainer <- .getWorkerContainer(cluster)

    setObjectData(cloudConfig, clusterData$cloudConfig)
    setObjectData(serverContainer, clusterData$serverContainer)
    setObjectData(workerContainer, clusterData$workerContainer)

    clusterData$settings$parallelBackendRegistered <- FALSE
    clusterData$settings$cluster <- cluster
    .setClusterSettings(cluster, as.environment(clusterData$settings))
    invisible(NULL)
}



