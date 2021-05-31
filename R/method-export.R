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



