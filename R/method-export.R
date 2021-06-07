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

#' Define the data object for a cloud private server
#'
#' Define the data object for a cloud private server. The data object
#' can be passed to `makeDockerCluster` and let the cluster
#' use the private server instead of the server from the cloud provider.
#'
#' @param publicIp Character(0) or Character(1), the public Ip of the server
#' @param publicPort Integer(0) or Integer(1), the public port of the server
#' @param privateIp Character(0) or Character(1), the private Ip of the server
#' @param privatePort Integer(0) or Integer(1), the private port of the server
#' @param password Character(1), the password for the server
#' @param serverWorkerSameLAN Logical(1), whether the server and works are in the same LAN
#' @param serverClientSameLAN Logical(1), whether the server and client are in the same LAN
#' @examples
#' CloudPrivateServer(publicIp = "192.168.1.1", publicPort = 1234)
#' @export
CloudPrivateServer <- function(publicIp = character(0),
                               publicPort = integer(0),
                               privateIp = character(0),
                               privatePort = integer(0),
                               password = "",
                               serverWorkerSameLAN = FALSE,
                               serverClientSameLAN = FALSE
                               ){
    list(publicIp = publicIp,
         publicPort = publicPort,
         privateIp = privateIp,
         privatePort = privatePort,
         password = password,
         serverWorkerSameLAN = serverWorkerSameLAN,
         serverClientSameLAN = serverClientSameLAN)
}

