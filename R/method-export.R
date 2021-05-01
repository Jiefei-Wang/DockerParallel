#' Wait the instances until they have the running status
#'
#' Wait the instances until they have the running status
#'
#' @inheritParams getDockerInstanceStatus
#' @param progressBar Logical, whether to show a progress bar while waiting
#' @param checkInterval Numeric, the time interval in seconds between two status checks.
#' @param maxWaitTime Numeric, the maximum wait time in seconds. There will be no error
#' if the wait time exceeds the maximum wait time.
#'
#' @returns
#' a logical vector indicating if the instances are running
#'
#' @export
waitInstanceUntilRunning<-function(provider, instanceHandles, progressBar = FALSE,
                                   checkInterval = 1, maxWaitTime = 60*5){
    on.exit(
        {
            if(progressBar){
                close(pb)
            }
        }
    )
    if(progressBar){
        pb <- txtProgressBar(min=0,max = length(instanceHandles), style=3)
    }
    startTime <- Sys.time()
    while(TRUE){
        instanceStatus <- getDockerInstanceStatus(provider=provider,
                                                  instanceHandles = instanceHandles,
                                                  verbose = FALSE)
        if(all(instanceStatus=="running")){
            return(TRUE)
        }
        if(any(instanceStatus=="stopped")){
            return(FALSE)
        }
        if(progressBar){
            setTxtProgressBar(pb, sum(instanceStatus=="running"))
        }
        if(difftime(Sys.time(), startTime, units = "secs")>maxWaitTime){
            break
        }
        Sys.sleep(checkInterval)
    }
    instanceStatus=="running"
}

#' Set the default cloud provider and container
#'
#' Set the default cloud provider and container. You must install the provider and container
#' before using them.
#'
#' @param cloudProvider The default cloud provider name, can be abbreviated
#' @param container The default container name, can be abbreviated
#' @examples
#' \dontrun{
#' clusterPreset(cloudProvider = "ECSFargateProvider", container = "BiocFEDRContainer")
#' cluster <- makeDockerCluster()
#' cluster
#' }
#' @return No return value
#' @export
clusterPreset<- function(
    cloudProvider = c("","ECSFargateProvider"),
    container = c("","BiocFEDRContainer", "BiocBPRPContainer", "baseFEDRContainer")
){
    cloudProvider <- match.arg(cloudProvider)
    container <- match.arg(container)

    provider <- NULL
    workerContainer <- NULL
    if(cloudProvider == "ECSFargateProvider"){
        loadPackage("ECSFargateProvider")
        eval(parse(text = "provider <- ECSFargateProvider::ECSFargateProvider()"))
    }

    if(container == "BiocFEDRContainer"){
        loadPackage("BiocFEDRContainer")
        eval(parse(text = "workerContainer <- BiocFEDRContainer::BiocFEDRWorkerContainer()"))
    }

    if(container == "BiocBPRPContainer"){
        loadPackage("BiocBPRPContainer")
        eval(parse(text = "workerContainer <- BiocBPRPContainer::BiocBPRPWorkerContainer()"))
    }
    if(container == "baseFEDRContainer"){
        loadPackage("baseFEDRContainer")
        eval(parse(text = "workerContainer <- baseFEDRContainer::baseFEDRWorkerContainer()"))
    }

    packageSetting$cloudProvider <- provider
    packageSetting$workerContainer <- workerContainer
    invisible(NULL)
}





