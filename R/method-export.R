#' Wait the instances until they are running
#'
#' Wait the instances until they are running
#'
#' @returns
#' a logical value indicating if the instances are running
#'
#' @export
waitInstanceUntilRunning<-function(provider, instanceHandles, progressBar = FALSE, waitTime = 1){
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
        Sys.sleep(waitTime)
    }
}









