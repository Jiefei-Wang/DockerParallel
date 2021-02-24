# config <- ecs_configuration()
createCluster<-function(clusterName){
    request <- ecs_get_json("create-cluster.json")
    request$clusterName <- clusterName
    response <- ecs_create_cluster(request)
    response
}

deleteCluster <- function(clusterName){
    request <- list(cluster = clusterName)
    response <- ecs_delete_cluster(request)
    response
}
listClusters <- function(){
    response <- ecs_list_clusters()
    get_resource_names(response)
}

configClusterName <- function(x){
    clusterName <- getECSCloudData(x, "clusterName")
    if(is.invalid(x, "clusterName")){
        clusterList <- listClusters()
        if(!is.empty(x@clusterName)){
            if(!any(clusterList==x@clusterName)){
                clusterName <- createCluster(x@clusterName)
            }
            clusterName <- x@clusterName
        }else{
            if(!any(clusterList==ECSDefault$clusterName)){
                createCluster(ECSDefault$clusterName)
            }
            clusterName <- ECSDefault$clusterName
        }
        setECSCloudData(x, "clusterName", clusterName)
    }
    clusterName
}
