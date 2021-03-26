# config <- ecs_configuration()
createCluster<-function(clusterName){
    tags <- ListToArray(list(DockerParallel = "DockerParallel"), name = "key")
    response <-ecs_create_cluster(
        clusterName = clusterName,
        tags = tags,
        capacityProviders = list("FARGATE"))
    response
}

deleteCluster <- function(clusterName){
    response <- ecs_delete_cluster(cluster = clusterName)
    response
}
listClusters <- function(){
    response <- ecs_list_clusters()
    ECSGetResourceNames(response)
}

configClusterName <- function(x){
    if(!x$clusterNameVerified){
        clusterList <- listClusters()
        if(all(clusterList!=x$clusterName)){
            createCluster(x$clusterName)
        }
        x$clusterNameVerified <- TRUE
    }
    x$clusterName
}
