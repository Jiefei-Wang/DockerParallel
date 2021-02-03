# config <- ecs_configuration()
ecs_create_cluster<-function(cluster_name){
  request <- ecs_get_json("create-cluster.json")
  request$clusterName <- cluster_name
  response <- ecs_POST("CreateCluster", request = request)
  response
}

ecs_delete_cluster <- function(cluster_name){
  request <- list(cluster = cluster_name)
  response <- ecs_POST("DeleteCluster", request = request)
  response
}
ecs_list_clusters <- function(){
  target <- "ListClusters"
  response <- ecs_POST(target)
  cluster_list <- unlist(response$clusterArns)
  while(!is.null(response$nextToken)){
    request <- list(nextToken = response$nextToken)
    response <- ecs_POST(target, request = request)
    cluster_list <- c(cluster_list, unlist(response$clusterArns))
  }
  get_resource_names(cluster_list)
}
ecs_config_cluster_name <- function(config){
  if(!is_valid(config, "cluster_name")){
    cluster_list <- ecs_list_clusters()
    if(!any(cluster_list==config$cluster_name)){
      ecs_create_cluster(config$cluster_name)
    }
    set_valid(config, "cluster_name")
  }
  config$cluster_name
}
