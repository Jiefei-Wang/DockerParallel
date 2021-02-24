createRoute <- function(cidr, gatewayId, routeTableId){
    query <- list(
        DestinationCidrBlock = cidr,
        GatewayId = gatewayId,
        RouteTableId = routeTableId
    )
    response <- ec2_create_route(query)
    response
}

deleteRoute <- function(routeTableId, cidr){
    query <- list(DestinationCidrBlock = cidr,
                  RouteTableId = routeTableId)
    response <- ec2_delete_route(query)
    response
}

listRoute<-function(routeTableId = NULL){
    query <- list()
    query[["RouteTableId.1"]] <- routeTableId
    response <- ec2_describe_route_tables(query)
    route_set <- response$item$routeSet

    destCidr <- lapply(route_set, function(x) x$destinationCidrBlock[[1]])
    destIpv6Cidr <- lapply(route_set, function(x) x$destinationIpv6CidrBlock[[1]])
    for(i in seq_along(destCidr)){
        if(is.null(destCidr[[i]])){
            destCidr[[i]] <- destIpv6Cidr[[i]]
        }
    }
    destCidr <- unlist(destCidr)
    gatewayIds <- vapply(route_set, function(x) x$gatewayId[[1]], character(1))
    states <- vapply(route_set, function(x) x$state[[1]], character(1))
    destCidr <- destCidr[states=="active"]
    gatewayIds <- gatewayIds[states=="active"]

    data.frame(cidr =destCidr,
               gateway = gatewayIds)
}



configDefaultRoute<-function(x){
    internetGatewayId <- configInternetGateway(x)
    routeTableId <- configRouteTable(x)
    defaultRouteInitialized <- getECSCloudData(x, "defaultRouteInitialized")
    if(is.null(defaultRouteInitialized)){
        routeList <- listRoute(routeTableId)
        if(!any(routeList$cidr=="0.0.0.0/0"&
                routeList$gateway==internetGatewayId)){
            createRoute("0.0.0.0/0",
                        internetGatewayId,
                        routeTableId)
        }
        setECSCloudData(x, "defaultRouteInitialized", TRUE)
    }
}


