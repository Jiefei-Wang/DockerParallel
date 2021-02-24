createRouteTable <- function(VPCId){
    query <- list(VpcId = VPCId)
    query[["TagSpecification.1.ResourceType"]] <-"route-table"
    query[["TagSpecification.1.Tag.1.Key"]] <-"docker-parallel-tag"
    query[["TagSpecification.1.Tag.1.Value"]] <-"docker-parallel-tag"
    response <- ec2_create_route_table(query)
    response
}

deleteRouteTable <- function(routeTableId){
    query <- list(RouteTableId = routeTableId)
    response <- ec2_delete_route_table(query)
    response
}

listRouteTables<-function(filterList = list(),
                          idFilter = NULL,
                          VPCFilter = NULL,
                          gatewayFilter = NULL){
    action <- "DescribeRouteTables"
    if(!is.null(idFilter)){
        filterList[["route-table-id"]] <- idFilter
    }
    if(!is.null(VPCFilter)){
        filterList[["vpc-id"]] <- VPCFilter
    }
    if(!is.null(gatewayFilter)){
        filterList[["route.gateway-id"]] <- gatewayFilter
    }
    query <- getFilter(filterList)
    response <- ec2_describe_route_tables(query)

    routeIds <- vapply(response, function(x)x$routeTableId[[1]], character(1))
    VPCIds <- vapply(response, function(x)x$vpcId[[1]], character(1))
    data.frame(routeId=routeIds,
               VPCId=VPCIds)
}

configRouteTable <- function(x){
    routeId <- getECSCloudData(x, "routeTableId")
    if(is.invalid(x, "routeTableId")){
        VPCId <- configVPCId(x)
        routeTableList <- listRouteTables(VPCFilter = VPCId)
        if(is.empty(x@routeTableId)){
            routeId <- routeTableList$routeId[1]
        }else{
            if(!any(routeTableList$routeId==x@routeTableId&
                    routeTableList$VPCId == VPCId
            )){
                stop("The route table <", x@routeTableId,"> does not exit ",
                     "or does not match with the VPC id")
            }
            routeId <- x@routeTableId
        }
        setECSCloudData(x, "routeTableId", routeId)
    }
    routeId
}

