createRouteTable <- function(vpcId){
    tagSpecification <- ECSTagTemplate
    tagSpecification[[1]]$ResourceType <- "route-table"
    response <- ec2_create_route_table(VpcId = vpcId,TagSpecification=tagSpecification)
    response$routeTableId[[1]]
}

deleteRouteTable <- function(routeTableId){
    response <- ec2_delete_route_table(RouteTableId = routeTableId)
    response
}

listRouteTables<-function(filterList = list(),
                          idFilter = NULL,
                          vpcFilter = NULL,
                          gatewayFilter = NULL){
    action <- "DescribeRouteTables"
    if(!is.null(idFilter)){
        filterList[["route-table-id"]] <- idFilter
    }
    if(!is.null(vpcFilter)){
        filterList[["vpc-id"]] <- vpcFilter
    }
    if(!is.null(gatewayFilter)){
        filterList[["route.gateway-id"]] <- gatewayFilter
    }
    response <- ec2_describe_route_tables(Filter=filterList)

    routeIds <- vapply(response, function(x)x$routeTableId[[1]], character(1))
    vpcIds <- vapply(response, function(x)x$vpcId[[1]], character(1))
    data.frame(routeId=routeIds,
               vpcId=vpcIds)
}

configRouteTable <- function(x){
    if(!x$routeTableVerified){
        vpcId <- configVpcId(x)
        routeTableList <- listRouteTables(filterList = ECSfilterList, vpcFilter = vpcId)
        if(is.empty(x$routeTableId)){
            if(nrow(routeTableList)==0){
                x$routeTableId <- createRouteTable(vpcId)
            }else{
                x$routeTableId <- routeTableList$routeId[1]
            }
        }else{
            if(!any(routeTableList$routeId==x$routeTableId&
                    routeTableList$vpcId == vpcId
            )){
                stop("The route table <", x@routeTableId,"> does not exit ",
                     "or does not match with the VPC id")
            }
        }
        x$routeTableVerified <- TRUE
    }
    x$routeTableId
}

