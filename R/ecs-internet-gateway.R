createInternetGateway <- function(){
    query <- list()
    query[["TagSpecification.1.ResourceType"]] <-"internet-gateway"
    query[["TagSpecification.1.Tag.1.Key"]] <-"docker-parallel-tag"
    query[["TagSpecification.1.Tag.1.Value"]] <-"docker-parallel-tag"
    response <- ec2_create_internet_gateway(query)
    response$internetGateway$internetGatewayId[[1]]
}
deleteInternetGateway <- function(gatewayId){
    gatewayList <- listInternetGateways(idFilter = gatewayId)
    if(all(gatewayList$gatewayId!=gatewayId)){
        return()
    }
    idx <- which(gatewayList$gatewayId==gatewayId)
    VPCId <- gatewayList$VPCId[idx]
    if(VPCId != "NULL"){
        detachInternetGateway(VPCId,gatewayId)
    }
    query <- list(InternetGatewayId=gatewayId)
    response <- ec2_delete_internet_gateway(query)
    response
}

listInternetGateways<-function(filterList = NULL,
                               VPCFilter = NULL, idFilter = NULL){
    action <- "DescribeInternetGateways"
    if(!is.null(VPCFilter)){
        filterList[["attachment.vpc-id"]] <- VPCFilter
    }
    if(!is.null(idFilter)){
        filterList[["internet-gateway-id"]] <- idFilter
    }
    query <- getFilter(filterList)

    response <- ec2_describe_internet_gateways(query)
    result <- lapply(response,processGateway)
    result <- do.call("rbind", result)
    if(is.null(result)){
        data.frame(gatewayId=character(),
                   VPCId=character())
    }else{
        result
    }

}

processGateway<-function(gateway){
    id <- gateway$internetGatewayId[[1]]
    attachedVPC <- lapply(gateway$attachmentSet,function(x)x$vpcId[[1]])
    idx <- which(!vapply(attachedVPC, is.null, logical(1)))
    if(length(idx)==0){
        attachedVPC<- "NULL"
    }else{
        attachedVPC <- attachedVPC[[idx]]
    }
    data.frame(gatewayId=rep(id,length(attachedVPC)),VPCId=attachedVPC)
}

configInternetGateway <- function(x){
    internetGatewayId <- getECSData(x, "internetGatewayId")
    if(is.null(internetGatewayId)){
        VPCId <- configVPCId(x)
        gatewayList <-
            listInternetGateways(
                VPCFilter = VPCId)
        if(is.empty(x@internetGatewayId)){
            if(nrow(gatewayList)!=0){
                internetGatewayId <- gatewayList$gatewayId[1]
            }else{
                internetGatewayId <- createInternetGateway()
            }
        }else{
            if(all(gatewayList$gatewayId!=x@internetGatewayId)){
                stop("The gateway id <",x@internetGatewayId,"> does not exist")
            }
            internetGatewayId <- x@internetGatewayId
        }
        gatewayList <-
            listInternetGateways(
                idFilter = internetGatewayId)
        if(gatewayList$VPCId=="NULL"){
            attachInternetGateway(
                VPCId, internetGatewayId)
        }
        setECSData(x, "internetGatewayId", internetGatewayId)
    }
    internetGatewayId
}

attachInternetGateway<-function(VPCId, gatewayId){
    query <- list(InternetGatewayId = gatewayId, VpcId = VPCId)
    response <- ec2_attach_internet_gateway(query)
    response
}

detachInternetGateway<-function(VPCId, gatewayId){
    query <- list(InternetGatewayId = gatewayId, VpcId = VPCId)
    response <- ec2_detach_internet_gateway(query)
    response
}
