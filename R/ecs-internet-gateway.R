createInternetGateway <- function(){
    tagSpecification <- ECSTagTemplate
    tagSpecification[[1]]$ResourceType <- "internet-gateway"
    response <- ec2_create_internet_gateway(TagSpecification=tagSpecification)
    response$internetGatewayId[[1]]
}
deleteInternetGateway <- function(gatewayId){
    gatewayList <- listInternetGateways(idFilter = gatewayId)
    if(all(gatewayList$gatewayId!=gatewayId)){
        return()
    }
    idx <- which(gatewayList$gatewayId==gatewayId)
    vpcId <- gatewayList$vpcId[idx]
    if(vpcId != "NULL"){
        detachInternetGateway(vpcId,gatewayId)
    }
    response <- ec2_delete_internet_gateway(InternetGatewayId=gatewayId)
    response
}

listInternetGateways<-function(filterList = list(),
                               vpcFilter = NULL, idFilter = NULL){
    if(!is.null(vpcFilter)){
        filterList[["attachment.vpc-id"]] <- vpcFilter
    }
    if(!is.null(idFilter)){
        filterList[["internet-gateway-id"]] <- idFilter
    }

    response <- ec2_describe_internet_gateways(Filter = filterList)
    result <- lapply(response,processGateway)
    result <- do.call("rbind", result)
    if(is.null(result)){
        data.frame(gatewayId=character(),
                   vpcId=character())
    }else{
        result
    }

}

processGateway<-function(gateway){
    id <- gateway$internetGatewayId[[1]]
    attachedVpc <- lapply(gateway$attachmentSet,function(x)x$vpcId[[1]])
    idx <- which(!vapply(attachedVpc, is.null, logical(1)))
    if(length(idx)==0){
        attachedVpc<- "NULL"
    }else{
        attachedVpc <- attachedVpc[[idx]]
    }
    data.frame(gatewayId=rep(id,length(attachedVpc)),vpcId=attachedVpc)
}

configInternetGateway <- function(x){
    if(!x$internetGatewayVerified){
        vpcId <- configVpcId(x)
        needAttach <- FALSE
        if(is.empty(x$internetGatewayId)){
            gatewayList <-
                listInternetGateways(
                    vpcFilter = vpcId,
                    filterList = ECSfilterList
                    )
            if(nrow(gatewayList)!=0){
                x$internetGatewayId <- gatewayList$gatewayId[1]
            }else{
                x$internetGatewayId <- createInternetGateway()
                needAttach <- TRUE
            }
        }else{
            gatewayList <-
                listInternetGateways(idFilter = x$internetGatewayId)
            if(nrow(gatewayList)!=1){
                stop("The gateway id <",x$internetGatewayId,"> does not exist")
            }
            currentVpcId <- gatewayList$vpcId[gatewayList$gatewayId==x$internetGatewayId]
            if(currentVpcId != "NULL" && currentVpcId!=vpcId){
                stop("The gateway id <",
                     x$internetGatewayId,
                     "> has been attached to a different VPC")
            }
            needAttach <- currentVpcId == "NULL"
        }

        if(needAttach){
            attachInternetGateway(
                vpcId, x$internetGatewayId
                )
        }
        x$internetGatewayVerified <- TRUE
    }
    x$internetGatewayId
}

attachInternetGateway<-function(vpcId, gatewayId){
    response <- ec2_attach_internet_gateway(InternetGatewayId = gatewayId, VpcId = vpcId)
    response
}

detachInternetGateway<-function(vpcId, gatewayId){
    response <- ec2_detach_internet_gateway(InternetGatewayId = gatewayId, VpcId = vpcId)
    response
}
