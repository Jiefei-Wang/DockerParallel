createVpc <- function(){
  # query <- list()
  # query$CidrBlock <- "10.0.0.0/16"
  # query[["TagSpecification.1.ResourceType"]] <-"vpc"
  # query[["TagSpecification.1.Tag.1.Key"]] <-"docker-parallel-tag"
  # query[["TagSpecification.1.Tag.1.Value"]] <-"docker-parallel-tag"

  CidrBlock <- "10.0.0.0/16"
  tagSpecification <- ECSTagTemplate
  tagSpecification[[1]]$ResourceType <- "vpc"

  response <- ec2_create_vpc(CidrBlock = "10.0.0.0/16", TagSpecification = tagSpecification)
  response$vpcId[[1]]
}

deleteVpc <- function(vpcId){
  gatewayList<- listInternetGateways(vpcFilter = vpcId)
  for(i in gatewayList$gatewayId){
    detachInternetGateway(vpcId, i)
  }
  securityGroupList <- listSecurityGroups(vpcFilter = vpcId)
  for(i in securityGroupList$groupId[securityGroupList$name!="default"]){
    deleteSecurityGroup(i)
  }
  subnetList <- listSubnets(vpcFilter = vpcId)
  for(i in subnetList$subnetId){
    deleteSubnet(i)
  }
  routeTableList <- listRouteTables(vpcFilter = vpcId)
  for(i in routeTableList$routeId){
    tryCatch(
      deleteRouteTable(i),
      error = function(e) e
    )
  }
  response <- ec2_delete_vpc(VpcId=vpcId)
  response
}

listVpcs<-function(filterList = list(), idFilter = NULL){
  if(!is.null(idFilter)){
    filterList[["vpc-id"]] <- idFilter
  }

  response <- ec2_describe_vpcs(Filter = filterList)
  vpc_ids <- vapply(response,function(x)x$vpcId[[1]],character(1))
  unname(vpc_ids)
}

configVpcId <- function(x){
  if(!x$vpcVerified){
    if(!is.empty(x$vpcId)){
      vpcList <- listVpcs(idFilter = x$vpcId)
      if(all(vpcList != x$vpcId)){
        stop("The VPC id <",x$vpcId,"> does not exist")
      }
    }else{
      vpcList <- listVpcs(
        filterList = ECSfilterList
      )
      if(length(vpcList)!=0){
        x$vpcId <- vpcList[1]
      }else{
        x$vpcId <- createVpc()
      }
    }
    x$vpcVerified <- TRUE
  }
  x$vpcId
}
