createVPC <- function(){
  query <- list()
  query$CidrBlock <- "10.0.0.0/16"
  query[["TagSpecification.1.ResourceType"]] <-"vpc"
  query[["TagSpecification.1.Tag.1.Key"]] <-"docker-parallel-tag"
  query[["TagSpecification.1.Tag.1.Value"]] <-"docker-parallel-tag"
  response <- ec2_create_vpc(query)
  response$vpc$vpcId[[1]]
}

deleteVPC <- function(VPCId){
  gatewayList<- listInternetGateways(VPCFilter = VPCId)
  for(i in gatewayList$gatewayId){
    detachInternetGateway(VPCId, i)
  }
  securityGroupList <- listSecurityGroups(VPCFilter = VPCId)
  for(i in securityGroupList$groupId[securityGroupList$name!="default"]){
    deleteSecurityGroup(i)
  }
  subnetList <- listSubnets(VPCFilter = VPCId)
  for(i in subnetList$subnetId){
    deleteSubnet(i)
  }
  query <- list(VpcId=VPCId)
  response <- ec2_delete_vpc(query)
  response
}

listVPCs<-function(filterList = list(), idFilter = NULL){
  if(!is.null(idFilter)){
    filterList[["vpc-id"]] <- idFilter
  }
  query <- getFilter(filterList)

  response <- ec2_describe_vpcs(query)
  vpc_ids <- vapply(response,function(x)x$vpcId[[1]],character(1))
  unname(vpc_ids)
}

configVPCId <- function(x){
  VPCId <- getECSData(x,"VPCId")
  if(is.null(VPCId)){
    if(!is.empty(x@VPCId)){
      VPCList <- listVPCs()
      if(all(VPCList != x@VPCId)){
        stop("The VPC id <",x@VPCId,"> does not exist")
      }
      VPCId <- x@VPCId
    }else{
      VPCList <- listVPCs(
        filterList = list(`tag:docker-parallel-tag`="docker-parallel-tag")
      )
      if(length(VPCList)!=0){
        VPCId <- VPCList[1]
      }else{
        VPCId <- createVPC()
      }
    }
    setECSData(x, "VPCId", VPCId)
  }
  VPCId
}
