createSubnet <- function(VPCId, cidr){
  query <- list()
  query$VpcId <- VPCId
  query$CidrBlock <- cidr
  query[["TagSpecification.1.ResourceType"]] <-"subnet"
  query[["TagSpecification.1.Tag.1.Key"]] <-"docker-parallel-tag"
  query[["TagSpecification.1.Tag.1.Value"]] <-"docker-parallel-tag"
  response <-  ec2_create_subnet(query)
  response$subnet$subnetId[[1]]
}
deleteSubnet <- function(subnetId){
  query <- list(SubnetId=subnetId)
  response <- ec2_delete_subnet(query)
  response
}

listSubnets<-function(filterList = list(), idFilter = NULL, VPCFilter = NULL){
  if(!is.null(idFilter)){
    filterList[["subnet-id"]] <- idFilter
  }
  if(!is.null(VPCFilter)){
    filterList[["vpc-id"]] <- VPCFilter
  }
  query <- getFilter(filterList)
  response <- ec2_describe_subnets(query)
  subnetIds <- vapply(response,function(x)x$subnetId[[1]],character(1))
  VPCIds <- vapply(response,function(x)x$vpcId[[1]],character(1))
  cidr <- vapply(response,function(x)x$cidrBlock[[1]],character(1))

  data.frame(subnetId= subnetIds, VPCId = VPCIds, cidr = cidr)
}


configSubnetId <- function(x){
  subnetId <- getECSData(x, "subnetId")
  if(is.null(subnetId)){
      VPCId <- configVPCId(x)
      subnetList <- listSubnets(VPCFilter = VPCId)
      if(is.empty(x@subnetId)){
        if(nrow(subnetList)!=0){
          subnetId <- subnetList$subnetId[1]
        }else{
          subnetId <- createSubnet(VPCId, "10.0.0.0/16")
        }
      }else{
        if(!any(subnetList$subnetId==x@subnetId&
                subnetList$VPCId==VPCId)){
          stop("The subnet id <",x@subnetId,"> does not exist")
        }
        subnetId <- x@subnetId
      }
      setECSData(x, "subnetId", subnetId)
  }
  subnetId
}
