createSubnet <- function(vpcId, cidr){
  tagSpecification <- ECSTagTemplate
  tagSpecification[[1]]$ResourceType <- "subnet"
  response <-  ec2_create_subnet(CidrBlock = cidr,
                                 VpcId = vpcId,
                                 TagSpecification= tagSpecification)
  response$subnetId[[1]]
}
deleteSubnet <- function(subnetId){
  response <- ec2_delete_subnet(SubnetId = subnetId)
  response
}

listSubnets<-function(filterList = list(), idFilter = NULL, vpcFilter = NULL){
  if(!is.null(idFilter)){
    filterList[["subnet-id"]] <- idFilter
  }
  if(!is.null(vpcFilter)){
    filterList[["vpc-id"]] <- vpcFilter
  }
  response <- ec2_describe_subnets(Filter = filterList)
  subnetIds <- vapply(response,function(x)x$subnetId[[1]],character(1))
  vpcIds <- vapply(response,function(x)x$vpcId[[1]],character(1))
  cidr <- vapply(response,function(x)x$cidrBlock[[1]],character(1))

  data.frame(subnetId= subnetIds, vpcId = vpcIds, cidr = cidr)
}


configSubnetId <- function(x){
  if(!x$subnetVerified){
      vpcId <- configVpcId(x)
      subnetList <- listSubnets(vpcFilter = vpcId)
      if(is.empty(x$subnetId)){
        if(any(subnetList$cidr=="10.0.0.0/16")){
          x$subnetId <- subnetList$subnetId[subnetList$cidr=="10.0.0.0/16"]
        }else{
          x$subnetId <- createSubnet(vpcId, "10.0.0.0/16")
        }
      }else{
        if(!any(subnetList$subnetId==x$subnetId)){
          stop("The subnet id <",x$subnetId,"> does not exist")
        }
      }
      x$subnetVerified
  }
  x$subnetId
}
