createSecurityGroup <- function(groupName, VPCId){
    query <- list()
    query$GroupDescription <- "Security group used by R worker nodes"
    query$GroupName <- groupName
    query$VpcId <- VPCId
    query[["TagSpecification.1.ResourceType"]] <-"security-group"
    query[["TagSpecification.1.Tag.1.Key"]] <-"docker-parallel-tag"
    query[["TagSpecification.1.Tag.1.Value"]] <-"docker-parallel-tag"
    response <- ec2_create_security_group(query)
    response$groupId[[1]]
}
deleteSecurityGroup <- function(groupId){
    query <- list(GroupId=groupId)
    response <- ec2_delete_security_group(query)
    response
}

listSecurityGroups <- function(filterList = list(),
                               VPCFilter = NULL,
                               nameFilter = NULL,
                               idFilter = NULL){
    if(!is.null(VPCFilter)){
        filterList[["vpc-id"]] <- VPCFilter
    }
    if(!is.null(nameFilter)){
        filterList[["group-name"]] <- nameFilter
    }
    if(!is.null(idFilter)){
        filterList[["group-id"]] <- idFilter
    }
    query <- getFilter(filterList)
    response <- ec2_describe_security_groups(query)

    groupNames <- vapply(response, function(x)x$groupName[[1]], character(1))
    groupIds <- vapply(response, function(x)x$groupId[[1]], character(1))
    groupVPCIds <- vapply(response, function(x)x$vpcId[[1]], character(1))

    data.frame(name=groupNames,
               groupId= groupIds,
               VPCId= groupVPCIds)
}

configSecurityGroupId <- function(x){
    securityGroupId <- getECSCloudData(x, "securityGroupId")
    if(is.invalid(x, "securityGroupId")){
        VPCId <- configVPCId(x)
        securityGroupList <- listSecurityGroups(VPCFilter = VPCId)
        if(is.empty(x@securityGroupId)&&is.empty(x@securityGroupName)){
            idx <- which(securityGroupList$name == ECSDefault$securityGroupName)
            if(length(idx)!=0){
                securityGroupName <- securityGroupList$name[idx[1]]
                securityGroupId <- securityGroupList$groupId[idx[1]]
            }else{
                securityGroupName <- ECSDefault$securityGroupName
                securityGroupId <-
                    createSecurityGroup(securityGroupName,
                                        VPCId)
            }
        }else{
            if(!is.empty(x@securityGroupName)){
                idx <- which(securityGroupList$name == x@securityGroupName)
                if(length(idx)==0){
                    stop("The security group name <",x@securityGroupName,"> does not exist")
                }
                securityGroupName <- x@securityGroupName
                securityGroupId <- securityGroupList$groupId[idx[1]]
            }else{
                idx <- which(securityGroupList$groupId == x@securityGroupId)
                if(length(idx)==0){
                    stop("The security group id <",x@securityGroupId,"> does not exist")
                }
                securityGroupName <- securityGroupList$name[idx[1]]
                securityGroupId <- x@securityGroupId
            }
        }
        setECSCloudData(x, "securityGroupId", securityGroupId)
        setECSCloudData(x, "securityGroupName", securityGroupName)
    }
    securityGroupId
}

#################################
# security group policy
#################################
createSecurityGroupIpv4Rule <- function(securityGroupId, port){
    action <- "AuthorizeSecurityGroupIngress"
    query <- list()
    query$GroupId <- securityGroupId
    query[["IpPermissions.1.FromPort"]] <- port
    query[["IpPermissions.1.ToPort"]] <- port
    query[["IpPermissions.1.IpProtocol"]] <- "tcp"
    query[["IpPermissions.1.IpRanges.CidrIp"]] <- "0.0.0.0/0"
    query[["IpPermissions.1.IpRanges.Description"]] <- "allow all ipv4 ssh access"
    response <- ec2_authorize_security_group_ingress(query)
    response
}
createSecurityGroupIpv6Rule <- function(securityGroupId, port){
    query <- list()
    query$GroupId <- securityGroupId
    query[["IpPermissions.1.FromPort"]] <- port
    query[["IpPermissions.1.ToPort"]] <- port
    query[["IpPermissions.1.IpProtocol"]] <- "tcp"
    query[["IpPermissions.1.Ipv6Ranges.CidrIpv6"]] <- "::/0"
    query[["IpPermissions.1.Ipv6Ranges.Description"]] <- "allow all ipv6 ssh access"
    response <- ec2_authorize_security_group_ingress(query)
    response
}

listSecurityInboundRule<-function(securityGroupId){
    action <- "DescribeSecurityGroups"
    filterList <- list("group-id" = securityGroupId)
    query <- getFilter(filterList)
    response <- ec2_describe_security_groups(query)

    from_port_list<-c()
    to_port_list<-c()
    ip_list<-c()
    for(i in response$item$ipPermissions){
        from_port <- i$fromPort[[1]]
        to_port <- i$toPort[[1]]
        ipv4_range <- vapply(i$ipRanges, function(x)x$cidrIp[[1]], character(1))
        ipv6_range <- vapply(i$ipv6Ranges, function(x)x$cidrIp[[1]], character(1))
        ip <- c(ipv4_range,ipv6_range)
        from_port_list <- c(from_port_list, rep(from_port,length(ip)))
        to_port_list <- c(to_port_list, rep(to_port,length(ip)))
        ip_list <- c(ip_list, ip)
    }
    data.frame(from = as.numeric(from_port_list), to = as.numeric(to_port_list), ip = ip_list)
}


ConfigInboundPermissions<-function(x, ports){
    if(is.null(getECSCloudData(x, "inboundPermissionInitialized"))){
        securityGroupId <- configSecurityGroupId(x)
        inboundRules <- listSecurityInboundRule(securityGroupId)
        for(i in ports)
            ConfigInboundPermissionsInternal(securityGroupId, inboundRules, i)
        setECSCloudData(x, "inboundPermissionInitialized", TRUE)
    }
}

ConfigInboundPermissionsInternal<-function(securityGroupId, inboundRules, port){
    ipv4Idx <- which(inboundRules$ip=="0.0.0.0/0")
    if(length(ipv4Idx)==0||
       all(inboundRules$from[ipv4Idx]>port|inboundRules$to[ipv4Idx]<port)){
        createSecurityGroupIpv4Rule(securityGroupId, port)
    }
    ipv6Idx <- which(inboundRules$ip=="::/0")
    if(length(ipv6Idx)==0||
       all(inboundRules$from[ipv6Idx]>port|inboundRules$to[ipv6Idx]<port)){
        createSecurityGroupIpv6Rule(securityGroupId, port)
    }
}

