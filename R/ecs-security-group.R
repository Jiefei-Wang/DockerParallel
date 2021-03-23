createSecurityGroup <- function(groupName, vpcId){
    tagSpecification <- ECSTagTemplate
    tagSpecification[[1]]$ResourceType <- "security-group"
    response <- ec2_create_security_group(
        GroupName = groupName,
        VpcId = vpcId,
        TagSpecification=tagSpecification,
        GroupDescription = "Security group used by R worker nodes")
    response$groupId[[1]]
}
deleteSecurityGroup <- function(groupId){
    response <- ec2_delete_security_group(GroupId=groupId)
    response
}

listSecurityGroups <- function(filterList = list(),
                               vpcFilter = NULL,
                               nameFilter = NULL,
                               idFilter = NULL){
    if(!is.null(vpcFilter)){
        filterList[["vpc-id"]] <- vpcFilter
    }
    if(!is.null(nameFilter)){
        filterList[["group-name"]] <- nameFilter
    }
    if(!is.null(idFilter)){
        filterList[["group-id"]] <- idFilter
    }
    response <- ec2_describe_security_groups(Filter = filterList)

    groupNames <- vapply(response, function(x)x$groupName[[1]], character(1))
    groupIds <- vapply(response, function(x)x$groupId[[1]], character(1))
    groupvpcIds <- vapply(response, function(x)x$vpcId[[1]], character(1))

    data.frame(name=groupNames,
               groupId= groupIds,
               vpcId= groupvpcIds)
}

configSecurityGroup <- function(x){
    if(!x$securityGroupVerified){
        vpcId <- configVpcId(x)
        securityGroupList <- listSecurityGroups(vpcFilter = vpcId)
        if(is.empty(x$securityGroupId)){
            idx <- which(securityGroupList$name == x$securityGroupName)
            if(length(idx)!=0){
                x$securityGroupId <- securityGroupList$groupId[idx[1]]
            }else{
                x$securityGroupId <-
                    createSecurityGroup(x$securityGroupName,
                                        vpcId)
            }
        }else{
            idx <- which(securityGroupList$groupId == x@securityGroupId)
            if(length(idx)==0){
                stop("The security group id <",x@securityGroupId,"> does not exist")
            }
            x$securityGroupName <- securityGroupList$name[idx[1]]
        }
        x$securityGroupVerified <- TRUE
    }
    x$securityGroupId
}

#################################
# security group policy
#################################
createSecurityGroupIpv4Rule <- function(securityGroupId, port){
    ipPermissions <- list(
        list(
            FromPort = port,
            ToPort = port,
            IpProtocol = "tcp",
            IpRanges.CidrIp = "0.0.0.0/0",
            IpRanges.Description = "allow all ipv4 access"
        )
    )
    response <- ec2_authorize_security_group_ingress(
        GroupId = securityGroupId,
        IpPermissions=ipPermissions
    )
    response
}
createSecurityGroupIpv6Rule <- function(securityGroupId, port){
    ipPermissions <- list(
        list(
            FromPort = port,
            ToPort = port,
            IpProtocol = "tcp",
            Ipv6Ranges.CidrIpv6 = "::/0",
            Ipv6Ranges.Description = "allow all ipv6 access"
        )
    )
    response <- ec2_authorize_security_group_ingress(
        GroupId = securityGroupId,
        IpPermissions=ipPermissions
    )
    response
}

listSecurityInboundRule<-function(securityGroupId){
    filterList <- list("group-id" = securityGroupId)
    response <- ec2_describe_security_groups(Filter = filterList)

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
    if(!x$inboundPermissionVerified){
        securityGroupId <- configSecurityGroup(x)
        inboundRules <- listSecurityInboundRule(securityGroupId)
        for(i in ports)
            ConfigInboundPermissionsInternal(securityGroupId, inboundRules, i)
        x$inboundPermissionVerified <- TRUE
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

