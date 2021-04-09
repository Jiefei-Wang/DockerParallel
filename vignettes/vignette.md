---
title: "vignette"
author: 
- name: Jiefei Wang
  affiliation: Roswell Park Comprehensive Cancer Center, Buffalo, NY
date: "2021-04-09"
output:
    BiocStyle::html_document:
        toc: true
        toc_float: true
vignette: >
  %\VignetteIndexEntry{vignette}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
package: DockerParallel
---



# Introduction
Parallel computing has became an important tool to analysis large and complex data. Using the `parallel` package to create local computing cluster is probably the most well-known method for the parallel computing in R's realm. As the advance of the cloud computing, there is a natural need to make R parallel compatible with the cloud. `DockerParallel` is a package that is designed for the cloud computation. It aims to provide easy-to-learn, highly scalable and low-cost tools to make the cloud computation possible.

The core component of `DockerParallel`, as its name implies, is the docker container. Container is a technique to package up code and all its dependencies in a standard unit and run it in an isolated environment from the host OS. By containerizing R's worker node, `DockerParallel` can easily deploy hundreds of identical workers in a cloud environment regardless of the host hardware and OS that run the nodes. In this vignette, we will demonstrate how to use `DockerParallel` to run a cluster using Amazon Elastic Compute Service(ECS). The purpose of this vignette is providing the basic usage of the package for the user. For more information, please see the R markdown file `advanced-topics` and `cookbook-for-developers`.

# The structure of `DockerParallel`
For understanding the structure of `DockerParallel`, imagine that if someone tells you to create an R parallel cluster on the cloud using the container, what question you will ask before you can deploy the cluster on the cloud? Generally speaking, the cluster depends on the answers to the three questions:

1. Which container should be used?
2. Who provides the container service?
3. What is the cluster configuration(e.g. worker number, CPU, memory)?

`DockerParallel` answers these questions via three components:  `Container`, `CloudProvider` and `CloudConfig`. These components can be summarized in the following figure

![](components.jpg)

We would not discuss the technical details of these components as they should be only interesting for the developer. Users only need to pick up the appropriate components and create the cluster with the properties they need. 

# The structure of ECS
Amazon provides Elastic Compute Service to take over the management of servers. By using ECS, the user only needs to prepare the container image and ECS will find the best server to run the container. ECS provides both the traditional server and fargate as the host machine of the container. For the traditional server, the user is able to select a set of hardware that can run the container. For the fargate launch type, it does not refer to any particular server. The choice of the server is determined by Amazon and is opaque to the user. The user only need to specify the CPU and memory that a container needs. Therefore, it greatly simplifies the deployment of the container. 

In this vignette, We use the foreach redis parallel backend and deploy the container using the ECS fargate launch type. Below is the diagram of how `DockerParallel` works with ECS and Bioconductor foreach redis container

![](fargate.jpg)

The cluster object is created in your local R session, but the workers and redis server are from Amazon ECS. Each docker container contains one or more R workers, they will receive jobs sent by your local R session and do the parallel computing. The workflow of the `DockerParallel` package is as follow

1. Select a cloud provider(default: ECSProvider)
2. Select a container(default: BiocFERContainer)
3. Create the cluster and run your parallel task

In the rest of the vignette we will introduce them step by step


# Select a cloud provider
Even though the topic says "select", currently the only available cloud provider is the before-mentioned ECS fargate provider. You can call `ECSFargateProvider` to create an ECS fargate provider 

```r
provider <- ECSFargateProvider()
provider
#> Cluster name:         R-worker-cluster 
#> Server task definition:      R-server-task-definition 
#> Worker task definition:      R-worker-task-definition 
#> Security group name:  R-parallel-security-group
```
We would not discuss the details of the cloud provider in this vignette as it is off-topic for using the package, users only need to know that the cloud provider is changeable and different providers provide different cloud service.

## Credentials
For communicating with the cloud, you need to authenticate with the cloud provider. Amazon cloud uses `access key id` and `secret access key` to verify your identity. You can find the instruction on how to download your credentials from [AWS Documentation]. The ECS fargate provider uses `aws_set_credentials` from the package `aws.ecx` to find your credentials

```r
aws.ecx::aws_set_credentials()
#> $access_key_id
#> [1] "AK**************OYX3"
#> 
#> $secret_access_key
#> [1] "mL**********************************XGGH"
#> 
#> $region
#> [1] "ap-southeast-1"
```
`aws_set_credentials` determines your credentials as well as the region of the cloud service. The region is the physical location of the cloud servers that will run your worker nodes. The function uses a variety of ways to find such information. The most important methods are as follow(sorted by the search order):

1. user-supplied values passed to the function

2. environment variables `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_DEFAULT_REGION`, and `AWS_SESSION_TOKEN`

You can either explicitly specify them or provide them as environment variables.

[AWS Documentation]: https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html#Using_CreateAccessKey

# Select a container
Similar to the cloud provider, the container is also changeable and provide different running environment and parallel backend. Currently the only available container is `BiocFERContainer`(stands for `Bioconductor ForEach Redis Container`). Users can create a `BiocFERContainer` worker container by

```r
workerContainer <- BiocFERWorkerContainer()
workerContainer
#> Bioconductor foreach redis container reference object
#>   Image:      dockerparallel/parallel-redis-worker 
#>   maxWorkers: 4 
#>   Environment variables:
```
This should be enough for creating the cluster object. However, if you want to have more control over the container object, the server can be created separately via

```r
serverContainer <- BiocFERServerContainer()
serverContainer
#> Bioconductor foreach redis container reference object
#>   Image:      dockerparallel/parallel-redis-server 
#>   maxWorkers: 1 
#>   Environment variables:
```


# Create the cluster and run your parallel task
Once you have selected the provider and containers, the cluster can be created by

```r
cluster <- makeDockerCluster(
  cloudProvider = provider,
  workerContainer = workerContainer,
  workerNumber = 1,
  workerCpu = 1024, workerMemory = 2048)
```
where `workerCpu` defines the CPU unit used by the worker container, 1024 CPU unit corresponds to a single CPU core. `workerMemory` defines the worker memory and the unit is `MB`. The server and worker containers will be implicitly generated from the container object you passed to the function. 

Until now, the cluster has not been started and nothing is running on the cloud, you need to start the cluster by

```r
cluster$startCluster()
#> Initializing the ECS provider
#> Launching server
#> Deploying server container
#> The cluster has 1 workers
#> Deploying worker container
#> Registering foreach redis backend, it might take a few minutes
```
Depending on the parallel backend, some clusters may supports adjusting the worker number after the cluster has been running

```r
cluster$setWorkerNumber(2)
#> Deploying worker container
```

Once the cluster has been started, you can use the `foreach` function to do the parallel computing as usual

```r
library(foreach)
foreach(i = 1:2)%dopar%{
   Sys.info()
}
#> [[1]]
#>                                          sysname                                          release 
#>                                          "Linux"                  "4.14.225-168.357.amzn2.x86_64" 
#>                                          version                                         nodename 
#>            "#1 SMP Mon Mar 15 18:00:02 UTC 2021" "ip-10-0-83-172.ap-southeast-1.compute.internal" 
#>                                          machine                                            login 
#>                                         "x86_64"                                        "unknown" 
#>                                             user                                   effective_user 
#>                                           "root"                                           "root" 
#> 
#> [[2]]
#>                                          sysname                                          release 
#>                                          "Linux"                  "4.14.225-168.357.amzn2.x86_64" 
#>                                          version                                         nodename 
#>            "#1 SMP Mon Mar 15 18:00:02 UTC 2021" "ip-10-0-83-172.ap-southeast-1.compute.internal" 
#>                                          machine                                            login 
#>                                         "x86_64"                                        "unknown" 
#>                                             user                                   effective_user 
#>                                           "root"                                           "root"
```
After finishing the computation, you can stop the cluster via

```r
cluster$stopCluster()
#> Stopping cluster
#> deregistering foreach backend
```
By default, the cluster will step itself if it has been removed from the R session, but we recommended to explicitly stop it after use.

# Advanced topic
In this document, we will introduce some advanced topics related to the package. Please note that the some features described in this section may not be available for all cloud providers and containers, they are only tested at the ECS fargate provider and Bioconductor foreach Redis container.

## Installing the system packages
Some R packages require the system dependence before they can be installed. The system packages can be added via `addSysPackages` in the container object. For example

```r
cluster$workerContainer$addSysPackages(c("package1", "package2"))
cluster$workerContainer$getSysPackages()
#> [1] "package1" "package2"
```
When the contain is deployed, the system packages will be installed via `apt-get install` before the start of the R workers. Note that this function only work with the incoming worker container. It has no effect on the current running container. The Bioconductor foreach Redis server container does not support this operation. You can also set the system packages using

```r
cluster$workerContainer$setSysPackages(c("package1", "package2"))
```
The difference between `addSysPackages` and `setSysPackages` is that the latter one will overwrite the existing package setting.

## Installing the R packages
Users can ask the workers to install the R packages before connecting with the server via `addRPackages` in the container object. The workers will install the packages through `AnVIL::install`, which provides the fast binary installation. If the binary is not available, it will call `BiocManager::install` to install the package. This makes it possible to install both the binary package and GitHub package. For example, the package `S4Vectors` can be installed via

```r
cluster$workerContainer$addRPackages("S4Vectors")
cluster$workerContainer$getRPackages()
#> [1] "S4Vectors"
```
Similar to `addSysPackages`, this feature does not work with the existing container. The Bioconductor foreach Redis server container does not support this operation. The `setRPackages` is also available if preserving the previous package setting is not required.

## SSH to the container
Sometimes it might be helpful to ssh into the container and look at the debug information. The package can search for your public key file through the environment variable `DockerParallelSSHPublicKey`. If the public key file is available, it can be used by the container object and allow you to access the container using your private key. If you want to explicitly provide a public key, this can be done via
```
setSSHPubKeyPath(publicKey = "Path-to-your-key")
```
If you want to see the current key file
```
getSSHPubKeyPath()
```
Note that the availability of the SSH service depends on the implementation of both the cloud provider and container. The formal one needs to allow the ssh access to the container and the latter one needs to run the SSH server inside the container. The ECS fargate provider and the Bioconductor foreach Redis container support the SSH access. If the Bioconductor foreach Redis container is used, users can find the worker's log file from `/workspace/log`


# Session info

```r
sessionInfo()
#> R version 4.0.4 (2021-02-15)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 10 x64 (build 19042)
#> 
#> Matrix products: default
#> 
#> locale:
#> [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
#> [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
#> [5] LC_TIME=English_United States.1252    
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] DockerParallel_0.99.0 readr_1.4.0           foreach_1.5.1        
#> 
#> loaded via a namespace (and not attached):
#>  [1] pillar_1.4.6        compiler_4.0.4      base64enc_0.1-3     iterators_1.0.13   
#>  [5] doRedis_2.0.1       tools_4.0.4         testthat_3.0.2      digest_0.6.27      
#>  [9] pkgload_1.1.0       jsonlite_1.7.2      evaluate_0.14       lifecycle_0.2.0    
#> [13] tibble_3.0.4        pkgconfig_2.0.3     rlang_0.4.10        cli_2.3.1          
#> [17] rstudioapi_0.13     curl_4.3            parallel_4.0.4      xfun_0.19          
#> [21] withr_2.3.0         httr_1.4.2          stringr_1.4.0       xml2_1.3.2         
#> [25] knitr_1.31          desc_1.2.0          vctrs_0.3.4         hms_0.5.3          
#> [29] rprojroot_2.0.2     glue_1.4.2          R6_2.5.0            redux_1.1.0        
#> [33] aws.ecx_1.0.4       adagio_0.7.1        magrittr_1.5        codetools_0.2-18   
#> [37] ellipsis_0.3.1      assertthat_0.2.1    aws.signature_0.6.0 stringi_1.5.3      
#> [41] crayon_1.3.4        rjson_0.2.20
```

