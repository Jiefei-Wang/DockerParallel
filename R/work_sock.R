get_R_worker_cmd <- function(R_exec, port, R_options=""){
  default_options <-"--default-packages=datasets,utils,grDevices,graphics,stats,methods -e \\\"workRSOCK <- tryCatch(parallel:::.slaveRSOCK, error=function(e) parallel:::.workRSOCK); workRSOCK()\\\" MASTER=localhost OUT=/dev/null TIMEOUT=2592000 XDR=FALSE"
  cmd <-paste(R_exec, default_options, paste0("PORT=",port) , paste(R_options))
  cmd
}

get_ssh_cmd<- function(ssh_exec, host, remote_port, local_port,
                           ssh_options="", ssh_command="", port = 22){
  ssh_opt <- paste(ssh_options, "-R", paste0(remote_port,":127.0.0.1:",local_port))
  if(port!=22){
    ssh_opt <- paste(ssh_opt, "-p", port)
  }

  cmd <-paste(ssh_exec, ssh_opt, host, paste(ssh_command))
  cmd
}
test1<-function(){
  host_ip <- "18.141.211.89"
  remote_port <- 11625
  local_port <- 11987
  ssh_exec <- "C:\\Windows\\System32\\OpenSSH\\ssh.exe"
  ssh_r <- get_R_worker_cmd("Rscript", remote_port)
  ssh_cmd <- get_ssh_cmd(ssh_exec,paste0("root@",host_ip),
                         remote_port,local_port,ssh_command = ssh_r)
  system(ssh_cmd, wait = TRUE, input = "")
}
test2<-function(){
  library(parallel)
  machineAddresses <-list(list(host='18.141.211.89',user='root',manual=T))
  cl <- makePSOCKcluster(machineAddresses)
  parallelly::makeClusterPSOCK
  parallelly::makeNodePSOCK
}


















