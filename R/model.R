# Read a file where there are the different Platform name
model.readTablePlatform <- function(path){
  file <- file.path(path, "platform", "platform.csv")
  if (file.exists(file)) {
    res <- read.csv(file = file, sep=",", stringsAsFactors = FALSE)
  } else {
    res <- data.frame("user" = character(0), "platformName" = character(0),
                      "password" = character(0), "platformURL" = character(0), "idPlatform" = integer(0), stringsAsFactors = FALSE)
    write.csv(res, file = file, row.names = FALSE)
  }
  res
}

# Read a file containing job R
model.readTableJob <- function(path){
  file <- file.path(path, "platform", "job.csv")
  if (file.exists(file)) {
    res <- read.csv(file = file, sep=",", stringsAsFactors = FALSE)
  } else {
    res <- data.frame("idJob" = integer(0), "idPlatform" = character(0), "capsule" = character(0), "category" = character(0),
                      "numVersion" = integer(0), "nameScript" = character(0), "nameJob" = character(0), stringsAsFactors = FALSE)
    write.csv(res, file = file, row.names = FALSE)
  }
  res
}

# Read a file containing the Platform where the job is upload
model.readThePlatform <- function(path){
  file <- file.path(path, "platform", "thePlatform.csv")
  if (file.exists(file)) {
    res <- read.csv(file = file, sep=",", stringsAsFactors = FALSE)
  } else {
    res <- data.frame("user" = character(0), "platformName" = character(0), 
                      "password" = character(0), "platformURL" = character(0), "idPlatform" = integer(0), stringsAsFactors = FALSE)
    write.csv(res, file = file, row.names = FALSE)
  }
  res
}

# Read a file containing the different platform where the user is access
model.readNamePlatform <- function(path){
  file <- file.path(path, "platform", "namePlatform.csv")
  if (file.exists(file)) {
    res <- read.csv(file = file, sep=",", stringsAsFactors = FALSE)
  } else {
    res <- data.frame("id" = integer(0), "Names" = character(0), stringsAsFactors = FALSE)
    write.csv(res, file = file, row.names = FALSE)
  }
  res
}

# Read a file containing the num where upgrade the job
model.readNumJob <- function(path){
  file <- file.path(path, "platform", "row.csv")
  if (file.exists(file)) {
    res <- read.csv(file = file, sep=",", stringsAsFactors = FALSE)
  } else {
    res <- data.frame("x" = character(0), stringsAsFactors = FALSE)
    write.csv(res, file = file, row.names = FALSE)
  }
  res
}

#' Update "Table Platform" when the new information about the platform is writing
#'
#' @param path Path to local persistent Saagie folder
#' @param input We don't know yet.
#'
#' @importFrom RCurl base64Encode
model.updateTablePlatform <- function(path, input){
  dataPlatform <- model.readTablePlatform(path)
  if(input$addPlatform){
    nb <- nrow(dataPlatform)+1
    dataPlatform[nb,1] <- input$user
    dataPlatform[nb,2] <- input$platformName
    dataPlatform[nb,3] <- base64Encode(input$password)
    dataPlatform[nb,4] <- input$platformURL
    namePlatform <- model.readNamePlatform(path)
    for(i in 1:nrow(namePlatform)){
      if(namePlatform[i,2]==input$platformName){
        dataPlatform[nb,5] <- namePlatform[i,1]
      }
    }
    write.csv(dataPlatform, file = file.path(path, "platform", "platform.csv"), row.names = FALSE)
    # If this is the first platform to be added, also add it to 'thePlatform.csv' (current platform)
    if (nrow(model.readThePlatform(path)) == 0) {
      write.csv(dataPlatform[nb, ], file = file.path(path, "platform", "thePlatform.csv"), row.names = FALSE)
    }
  }
}
#' Add the Job R in file (Depends on the platform)
#'
#' @param path Path to local persistent Saagie folder
#'
#' @importFrom httr GET authenticate content
#' @importFrom RCurl base64Decode
#' @return We don't know yet.
#'
model.JobRPlatform <- function(path){
  dataPlatform <- model.readThePlatform(path)
  withProgress({
    reponse <- GET(paste(dataPlatform[1,4],"/api/v1/platform/",dataPlatform[1,5],"/job",sep=""),
                   authenticate(dataPlatform[1,1],base64Decode(dataPlatform[1,3]),type="basic"))
  }, message = "Retrieving list of jobs from Saagie")
  # TODO: Replace "as = 'parsed'" since it is strongly discouraged in a package. See 'help(content)'
  job <- content(reponse,type="application/json")
  j=1
  listJob <- model.readTableJob(path)
  for(i in seq_along(job)){
    if(job[[i]]["capsule_code"]=="r"){
      listJob[j, 1:4] <- job[[i]][1:4]
      j = j+1
    }
  }
  write.csv(listJob, file = file.path(path, "platform", "job.csv"), row.names = FALSE)
}

# Post a Job in the platform
model.postJob <- function(path, input, pathNameFile){
  thePlatform <- model.readThePlatform(path)
  fileName <- model.uploadFile(thePlatform, pathNameFile)
  print(paste("File NAME : ", fileName))
  reponseAdd <- model.uploadJob(input,thePlatform,fileName)
  return(list(ThePlatform = thePlatform, ReponseAdd = reponseAdd))
}

# Post a File
model.uploadFile <- function(thePlatform, pathNameFile){
  reponseFile <- POST(paste(thePlatform[1,4],"/api/v1/platform/",thePlatform[1,5],"/job/upload",sep=""),
                      authenticate(thePlatform[1,1],base64Decode(thePlatform[1,3]),type="basic"),
                      body=list(file = httr::upload_file(pathNameFile)), httr::verbose())
  fileName <- content(reponseFile,type="application/json")
  return(fileName)
}
# Post a Job
model.uploadJob <- function(input,thePlatform,fileName){
  req_ <-  POST(paste(thePlatform[1,4],"/api/v1/platform/",thePlatform[1,5],"/job",sep=""),
                authenticate(thePlatform[1,1],base64Decode(thePlatform[1,3]),type="basic"),
                body=list(platformId = thePlatform[1,5], capsule_code = "r", category = "processing",
                          manual = TRUE, current =list(template=input$createCommandLine, file=fileName[[1]]), options=list(""),
                          name = input$createJobName, retry = "",
                          schedule = "R0/2016-05-30T10:27:49.635Z/P0Y0M1DT0H0M0S", email=input$createEmail), encode="json")
}

#' Run a Job
#'
#' @param thePlatform don't know yet
#' @param reponseAdd don't know yet
#'
#' @return don't know yet
#' @importFrom httr POST
#'
model.runJob <- function(thePlatform,reponseAdd){
  idJobPlatform <- model.idJobPlatform(reponseAdd)
  urlRunJob <- paste(thePlatform[1,4],"/api/v1/platform/",thePlatform[1,5],"/job/",idJobPlatform,"/run",sep="")
  req_ <-POST(urlRunJob,authenticate(thePlatform[1,1],base64Decode(thePlatform[1,3]),type="basic"))
  return(idJobPlatform)
}

model.idJobPlatform <- function(reponseAdd){
  addDeployName <- content(reponseAdd,type="application/json")
  idJobPlatform <- addDeployName[[1]]
  return(idJobPlatform)
}

# Empty the file who containing the Job
model.rmJob <- function(path){
  # data <- model.readTableJob(path = path)
  # # data <- read.csv(file= file.path(path, "platform", "job.csv"),sep=",", stringsAsFactors = FALSE)
  # if(nrow(data)!=0){
  #   for(i in 0:nrow(data)){
  #     data <- data[-i,]
  #   }
  #   data <- write.csv(data,file = file.path(path, "platform", "job.csv"), row.names = FALSE)
  # }
  data <- data.frame("idJob" = integer(0), "R" = character(0), "Category" = character(0), "Script" = character(0), stringsAsFactors = FALSE)
  write.csv(data, file = file.path(path, "platform", "job.csv"), row.names = FALSE)
}

# Add the default platform
model.defaultPlatform <- function(path){
  platformRun <- model.readThePlatform(path)
  userGo <- platformRun[1,1]
  platformNameGo <- platformRun[1,2]
  return(list(UserGo = userGo, PlatformNameGo = platformNameGo))
}

# Add the platform in file "ThePlatform.csv"
model.addThePlatformInFile <- function(path, userGo,platformNameGo,mdp,adressPlatform,numPlatform){
  platformRun <- model.readThePlatform(path = path)
  platformRun[1,1] <- userGo
  platformRun[1,2] <- platformNameGo
  platformRun[1,3] <- mdp
  platformRun[1,4] <- adressPlatform
  platformRun[1,5] <- numPlatform
  if (complete.cases(platformRun)) write.csv(platformRun,file = file.path(path, "platform", "thePlatform.csv"), row.names = FALSE)
  invisible(NULL)
}

# Add the select platform
model.selectPlatform <- function(path, nb_row){
  readRow <- model.readTablePlatform(path)
  userGo <- readRow[nb_row,1]
  platformNameGo <- readRow[nb_row,2]
  mdp <- readRow[nb_row,3]
  adressPlatform <- readRow[nb_row,4]
  numPlatform <- readRow[nb_row,5]
  return(list(UserGo = userGo,PlatformNameGo = platformNameGo,Mdp = mdp,AdressPlatform = adressPlatform,NumPlatform = numPlatform))
}

# Empty the file who containing the names platform
model.rmNamePlatform <- function(path, namePlatform){
  if(nrow(namePlatform)!=0){
    for(i in 0:nrow(namePlatform)){
      namePlatform <- namePlatform[-i,]
    }
    write.csv(namePlatform,file = file.path(path, "platform", "namePlatform.csv"), row.names = FALSE)
  }
}

# Add the different name platform where the user is access (in file namePlatform.csv)
model.addNamePlatform <- function(path, repPlatform) {
  # namePlatform <- read.csv(file = file.path(path, "platform", "namePlatform.csv"), sep=",", stringsAsFactors = FALSE)
  namePlatform <- model.readNamePlatform(path = path)
  nb <- nrow(namePlatform)+1
  for(i in repPlatform){
    namePlatform[nb,1] <- i$id
    namePlatform[nb,2] <- i$name
    nb <- nb+1
  }
  write.csv(namePlatform,file = file.path(path, "platform", "namePlatform.csv"), row.names = FALSE)
}

# Recover the name Platform when the success connection
model.recoverNamePlatform <- function(path, input){
  platform <- GET(paste(input$platformURL,"/api/v1/platform", sep=""),
                  authenticate(input$user,input$password,type="basic"))
  repPlatform <- content(platform,type="application/json")
  model.addNamePlatform(path, repPlatform)
  # Return result of API request
  platform
}

# Upgrade a job in the platform Saagie
model.upgradeJob <- function(path_to_persistent_saagie_files, input,idJob,pathNameFile){
  thePlatform <- model.readThePlatform(path_to_persistent_saagie_files)
  model.infoJobUpgrade(thePlatform,idJob)
  fileName <- model.uploadFile(thePlatform,pathNameFile)
  version <- model.newVersion(input,thePlatform,idJob,fileName)
  reponseAdd <- GET(paste(thePlatform[1,4],"/api/v1/platform/",thePlatform[1,5],"/job/",idJob, sep=""),
                    authenticate(thePlatform[1,1],base64Decode(thePlatform[1,3]),type="basic"))
  #model.runJob(thePlatform, reponseAdd)
  print(reponseAdd)
  return(list(ThePlatform = thePlatform, ReponseAdd = reponseAdd))
}

# Post a new version (upgrade)
model.newVersion <- function(input,thePlatform,idJob,fileName){
  req_ <- POST(paste(thePlatform[1,4],"/api/v1/platform/",thePlatform[1,5],"/job/",idJob,"/version", sep=""),
               authenticate(thePlatform[1,1],base64Decode(thePlatform[1,3]),type="basic"),
               body=list(platformId = 2, capsule_code = "r", category = "processing",
                         manual = TRUE, current =list(template=input$upgradeCommandLine, file=fileName[[1]]), options=list(""),
                         retry = "",
                         schedule = "R0/2016-05-30T10:27:49.635Z/P0Y0M1DT0H0M0S"), encode="json")
}

# Informations about the job upgrade
model.infoJobUpgrade <- function(thePlatform,idJob){
  reponse <- GET(paste(thePlatform[1,4],"/api/v1/platform/",thePlatform[1,5],"/job/",idJob,sep=""),
                 authenticate(thePlatform[1,1],base64Decode(thePlatform[1,3]),type="basic"))
  infoJob <- content(reponse,type="application/json")
}
# Post a upgrade
model.postUpgrade <- function(thePlatform,idJob,fileName){
  GET(paste(thePlatform[1,4],"/api/v1/platform/",thePlatform[1,5],"/job/",idJob, sep=""),
      authenticate(thePlatform[1,1],base64Decode(thePlatform[1,3]),type="basic"),
      body=list(platformId = 2, capsule_code = "r", category = "processing",
                manual = TRUE, current =list(template="Rscript {file} arg1 arg2", file=fileName[[1]]), options=list(""),
                name = "charline", retry = "",
                schedule = "R0/2016-05-30T10:27:49.635Z/P0Y0M1DT0H0M0S", email="char"), encode="json")
}

# Return a current version, name job and name script of job R
model.currentVersion <- function(path,jobs,thePlatform){
  withProgress({
    for (i in seq_len(nrow(jobs))) {
      response <- GET(paste0(thePlatform[1, 4], "/api/v1/platform/", thePlatform[1, 5], "/job/", jobs[i,1]),
                      authenticate(thePlatform[1, 1], base64Decode(thePlatform[1, 3]), type = "basic"))
      detailsJob <- content(response, type = "application/json")
      # jobs[i, 5] <- detailsJob[[3]][[3]]
      jobs[i, "numVersion"] <- detailsJob[["current"]][["number"]]
      # jobs[i, 6] <- detailsJob[[3]][[5]]
      jobs[i, "nameScript"] <- detailsJob[["current"]][["file"]]
      # jobs[i, 7] <- detailsJob[[7]]
      jobs[i, "nameJob"] <- detailsJob$name
    }
  }, message = view.messageBarProgress())
  jobs <- jobs[order(jobs[,1],decreasing = T),]
  write.csv(jobs,file = file.path(path, "platform", "job.csv"), row.names = FALSE)
  #jobs <- jobs[, -1:-3]
  #print(jobs[i, 6])
  return(jobs)
}

# Recover a log
model.showLog <- function(thePlatform,idJob){
  reponseLog <- GET(paste(thePlatform[1,4],"/api/v1/platform/",thePlatform[1,5],"/job/",idJob, "/jobinstance", sep=""),
                    authenticate(thePlatform[1,1],base64Decode(thePlatform[1,3]),type="basic"))
  infoLog <- content(reponseLog, type="application/json")
  idLog <- infoLog[[1]]$id
  test <- GET(paste(thePlatform[1,4],"/api/v1/jobinstance/",idLog, sep=""),
              authenticate(thePlatform[1,1],base64Decode(thePlatform[1,3]),type="basic"))
  logsJob <- content(test, type="application/json")
  return(list(LogsJob = logsJob, IdLog = idLog))
}

# Recover a log "stdout" (when the job is Run) in file "stdout.csv"
model.downloadStdout <- function(thePlatform,test){
  data <- GET(paste(thePlatform[1,4],"/api/v1/jobinstance/",test[['IdLog']],"/stdout", sep=""),
              authenticate(thePlatform[1,1],base64Decode(thePlatform[1,3]),type="basic"))
  stdout <- content(data, as="text", type="read")
  pathFile <- paste(getwd(),"/inst/file",sep="")
  if (!dir.exists(pathFile)) dir.create(pathFile)
  write.csv(stdout, file = file.path(pathFile, "saagie-stdout.csv"), row.names = FALSE)
}

# Recover a log "stderr" (when the job is Run) in file "stderr.csv"
model.downloadStderr <- function(thePlatform, test){
  data <- GET(paste(thePlatform[1,4],"/api/v1/jobinstance/",test[['IdLog']],"/stderr", sep=""),
              authenticate(thePlatform[1,1],base64Decode(thePlatform[1,3]),type="basic"))
  stderr <- content(data,as="text", type="read")
  pathFile <- paste(getwd(),"/inst/file",sep="")
  if (!dir.exists(pathFile)) dir.create(pathFile)
  write.csv(stderr, file = file.path(pathFile, "saagie-stderr.csv"), row.names = FALSE)
}

# Write a file who upload in a platform
model.writeFile <- function(document, nameFile) {
  #pathNameFile <- paste("platform/", nameFile, ".R", sep="")
  pathNameFile <- tempfile()
  write(document, pathNameFile)
  return(pathNameFile)
}

# Remove a file who upload in a platform
model.removeFile <- function(pathNameFile){
  file.remove(pathNameFile)
}
#' Remove no script R and displays the name of script R
#'
#' @param jobs We don't know yet.
#'
#' @return We don't know yet.
#' @importFrom data.table tstrsplit
#' @importFrom stringr str_sub
#'
model.removeLinkedNoR <- function(jobs){
  for(i in 1:nrow(jobs)){
    if((str_sub(jobs[i,3],-2, -1) == ".R")){
      if(length(tstrsplit(jobs[i,3],"-"))==2){
        name <- tstrsplit(jobs[i,3],"-")
        jobs[i,3] <- name[[2]]
      }else{
        jobs[i,3] <- jobs[i,3]
      }
    }else if ((str_sub(jobs[i,3],-2, -1) == ".r")){
      if(length(tstrsplit(jobs[i,3],"-"))==2){
        name <- tstrsplit(jobs[i,3],"-")
        jobs[i,3] <- name[[2]]
      }else{
        jobs[i,3] <- jobs[i,3]
      }
    }else if ((str_sub(jobs[i,3],-4, -1) == ".zip")){
      jobs[i,3] <- "File Zip"
    }else if ((str_sub(jobs[i,3],-3, -1) == ".py")){
      jobs[i,3] <- "File Python"
    }else if ((str_sub(jobs[i,3],-3, -1) == ".fr") || (str_sub(jobs[i,3],-3, -1) == ".com")){
      jobs[i,3] <- "Web"
    }else if ((str_sub(jobs[i,3],-2, -2) != ".")){
      jobs[i,3] <- jobs[i,3]
    }
  }
  return(jobs)
}