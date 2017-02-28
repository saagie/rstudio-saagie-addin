# Check if all fields (containing the information of the platform) are OK
#' Title
#'
#' @param path 
#' @param input 
#'
#' @return
#' @export
#'
#' @examples
validator.infoPlatform <- function(path, input){
  if(is.null(input$platformURL) || input$platformURL == "" ||
     is.null(input$user) || input$user == "" ||
     is.null(input$password) || input$password == "" || input$testConnection == FALSE){
    shinyjs::disable("addSelectPlatform")
  }else{
    validator.testConnection(path, input)
  }
}

# Check the connection to the platform
#' Title
#'
#' @param path 
#' @param input 
#'
#' @return
#' @export
#'
#' @examples
validator.testConnection <- function(path, input){
  # repCon <- GET(paste(input$platformURL,"/login",sep=""),authenticate(input$user,input$password,type="basic"))
  repCon <- model.recoverNamePlatform(path, input)
  status <- httr::http_status(repCon)
  namePlatform <- model.readNamePlatform(path)
  model.rmNamePlatform(path, namePlatform)
  # One cannot rely on 'status' == 'Success' or 200 because bad login results in 200
  if(status[[1]]=="Success"){
    model.recoverNamePlatform(path, input)
    view.successConnection()
  }else{
    view.errorConnection()
  }
}

#' Check if all fields (containing job information) are OK
#'
#' @param input We don't know yet.
#'
#' @return We don't know yet.
#' @importFrom shinyjs info
#'
validator.infoJob <- function(input){
  shinyjs::disable("runAddDeploy")
  observeEvent(input$addDeploy,{
    if(is.null(input$createJobName) || input$createJobName == ""){
      info("Your job name isn't filled.")
    }else{
      shinyjs::enable("runAddDeploy")
    }
  })
}

#' Title
#'
#' @param input 
#'
#' @return
#' @export
#'
#' @examples
validator.mail <- function(input){
  if(input$createEmail == "" || is.null(input$createEmail)){
    shinyjs::enable("addDeploy")
  }else{
    mail <- tstrsplit(input$createEmail,"@")
    if (length(mail) == 2){
      if(mail[[1]] == ""){
        shinyjs::disable("addDeploy")
      }else{
        mail <- tstrsplit(mail[[2]],"\\.")
        if(length(mail) == 2){
          shinyjs::enable("addDeploy")
        }else{
          shinyjs::disable("addDeploy")
        }
      }
    }else{
      shinyjs::disable("addDeploy")
    }
  }
}

#' Title
#'
#' @param file 
#'
#' @return
#' @export
#'
#' @examples
validator.filePlatformConform <- function(res,file){
  if(length(names(res)) == 5){
    if(!((names(res[1]) == "user") && (names(res[2]) == "platformName") && (names(res[3]) == "password") && (names(res[4]) == "platformURL") && (names(res[5]) == "idPlatform"))){
      res <- data.frame("user" = character(0), "platformName" = character(0),
                        "password" = character(0), "platformURL" = character(0), "idPlatform" = integer(0), stringsAsFactors = FALSE)
      write.csv(res, file = file, row.names = FALSE)
    }
  }else{
    res <- data.frame("user" = character(0), "platformName" = character(0),
                      "password" = character(0), "platformURL" = character(0), "idPlatform" = integer(0), stringsAsFactors = FALSE)
    write.csv(res, file = file, row.names = FALSE)
  }
  res
}

#' Title
#'
#' @param res 
#' @param file 
#'
#' @return
#' @export
#'
#' @examples
validator.fileNamesPlatformConform <- function(res,file){
  if(length(names(res)) == 2){
    if(!((names(res[1]) == "id") && (names(res[2]) == "Names"))){
      res <- data.frame("id" = integer(0), "Names" = character(0), stringsAsFactors = FALSE)
      write.csv(res, file = file, row.names = FALSE)
    }
  }else{
    res <- data.frame("id" = integer(0), "Names" = character(0), stringsAsFactors = FALSE)
    write.csv(res, file = file, row.names = FALSE)
  }
  res
}

#' Title
#'
#' @param res 
#' @param file 
#'
#' @return
#' @export
#'
#' @examples
validator.fileTableJobConform <- function(res,file){
  if(length(names(res)) == 7){
    if(!(names(res[1]) == "idJob" && (names(res[2]) == "idPlatform") && (names(res[3]) == "capsule") && (names(res[4]) == "category")
         && (names(res[5]) == "numVersion") && (names(res[6]) == "nameScript") && (names(res[7]) == "nameJob"))){
      res <- data.frame("idJob" = integer(0), "idPlatform" = character(0), "capsule" = character(0), "category" = character(0),
                        "numVersion" = integer(0), "nameScript" = character(0), "nameJob" = character(0), stringsAsFactors = FALSE)
      write.csv(res, file = file, row.names = FALSE)
    }
  }else{
    res <- data.frame("idJob" = integer(0), "idPlatform" = character(0), "capsule" = character(0), "category" = character(0),
                      "numVersion" = integer(0), "nameScript" = character(0), "nameJob" = character(0), stringsAsFactors = FALSE)
    write.csv(res, file = file, row.names = FALSE)
  }
  res
}