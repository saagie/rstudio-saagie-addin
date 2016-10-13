# Check if all fields (containing the information of the platform) are OK
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
