# The first view
view.activate <- function() {
  fluidPage(
    includeCSS(file.path(system.file(package = "Saagie"), "css", "style.css")),
    shinyjs::useShinyjs(),
    
    # Formatting the header
    shinyjs::hidden(div(id = "barAddPlatform", view.barAddPlatform())),
    shinyjs::hidden(div(id = "barUpgradeJob", view.barUpgradeJob())),
    shinyjs::hidden(div(id = "barSelectCreateJob", view.barSelectCreatePlatform())),
    shinyjs::hidden(div(id = "barCreateNewJob", view.barCreateNewJob())),
    shinyjs::hidden(div(id = "barStateJob", view.barStateJob())),
    div(id = "barSelectPlatform", view.barSelectPlatform()),
    
    # Add the platform default in view
    br(),
    align="center",
    tags$style(type='text/css',
               '#rowSelectPlatform {background-color: #EBECEC; color: #595B60;
               width:70%; border:1px; font-weight:bold; text-align:center;}'),
    div(id="defaultPlatform", verbatimTextOutput('rowSelectPlatform')),
    shinyjs::hidden(
      tags$style(type = 'text/css', 
                 '#rowSelectJob {background-color: #EBECEC; color: #595B60;
                      width:70%; border:1px; font-weight:bold; text-align:center;}'),
      div(id="defaultUpgradeJob",verbatimTextOutput('rowSelectJob'))
    ),

    # Formatting the page "Add Platform"
    shinyjs::hidden(div(id = "pageAddPlatform", view.pageAddPlatform())), 

    # Formatting the first page "Select Platform"
    div(id = "pageSelectPlatform", view.pageSelectPlatform()), 

    # Formatting the page "Upgrade Job"
    shinyjs::hidden(div(id = "pageUpgradeJob", view.pageUpgradeJob())),
    
    # Formatting the page "Select or Create Job"
    shinyjs::hidden(div(id = "pageSelectCreateJob", view.pageSelectCreateJob())),
    
    # Formatting the page "Create New Job"
    shinyjs::hidden(div(id = "pageCreateNewJob", view.pageCreateNewJob())), 
    
    # Formatting the page "State of Job"
    shinyjs::hidden(div(id = "pageStateJob", view.pageStateJob())), 

    br(), br(),
    imageOutput("heron")
  )
}

# List all pages
view.allPages <- list("pageAddPlatform", "barAddPlatform",
                      "barUpgradeJob","pageUpgradeJob",
                      "barSelectPlatform", "pageSelectPlatform",
                      "pageSelectCreateJob","barSelectCreateJob",
                      "pageCreateNewJob","barCreateNewJob",
                      "defaultPlatform", "defaultUpgradeJob",
                      "pageStateJob", "barStateJob")

# Hide pages
view.hidePages <- function() {
  for (page in view.allPages) {
    shinyjs::hide(page)
  }
}

#' Show the page "Add Platform"
#'
#' @return We don't know yet.
#' @importFrom shinyjs show
#'
view.showAddPlatform <- function() {
  view.hidePages()
  show("pageAddPlatform", anim = TRUE)
  show("barAddPlatform")
}

# Show the page "Select Or Create Job
view.showSelectCreateJob <- function() {
  view.hidePages()
  show("defaultPlatform")
  show("defaultUpgradeJob")
  show("pageSelectCreateJob")
  show("barSelectCreateJob")
}

# Show the page "Create a new Job"
view.showCreateNewJob <- function(){
  view.hidePages()
  show("defaultPlatform")
  show("pageCreateNewJob")
  show("barCreateNewJob")
}

# Show the page "Upgrade a Job"
view.showUpgrade <- function(){
  view.hidePages()
  show("defaultPlatform")
  show("defaultUpgradeJob")
  show("pageUpgradeJob")
  show("barUpgradeJob")
}

# Show the page "Select a platform"
view.showSelectPlatform <- function(){
  view.hidePages()
  show("defaultPlatform")
  show("barSelectPlatform")
  show("pageSelectPlatform")
}

# Show the page "Success Job Or Error Job"
view.showStateJob <- function(){
  view.hidePages()
  show("barStateJob")
  show("pageStateJob")
}

# Show the table containing the names of platform (in the page "Select a platform")
view.showTablePlatform <- function(dataPlatform,output){
  dataPlatformCopy <- dataPlatform
  dataPlatformCopy <- dataPlatformCopy[,-3:-5]
  names(dataPlatformCopy) <-c("User","Platform Name")
  output$table <- DT::renderDataTable(DT::datatable(
    dataPlatformCopy,
    options=list(searching=FALSE, paging=FALSE, info=FALSE),
    selection = list(mode = "single", selected = 1, target = "row")
  ), server = TRUE)
}

# Show the table containing the names of Job (in page "Select or Create New Job")
view.showTableJob <- function(jobs,output){
  # names(jobs) <-  c("Category", "Current Version","Linked R Script","Job Name")
  #for(i in 1:nrow(jobs)) 
     #jobs[i,8] <- nrow(jobs)-i
  #jobs <- jobs[order(jobs[,5],decreasing=F), ]
  #jobs <- jobs[-5]
  output$newJob <- DT::renderDataTable(DT::datatable(
    jobs[order(jobs[,1],decreasing = T),],options=list(searching=FALSE, paging=FALSE, info=FALSE),
    selection = list(mode = "single", selected = 1, target = "row")
  ), server=TRUE)
}

# Details page "Add Platform"
view.pageAddPlatform <- function(){
  fluidRow(
    column(6,
           textInput("platformURL", "Platform URL :", value = "https://manager.prod.saagie.io"),
           textInput("user", "User :"),
           passwordInput("password", "Password :"),
           actionButton("testConnection", "Connection",
                        style='background-color:#EBECEC; color:#595B60; font-weight: bold'),
           br(),
           br(),
           shinyjs::hidden(htmlOutput("selectPlatformName")),
           actionButton("addSelectPlatform", "  Add  ",
                        style='background-color:#EBECEC; color:#595B60; font-weight: bold')
    ))
}

# Details page "Select Platform"
view.pageSelectPlatform <- function(){
  fluidRow(column(12,
                  actionButton("createJob", "Select or create a new job",
                               style='background-color:#EBECEC; color:#595B60; font-weight: bold;'),
                  br(),
                  br(),
                  DT::dataTableOutput("table")
  ))
}

# Details page "Upgrade Job"
view.pageUpgradeJob <- function(){
  fluidRow(
    column(6,
           br(),
           textInput("upgradeCommandLine", "Command Line", value="Rscript {file} arg1 arg2"),
           #checkboxInput("viewDocumentUpgrade","Preview the code"),
           shinyjs::hidden(div(id = "viewCodeUpgrade",uiOutput("documentUpgrade",
                                                               container = rCodeContainer,
                                                               inline=TRUE))),
           # mainPanel(
           #   tabsetPanel(type= "tabs",
           #               tabPanel("Manual",textInput("manualUpgrade","")),
           #               tabPanel("Schedule",textInput("scheduleUpgrade",""))
           #   )
           # ),
           br(),
           actionButton("upgradeDeploy","Upgrade & deploy", icon = icon("upload", lib="font-awesome"),
                        style='background-color:#EBECEC; color:#595B60; font-weight: bold')
    ),
    column(6,
           br(),
           shinyjs::hidden(actionButton('downloadDataStdoutUpgrade', 'Download STDOUT',
                                        style='background-color:#EBECEC; color:#595B60; font-weight: bold',
                                        icon = icon("download", lib = "font-awesome"))),
           shinyjs::hidden(actionButton('downloadDataStderrUpgrade', 'Download STDERR',
                                        style='background-color:#EBECEC; color:#595B60; font-weight: bold',
                                        icon = icon("download", lib = "font-awesome")))
           )
  )
}

# Details page "Select or Create Job"
view.pageSelectCreateJob <- function(){
  fluidRow(
    column(6,
           actionButton("createNewJob", "Create a new job from R Script",
                        style='background-color:#EBECEC; color:#595B60; font-weight: bold'),
           actionButton("upgradeJob", "Upgrade a job",
                        style='background-color:#EBECEC; color:#595B60; font-weight: bold'),
           br(),
           br(),
           DT::dataTableOutput("newJob"))
  )
}

# Details page "Create a New Job"
view.pageCreateNewJob <- function(){
  fluidRow(
    column(6,
           textInput("createJobName","Job Name",value=view.recoverNameFile()),
           textInput("createCommandLine", "Command Line", value="Rscript {file} arg1 arg2"),
           textInput("createEmail","Email"),
           #checkboxInput("viewDocument","Preview the code"),
           shinyjs::hidden(div(id = "viewCode",uiOutput("document", container = rCodeContainer, inline=TRUE))),
           # mainPanel(
           #   #align="center",
           #   tabsetPanel(type= "tabs",
           #               #align="center",
           #               tabPanel("Manual",textInput("manualCreate","")),
           #               tabPanel("Schedule",textInput("scheduleCreate",""))
           #   )
           # ),
           column(12,
                  actionButton("addDeploy","Add & deploy",
                               style='background-color:#EBECEC; color:#595B60; font-weight: bold')
                  # actionButton("runAddDeploy", "RUN",
                  #              style='background-color:#EBECEC; color:#595B60; font-weight: bold')
           )
           # br(),
           # column(12,
           #        shinyjs::hidden(actionButton("downloadDataStdout","Download STDOUT",
           #                                     icon = icon("download", lib = "font-awesome"),
           #                                     style='background-color:#EBECEC; color:#595B60; font-weight: bold')
           #        ),
           #        shinyjs::hidden(actionButton("downloadDataStderr", "Download STDERR",
           #                                     icon = icon("download", lib = "font-awesome"),
           #                                     style='background-color:#EBECEC; color:#595B60; font-weight: bold')
           #        )
           # )
  ))
}

# Details page "Success Job Or Error Job"
view.pageStateJob <- function(){
  fluidRow(
    column(6,
           shinyjs::hidden(div(id = "successStateAddJob", "Successfully deployed")),
           shinyjs::hidden(div(id = "errorStateAddJob", "Failed deployed")),
           shinyjs::hidden(div(id = "successUpgradeJob", "Upgrade successful")),
           shinyjs::hidden(div(id = "errorUpgradeJob", "Upgrade failed")),
           br(),
           shinyjs::hidden(div(id = "descriptionErrorAdd", htmlOutput("descriptionErrorAddJob"))),
           shinyjs::hidden(div(id = "descriptionErrorUpgrade", htmlOutput("descriptionErrorUpgradeJob"))),
           shinyjs::hidden(div(id = "detailTab",htmlOutput('linkDetailTab'))),
           shinyjs::hidden(div(id = "detailVersion",htmlOutput('linkDetailVersion'))),
           br(),
           actionButton("backJobList","Back to job list",
                        style='background-color:#EBECEC; color:#595B60; font-weight: bold'))
  )
}

#' Details of bar page "Add Platform"
#'
#' @return We don't know yet.
#' @importFrom miniUI miniTitleBarButton gadgetTitleBar
#'
view.barAddPlatform <- function(){
  gadgetTitleBar("Add a platform",right = NULL,
                 left = miniTitleBarButton("previousAddPlatform", "Back",primary=FALSE))
}

# Details of bar page "Upgrade Job"
view.barUpgradeJob <- function(){
  gadgetTitleBar("Upgrade a job",right = NULL,
                 left = miniTitleBarButton("previousUpgradeJob", "Back",primary=FALSE))
}

# Details of bar page "Select or Create a platform"
view.barSelectCreatePlatform <- function(){
  gadgetTitleBar("Select or create a new job",
                 right = miniTitleBarButton("refresh",icon("refresh", lib="font-awesome"), primary=FALSE),
                 left = miniTitleBarButton("previousSelectCreateJob", "Back",primary=FALSE))
}

# Details of bar page "Create a new Job"
view.barCreateNewJob <- function(){
  gadgetTitleBar("Create a new job",right = NULL,
                 left = miniTitleBarButton("previousBarCreateNewJob", "Back",primary=FALSE))
}

#' Details of bar page "Select a platform"
#'
#' @return We don't know yet.
#' @importFrom miniUI miniTitleBarCancelButton
#'
view.barSelectPlatform <- function(){
  gadgetTitleBar("Select a platform", right = actionButton("addPlatform","Add a platform"),
                 left = miniTitleBarCancelButton("cancel", "Close", primary=FALSE))
}

# Details of bar page "State of job"
view.barStateJob <- function(){
  gadgetTitleBar("State of Job",right = NULL,
                 left = miniTitleBarCancelButton("cancelAfterUpload", "Close", primary=FALSE))
}

# Displays the choice of platform
view.selectPlatform <- function(userGo,platformNameGo){
  cat("User : ", userGo)
  cat("\n")
  cat("Platform Name : ", platformNameGo)
}

# Displays or Hide the bouton "Select or Create a new Job" (in the page "Select a platform")
view.activeSelectCreateJob <- function(userGo){
  if(identical(userGo, character(0))){
    shinyjs::disable("createJob")
  }else{
    shinyjs::enable("createJob")
    return(1)
  }
}

# Displays a message error when the user select many platform (in the page "Select a platform")
# view.multiplePlatform <- function(){
#   shinyjs::disable("createJob")
#   info("Select just one platform !")
# }

# Displays a message error when the user select many job (in the page "Select or Create a platform")
# view.multipleJob <- function(){
#   shinyjs::disable("upgradeJob")
#   info("Select just one job !")
# }

# Displays different fields when there is a success connection
view.successConnection <- function(){
  shinyjs::enable("addSelectPlatform")
  show("selectPlatformName")
  info("Success connection")
}

# Displays a message when connection error
view.errorConnection <- function(){
  info("Connection error")
}

# Recover the name file R Script
view.recoverNameFile <- function(){
  tryCatch({
    nameFileR <- rstudioapi::getActiveDocumentContext()
    nameFileR <- nameFileR[2]
    nameFileR <- tstrsplit(nameFileR,"/")
    num <- length(nameFileR)
    nameFileR <- nameFileR[num]
    nameFileR <- tstrsplit(nameFileR,".R")
    nameFileR <- nameFileR[1]
  },
  error = function(cond) {
    stopApp(returnValue = invisible())
  })
  #nameFileR <- rstudioapi::getActiveDocumentContext()
}

# Displays the script who Run in a platform
# view.script <- function(input){
#   if(input$viewDocument == TRUE || input$viewDocumentUpgrade == TRUE ){
#     show("viewCode")
#   }else{
#     shinyjs::hide("viewCode")
#   }
# }

# Displays the script who Run in a platform
# view.scriptUpgrade <- function(input){
#   if(input$viewDocumentUpgrade == TRUE ){
#     show("viewCodeUpgrade")
#   }else{
#     shinyjs::hide("viewCodeUpgrade")
#   }
# }

# Displays the name Job who upgrade
view.nameJobUpgrade <- function(nameJob){
  cat("Upgrade a job : ")
  cat(nameJob)
}

# Displays LOG
view.log <- function(logsJob){
  if((is.null(logsJob$logs_err)) || (logsJob$logs_err == "")){
    info(paste("Status = ", logsJob$status, "\n\n",
               "Logs Error = NO LOG\n\n",
               "Logs Out = ", logsJob$logs_out, "\n\n", sep=""))
  }else if ((is.null(logsJob$logs_out)) || (logsJob$logs_out == "")){
    info(paste("Status = ", logsJob$status, "\n\n",
               "Logs Error = ", logsJob$logs_err, "\n\n",
               "Logs Out = NO LOG\n\n", sep=""))
  }else if ((is.null(logsJob$logs_err) || (logsJob$logs_err == "")) &
            (is.null(logsJob$logs_out) || (logsJob$logs_out == ""))){
    info(paste("Status = ", logsJob$status, "\n\n",
               "Logs Error = NO LOG\n\n",
               "Logs Out = NO LOG\n\n", sep=""))
  }else{
    info(paste("Status = ", logsJob$status, "\n\n",
               "Logs Error = ", logsJob$logs_err, "\n\n",
               "Logs Out = ", logsJob$logs_out, "\n\n", sep=""))
  }
}

# Displays two buttons for download Log
view.downloadLog <- function(){
  show("downloadDataStdout")
  show("downloadDataStderr")
}

# Displays two buttons for download Log (Upgrade)
view.downloadLogUpgrade <- function(){
  show("downloadDataStdoutUpgrade")
  show("downloadDataStderrUpgrade")
}

# To be completely removed. When turning the 'Run' functionnality back on, use the more appropriate shiny::withProgress
# view.BarProgress <- function(){
#   if (identical(.Platform$OS.type, "windows")) {
#     pb <- winProgressBar
#     setPb <- setWinProgressBar
#   } else {
#     pb <- tcltk::tkProgressBar
#     setPb <- tcltk::setTkProgressBar
#   }
#   initializeBar <- pb(title="Run the job And Recover logs", label="0% done", min=0, max=100, initial=0)
#   for(i in 1:100) {
#     Sys.sleep(0.1) # slow down the code for illustration purposes
#     info <- sprintf("%d%% done", round((i/100)*100))
#     setPb(initializeBar, i/(100)*100, label=info)
#   }
#   close(initializeBar)
# }

# Displays a message where is writing the file Stdout
# view.messagePathStdout <- function(){
#   print(paste("Hey I'm writing to saagie-stdout.csv in", getwd(), "/inst/file", sep=""))
# }

# Displays a message where is writing the file Stderr
# view.messagePathStderr <- function(){
#   print(paste("Hey I'm writing to saagie-stderr.csv in", getwd(), "/inst/file", sep=""))
# }

# Displays a message when close addin
view.messageClose <- function(){
  message("Cancelled Saagie interaction")
}

# Displays a on-hold message 
view.messageBarProgress <- function (){
  "Retrieving details for each job from Saagie"
}

# List element of page "State Job"
view.allPageState <- list("successStateAddJob","detailTab",
                          "errorStateAddJob","descriptionErrorAdd",
                          "successUpgradeJob","detailVersion",
                          "errorUpgradeJob", "descriptionErrorUpgrade")

# Hide element of Page "State Job"
view.hidePageState <- function() {
  for (page in view.allPageState) {
    shinyjs::hide(page)
  }
}

# Displays Success Job
view.successAddJob <- function(){
  view.hidePageState()
  show("successStateAddJob")
  show("detailTab")
}
# Displays Error Job
view.errorAddJob <- function(){
  view.hidePageState()
  show("errorStateAddJob")
  show("descriptionErrorAdd")
}

# Displays Success Upgrade Job
view.successUpgradeJob <- function(){
  view.hidePageState()
  show("successUpgradeJob")
  show("detailVersion")
}

# Displays Error Upgrade Job
view.errorUpgradeJob <- function(){
  view.hidePageState()
  show("errorUpgradeJob")
  show("descriptionErrorUpgrade")
}