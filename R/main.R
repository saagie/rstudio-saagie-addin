# source("R/library.R")
# source("R/view.R")
# source("R/validator.R")
# source("R/model.R")
#' Saagie Addin Main Function
#'
#' @param data We don't know yet.
#' @param xvar We don't know yet.
#' @param yvar We don't know yet.
#'
#' @return We don't know yet.
#' @import shiny
#' @importFrom utils read.csv write.csv
#' @export
#'
Saagie <- function(data, xvar, yvar) {
  
  # Displays the User Interface
  ui <- shinyUI(view.activate())
  
  # Interaction
  server <- function(input, output, session) {
    
    # Better to use rappdirs
    path_to_persistent_saagie_files <- rappdirs::user_data_dir(appname = "rstudio-saagie-addin", appauthor = "Saagie")
    if (!dir.exists(path_to_persistent_saagie_files)) dir.create(path_to_persistent_saagie_files, recursive = TRUE)
    if (!dir.exists(file.path(path_to_persistent_saagie_files, "platform"))) dir.create(file.path(path_to_persistent_saagie_files, "platform"))
    
    # Displays the table containing the name of the platform and the names of users
    dataPlatform <- model.readTablePlatform(path_to_persistent_saagie_files)
    view.showTablePlatform(dataPlatform,output)
    
    # Displays the page "Add a platform"
    observeEvent(input$addPlatform, view.showAddPlatform())
    
    # Displays the page "Select a platform" and update the table
    observeEvent(input$addSelectPlatform, {
      model.updateTablePlatform(path_to_persistent_saagie_files, input)
      dataPlatform <- model.readTablePlatform(path_to_persistent_saagie_files)
      view.showTablePlatform(dataPlatform,output)
      view.showSelectPlatform()
    })
    
    # Displays the page "Upgrade a job"
    observeEvent(input$upgradeJob, view.showUpgrade())
    
    # Control if the fields are not empty in the page "Add Platform"
    # observe(validator.infoPlatform(path_to_persistent_saagie_files, input))
    observeEvent(input$testConnection, validator.testConnection(path_to_persistent_saagie_files, input))
    
    # Control if the fields "mail" is correct
    observe(validator.mail(input))
    
    # Displays the page "Select or create a new job"
    observeEvent(input$createJob, { # | input$refresh | input$previousUpgradeJob | input$previousBarCreateNewJob, {
      if (nrow(model.readTablePlatform(path_to_persistent_saagie_files)) == 0) {
        info("Add at least one platform")
      } else {
        # withProgress({
        # model.rmJob(path_to_persistent_saagie_files)
        # API request to retrieve list of jobs and write to local file
        model.JobRPlatform(path_to_persistent_saagie_files)
        # Read list of jobs from local file
        jobs <- model.readTableJob(path_to_persistent_saagie_files)
        # Read 'current' platform from local file
        thePlatform <- model.readThePlatform(path_to_persistent_saagie_files)
        # Loop over jobs to get their details. One API request per job!
        jobs <- model.currentVersion(jobs,thePlatform)
        # Loop over jobs to classify them according to file name extensions etc
        jobs <- model.removeLinkedNoR(jobs)
        # }, message = "Retrieving list of R jobs from Saagie")
        view.showTableJob(jobs,output)
        view.showSelectCreateJob()
      }
    })
    
    # Displays the page "Create a new job"
    observeEvent(input$createNewJob, view.showCreateNewJob())
    
    # Control if the field "Job Name" isn't empty in the page "Create a New Job"
    observe(validator.infoJob(input))
    
    # Displays the previous page ("Add Platform" -> "Select Platform")
    observeEvent(input$previousAddPlatform,view.showSelectPlatform())
    
    # Displays the page "Select or Create Job" when click into button "Back to job"
    observeEvent(input$backJobList, {
      model.JobRPlatform(path_to_persistent_saagie_files)
      jobs <- model.readTableJob(path_to_persistent_saagie_files)
      thePlatform <- model.readThePlatform(path_to_persistent_saagie_files)
      jobs <- model.currentVersion(jobs,thePlatform)
      jobs <- model.removeLinkedNoR(jobs)
      view.showTableJob(jobs,output)
      view.showSelectCreateJob()
    })    
    
    # Factorized with input$createJob ??
    # Displays the previous page ("Upgrade Job" -> "Select Platform")
    # observeEvent(input$previousUpgradeJob, view.showSelectCreateJob())
    observeEvent(input$previousUpgradeJob, {
      # withProgress({
      # model.rmJob(path_to_persistent_saagie_files)
      model.JobRPlatform(path_to_persistent_saagie_files)
      jobs <- model.readTableJob(path_to_persistent_saagie_files)
      thePlatform <- model.readThePlatform(path_to_persistent_saagie_files)
      jobs <- model.currentVersion(jobs,thePlatform)
      jobs <- model.removeLinkedNoR(jobs)
      # }, message = "Retrieving list of jobs from Saagie")
      view.showTableJob(jobs,output)
      view.showSelectCreateJob()
    })
    
    # Displays the previous page ("Select or create job" -> "Select Platform")
    observeEvent(input$previousSelectCreateJob,view.showSelectPlatform())
    
    # Factorized with input$createJob ??
    # Displays the previous page ("Create New Job" -> "Select or Create a new Job")
    # observeEvent(input$previousBarCreateNewJob, view.showSelectCreateJob())
    observeEvent(input$previousBarCreateNewJob, {
      # withProgress({
      # model.rmJob(path_to_persistent_saagie_files)
      model.JobRPlatform(path_to_persistent_saagie_files)
      jobs <- model.readTableJob(path_to_persistent_saagie_files)
      thePlatform <- model.readThePlatform(path_to_persistent_saagie_files)
      jobs <- model.currentVersion(jobs,thePlatform)
      jobs <- model.removeLinkedNoR(jobs)
      # }, message = "Retrieving list of jobs from Saagie")
      view.showTableJob(jobs,output)
      view.showSelectCreateJob()
    })
    
    # Choice of the platform and Add the platform in file containing the platform
    output$rowSelectPlatform = renderPrint({
      nb_row = input$table_rows_selected
      if (length(nb_row) == 0){
        infoPlatformDefault = model.defaultPlatform(path_to_persistent_saagie_files)
        view.selectPlatform(infoPlatformDefault[['UserGo']],infoPlatformDefault[['PlatformNameGo']])
      } else if (length(nb_row) == 1) {
        infoPlatform = model.selectPlatform(path_to_persistent_saagie_files, nb_row)
        view.selectPlatform(infoPlatform[['UserGo']],infoPlatform[['PlatformNameGo']])
        active = view.activeSelectCreateJob(infoPlatform[['UserGo']])
        if(active == 1){
          model.addThePlatformInFile(path_to_persistent_saagie_files,
                                     infoPlatform[['UserGo']],infoPlatform[['PlatformNameGo']],infoPlatform[['Mdp']],
                                     infoPlatform[['AdressPlatform']],infoPlatform[['NumPlatform']])
        }
      }
      # else{
      #   view.multiplePlatform()
      # }
    })
    
    # Reactive the file containing the name of platform
    searchResult <- reactive({
      model.readNamePlatform(path_to_persistent_saagie_files)
    })
    
    # Select the Platform
    output$selectPlatformName <- renderUI({
      selectInput("platformName", "Platform Name", searchResult()[,2])
    })
    
    # Select the job who upgrade
    output$rowSelectJob = renderPrint({
      nb_row = input$newJob_rows_selected
      if (length(nb_row) == 0){
        shinyjs::disable("upgradeJob")
      } else if (length(nb_row) == 1) {
        shinyjs::enable("upgradeJob")
        write.csv(nb_row, file = file.path(path_to_persistent_saagie_files, "platform", "row.csv"), row.names = FALSE)
        jobs <- model.readTableJob(path_to_persistent_saagie_files)
        nameJob <- jobs[nb_row,4]
        #print(jobs)
        view.nameJobUpgrade(nameJob)
      }
      # else{
      #   view.multipleJob()
      # }
    })

    tryCatch({
      context <- rstudioapi::getSourceEditorContext()
    },
      error = function(cond) {
      info("A script should be open in the RStudio Source Editor before uploading it to Saagie.")
      stopApp(cond)
    })
    
    tryCatch({
      context <- rstudioapi::getActiveDocumentContext()
      j <- 0
      script <- data.frame(context[3])
      for(i in 1:nrow(script)){
        if(script[i,] == ""){
          j <- j+1
        }
      }
      if(nrow(script)==j){
        info("A script should be open in the RStudio Source Editor before uploading it to Saagie.")
        stopApp(returnValue = invisible())
      }
    })
    
    # Formate the document who containing the R Script
    reactiveDocument <- reactive({
      tryCatch({
        formatR::tidy_source(text = context$contents, output = FALSE)$text.tidy
      },
      error = function(cond) {
        info("Code could not be automatically formatted.\nIt may be caused by syntactically incorrect R code, or by comments in the middle of an expression.\nThe add-in will now procede with unformatted code.")
        context$contents
      })
    })
    
    # # Displays a code of script R
    # output$document <- renderCode({
    #   document <- reactiveDocument()
    #   highlightCode(session, "document")
    #   document
    # })
    # # Displays a code of script R (Upgrade)
    # output$documentUpgrade <- renderCode({
    #   documentUpgrade <- reactiveDocument()
    #   highlightCode(session, "documentUpgrade")
    #   documentUpgrade
    # })
    
    # Reactive the previous of Script R
    # observeEvent(input$viewDocument,{view.script(input)})
    
    # Reactive the previous of Script R (Upgrade)
    # observeEvent(input$viewDocumentUpgrade, {view.scriptUpgrade(input)})
    
    # Add a job in the platform Saagie 
    # And show the page "State of job"
    observeEvent(input$addDeploy,{
      document <- reactiveDocument()
      nameFile <- view.recoverNameFile()
      pathNameFile <- model.writeFile(document,nameFile)
      info <- model.postJob(path_to_persistent_saagie_files, input, pathNameFile)
      view.showStateJob()
      idJobPlatform <- model.idJobPlatform(info[['ReponseAdd']])
      urlDetailTab <- paste(info$ThePlatform[4],"/#/manager/", info$ThePlatform[5], "/job/", idJobPlatform, sep="")
      if((info$ReponseAdd[2]) == 200){
        view.successAddJob()
      }else{
        view.errorAddJob()
      }
      output$linkDetailTab <- renderUI({
        tags$a("Go to Job", href=urlDetailTab, target="_blank")
      })
      output$descriptionErrorAddJob <- renderUI({
        paste("Status : ", info$ReponseAdd[2])
      })
      
      # observeEvent(input$runAddDeploy,{
      #   idJobPlatform <- model.runJob(info[['ThePlatform']],info[['ReponseAdd']])
      #   view.BarProgress()
      #   log <- model.showLog(info[['ThePlatform']],idJobPlatform)
      #   view.downloadLog()
      #   view.log(log[['LogsJob']])
      #   model.removeFile(pathNameFile)
      #   # Download Log STDOUT
      #   observeEvent(input$downloadDataStdout, {
      #     model.downloadStdout(info[['ThePlatform']],log)
      #   })
      #   # Download Log STDERR
      #   observeEvent(input$downloadDataStderr, {
      #     model.downloadStderr(info[['ThePlatform']],log)
      #   })
      # })
    })
    
    
    # Upgrade the Job
    # And show the page "State of job"
    observeEvent(input$upgradeDeploy,{
      document <- reactiveDocument()
      nameFile <- view.recoverNameFile()
      pathNameFile <- model.writeFile(document,nameFile)
      test <- model.readNumJob(path_to_persistent_saagie_files)
      jobs <- model.readTableJob(path_to_persistent_saagie_files)
      value <- test[1,1]
      idJob <- jobs[value,1]
      nameJob <- jobs[value,4]
      info <- model.upgradeJob(path_to_persistent_saagie_files, input,idJob,pathNameFile)
      view.showStateJob()
      urlDetailVersion <- paste(info$ThePlatform[4],"/#/manager/", info$ThePlatform[5], "/job/", idJob, "/versions", sep="")
      if((info$ReponseAdd[2]) == 200){
         view.successUpgradeJob()
      }else{
         view.errorUpgradeJob()
      }
      output$linkDetailVersion <- renderUI({
        tags$a("Go to Job", href=urlDetailVersion, target="_blank")
      })
      output$descriptionErrorUpgradeJob <- renderUI({
        paste("Status : ", info$ReponseAdd[2])
      })
      
      # view.BarProgress()
      # log <- model.showLog(thePlatform,idJob)
      # view.downloadLogUpgrade()
      # view.log(log[['LogsJob']])
      # model.removeFile(pathNameFile)
      # # Download Log STDOUT
      # observeEvent(input$downloadDataStdoutUpgrade, {
      #   model.downloadStdout(thePlatform,log)
      #   view.messagePathStdout()
      # })
      # # Download Log STDERR
      # observeEvent(input$downloadDataStderrUpgrade, {
      #   model.downloadStderr(thePlatform,log)
      #   view.messagePathStderr()
      # })
    })
    
    # Factorized with input$createJob ??
    # Refresh a page "Select or Create new Job"
    observeEvent(input$refresh, {
      # withProgress({
      # model.rmJob(path_to_persistent_saagie_files)
      model.JobRPlatform(path_to_persistent_saagie_files)
      jobs <- model.readTableJob(path_to_persistent_saagie_files)
      thePlatform <- model.readThePlatform(path_to_persistent_saagie_files)
      jobs <- model.currentVersion(jobs,thePlatform)
      jobs <- model.removeLinkedNoR(jobs)
      # }, message = "Retrieving list of jobs from Saagie")
      view.showTableJob(jobs,output)
      view.showSelectCreateJob()
    })
    
    # Three functions for formate the document who containing the R Script
    injectHighlightHandler <- function() {
      code <- "
      Shiny.addCustomMessageHandler('highlight-code', function(message) {
      var id = message['id'];
      setTimeout(function() {
      var el = document.getElementById(id);
      hljs.highlightBlock(el);
      }, 100);
      });
      "
      tags$script(code)
    }
    includeHighlightJs <- function() {
      resources <- system.file("www/shared/highlight", package = "shiny")
      list(
        includeScript(file.path(resources, "highlight.pack.js")),
        includeCSS(file.path(resources, "rstudio.css")),
        injectHighlightHandler()
      )
    }
    highlightCode <- function(session, id) {
      session$sendCustomMessage("highlight-code", list(id = id))
    }
    # Style of R Code
    rCodeContainer <- function(...) {
      code <- HTML(as.character(tags$code(class = "language-r", ...)))
      div(pre(code))
    }
    
    # Displays Image Saagie
    output$heron <- renderImage({
      # What is input$n ???
      # filename <- file.path(system.file(package = "Saagie"), "pictures", paste0('40-01', input$n, '.jpg'))
      filename <- file.path(system.file(package = "Saagie"), "pictures", paste0('40-01', '.jpg'))
      list(src = filename)
    }, deleteFile = FALSE)
    
    # Replace the default stopApp(stop( ... )) with stopApp(message( ... ))
    # because we set stopOnCancel to FALSE in runGadget()
    observeEvent(input$cancel, {
      stopApp()
    })
    
    observeEvent(input$cancelAfterUpload, {
      stopApp()
    })
    
    # Deal with closing the add-in with the 'x' box instead of the 'Cancel' button
    session$onSessionEnded(function() {
      stopApp(view.messageClose())
    })
    
}
  
  # The default stopOnCancel = TRUE causes an 
  # error (stopApp(stop("User cancel", call. = FALSE))) when the Cancel button is clicked
  # TODO: Maybe find a better way to handle this error (current solution : replace the default with stopApp(message()))
  runGadget(ui, server, viewer = dialogViewer("Saagie"), stopOnCancel = FALSE)
  # runGadget(ui, server, viewer = browseURL("shiny.rstudio.com/tutorial", browser = getOption("browser")), stopOnCancel = FALSE)
}