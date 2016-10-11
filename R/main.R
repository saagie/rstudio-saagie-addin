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
  
  # To be moved to somewhere global. Write/read of csv files should happen in "~/.rstudio-desktop/Saagie/"
  # (TODO: deal with RStudio Server (which has "~/.rstudio/" instead of "~/.rstudio-desktop/"))
  # if(!dir.exists("~/.rstudio-desktop/Saagie") dir.create("~/.rstudio-desktop/Saagie")
  # DONE

  # Displays the User Interface
  ui <- shinyUI(view.activate())

  server <- function(input, output, session) {
  
    # # A persistent directory for the Saagie addin in the hidden ".rstudio" directory
    # if (identical(.Platform$OS.type, "windows")) {
    #   path_to_persistent_saagie_files <- file.path(win_path_env("local"), "RStudio-Desktop", "Saagie")
    # } else {
    #   # Assume we are on Linux or Mac OS
    #   # Deal with RStudio Desktop VS RStudio Server
    #   hiddendir <- if (rstudioapi::versionInfo()$mode == "desktop") ".rstudio-desktop" else ".rstudio"
    #   path_to_persistent_saagie_files <- file.path("~", hiddendir, "Saagie")
    # }
    
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
    observe(validator.infoPlatform(path_to_persistent_saagie_files, input))

    # Control if the fields "mail" is correct
    observe(validator.mail(input))

    # Displays the page "Select or create a new job"
    observeEvent(input$createJob,{
      withProgress({
        # model.rmJob(path_to_persistent_saagie_files)
        model.JobRPlatform(path_to_persistent_saagie_files)
        jobs <- model.readTableJob(path_to_persistent_saagie_files)
        thePlatform <- model.readThePlatform(path_to_persistent_saagie_files)
        jobs <- model.currentVersion(jobs,thePlatform)
        jobs <- model.removeLinkedNoR(jobs)
        view.showTableJob(jobs,output)
        view.showSelectCreateJob()
      },
      message = "Retrieving list of R jobs from Saagie")
    })

    # Displays the page "Create a new job"
    observeEvent(input$createNewJob, view.showCreateNewJob())

    # Control if the field "Job Name" isn't empty in the page "Create a New Job"
    observe(validator.infoJob(input))

    # Displays the previous page ("Add Platform" -> "Select Platform")
    observeEvent(input$previousAddPlatform,view.showSelectPlatform())

    # Displays the previous page ("Upgrade Job" -> "Select Platform")
    observeEvent(input$previousUpgradeJob,view.showSelectCreateJob())

    # Displays the previous page ("Select or create job" -> "Select Platform")
    observeEvent(input$previousSelectCreateJob,view.showSelectPlatform())

    # Displays the previous page ("Create New Job" -> "Select or Create a new Job")
    observeEvent(input$previousBarCreateNewJob,view.showSelectCreateJob())

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
      }else{
        view.multiplePlatform()
      }
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
        view.nameJobUpgrade(nameJob)
      }else{
        view.multipleJob()
      }
    })

    context <- rstudioapi::getActiveDocumentContext()
    # Formate the document who containing the R Script
    reactiveDocument <- reactive({
      formatted <- formatR::tidy_source(
        text = context$contents,
        output = FALSE
      )$text.tidy
      formatted
    })

    # Displays a code of script R
    output$document <- renderCode({
      document <- reactiveDocument()
      highlightCode(session, "document")
      document
    })

    # Displays a code of script R (Upgrade)
    output$documentUpgrade <- renderCode({
      documentUpgrade <- reactiveDocument()
      highlightCode(session, "documentUpgrade")
      documentUpgrade
    })

    # Reactive the previous of Script R
    observeEvent(input$viewDocument,{view.script(input)})

    # Reactive the previous of Script R (Upgrade)
    observeEvent(input$viewDocumentUpgrade, {view.scriptUpgrade(input)})

    # Add a job in the platform Saagie
    observeEvent(input$addDeploy,{
      document <- reactiveDocument()
      nameFile <- view.recoverNameFile()
      pathNameFile <- model.writeFile(document,nameFile)
      info <- model.postJob(path_to_persistent_saagie_files, input, pathNameFile)
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
    observeEvent(input$upgradeDeploy,{
      document <- reactiveDocument()
      nameFile <- view.recoverNameFile()
      pathNameFile <- model.writeFile(document,nameFile)
      test <- model.readNumJob(path_to_persistent_saagie_files)
      jobs <- model.readTableJob(path_to_persistent_saagie_files)
      value <- test[1,1]
      idJob <- jobs[value,1]
      nameJob <- jobs[value,4]
      thePlatform <- model.upgradeJob(path_to_persistent_saagie_files, input,idJob,pathNameFile)
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

    # Refresh a page "Select or Create new Job"
    observeEvent(input$refresh, {
      withProgress({
        # model.rmJob(path_to_persistent_saagie_files)
        model.JobRPlatform(path_to_persistent_saagie_files)
        jobs <- model.readTableJob(path_to_persistent_saagie_files)
        thePlatform <- model.readThePlatform(path_to_persistent_saagie_files)
        jobs <- model.currentVersion(jobs,thePlatform)
        jobs <- model.removeLinkedNoR(jobs)
        view.showTableJob(jobs,output)
        view.showSelectCreateJob()
      },
      message = "Retrieving list of jobs from Saagie")
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
      stopApp(message(""))
    })
    # Deal with closing the add-in with the 'x' box instead of the 'Cancel' button
    session$onSessionEnded(function() {
      stopApp(message("Cancelled Saagie interaction"))
    })
  
    
  }
  
  # The default stopOnCancel = TRUE causes an 
  # error (stopApp(stop("User cancel", call. = FALSE))) when the Cancel button is clicked
  # TODO: Maybe find a better way to handle this error (current solution : replace the default with stopApp(message()))
  runGadget(ui, server, viewer = dialogViewer("Saagie"), stopOnCancel = FALSE)
}
