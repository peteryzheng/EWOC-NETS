library(shiny)
library(ggplot2)
library(shinyjs)
onSessionEnded = function(callback) {
  "Registers the given callback to be invoked when the session is closed
  (i.e. the connection to the client has been severed). The return value
  is a function which unregisters the callback. If multiple callbacks are
  registered, the order in which they are invoked is not guaranteed."
  return(.closedCallbacks$register(callback))
}
`%then%` <- shiny:::`%OR%`

shinyServer(
  function(input,output){
    #session$onSessionEnded(function() {
    #  stopApp()
    #})
    #################### Dynamic Input pages #################### 
    useShinyjs()
    rv <- reactiveValues(page = 1)
    observe({
      toggleState(id = "prevBtn", condition = rv$page > 1)
      toggleState(id = "nextBtn", condition = rv$page < 2)
      hide(selector = ".page")
      show(sprintf("step%s", rv$page))
    })
    observe({
      shinyjs::toggleState("nextBtn",
                           condition = # Make sure every toxicity have a value
                             !is.null(input$l1tox) &&
                             !is.null(input$l2tox) &&
                             !is.null(input$l3ntox) &&
                             !is.null(input$l4ntox) &&
                             !is.null(input$l3tox) &&
                             !is.null(input$l4tox) &&
                             # Make sure every toxicity have a numeric value
                             !is.na(as.numeric(input$l1tox)) &&
                             !is.na(as.numeric(input$l2tox)) &&
                             !is.na(as.numeric(input$l3ntox)) &&
                             !is.na(as.numeric(input$l4ntox)) &&
                             !is.na(as.numeric(input$l3tox)) &&
                             !is.na(as.numeric(input$l4tox))
                           )
    })
    observe({
      shinyjs::toggleState("calculate"
                           #condition = checkProb()
                           )
    })
    navPage <- function(direction) {
      rv$page <- rv$page + direction
    }
    observeEvent(input$prevBtn, navPage(-1))
    observeEvent(input$nextBtn, navPage(1))
    
    output$page1 <- renderUI({
      tagList(
        h2("Step 1"),
        h3("Please list all instances for each toxicity level."),
        br(),
        # Input for all toxicity incidences
        tags$div(class = "widgets",
                 textInput("l1tox",
                           "Please enter the total number of instances for grade 1 toxicity:",
                           "0",placeholder = "Enter an integer"),
                 textInput("l2tox",
                           "Please enter the total number of instances for grade 2 toxicity:",
                           "0",placeholder = "Enter an integer"),
                 textInput("l3ntox",
                           "Please enter the total number of instances for grade 3 non-DLT:",
                           "0",placeholder = "Enter an integer"),
                 textInput("l4ntox",
                           "Please enter the total number of instances for grade 4 non-DLT:",
                           "0",placeholder = "Enter an integer"),
                 textInput("l3tox",
                           "Please enter the total number of instances for grade 3 DLT:",
                           "0",placeholder = "Enter an integer"),
                 textInput("l4tox",
                           "Please enter the total number of instances for grade 4 DLT:",
                           "0",placeholder = "Enter an integer")
                 
        ),
                 

        textOutput("errorMessage")
      )
    })
    output$page2 <- renderUI({
      tagList(
        #h2("Step 2"),
        #h3("Please Enter the True Probability of Dose Limiting Toxicity (Pdlt) and dosages (optional) for each Dose Level Below:"),
        #br(),
        #uiOutput("dynamicInputs"),
        #h4("Please note: All Doses have to be entered for calculation"),
        actionButton("calculate","Calculate", style="color: #004990")
      )
    })
    
    datatox <- reactive({
      # Validate that all toxicity have been filled out before proceeding
      validate(
        need(try(!is.null(input$l1tox)),"Grade 1 toxicity invalid") %then%
        need(try(!is.null(input$l2tox)),"Grade 2 toxicity invalid") %then%
        need(try(!is.null(input$l3ntox)),"Grade 3 non-DLT invalid") %then%
        need(try(!is.null(input$l4ntox)),"Grade 4 non-DLT invalid") %then%
        need(try(!is.null(input$l3tox)),"Grade 3 DLT invalid") %then%
        need(try(!is.null(input$l4tox)),"Grade 4 DLT invalid") %then%
        need(try(!is.na(as.numeric(input$l1tox))),"Grade 1 toxicity NOT a Number") %then%
        need(try(!is.na(as.numeric(input$l2tox))),"Grade 2 toxicity NOT a Number") %then%
        need(try(!is.na(as.numeric(input$l3ntox))),"Grade 3 non-DLT NOT a Number") %then%
        need(try(!is.na(as.numeric(input$l4ntox))),"Grade 4 non-DLT NOT a Number") %then%
        need(try(!is.na(as.numeric(input$l3tox))),"Grade 3 DLT NOT a Number") %then%
        need(try(!is.na(as.numeric(input$l4tox))),"Grade 4 DLT NOT a Number") 
      )
      #reactive function to return toxicity data
      toxdf <- data.frame(input$l1tox, input$l2tox, input$l3ntox, input$l4ntox, input$l3tox, input$l4tox)
      # Create dataframe for column names
      tempColNames <- c(1:ncol(toxdf))
      for (i in 1:ncol(toxdf)){
        tempColNames[i] <- paste0("Adjusted Grade",i)
      }
      # Give table column names
      
      colnames(toxdf) <- tempColNames
      rownames(toxdf) <- c("Number of toxicities")
      # Give the inverted version of the table with format 1 column 6 rows
      return(t(toxdf))
    })
    
    output$datatox <- renderTable({
      if (is.null(input$calculate) ){
        return()
      }
      else{
        input$calculate
        if(input$calculate == 0)
          return()
        else
          isolate(
            if(is.null(datatox())){return()}
            else{
              return(datatox())
            }
          )
      }
    },
    include.rownames=TRUE,align = 'c'
    )
    
    maxtox <- reactive({
      # Get max adjusted toxicity grade
      toxdf <- datatox()
      maxgrade <- 0
      for(i in nrow(toxdf):1){
        # biggest grade with toxicity
        if(toxdf[i,1] != 0){
          maxgrade <- i
          break
        }
      }
      return(maxgrade)
    })
    
    etscore <- reactive({
      # Find ETS of patient
      toxdf <- datatox()
      tottoxnum <- sum(as.numeric(toxdf[,1]))
      print(toxdf)
      print(class(toxdf))
      
      if(tottoxnum == 0){
        return(0)
      }
      else if (tottoxnum == 1){
        if(maxtox() == 1){
          return(0.1)
        }
        else{
          return(maxtox() - 1)
        }
      }
      else{
        # Parameter values for each patient
        alphavalue <- -2
        betavalue <- 0.25
        # wi value for each toxicity
        wivalue <- rep(1,tottoxnum)
        # expanding the toxdf
        tottoxdf <- NULL
        for(i in 1:nrow(toxdf)){
          tottoxdf <- c(tottoxdf,rep(i,toxdf[i,1]))
        }
        # Get value of summation expression
        summationexp <- 0
        for(i in 1:length(tottoxdf)){
          summationexp <- summationexp + wivalue[i]*tottoxdf[i]/maxtox()
        }
        # e expression in the equation
        natexp <- exp(alphavalue+betavalue * (summationexp-1))
        return(maxtox()-1 + natexp/(1+natexp))
      }
    })
    
    output$toxicityscores <- renderTable({
      if (is.null(input$calculate) ){
        return()
      }
      else{
        input$calculate
        if(input$calculate == 0)
          return()
        else
          isolate(
            if(is.null(datatox())){return()}
            else{
              toxscoretable <- data.frame(matrix(ncol = 1,nrow = 3))
              rownames(toxscoretable) <- c('Maximum Adjusted Grade','ETS','NETS')
              colnames(toxscoretable) <- c('Scores')
              toxscoretable[1,1] <- maxtox()
              toxscoretable[2,1] <- etscore()
              toxscoretable[3,1] <- etscore()/6
              return(toxscoretable)
            }
          )
      }
    },
    include.rownames = TRUE, align = 'c')
  }
)

