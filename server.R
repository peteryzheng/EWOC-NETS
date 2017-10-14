library(DT)
library(shiny)
library(ggplot2)
library(shinyjs)
library(dplyr)
library(digest)

onSessionEnded = function(callback) {
  "Registers the given callback to be invoked when the session is closed
  (i.e. the connection to the client has been severed). The return value
  is a function which unregisters the callback. If multiple callbacks are
  registered, the order in which they are invoked is not guaranteed."
  return(.closedCallbacks$register(callback))
}
`%then%` <- shiny:::`%OR%`

assign("globaltox", NULL, envir = .GlobalEnv)
globaltox <- data.frame(matrix(ncol = 11,nrow = 0))
tempColNames <- rep('',6)
for (i in 1:6){
  tempColNames[i] <- paste0("Adjusted Grade",i)
}
colnames(globaltox) <- c('Patient ID','Dose Level',tempColNames,'Maximum Adjusted Grade','ETS','NETS')

shinyServer(
  function(input,output){
    #session$onSessionEnded(function() {
    #  stopApp()
    #})
    #################### Dynamic Input pages #################### 
    useShinyjs()
    rv <- reactiveValues(page = 1)
    
    navPage <- function(direction) {
      rv$page <- rv$page + direction
    }
    observeEvent(input$prevBtn, navPage(-1))
    observeEvent(input$nextBtn, navPage(1))
    
    toxnrow <- reactiveValues(dtrow = nrow(get('globaltox',envir = .GlobalEnv)))
    
    updatedt <- function(){
      print( nrow(get('globaltox',envir = .GlobalEnv)))
      if(is.null(nrow(get('globaltox',envir = .GlobalEnv)))){
        toxnrow$dtrow = 0
      }
      else{
        toxnrow$dtrow = nrow(get('globaltox',envir = .GlobalEnv))
      }
    }
    
    observeEvent(input$formsubmission,updatedt())
    
    # Mandatory toxicity fileds
    mandatoryfieldstox <- c('doselevel','l1tox','l2tox','l3ntox','l4ntox','l3tox','l4tox')
    # Mandatory parameter fields
    mandatoryfieldsparameter <- c('alphavalue','betavalue','pDLTvalue')
    
    observe({
      # js for page buttons
      toggleState(id = "prevBtn", condition = rv$page > 1)
      toggleState(id = "nextBtn", condition = rv$page < 2)
      hide(selector = ".page")
      show(sprintf("step%s", rv$page))
      # js script for mandatory tox fields
      mandatorytox <- vapply(
        mandatoryfieldstox,
        function(x){
          !is.null(input[[x]]) && !is.na(as.numeric(input[[x]]))
        },
        logical(1)
      )
      filledtox <- all(mandatorytox)
      shinyjs::toggleState("formsubmission",
                           condition = # Make sure every toxicity have a value
                             filledtox
                           )
      # js script for mandatory parameter fields   
      mandatoryparameters <- vapply(
        mandatoryfieldsparameter,
        function(x){
          # Make sure every value is not NULL or non-numeric
          !is.null(input[[x]]) && !is.na(as.numeric(input[[x]]))
        },
        logical(1)
      )
      # Boolean value indicating if mandatory fields are filled
      filledpara <- all(mandatoryparameters)
      shinyjs::toggleState('nextBtn',
                           condition = # Make sure all parameters are valid before allowing input on second page
                             filledpara &&
                             as.numeric(input$pDLTvalue) <= 100
      )
      # js for calculate button
      shinyjs::toggleState('calculating',
                           condition = 
                             (toxnrow$dtrow > 0) || 
                             !is.null(toxnrow$dtrow))
    })

    # Input parameters (alpha beta), calculate TNETS, import button
    output$page1 <- renderUI({
      tagList(
        h2("Step 1"),
        h3("Please input all relevant parameters"),
        br(),
        tags$div(
          class = 'widgets',
          textInput('alphavalue','Please enter value for alpha: ','-2'),
          textInput('betavalue','Please enter value for beta: ','0.25'),
          textInput('pDLTvalue','Please enter value for expected percentage of DLT : (maximum of 100) ','33'),
          h3(textOutput('TNETSvalue')),
          br(),
          actionButton('dataimport','Import',
                       style="background-color: #004990;color: white"),
          textOutput('errorMessagePara')
        )
      )
    })
    
    output$page2 <- renderUI({
      tagList(
        h2("Step 2"),
        h3("Please list all instances for each toxicity level"),
        br(),
        # Input for all toxicity incidences
        tags$div(class = "widgets",
                 textInput('ptID','Please enter Patient ID(optional):',''),
                 textInput('doselevel','Please enter dose level:','0'),
                 textInput("l1tox",
                           "Please enter the total number of instances for grade 1 toxicity:",
                           "0",placeholder = "Enter an integer"),
                 textInput("l2tox",
                           "Please enter the total number of instances for grade 2 toxicity:",
                           "0",placeholder = "Enter an integer"),
                 textInput("l3ntox",
                           "Please enter the total number of instances for grade 3 non-DLT : ",
                           "0",placeholder = "Enter an integer"),
                 textInput("l4ntox",
                           "Please enter the total number of instances for grade 4 non-DLT : ",
                           "0",placeholder = "Enter an integer"),
                 textInput("l3tox",
                           "Please enter the total number of instances for grade 3 DLT : ",
                           "0",placeholder = "Enter an integer"),
                 textInput("l4tox",
                           "Please enter the total number of instances for grade 4 DLT : ",
                           "0",placeholder = "Enter an integer"),
                 textOutput("toxerr"),
                 br(),
                 fluidRow(
                  column(4,actionButton('formsubmission','Submit',
                                        style="background-color: #004990;color: white")),
                  column(6,actionButton('calculating',"Calculate", style="color: #004990"))
                 )
        )
      )
    })
    
    doselevelreactive <- reactive({
      # Debug dose level
      validate(
        need(try(!is.null(input$doselevel)),'Dose level is invalid') %then%
          need(try(!is.na(as.numeric(input$doselevel))),'Dose level is not a number')
      )
      return(as.numeric(input$doselevel))
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
        need(try(!is.na(as.numeric(input$l4tox))),"Grade 4 DLT NOT a Number") %then%
        need(try(!is.null(input$doselevel)),'Dose level is invalid') %then%
        need(try(!is.na(as.numeric(input$doselevel))),'Dose level is not a number')
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
    
    datatoxmand <- reactive({
      # Validate that all toxicity have been filled for error message
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
          need(try(!is.na(as.numeric(input$l4tox))),"Grade 4 DLT NOT a Number") %then%
          need(try(!is.null(input$doselevel)),'Dose level is invalid') %then%
          need(try(!is.na(as.numeric(input$doselevel))),'Dose level is not a number')
      )
      return(TRUE)
    })
    
    # Toxicity error messages
    output$toxerr <- renderText(
      if(!datatoxmand()){
        head(datatoxmand())
      }
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
    
    # Reactive value for alpha
    alphavaluereactive <- reactive({
      # Debug clauses
      validate(
        need(try(!is.null(input$alphavalue)),'Alpha value is invalid') %then%
        need(try(!is.na(as.numeric(input$alphavalue))),'Alpha value is not a number')
      )
      return(as.numeric(input$alphavalue))
    })
    
    # Reactive value for beta
    betavaluereactive <- reactive({
      # Debug clauses
      validate(
        need(try(!is.null(input$betavalue)),'Alpha value is invalid') %then%
        need(try(!is.na(as.numeric(input$betavalue))),'Alpha value is not a number')
      )
      return(as.numeric(input$betavalue))
    })
    
    ptIDreactive <- reactive({
      validate(
        need(try(!is.null(input$ptID)),'Patient ID is invalid') 
      )
      return(input$ptID)
    })
    
    # reactive function for error message
    parareactive <- reactive({
      # Validate all conditions on page 1 for error messages
      validate(
        need(try(!is.null(input$alphavalue)),'Alpha value is invalid') %then%
        need(try(!is.na(as.numeric(input$alphavalue))),'Alpha value is not a number') %then%
        need(try(!is.null(input$betavalue)),'Alpha value is invalid') %then%
        need(try(!is.na(as.numeric(input$betavalue))),'Alpha value is not a number') %then%
        need(try(!is.null(input$pDLTvalue)),'Expected percentage of DLT is invalid') %then%
        need(try(!is.na(as.numeric(input$pDLTvalue))),'Expected percentage of DLT is not a number') %then%
        need(try(as.numeric(input$pDLTvalue) <= 100),'Expected percentage of DLT is bigger than 100%') 
      )
      return(TRUE)
    })
    
    # Output error messages from previous methods
    output$errorMessagePara <- renderText({
      if(!parareactive()){
        head(parareactive())
      }
    })
    
    # Calculate TNETS
    TNETSreactive <- reactive({
      validate(
        need(try(!is.null(input$pDLTvalue)),'Expected percentage of DLT is invalid') %then%
        need(try(!is.na(as.numeric(input$pDLTvalue))),'Expected percentage of DLT is not a number') %then%
        need(try(as.numeric(input$pDLTvalue) <= 100),'Expected percentage of DLT is bigger than 100%')
      )
      toxpercentage <- as.numeric(input$pDLTvalue)
      # Assuming non-tox is 7% and the nontox dose grade is 1:1:1:1
      nontoxpercentage <- (100-7-toxpercentage)/4
      # nontoxpercentage times average NETS of each toxicity grade
      TNETScalc <- 0.07 * 0 +
                    nontoxpercentage * (0.092 + 0.250 + 0.417 + 0.583)+
                    toxpercentage/2 * (0.75 + 0.917)
      return(TNETScalc/100)
    })
    
    output$TNETSvalue <- renderText({
      paste0('\n TNETS : ',TNETSreactive())
      
    })
    
    etscore <- reactive({
      # Find ETS of patient
      toxdf <- datatox()
      tottoxnum <- sum(as.numeric(toxdf[,1]))
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
        alphavalue <- alphavaluereactive()
        betavalue <- betavaluereactive()
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

    # Reactive methods for new table
    toxrow <- reactive({
      # get toxicity data for each patient
      tmptox <- t(datatox())
      # create patient rowname
      rownames(tmptox) <- c('Patient')
      toxscoretable <- data.frame(matrix(ncol = 3,nrow = 1))
      # NETS data frame
      colnames(toxscoretable) <- c('Maximum Adjusted Grade','ETS','NETS')
      rownames(toxscoretable) <- c('Patient')
      toxscoretable[1,1] <- maxtox()
      toxscoretable[1,2] <- etscore()
      toxscoretable[1,3] <- etscore()/6
      # create row for each patient
      tmptoxrow <- cbind(ptIDreactive(),doselevelreactive(),tmptox,toxscoretable)
      return(tmptoxrow)
    })
    
    # Output for toxicity score for each submisison    
    output$toxicityscores <- DT::renderDataTable({
      if (is.null(input$formsubmission) ){
        return()
      }
      else{
        input$formsubmission
        if(input$formsubmission == 0)
          return()
        else
          isolate(
            if(is.null(datatox())){return()}
            else{
              tmptoxrow <- toxrow()
              colnames(tmptoxrow) <- colnames(globaltox)
              if(is.null(nrow(get('globaltox',envir = .GlobalEnv)))){
                rownames(tmptoxrow) <- paste0('Patient ',1)
              }
              else{
                rownames(tmptoxrow) <- paste0('Patient ',(nrow(get('globaltox',envir = .GlobalEnv))+1))
              }
              # assign global variable hence <<-
              assign('globaltox',rbind(get('globaltox',globalenv()),tmptoxrow),globalenv())
              #globaltox <<- rbind(globaltox,tmptoxrow)
              return(get('globaltox',envir = .GlobalEnv))
            }
          )
      }
    },
    options = list(searching = TRUE, lengthChange = TRUE,orderClasses = TRUE,scrollX = TRUE,className = 'dt-center')
    )
    
    # Downloading files of all patients
    output$downFile <- downloadHandler(
      filename = function(){
        paste("Patient toxicities information","csv",sep = ".")
      },
      content = function(file){
        temp <- output$toxicityscores
        write.csv(temp,file,row.names = FALSE)
        write.table(otherstats(), file, append = TRUE,sep = ',')
      }
    )
  }
)

