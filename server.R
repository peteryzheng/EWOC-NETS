library(DT)
library(shiny)
library(ggplot2)
library(shinyjs)
library(dplyr)
library(digest)

##### WHICH BASE LOG DO WE USE ??????? #######
logit <- function(num){
  return(log(num/(1-num)))
}

posteriorcalc <- function(pregamma, prerho, simulationtime, confidentsimulation, toxinfo, tnets){
  simrho <- rep(NA,simulationtime) # array for simulation results for rho
  simgamma <- rep(NA,simulationtime) # array for simulation results for gamma
  simrho[1] <- prerho
  simgamma[1] <- pregamma
  
  for(i in 2:simulationtime){
    simrho[i] <- updaterho(pregamma,simrho[i-1],toxinfo,tnets)
  }
  for(i in 2:simulationtime){
    simgamma[i] <- updategamma(simgamma[i-1],mean(simrho[(simulationtime-confidentsimulation):simulationtime]),
                               toxinfo,tnets)
  }
  return(list(simrho,simgamma))
}

likelihood <- function(gamma,rho,toxinfo,tnets){
  beta0<-(logit(rho)*gamma-logit(tnets)*0)/(gamma-0)    #beta1 in logistic function
  #print(beta0)
  beta1<-(logit(tnets)-logit(rho))/(gamma-0)  #beta1 in logistic function
  #print(beta1)
  si <- toxinfo$NETS
  #print(si)
  
  #print(toxinfo$`Dose Level`)
  tmpeq <-exp(beta0+beta1*(toxinfo$`Dose Level`))/(1+exp(beta0+beta1*(toxinfo$`Dose Level`)))
  #print(tmpeq)
  
  
  likelihood<-prod((tmpeq^si)*(1-tmpeq)^(1-si))   #quasi likelihood of all recent patients

  return(likelihood)
}

updaterho <- function(gamma,prevrho,toxinfo,tnets){
  tmprho <- runif(1,0,tnets) # Random Number from 0 to 1
  oldlikelihood <- likelihood(gamma,prevrho,toxinfo,tnets)
  #print('Oldlikelihood')
  #print(oldlikelihood)
  newlikelihood <- likelihood(gamma,tmprho,toxinfo,tnets)
  #print('New likelihood')
  #print(newlikelihood)
  
  ratiotmprho <- newlikelihood/oldlikelihood
  #print(paste0('ratio: ',ratiotmprho))
  

  
  if(ratiotmprho < 1){
    tmptmprho<-runif(1,0,1)
    # randomly save tmprho or new number if the current iteration is less likely due to likelihood function
    if(tmptmprho<ratiotmprho){
      rho_next<-tmprho
    }
    else{
      rho_next<-prevrho
    }
  }
  else{
    rho_next <- tmprho
  }
  return(rho_next)
}

updategamma <- function(prevgamma,rho,toxinfo,tnets){
  tmpgamma <- runif(1,0,100) # Random Number from 0 to 1
  oldlikelihood <- likelihood(prevgamma,rho,toxinfo,tnets)
  newlikelihood <- likelihood(tmpgamma,rho,toxinfo,tnets)
  ratiotmpgamma <- newlikelihood/oldlikelihood
  if(ratiotmpgamma == 'NaN'){
    ratiotmpgamma <- 1
  }
  
  if(ratiotmpgamma < 1){
    tmptmpgamma<-runif(1,0,1)
    # randomly save tmpgamma or new number if the current iteration is less likely due to likelihood function
    if(tmptmpgamma<ratiotmpgamma){
      gamma_next<-tmpgamma
    }
    else{
      gamma_next<-prevgamma
    }
  }
  else{
    gamma_next <- tmpgamma
  }
  return(gamma_next)
}

onSessionEnded = function(callback) {
  "Registers the given callback to be invoked when the session is closed
  (i.e. the connection to the client has been severed). The return value
  is a function which unregisters the callback. If multiple callbacks are
  registered, the order in which they are invoked is not guaranteed."
  return(.closedCallbacks$register(callback))
}
`%then%` <- shiny:::`%OR%`

# Initializing global variable
assign("globaltox", data.frame(matrix(ncol = 11,nrow = 0)), envir = .GlobalEnv)
tempColNames <- rep('',6)
for (i in 1:6){
  tempColNames[i] <- paste0("Adjusted Grade",i)
}
# Assigning column names
colnames(.GlobalEnv$globaltox) <- c('Patient ID','Dose Level',tempColNames,'Maximum Adjusted Grade','ETS','NETS')

# Global variables to keep track of each button
assign("importbuttontracker", 0, envir = .GlobalEnv)
assign("nextBtntracker", 0, envir = .GlobalEnv)
assign("deleteRowtracker", 0, envir = .GlobalEnv)

shinyServer(
  function(input,output){
    #session$onSessionEnded(function() {
    #  stopApp()
    #})
    #################### Dynamic Input pages #################### 
    useShinyjs()
    # Reactive value for pages
    rv <- reactiveValues(page = 1)
    
    # shiny js for switching pages
    navPage <- function(direction) {
      rv$page <- rv$page + direction
    }
    observeEvent(input$prevBtn, navPage(-1))
    observeEvent(input$nextBtn, navPage(1))
    # Reactive value for number of rows of globaltox
    toxnrow <- reactiveValues(dtrow = nrow(get('globaltox',envir = .GlobalEnv)))
    
    # Reactive function to update toxnrow value
    updatedt <- function(){
      if(is.null(nrow(get('globaltox',envir = .GlobalEnv)))){
        # assign 0 if globaltox is null (will mess up the values)
        toxnrow$dtrow = 0
      }
      else{
        toxnrow$dtrow = nrow(get('globaltox',envir = .GlobalEnv))
      }
    }
    
    # Only refresh every time next/prev/ delete is clicked 
    observeEvent(c(input$nextBtn,input$prevBtn,input$deleteRow),{    
      updatedt()
      output$toxicityscores <- renderDataTable({
        get('globaltox',envir = .GlobalEnv)
      })
    })
    # For deleting rows
    rowvalues <- reactiveValues(globaltoxworking = get('globaltox',envir = .GlobalEnv))
    observeEvent(input$deleteRow,deletingrow(input$toxicityscores_rows_selected))
    deletingrow <- function(rowselected){
      print(input$deleteRow[[1]])
      if (!is.null(rowselected)) {
        #print(input$toxicityscores_rows_selected)
        tmpdt <- get('globaltox',envir = .GlobalEnv)
        tmpdt <- tmpdt[-as.numeric(rowselected),]
        assign('globaltox',tmpdt,globalenv())
        print(paste0('Delete function used: ',nrow(get('globaltox',envir = .GlobalEnv))))
      }
    }
    
    # Only refresh every time submit button is clicked
    observeEvent(input$formsubmission,{    
      updatedt()
      tmptoxrow <- toxrow()
      colnames(tmptoxrow) <- colnames(globaltox)
      # assign global variable
      assign('globaltox',rbind(get('globaltox',globalenv()),tmptoxrow),globalenv())
      output$toxicityscores <- renderDataTable({
        get('globaltox',envir = .GlobalEnv)
      })
    })
    
    # refresh globaltox when import button is clicked for input files
    observeEvent(input$importbutton,{
      updateimport()
      output$toxicityscores <- renderDataTable({
        get('globaltox',envir = .GlobalEnv)
      })
    })
    updateimport <- function(){
      incomingFile <- input$csvimport
      if(is.null(input$csvimport)){
        return(NULL)
      }
      else{
        incomingdt <- read.csv(incomingFile$datapath,header = TRUE,row.names = 1,check.names=FALSE)
        tmpglobaltox <- get('globaltox',envir = .GlobalEnv)
        #print(tmpglobaltox)
        if(ncol(incomingdt) == ncol(tmpglobaltox) &&
           all(colnames(incomingdt)== colnames(tmpglobaltox))){
          assign('globaltox',incomingdt,globalenv())
          #print(paste0('Imported ',get('globaltox',envir = .GlobalEnv)))
        }
      }
    }
    
    nextdosecalc <- reactive({
      simulationtime <- 30000
      confidentsimulation <- 5000
      valTNETS <- TNETSreactive()
      # Preset values for gamma and rho to be updated by MCMC
      pregamma <- 50
      prerho <- 0.05
      toxinfo <- get('globaltox',envir = .GlobalEnv)[,c(2,11)]
      print(toxinfo)
      # posteriorcalc function updata posterior distribution of gamma and rho
      posteriors <- posteriorcalc(pregamma, prerho, simulationtime, confidentsimulation, toxinfo, valTNETS)
      rhoresults <- posteriors[[1]][(simulationtime-confidentsimulation):simulationtime]
      gammaresults <- posteriors[[2]][(simulationtime-confidentsimulation):simulationtime]
      return(list(rhoresults,gammaresults))
    })
    # refresh plots and outputs when calculate button is clicked
    observeEvent(input$calculating,{
      results <- nextdosecalc()
      output$tableoutputs <- renderDataTable({
        quantilevector <- rep(0.05,20)
        for (i in 1:length(quantilevector)){
          quantilevector[i] <- quantilevector[i] + 0.05*(i-1)
        }
        print(quantilevector)
        rhodf <- data.frame(
          round(
            quantile(results[[1]],quantilevector),digits = 4
          )
        )
        print(rhodf)
        gammadf <- data.frame(
          round(
            quantile(results[[2]],quantilevector),digits = 4
          )
        )
        returndf <- cbind(rhodf,gammadf)
        colnames(returndf) <- c('rho0','gamma')
        rownames(returndf) <- quantilevector
        return(returndf)
      })
      output$rhooutputs <- renderPlot({
        print('done')
        quantilevector <- rep(0.05,20)
        for (i in 1:length(quantilevector)){
          quantilevector[i] <- quantilevector[i] + 0.05*(i-1)
        }
        barplot(quantile(results[[1]],quantilevector))
      })
      output$gammaoutputs <- renderPlot({
        print('done')
        quantilevector <- rep(0.05,20)
        for (i in 1:length(quantilevector)){
          quantilevector[i] <- quantilevector[i] + 0.05*(i-1)
        }
        print(quantilevector)
        barplot(quantile(results[[2]],quantilevector))
      })
      
    })
    
    # Mandatory toxicity fileds
    mandatoryfieldstox <- c('doselevel','l1tox','l2tox','l3ntox','l4ntox','l3tox','l4tox')
    # Mandatory parameter fields
    mandatoryfieldsparameter <- c('alphavalue','betavalue','pDLTvalue')
    # Mandatory EWOC fields
    mandatoryfieldsewoc <- c('prerho0','pregamma','xmin','xmax')
    
    observe({
      # js for page buttons
      toggleState(id = "prevBtn", condition = rv$page > 1)
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
      # js script for mandatory ewoc fields   
      mandatoryewoc <- vapply(
        mandatoryfieldsewoc,
        function(x){
          # Make sure every value is not NULL or non-numeric
          !is.null(input[[x]]) && !is.na(as.numeric(input[[x]]))
        },
        logical(1)
      )
      # Boolean value indicating if mandatory fields are filled
      filledpara <- all(mandatoryparameters)
      filledewoc <- all(mandatoryewoc)
      shinyjs::toggleState('nextBtn',
                           condition = # Make sure all parameters are valid before allowing input on second page
                             (filledpara && 
                                # parameters are conforming to certain conditions
                                as.numeric(input$pDLTvalue) <= 100 &&
                                rv$page < 2)||
                             (filledewoc &&
                                # ewoc parameters are conforming to certain conditions
                                as.numeric(input$xmax)>=0 && as.numeric(input$xmin)>=0 &&   
                                as.numeric(input$prerho0)<1 && as.numeric(input$prerho0)>=0 &&                              as.numeric(input$pregamma)<as.numeric(input$xmax) &&
                                as.numeric(input$pregamma)>as.numeric(input$xmin) &&                        
                                rv$page < 3)
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
          tags$div(
            class = 'fileImport',
            fileInput('csvimport','Import .csv File',
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
                      )
          ),
          actionButton('importbutton','Import',style="background-color: #004990;color: white"),
          textOutput('errorMessagePara'),
          actionButton('deleteRow','Delete',style="background-color: #004990;color: white")
          
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
                 textInput('prerho0','Please enter priori rho0:','0.05'),
                 textInput('pregamma','Please enter priori gamma:','50'),
                 textInput('xmin','Please enter minimum dose level:','0'),
                 textInput('xmax','Please enter maximum dose level:','100'),
                 textOutput("priorerr")
        )
      )
    })
    
    dataprior <- reactive({
      # Validate that all toxicity have been filled for error message
      validate(
        need(try(!is.null(input$prerho0)),"Priori rho0 invalid") ,
        need(try(!is.null(input$pregamma)),"Priori gamma invalid") ,
        need(try(!is.null(input$xmin)),"Minimum dose level invalid") ,
        need(try(!is.null(input$xmax)),"Maximum dose level invalid") ,
        need(try(!is.na(as.numeric(input$prerho0))),"Priori rho0 NOT a Number") ,
        need(try(!is.na(as.numeric(input$pregamma))),"Priori gamma NOT a Number") ,
        need(try(!is.na(as.numeric(input$xmin))),"Minimum dose level NOT a Number"),
        need(try(!is.na(as.numeric(input$xmax))),"Maximum dose level NOT a Number"),
        need(try(!is.na(as.numeric(input$xmax))),"Maximum dose level NOT a Number"), 
        need(try(as.numeric(input$xmax)>=0 && as.numeric(input$xmin)>=0 ),"Dose level must be POSITIVE"), 
        need(try(as.numeric(input$prerho0)<1 && as.numeric(input$prerho0)>=0 ),"Rho0 must be between [0,1)"),
        need(try(as.numeric(input$pregamma)<as.numeric(input$xmax) &&
                   as.numeric(input$pregamma)>as.numeric(input$xmin)),"Gamma must be between minimum and maximum dose level")
        
        
      )
      return(TRUE)
    })
    
    # Toxicity error messages
    output$priorerr <- renderText(
      if(!dataprior()){
        head(dataprior())
      }
    )
    
    output$page3 <- renderUI({
      tagList(
        h2("Step 3"),
        h3("Please list all instances for each toxicity level"),
        br(),
        # Input for all toxicity incidences
        tags$div(class = "widgets",
                 textInput('ptID','Please enter Patient ID(optional):','NA'),
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
        need(try(!is.null(input$doselevel)),'Dose level is invalid') ,
          need(try(!is.na(as.numeric(input$doselevel))),'Dose level is not a number')
      )
      return(as.numeric(input$doselevel))
    })
    
    datatox <- reactive({
      # Validate that all toxicity have been filled out before proceeding
      validate(
        need(try(!is.null(input$l1tox)),"Grade 1 toxicity invalid") ,
        need(try(!is.null(input$l2tox)),"Grade 2 toxicity invalid") ,
        need(try(!is.null(input$l3ntox)),"Grade 3 non-DLT invalid") ,
        need(try(!is.null(input$l4ntox)),"Grade 4 non-DLT invalid") ,
        need(try(!is.null(input$l3tox)),"Grade 3 DLT invalid") ,
        need(try(!is.null(input$l4tox)),"Grade 4 DLT invalid") ,
        need(try(!is.na(as.numeric(input$l1tox))),"Grade 1 toxicity NOT a Number") ,
        need(try(!is.na(as.numeric(input$l2tox))),"Grade 2 toxicity NOT a Number") ,
        need(try(!is.na(as.numeric(input$l3ntox))),"Grade 3 non-DLT NOT a Number") ,
        need(try(!is.na(as.numeric(input$l4ntox))),"Grade 4 non-DLT NOT a Number") ,
        need(try(!is.na(as.numeric(input$l3tox))),"Grade 3 DLT NOT a Number") ,
        need(try(!is.na(as.numeric(input$l4tox))),"Grade 4 DLT NOT a Number") ,
        need(try(!is.null(input$doselevel)),'Dose level is invalid') ,
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
        need(try(!is.null(input$l1tox)),"Grade 1 toxicity invalid") ,
          need(try(!is.null(input$l2tox)),"Grade 2 toxicity invalid") ,
          need(try(!is.null(input$l3ntox)),"Grade 3 non-DLT invalid") ,
          need(try(!is.null(input$l4ntox)),"Grade 4 non-DLT invalid") ,
          need(try(!is.null(input$l3tox)),"Grade 3 DLT invalid") ,
          need(try(!is.null(input$l4tox)),"Grade 4 DLT invalid") ,
          need(try(!is.na(as.numeric(input$l1tox))),"Grade 1 toxicity NOT a Number") ,
          need(try(!is.na(as.numeric(input$l2tox))),"Grade 2 toxicity NOT a Number") ,
          need(try(!is.na(as.numeric(input$l3ntox))),"Grade 3 non-DLT NOT a Number") ,
          need(try(!is.na(as.numeric(input$l4ntox))),"Grade 4 non-DLT NOT a Number") ,
          need(try(!is.na(as.numeric(input$l3tox))),"Grade 3 DLT NOT a Number") ,
          need(try(!is.na(as.numeric(input$l4tox))),"Grade 4 DLT NOT a Number") ,
          need(try(!is.null(input$doselevel)),'Dose level is invalid') ,
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
        need(try(!is.null(input$alphavalue)),'Alpha value is invalid') ,
        need(try(!is.na(as.numeric(input$alphavalue))),'Alpha value is not a number')
      )
      return(as.numeric(input$alphavalue))
    })
    
    # Reactive value for beta
    betavaluereactive <- reactive({
      # Debug clauses
      validate(
        need(try(!is.null(input$betavalue)),'Alpha value is invalid') ,
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
        need(try(!is.null(input$alphavalue)),'Alpha value is invalid') ,
        need(try(!is.na(as.numeric(input$alphavalue))),'Alpha value is not a number') ,
        need(try(!is.null(input$betavalue)),'Alpha value is invalid') ,
        need(try(!is.na(as.numeric(input$betavalue))),'Alpha value is not a number') ,
        need(try(!is.null(input$pDLTvalue)),'Expected percentage of DLT is invalid') ,
        need(try(!is.na(as.numeric(input$pDLTvalue))),'Expected percentage of DLT is not a number') ,
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
        need(try(!is.null(input$pDLTvalue)),'Expected percentage of DLT is invalid') ,
        need(try(!is.na(as.numeric(input$pDLTvalue))),'Expected percentage of DLT is not a number') ,
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
    
    # Reactive methods for calculating via MCMC
    # Predefined min and max dose on a scale of 0 to 100
    

    
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
    

    #,options = list(searching = TRUE, lengthChange = TRUE,orderClasses = TRUE,scrollX = TRUE,className = 'dt-center'
    #               ,selection = list(selected = input$deleteRow)
    #               )
    #)
    
    # Downloading files of all patients
    output$downFile <- downloadHandler(
      filename = function(){
        paste("Patient toxicities information","csv",sep = ".")
      },
      content = function(file){
        temp <- get('globaltox',envir = .GlobalEnv)
        write.csv(temp,file,row.names = TRUE)
      }
    )
  }
)

