##### inputs and outputs #####

#workingdirectory = "D:/docs/Spring 2017/499R/Shiny app"
#setwd(workingdirectory)
#outputdirectory = "D:/docs/Spring 2017/499R/Shiny app"
#install.packages("shiny")
library(shiny)
library(shinyjs)

##### define UI #####
shinyUI(fillPage( #fluidpage based on different browser
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),
  useShinyjs(),
  #Application title
  #sidebarlayout
  tags$body(
    tags$div(
      class = "container",
      tags$div(
        class = "title",
        tags$img(src='logo.png',class = "header"),
        tags$div(
          tags$h1("Calculator for Operating Characteristics of 3+3 Designs"),
          tags$h2("by  Dr. Zhengjia Chen & Youyun Zheng"),
          class = "header"
        )
      ),
      tags$div(
        class = "panel",
        sidebarLayout(
          #sidebarpanel
          tags$div(class="side",
                   sidebarPanel(
                     actionButton("prevBtn", "< Previous",style="color: #004990"),
                     actionButton("nextBtn", "Next >",style="color: #004990"),
                     hidden(
                       lapply(seq(2), function(i) {
                         div(
                           class = "page",
                           id = paste0("step", i),
                           if(i == 1){
                             uiOutput("page1")
                             
                           }
                           else if (i == 2){
                             uiOutput("page2")
                           }
                           #else if (i == 3){
                           #   uiOutput("page3")
                           # }
                         )
                       })
                     ),
                     br()
                   )
          ),
          #mainpanel
          mainPanel(
            tags$div(
              class = "main",
              tabsetPanel(
                tabPanel(
                  title = h1("Toxicity Info",class = "main"),
                  tableOutput('datatox'),
                  tableOutput('toxicityscores')
                ),
                tabPanel(
                  title = h1("Table Output",class = "main")
                  #tableOutput()
                ),
                tabPanel(
                  title = h1("Plot Output", class = "main")
                ),
                tabPanel(
                  title = h1("User Manual", class = "main"),
                  tags$div(class = "UserManual",
                   tags$h1("User Manual", class = "UserManual"),
                   tags$h2("1. How to use this calculator", class = "UserManual"),
                   tags$h3(
                     class = "UserManual",
                     HTML(
                       "1.1 To use this calculator, please finish inputting information on two separate pages:<br><br>"
                     )
                   ),
                   tags$div(
                     class = "piccontainer2",
                     tags$figure(
                       class = "piccontainer2",
                       tags$img(src='step1.png',class = "figure2"),
                       tags$h4("Figure 1. User Interface of page 1 of the calculator."
                               , class = "legend")
                     ),
                     tags$h1(HTML("On page 1, you will be able to let the calculator know:<br><br>
                                  <p class = 'tab1'>1. Whether your clinical trial is with or without dose deescalation <br><br></p>
                                  <p class = 'tab2'>a. The radio button gives you two options: without dose de-escalation or with dose de-escalation<br><br></p>
                                  <p class = 'tab1'>2. How many dose levels there will be in your clinical trial<br><br></p> 
                                  <p class = 'tab2'>a. The text Input window lets you input an integer number bigger than 0<br><br></p>
                                  "
                     ), class = "piccontainer2")
                     ),
                   tags$div(
                     class = "piccontainer2",
                     tags$figure(
                       class = "piccontainer2",
                       tags$img(src='step2.png',class = "figure2"),
                       tags$h4("Figure 2. User Interface of page 2 of the calculator."
                               , class = "legend")
                     ),
                     tags$h1(HTML("On page 2, you will be able to let the calculator know:<br><br>
                                  <p class = 'tab1'>1. What the probabilities of dose limiting toxicity are for each dose level <br><br></p>
                                  <p class = 'tab2'>a. The number of text inputs for probabilities of dose limiting toxicity corresponds to however many dose levels were indicated<br><br></p>
                                  <p class = 'tab1'>2. What the dosages are for each dose level (optional)<br><br></p> 
                                  <p class = 'tab2'>a. The number of text inputs for dosages corresponds to however many dose levels were indicated<br><br></p>
                                  <p class = 'tab2'>b. If nothing is entered, 'na' will appear in the corresponding location<br><br></p>
                                  Click the 'Calculate' button to obtain results"
                     ), class = "piccontainer2")
                     ),
                   tags$h3(
                     class = "UserManual",
                     HTML(
                       "1.2 To see the results, please toggle between two separate panels:<br><br>"
                     )
                   ),
                   tags$div(
                     class = "piccontainer2",
                     tags$figure(
                       class = "piccontainer2",
                       tags$img(src='result1.png',class = "figure"),
                       tags$h4("Figure 3. The 'Table Output' page for results"
                               , class = "legend")
                     ),
                     tags$h1(HTML("All calculation results will be shown on this page. Some key operating characteristics include:<br><br>
                                  <p class = 'tab1'>1. Probability of the Dose Level Chosen as MTD <br><br></p>
                                  <p class = 'tab1'>2. Expected Number of Patients Treated at the Dose Level<br><br></p> 
                                  <p class = 'tab1'>3. Expected Number of Dose Limiting Toxicity at the Dose Level<br><br></p>
                                  <p class = 'tab1'>4. Overall Rate of Dose Limiting Toxicity<br><br></p>
                                  <p class = 'tab1'>5. Expected Overall Number of Patients<br><br></p>
                                  Tables can be downloaded as CSV files
                                  "
                     ), class = "piccontainer2")
                     ),
                   tags$div(
                     class = "piccontainer2",
                     tags$figure(
                       class = "piccontainer2",
                       tags$img(src='result2.png',class = "figure"),
                       tags$h4("Figure 4. The 'Plot Output' page for results"
                               , class = "legend")
                     ),
                     tags$h1(HTML("6 plots are generated from key operating charactersitics and can be found on the 'Plot Output' page<br><br>
                                  <p class = 'tab1'>1. Probability of the Dose Level Chosen as MTD for each Dose Level and by Probability of Dose Limiting Toxicity <br><br></p>
                                  <p class = 'tab1'>2. Expected Number of Patients Treated for each Dose Level and by Probability of Dose Limiting Toxicity<br><br></p> 
                                  <p class = 'tab1'>3. Expected Number of Dose Limiting Toxicity at the Dose Level for each Dose Level and by Probability of Dose Limiting Toxicity<br><br></p>
                                  All plots can downloaded together as PDF files, or can be downloaded separately as PNG files<br><br>
                                  "
                     ), class = "piccontainer2")
                     ),
                   tags$h2("2. How clinical trial designs work", class = "UserManual"),
                   tags$h3(
                     class = "UserManual",
                     HTML(
                       "2.1 Standard 3+3 design without dose de-escalation<br><br>"
                     )
                   ),
                   tags$div(
                     class = "piccontainer",
                     tags$figure(
                       class = "piccontainer",
                       tags$img(src='Without.jpg',class = "figure"),
                       tags$h4("Figure 5. The flow chart of algorithmic-based clinical trial designs without dose de-escalation"
                               , class = "legend")
                     ),
                     tags$h1(HTML("To conduct a Phase I clinical trial with standard 3+3 design without dose de-escalation, the work flow for each dose level is presented as shown in Figure 5. Starting from dose 1, a cohort of 3 patients are entered each time and treated at the recommended dose level. The next cohort of 3 patients will be hold on until the toxicity responses of previous cohort of 3 patients have obtained and the newly recommended dose level has been determined.  Assuming that the newly recommended dose level to be tested is the dose level i and the new cohort of 3 patients are treated at the dose level i<br><br>
                                  <p class = 'tab1'>1. If there is no DLT among the first cohort of 3 patients treated at the dose level i, then dose escalation is recommended<br><br></p>
                                  <p class = 'tab1'>2. If there is 1 DLT among the first cohort of 3 patients treated at the dose level i, an additional cohort of 3 patients will be entered and treated at the dose i<br><br></p> 
                                  <p class = 'tab2'>a. If there is no DLT among the second cohort of 3 patients treated at the dose level i, dose escalation is recommended<br><br></p>
                                  <p class = 'tab2'>b. If there is 1 or more DLTs among the second cohort of 3 patients treated at the dose level i, then dose i-1 will be determined as MTD<br><br></p>
                                  <p class = 'tab1'>3. If there are 2 or 3 DLT among the first cohort of 3 patients treated at the dose level i, then the dose i-1 will be determined as MTD directly<br><br></p>
                                  When dose escalation is not indicated at dose level 1 (MTD = dose 0), and dose level 0 is chosen as MTD, or when dose escalation is indicated at dose level n (MTD >= dose n), the trial is unable to determine MTD<br> <br>
                                  "
                     ), class = "piccontainer")
                     ),
                   tags$h3(
                     class = "UserManual",
                     HTML(
                       "2.1 Standard 3+3 design with dose de-escalation<br><br>"
                     )
                   ),
                   tags$div(
                     class = "piccontainer",
                     tags$figure(
                       class = "piccontainer",
                       tags$img(src='With.jpg',class = "figure"),
                       tags$h4("Figure 6. The flow chart of algorithmic-based clinical trial designs with dose de-escalation"
                               , class = "legend")
                     ),
                     tags$h1(
                       class = "piccontainer",
                       HTML(
                         "To execute Phase I clinical trials with rule-based standard 3+3 design with dose de-escalation, the work flow for each dose level is presented as shown in Figure 6. Starting from dose 1, a cohort of 3 patients are entered each time and treated at the recommended dose level. The next cohort of 3 patients will be hold on until the toxicity responses of previous cohort of 3 patients have obtained and the newly recommended dose level has been determined.  Assuming that the newly recommended dose level to be tested is the dose level i and the new cohort of 3 patients are treated at the dose level i
                         <br><br>
                         <p class = 'tab1'>1.	If there is no DLT among the first cohort of 3 patients treated at the dose level i, then dose escalation is recommended<br><br></p>
                         <p class = 'tab1'>2.	If there is 1 DLT among the first cohort of 3 patients treated at the dose level i, an additional cohort of 3 patients will be entered and treated at the dose i<br><br></p>
                         <p class = 'tab2'>a.	If there is no DLT among the second cohort of 3 patients treated at the dose level i, dose escalation is recommended<br><br></p>
                         <p class = 'tab2'>b.	If there is 1 or more DLTs among the second cohort of 3 patients treated at the dose level i, then dose de-escalation will occur and the dose i-1 will be tested further with new cohort of 3 patients<br><br></p>
                         <p class = 'tab1'>3.	If there are 2 or 3 DLT among the first cohort of 3 patients treated at the dose level i, then dose de-escalation will occur and the dose i-1 will be tested further with new cohort of 3 patients<br><br></p>
                         When dose de-escalation is recommended:<br><br>
                         <p class = 'tab1'>1.	If 6 patients have already been treated at the dose i-1, which means none or 1 out of the 6 patients in two cohorts experience DLT at dose i-1, dose i-1 will be determined as MTD<br><br></p>
                         <p class = 'tab1'>2.	Otherwise, there is only one cohort of 3 patients have been treated at the dose i-1 and none DLT among the 3 patients, an additional cohort of 3 patients will be entered and treated at the dose level i-1<br><br></p>
                         <p class = 'tab2'>a. If none or 1 out of the new cohort of 3 patients experience DLT, then dose level i-1 will be determined as MTD<br><br></p>
                         <p class = 'tab2'>b.	o	If 2 or 3 patients out of the new cohort of 3 patients experience DLT, further dose de-escalation is required and a further lower dose level (such as the dose level i-2) will be tested. The procedure of dose de-escalation will repeat until a MTD is determined or all dose levels have been determined to be over-toxic and no MTD is determined<br><br></p>
                         <p class = 'tab1'>3.	Dose de-escalation will be continued to dose i-2 or so on if recommended until dose 0<br><br></p>
                          When dose escalation is not indicated at dose level 1 (MTD = dose 0), and dose level 0 is chosen as MTD, or when dose escalation is indicated at dose level n (MTD >= dose n), the trial is unable to determine MTD<br> <br>
                         "
                       )
                     )
                   ),
                   tags$h4(
                     HTML(
                       "
                        <p class = 'tab1'>Reference <br><br></p>
                        <p class = 'tab2'>Lin, Yong, and Weichung J. Shih. 'Statistical properties of the traditional algorithm‐based designs for phase I cancer clinical trials.' Biostatistics 2.2 (2001): 203-215.</p>
                       "
                     )
                   )
                 )
               )                
              )
            )
          )
        )
      )
    )
  )
))
