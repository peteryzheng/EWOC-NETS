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
          tags$h1("EWOC-NETS calculator"),
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
                  br(),
                  downloadButton('downFile','Download current information'),
                  br(),
                  tags$div(
                    class = 'toxtable',
                    DT::dataTableOutput(outputId = "toxicityscores", width = "100%")
                  )
                ),
                tabPanel(
                  title = h1("Table Output",class = "main")
                  #tableOutput()
                ),
                tabPanel(
                  title = h1("Plot Output", class = "main")
                )
              )
            )
          )
        )
      )
    )
  )
))
