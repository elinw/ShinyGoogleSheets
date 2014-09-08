library(shiny)


shinyUI(fluidPage(
        # Application title
        titlePanel("Analyze your spreadsheet"),
        
        sidebarLayout(
                sidebarPanel(
                        helpText("Choose your data and variables."),
                       uiOutput("dataset"),
                        
                       uiOutput("dependent"),
                       uiOutput("independent"),
                       uiOutput("control"),
                       uiOutput("subsetValue")
                        
               ),
                mainPanel(
                         htmlOutput("title"),
                         tabsetPanel(     
                                 
                                 tabPanel("Table",
                                          
                                         htmlOutput("table"),
                                          
                                         htmlOutput("datasummary")
                                 ),
                                 tabPanel("Plot",
                                         plotOutput("plot")
                                 ),
                                 tabPanel("About",
                                          includeMarkdown("./docs/about.Rmd")         
                                 )
                        )
                
                 )
        )
        )
)