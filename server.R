library(shiny)

library(Hmisc)
library(xtable)
library(hwriter)
library(data.table)
library(XML)
source("./scripts/crosstabs.R")
source('./scripts/cleanGoogleTable.R')
source('./scripts/readGoogleSheet.R')
emptymatch<-"."
## Change this to your source spreadsheet.
## The file can contain exactly one source sheet which should contain the links
## the spreadsheets to analyze and other information. See ReadMe.md
dataListUrl <- readLines("./datalistsource.txt", n =1)


shinyServer(function(input, output) {
        readgdatasetlist <- readGoogleSheet(dataListUrl)
        datasetList <- cleanGoogleTable(readgdatasetlist)
        
        oldnames <- colnames(datasetList)
        newnames <- c("timestamp", "courseId", "sheeturl", "title", "description")
        colnames(datasetList) <- newnames
        datasetList$alias <- gsub(" ", "_", paste0(datasetList$courseId, datasetList$title))
        
        
        output$dataset <-renderUI({   
                selectInput("dataset", "Select your data", choices = c(None=".", datasetList$courseId ), selected=".")
        }) 
         getDatasetName<- reactive({
                 
                datasetname <- input$dataset

                datasetname
         })

        getDatsetInfo<-function()
         {                
                myrow <- subset(datasetList, datasetList$courseId == input$dataset)
  
                # Check for duplicate names and use the first row with that name.
                if (nrow(myrow) != 1 )
                {
                       myrow<-myrow[1,]
                }
                datasetInfo<<-myrow

                read1 <- readGoogleSheet(myrow$sheeturl)
                datasetname <- cleanGoogleTable(read1)
              
                
                 varnames<-colnames(datasetname)
                # We need to strip out invalid characters and spaces
                varnames<-sub("]","__", gsub(" ", "_", varnames))
                varnames<-sub("[", "_", varnames, fixed = TRUE)
                varnames<-sub("?", "", varnames, fixed = TRUE)
                varnames<-gsub(",", "", varnames, fixed = TRUE)
                colnames(datasetname)<-varnames
                varnames<-varnames[varnames != "Timestamp"]

                ## Google seems to add some non ascii to the end of the last value in a factor               
                convertNonAscii<-function(varname)
                {
                        if (class(datasetname[[varname]]) == "factor" | class(datasetname[[varname]]) == "character")
                        {
                                datasetname[[varname]]<-iconv(datasetname[[varname]], "latin1", "ASCII", sub="")        
                        }
                }
                datasetname<-as.data.frame(sapply(varnames, convertNonAscii ))
                
                datasetname<<-NULL
                datasetname<<-datasetname
                varnames<<-NULL
                varnames<<-varnames
 
                varnames
                
         }
 
        output$dependent<-renderUI({
                
                if (!is.null(input$dataset) & !identical(input$dataset, emptymatch ) )
                {
                        dataset<-input$dataset
                        
                        getDatsetInfo()                        
                        
                        selectInput("dependent", 
                                    label = "Choose a dependent variable ",                   
                                    choices<- c(None='.', varnames),
                                    selected = '.'
                        )
                } else {
                        selectInput("dependent", 
                                    label = "Choose an dependent variable ",
                                    choices<- c(None='.'),
                                    selected = '.'
                        )
                        
                }
                 
        })
        output$independent<-renderUI({
                
                if (!is.null(input$dataset) & !identical(input$dataset, emptymatch ))
                {
                        dataset<-input$dataset
                        
                        getDatsetInfo()                        
                        
                        selectInput("independent", 
                                    label = "Choose an independent variable ",                   
                                    choices<- c(None='.', varnames),
                                    selected = '.'
                        )
                } else {
                        selectInput("independent", 
                                    label = "Choose an independent variable ",
                                    choices<- c(None='.'),
                                    selected = '.'
                        )
                        
                }
        })

               output$datasummary<-renderText({
                        
                        if (!is.null(input$dataset) & !identical(input$dataset, emptymatch))
                        {      
                                #To start off these are set to null.
                                ndims<<-NULL
                                ctab<<-NULL
           
                                text1<-paste("<h3>Information about this data set</h3>")
                                ## Include an HTML description in the data file.
                                text4<-paste0("<h4>",as.character(datasetInfo$title), "</h4>")
                                text2<-as.character(datasetInfo$description)
                                text3<-paste(text1, text4, text2, sep="<br><br> ")
                                
                                return(text3)
                        } else {
                                return("<p>Please choose a data set</p>")
                                
                        }
                        
                 }) 
                output$title<-renderText({
                        title<-""
                        if (!identical(input$dependent, emptymatch) & !is.null(input$dependent) )
                        {
                                title<-paste0("<h4>",input$dependent)
                                
                        }
                        
                        if (!is.null(input$independent) & !identical(input$independent, emptymatch))
                        {
                                title<-paste(title,"by", input$independent, "<br/>", sep=" ")
                                
                        }
                        if (!is.null(input$control) & !identical(input$control, emptymatch))
                        {
                                title<-paste0(title, "Controlling for ", input$control, " = ", input$subsetValue, "<br/>")      
                        }
                        if (title != "")
                        {
                                title<-paste0(title, " in Percents</h4>")
                                title<-gsub("_", " ", title)
                        }
                        
                       title
                })
        output$control<-renderUI({ 
                
                if (!is.null(input$dataset) & !identical(input$dataset, emptymatch )
                    
                )
                {
                        
                        getDatsetInfo()
                        
                        selectInput("control", 
                                    label = "Choose a control variable ",
                                    choices<- c(None='.', varnames),
                                    selected = '.'
                        )
                } else {
                        selectInput("control", 
                                    label = "Choose a control variable ",
                                    choices<- c(None='.'),
                                    selected = '.'
                        )
                        
                }
        })
        
        output$subsetValue<-
                renderUI({
                        if ((!is.null(input$dataset) & !identical(input$dataset, emptymatch ))
                            & (!is.null(input$control) & !identical(input$control, emptymatch ))
                            
                        )
                        {
                                ##dataset<-as.data.frame(datasetname)
                                
                                subset<-datasetname[[input$control]]

                                subsetValues<-names(table(subset[1]))
                                
                                selectInput("subsetValue", 
                                            label = "Choose a subset within the control",                   
                                            choices<- c(None='.', subsetValues),
                                            selected = '.'
                                )
                                
                        } else {
                                selectInput("subsetValue", 
                                            label = "Choose a subset within the control",
                                            choices<- c(None='.'),
                                            selected = '.'
                                )
                                
                        }
                        
                })
        
                 output$table <- renderTable({
                        if (( !is.null(input$dataset) &  !identical(input$dataset, emptymatch) )
                            & ( !identical(input$dependent, emptymatch) & !is.null(input$dependent) )
                            
                        )
                        {       
                        
                        # For now if there is a control variable, subset based on the value selected
                        # This is ugly but it works with xtable which shiny depends on.
                        if (!is.null(input$subsetValue) & !identical(input$subsetValue, emptymatch) 
                            & !is.null(input$control) & !identical(input$control, emptymatch)
                        )
                        {
                                datasetnamec<-datasetname[datasetname[[input$control]] == input$subsetValue,]
                                
                        }
                        
                        # Must be in the correct order.
                        factorsToUse<-c(input$dependent, input$independent)
                        
                        factorsToUse<-factorsToUse[factorsToUse != "."]
                        ctab<<-NULL
                        
                        if (!exists("datasetnamec"))
                        {
                                
                                ctab<<-creatextab( factorsToUse, datasetname)
                        } else {
                                ctab<-creatextab( factorsToUse, datasetnamec)
                        }
                        dimctab<-dim(ctab)
                        
                        ndims<<-NULL
                        ndims<<-length(dimctab)
                        
                        if (ndims == 2)
                        {
                                cnames<-colnames(ctab)
                                ncols<-dimctab[2]
                                
                                ctab<-prop.table(ctab,2)*100
                                # Because of datasetc
                                ctab<<-ctab
                                
                                
                        } else if (ndims == 1)
                        {
                                
                                ctab<<-prop.table(ctab)*100
                        }                 
                        
                        return(ctab)
                        
                } 
        
        })
        output$plot<-renderPlot({
                
                if (!is.null(input$dataset) & !identical(input$dataset, emptymatch ) & !is.null(ndims))
                {
                        if (!identical(input$dependent, emptymatch ) & identical(input$independent, emptymatch ) 
                            & identical(input$subsetValue, emptymatch )
                            & identical(input$control, emptymatch )
                            
                            & ndims == 1)
                        {
                                plot<-barplot(ctab, beside = TRUE)
                        } else if ((ndims == 2 &  (!is.null(input$dependent) & !identical(input$dependent,emptymatch) )
                                    | (!is.null(input$independent) & !identical(input$independent,emptymatch) ))
                                   & (identical(input$subValue,emptymatch) | is.null(input$subValue))
                                   
                        ) 
                                
                        {
                                
                                plot<-barplot(ctab, beside = FALSE, legend = TRUE, ylab="Percent")
                               ## ndims<<-NULL
                              ## ctab<<-NULL
                        } else if (
                                ((!is.null(input$dependent) & !identical(input$dependent,emptymatch) )
                                 & (!is.null(input$independent) & identical(input$independant,emptymatch))
                                 & (!is.null(input$control) & !identical(input$control,emptymatch))
                                 & !identical(input$subvalue,emptymatch) & ndims == 2)
                        )
                        {
                                                              
                                ## Change this to show grouped plot?
                                plot<-barplot(ctab, beside = TRUE, legend=TRUE, ylab ="Percent")
                        }
                        
                        plot
                }                
                
        })
        
}
)
