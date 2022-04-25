# TODO: Add comment
# 
# Author: KirkLi
###############################################################################
count <- 0
# Define server logic for random distribution application
shinyServer(function(input, output) {
            inputTextarea <- function(inputId,  label="",value="", nrows=10, ncols=10) {
                tagList(
                        singleton(tags$head(tags$script(src = "textarea.js"))),
                        tags$label(label, `for` = inputId),
                        tags$textarea(id = inputId,
                                class = "inputtextarea",
                                rows = nrows,
                                cols = ncols,
                                as.character(value))
                )
            }
						
						
						inputData <- reactiveFileReader(1000,session=NULL, filePath="crsp.short.6.csv", read.csv,fill=T,header=T, na.strings=c("NA","."))
						
            
            output$text <- renderPrint({
                        if(is.null(input$file1) & input$dataset)
                            cat("Please load data.\n")
                        else{
                            if(!is.null(input$file1))cat("first 10 rows of the return data: \n")}
                    }
            )
            
            output$summary <-  renderDataTable({
                        if(!input$dataset){
														mydata <-mydata.raw<- inputData()
                            rownames(mydata) <- mydata[,1]
                            colnames(mydata.raw)[1] <- "date"
                            mydata <- mydata[,-1]
                            date <- mydata.raw[,1]
                            cbind(date,round(mydata,2))
                        }else{
                            if(!is.null(input$file1)){
                                inFile1 <- input$file1
                                mydata <- mydata.raw <- read.csv(
                                        inFile1$datapath, 
                                        header=input$header1, 
                                        sep=input$sep1, 
                                        quote=input$quote1)
                                rownames(mydata) <- mydata[,1]
                                colnames(mydata.raw)[1] <- "date"
                                mydata <- mydata[,-1]
                                date <- mydata.raw[,1]
                                cbind(date,round(mydata,2))}
                        }
#						}
                    })
            
            metric.list <- reactive({
                        if(length(input$mychooser$right)>=1)
                            table.Performance.input.shiny(metrics=input$mychooser$right)
                        else return()
                    })
#		
            nmetric<- reactive({
                        length(metric.list())
                    })
#		
            
            output$selection <- renderPrint(
#					nrows <<- length(input$mychooser$right)
                    input$mychooser$right
            )
            
            ct=1:50
            eval(parse(text=paste0("output$para.",ct," <- renderUI({
                                            if(length(input$mychooser$right)>= ",ct," ){
                                            count <- ",ct,"
                                            inputId = eval(parse(text=paste0('\"para.',count,'\"')))
                                            label= eval(parse(text=paste0('paste0(names(metric.list())[',count,'],\":\")')))
                                            value= eval(parse(text=paste0('paste0(names(metric.list()[[',count,']]),\"=\",metric.list()[[',count,']],collapse=\"\n\")')))
                                            if(nchar(value)>=2 & length(value)>0) # colum sign 
                                            inputTextarea(inputId,label,value,nrow=5,ncol=10)
                                            else return()
                                            
                                            }
                                            else return()
                                            })")))
            
            
            eval(parse(text=paste0("metric.list.m.",ct,"<- reactive({if(length(input$mychooser$right)>=",ct,"){if(length(input$para.",ct,")>0){
                                            l1 <- unlist(strsplit(input$para.",ct,",'\n'))
                                            l1 <- strsplit(l1,'=')
                                            temp <- metric.list()[[",ct,"]]
                                            temp[unlist(lapply(l1,'[[',1))]	<- 					unlist(lapply(l1,'[[',2))} else return()
                                            temp
                                            } 
                                            else{return()}
                                            })")))
            
            output$result <-renderDataTable({
                        if(!input$dataset){
                            mydata <-mydata.raw<- inputData()
                            rownames(mydata) <- mydata[,1]
                            colnames(mydata.raw)[1] <- "date"
                            mydata <- mydata[,-1]
                        }else{
                            if(!is.null(input$file1)){
                                inFile1 <- input$file1
                                mydata <- mydata.raw <- read.csv(
                                        inFile1$datapath, 
                                        header=input$header1, 
                                        sep=input$sep1, 
                                        quote=input$quote1)
                                rownames(mydata) <- mydata[,1]
                                colnames(mydata.raw)[1] <- "date"
                                mydata <- mydata[,-1]
                            }}
                        metrics <- input$mychooser$right
                        if(length(input$mychooser$right)>0){
                            metricsOptArgVal <-list()
                            string.use <- 
                                    paste0("list(",paste0("metric.list.m.",1:50,"()",collapse=","),")")
                            metricsOptArgVal <- eval(parse(text=string.use						
                                    ))
                            
                            names(metricsOptArgVal) <- metrics
                            res <<- table.Performance.output.shiny(R=mydata,metricsOptArgVal= metricsOptArgVal,metrics=metrics,metricsNames=NULL)
                            cbind(metrics,res) 
                        } else return()
                    })
            output$downloadData <- downloadHandler(
                    # This function returns a string which tells the client
                    # browser what name to use when saving the file.
                    filename = function() {
                        paste0("PerformanceMetricTable_",Sys.Date(),".",input$filetype)
                    },
                    
                    # This function should write data to a file given to it by
                    # the argument 'file'.
                    content = function(file) {
                        sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
                        # Write to a file specified by the 'file' argument
                        write.table(res, file, sep = sep,
                                row.names = TRUE,col.names=NA)
                    })
            
            
        })


