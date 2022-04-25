# TODO: Add comment
# 
# Author: kirkli
###############################################################################
testObject <- function(object)
{
  exists(as.character(substitute(object)))
}

# Define server logic for random distribution application
shinyServer(function(input, output) {                
   
  output$text <- renderPrint({
    if(input$goButton1==0) {cat("Please load data.\n")
                            cat("Sample input csv file: \n")
    }
    else{    
      cat("first 10 rows of the return data")}})
  
  
  output$ui1 <-renderUI({
    if ((input$is.data.preload))
      return()
    else{    
  fileInput('file1', 'Choose Data File in CSV Format',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
    }
    })
  output$ui2 <-renderUI({
    if ((input$is.data.preload))
      return()
    else{    
      checkboxInput('header1', 'Header', TRUE)}
  })
  
  
  output$ui3 <-renderUI({
    if ((input$is.data.preload))
      return()
    else{    
      radioButtons('sep1', 'Separator',
                   c("Comma"=',',
                     "Semicolon"=';',
                     "Tab"='\t'),
                   ',')}
    })
  
  output$ui4 <-renderUI({
    if ((input$is.data.preload))
      return()
    else{    
      radioButtons('quote1', 'Quote',
                   c("None"='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')}
    })
  
  output$ui5 <-renderUI({
    if ((input$is.data.preload))
      return()
    else{    
      fileInput('file2', 'Choose Group File in CSV Format',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))}
    })
  
  output$ui6 <-renderUI({
    if ((input$is.data.preload))
      return()
    else{    
      checkboxInput('header2', 'Header', TRUE)}
    })
  
  output$ui7 <-renderUI({
    if ((input$is.data.preload))
      return()
    else{    
      radioButtons('sep2', 'Separator',
                   c("Comma"=',',
                     "Semicolon"=';',
                     "Tab"='\t'),
                   ',')}
    })
  
  output$ui8 <-renderUI({
    if ((input$is.data.preload))
      return()
    else{    
      radioButtons('quote2', 'Quote',
                   c("None"='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')}
  })
  
  output$contents <- renderTable({
    if(input$goButton1==0) {
      test <- read.csv("crsp.short.6.csv")
      head(test)
    }
    else{
      if(input$is.data.preload==T){
        mydata <<- read.csv("crsp.short.6.csv")
      }else{
        test <- read.csv("crsp.short.6.csv")
            print(head(test))    
        if(is.null(input$file1))cat("input data\n")
        inFile1 <- input$file1
        mydata <<- read.csv(
          inFile1$datapath, 
          header=input$header1, 
          sep=input$sep1, 
          quote=input$quote1)}
      head(mydata,10)}
  })
  
  output$text2 <- renderPrint({
    if(input$goButton1==0) {cat("Please load group information,\n")
                            cat("Sample input csv file: \n")
    }
    else{cat("group information")}})
  
  
  output$contents2 <- renderTable({
    if(input$goButton1==0){   test2 <- read.csv("crsp.short.6.group.csv")
                              print(head(test2))    }
    else{
      if(input$is.data.preload==T){
        mygroup <<- read.csv("crsp.short.6.group.csv")
      }else{
        if(is.null(input$file2))cat("input group info\n")
        inFile2 <- input$file2
        mygroup <<- read.csv(
          inFile2$datapath, 
          header=input$header2, 
          sep=input$sep2, 
          quote=input$quote2)}
      t(mygroup)}
  })
  
  output$plot <- renderPlot({ 
    if(input$goButton1==0) return()
    else{
      S <- do.call(efrontplot.shiny, list(
        mydata,
        mygroup,
        list(input$is.sum,
             input$is.lo,
             input$is.box,
             input$is.group,
             input$is.turnover,
             input$is.propcost),
        
        list(sum=input$sum, 
             upper=input$upper,
             lower=input$lower, 
             upper.group=input$upper.group, 
             lower.group=input$lower.group,
             toc=input$toc,
             ptc=input$ptc)))
      
    }})
  
  output$summary <- renderPrint({
				list(
						mydata,
						mygroup,
						list(input$is.sum,
								input$is.lo,
								input$is.box,
								input$is.group,
								input$is.turnover,
								input$is.propcost),
						
						list(sum=input$sum, 
								upper=input$upper,
								lower=input$lower, 
								upper.group=input$upper.group, 
								lower.group=input$lower.group,
								toc=input$toc,
								ptc=input$ptc))
  })
  
})



