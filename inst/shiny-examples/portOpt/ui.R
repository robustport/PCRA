# TODO: Add comment
# 
# Author: kirkli
###############################################################################
#library(shinyIncubator)
# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Efficient Frontier with Constraints"),
  sidebarPanel(
    h3("Input Data"),
    wellPanel(
      checkboxInput('is.data.preload', 'Use preloaded sample data?', FALSE)),
#     p("Otherwise just click 'Go' and load your own data"),

    tags$hr(),

      uiOutput("ui1"),
      uiOutput("ui2"),
      uiOutput("ui3"),
      uiOutput("ui4"),
      uiOutput("ui5"),
      uiOutput("ui6"),
      uiOutput("ui7"),
      uiOutput("ui8"),

    br(),
  actionButton("goButton1", "Go!"),
    
    h3("Specify Constraints"),
    wellPanel(
      checkboxInput(inputId = "is.sum", label = "sum",               value = TRUE),
      checkboxInput(inputId = "is.lo", label = "long only",          value = FALSE),
      checkboxInput(inputId = "is.box",  label = "box",              value = FALSE),
      checkboxInput(inputId = "is.group", label = "group",           value = FALSE),
      checkboxInput(inputId = "is.turnover", label = "turnover",     value = FALSE),
      checkboxInput(inputId = "is.propcost", label = "propcost",     value = FALSE)
    ),
    #         
    sliderInput("sum", 
                "sum (sum constriant):", 
                value = 1,
                min = 0.1, 
                max = 1,
                step= 0.01),
    
    sliderInput("lower", 
                "lower bound on each stock weight (box constraint):", 
                value = -1,
                min = -1, 
                max = 1,
                step= 0.01),
    
    sliderInput("upper", 
                "upper bound on each stock weight (box constraint):", 
                value = 1,
                min = -1, 
                max = 1,
                step= 0.01),
    
    sliderInput("lower.group", 
                "lower bound each group (group constraint):", 
                value = -1,
                min = -1, 
                max = 1,
                step= 0.01),
    
    sliderInput("upper.group", 
                "upper bound each group (group constraint):", 
                value = 1,
                min = -1, 
                max = 1,
                step= 0.01),
    
    sliderInput("toc", 
                "toc (turnover constraint):", 
                value = 1,
                min = 0.01, 
                max = 1,
                step= 0.01),
    
    sliderInput("ptc", 
                "ptc (proportional cost constraint):", 
                value = 0,
                min = 0, 
                max = 0.5,
                step= 0.01)
    
  )
  ,
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Input",
               textOutput('text'),
               tableOutput('contents'),
               textOutput('text2'),
               tableOutput('contents2')),
      tabPanel("Plot",
               plotOutput(outputId="plot",height="1000px",width="100%")),
			tabPanel("Summary",verbatimTextOutput("summary"))
    ))
))

