library(shiny)
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggbeeswarm)

physData <- read.csv("result.csv", check.names = FALSE,na.strings=c("NA","NaN", " ",""))
physData <- physData[which(physData$cohort==2018), ]
physData <- physData[!is.na(physData$Subject),]
academicYears <- unique(physData$academicYear)

physData$academicYear <- factor(physData$academicYear, levels = academicYears)
choicesApp<-physData[which(physData$isStandardised == FALSE),]
choicesApp<-choicesApp[, (names(choicesApp) %in% names(choicesApp)[1:2])]
choicesApp<-unique(choicesApp)
# Random Name Generator
choicesApp$eyeballName <- randomNames(nrow(choicesApp),gender=1, ethnicity=5, name.order="first.last",name.sep=" ",sample.with.replacement = TRUE)
choicesApp <- choicesApp[order(choicesApp$eyeballName),]
choicesApp <- setNames(as.character(choicesApp$CCODE), choicesApp$eyeballName)


ui <- navbarPage("Reportoire",
                 tabPanel("Individual Year",fluidPage(
                   title='Exam results',
                   
                   fluidRow(
                     column(3,
                            selectInput("academicYear", label ="Year", 
                                        choices = unique(physData$academicYear), selected = "FALSE")
                     ),
                     
                     column(3,
                            selectInput("isStandardised", label ="Standardised", 
                                        choices = list("TRUE" = "TRUE", "FALSE" = "FALSE"), selected = "FALSE")
                     ),
                     column(3,
                            selectizeInput(
                              'CCODE', 'Student', choices = choicesApp,
                              options = list(
                                placeholder = 'Please select a student',
                                onInitialize = I('function() { this.setValue(""); }')
                              )
                            )
                            
                            
                            
                     )),
                   
                   hr(),
                   plotOutput('plot1', height="500")
                 ))
                 ,
                 tabPanel("Attainment over time",fluidPage(
                   title='Exam results',
                   
                   fluidRow(
                     
                     column(3,
                            selectInput("isStandardised2", label ="Standardised", 
                                        choices = list("TRUE" = "TRUE", "FALSE" = "FALSE"), selected = "FALSE")
                     ),
                     column(3,
                            selectizeInput(
                              'CCODE2', 'Student', choices = choicesApp,
                              options = list(
                                placeholder = 'Please select a student',
                                onInitialize = I('function() { this.setValue(""); }')
                              )
                            ))
                   ),
                   
                   hr(),
                   plotOutput('plot2', height="500")
                 )))



server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    data <- physData[which(
      physData$isStandardised==input$isStandardised & physData$academicYear==input$academicYear
    ), ]
    if (input$CCODE==''){
    }else{
      student.data <- data[which(
        data$CCODE==input$CCODE
      ), ]
      data<-data[which(
        data$Subject %in% student.data$Subject
      ), ]
    }
    
    colourFactor<-if(is.null(input$Groups)){""}else{input$Groups}
    
    p<-ggplot(data,aes_string("Subject", "Score")) + geom_quasirandom(size=3)+theme_minimal()+labs(title = paste(
      " End-of-Topic Test Scores ",
      if (input$isStandardised == TRUE) {
        "(Standardised) "
      } else{
        "(Raw) "
      },
      sep = ""
    ), y =if (input$isStandardised == TRUE) {
      "Standardised Score"
    } else{
      "Reported Percentage Score"
    }
    )+theme(text=element_text(size=18),axis.text.x = element_text(angle = 45, hjust = 1))
    if (input$CCODE==''){
      p
    }else{
      p+geom_point(data=student.data, aes(x=student.data$Subject, y=student.data$Score),colour = "red",size=6)
    }
    
    
  } )
  
  output$plot2 <- renderPlot({
    data <- physData[which(
      physData$isStandardised==input$isStandardised2
    ), ]
    if (input$CCODE2==''){
    }else{
      student.data <- data[which(
        data$CCODE==input$CCODE2
      ), ]
      # data<-data[which(
      #   data$Subject %in% student.data$Subject
      # ), ]
    }
    
    p<-ggplot(data,aes(academicYear, Score))+ geom_quasirandom(size=3, alpha=0.7)+guides(col = guide_legend(reverse = TRUE))+theme_minimal()+labs(
      y =if (input$isStandardised2 == TRUE) {
      "Standardised Score"
    } else{
      "Reported Percentage Score"
    }
    )+theme(text=element_text(size=18),axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap( ~Subject)
    if (input$CCODE2==''){
    p
    }else{
    p+geom_point(data=student.data, aes(x=academicYear, y=Score),shape=4,stroke=2,colour = "red",size=3)
  }

    
  } )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

