library(shiny)
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggbeeswarm)

physData <- read.csv("2017Year10.csv", check.names = FALSE,na.strings=c("NA","NaN", " ",""))
testOrder<-names(physData)[10:ncol(physData)]
physData <- gather(physData,Subject, Score, 10:ncol(physData))
physData$isStandardised <- rep(FALSE,nrow(physData))


sphysData <- read.csv("2017Year10.csv", check.names = FALSE,na.strings=c("NA","NaN", " ",""))

holder <- sphysData[, (names(sphysData) %in% names(sphysData)[1:9])]
sphysData <- sphysData[,!(names(sphysData) %in% names(holder))]
sphysData <- as.data.frame(scale(sphysData))
sphysData <- cbind(holder,sphysData)
rm(holder)
sphysData <- gather(sphysData,Subject, Score, 10:ncol(sphysData))
sphysData$isStandardised <- rep(TRUE,nrow(sphysData))
physData <- rbind(physData,sphysData)
physData <- physData[which(!is.na(physData$Score)),]
physData$Subject<-factor(physData$Subject, levels=testOrder)
rm(sphysData)
physData<-physData[, !(names(physData) %in% names(physData)[5:9])]
physData$Boarder <- as.character(physData$Boarder)
physData$Boarder <- ifelse(is.na(physData$Boarder), 
                             'No', physData$Boarder)

choicesApp<-physData[which(physData$isStandardised == FALSE),]
choicesApp<-choicesApp[, (names(choicesApp) %in% names(choicesApp)[1:3])]
choicesApp<-unique(choicesApp)
choicesApp <- choicesApp[order(choicesApp$LastName),]
choicesApp <- setNames(as.character(choicesApp$CCODE), paste(choicesApp$FirstName,choicesApp$LastName))

ui <- navbarPage("Reportoire",
                 tabPanel("Student Results",fluidPage(
                   title='Exam results',
                   
                   fluidRow(
                     
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
                 tabPanel("Monitoring Groups",fluidPage(
                   title='Exam results',
                   
                   fluidRow(
                     
                     column(3,
                            selectInput("isStandardised2", label ="Standardised", 
                                        choices = list("TRUE" = "TRUE", "FALSE" = "FALSE"), selected = "FALSE")
                     ),
                     column(3,
                            selectInput("Groups", label = "Groups", 
                                        choices = names(physData)[4])
                     )
                     
                   ),
                   
                   hr(),
                   plotOutput('plot2', height="500")
                 )))



server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    data <- physData[which(
      physData$isStandardised==input$isStandardised
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
    
    
    ggplot(data,aes_string("Subject", "Score")) + geom_quasirandom(size=3)+theme_minimal()+labs(title = paste(
      " End-of-Topic Test Scores ",
      if (input$isStandardised2 == TRUE) {
        "(Standardised) "
      } else{
        "(Raw) "
      },
      sep = ""
    ), y =if (input$isStandardised2 == TRUE) {
      "Standardised Score"
    } else{
      "Reported Percentage Score"
    }
    )+theme(text=element_text(size=18),axis.text.x = element_text(angle = 45, hjust = 1))+ facet_grid(reformulate(input$Groups,"."))
    
    
  } )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

