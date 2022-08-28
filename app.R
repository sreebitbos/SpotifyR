#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(knitr)
library(pander)
library(shinyWidgets)
library(shinythemes)
library(DT)

top50 = read.csv("top50.csv")

ui <- fluidPage(theme = shinytheme("flatly"),
                
                
                navbarPage("Hit Song Predictor",
                           tabPanel("Home",
                                    sidebarLayout(
                                      sidebarPanel(
                                      ),
                                      mainPanel(
                                        titlePanel("Predicting Hit Songs Through Machine Learning"),
                                        #br(),
                                        tags$div(
                                          tags$h5("This application uses Spotify's Top 50 Songs in 2019 to predict the popularity of songs in the future based on a wide variety of characteristics.")),
                                        br(),
                                        br(),
                                        tags$div(
                                          tags$h4("To view the multiple linear regression model, navigate to the Multiple Linear Regression tab. To view the Hit or Miss song predictor, navigate to Hit or Miss? tab.")),
                                        br(),
                                        br(),
                                        tags$div(
                                          tags$h4("The Multiple Linear Regression tab displays the Top 50 Songs in 2019 dataset. This data was taken from Spotify. The tab then computes parameters of the independent variables and plots the regression model of Valence against Popularity. The tab also interprets results." )),
                                        br(),
                                        br(),
                                        tags$div(
                                          tags$h4("The Hit or Miss? tab uses the regression model computed in the Multiple Linear Regression tab to predict whether a song may be a hit or a miss. This prediction is based on many independent
                                  variables that characterize a song from Beats per Minute to the Energy of the song. The tab allows the user to use the sliding scale to input an appropriate value for the corresponding variables and see 
                                  how the popularity level goes up or down. A song above a certain popularity level is considered a Hit song whereas a song below this level is considered a Miss. ")
                                        ),
                                        br(),
                                        br(),
                                        tags$div(
                                          tags$h4("I decided to choose this specific Spotify dataset, because it was small and had many independent variables. This topic and dataset was interesting to me, because I was very curious about patterns within musical characteristics and whether popular songs tend to have similar characteristics. However, the analysis showed that this is not always the case. I analyzed the data using a linear regression model and decision tree. I used the decision tree initially to explore the data and understand relationships that drive the popularity of a song. I decided to create a Hit or Miss predictor to allow functionality for a user to directly explore what variables strongly affect popularity and what variables do not affect popularity as much. I also decided to use a regression model due to the number of variables available. After conducting the analysis, I found that these independent variables are not very highly correlated with popularity. The variable that most strongly affected popularity was Valence, the mood of a song. In fact, it was inversely correlated so that the lower the Valence level, the more popular the song would be."))
                                      )
                                      
                                      
                                      
                                    )
                           ),
                           
                           tabPanel("Multiple Linear Regression",
                                    titlePanel("Multiple Linear Regression of Spotify's Top 50 Songs in 2019"),
                                    h4("Sreebitbos"),
                                    withMathJax(),
                                    tabPanel("Top 50 Songs in 2019",  
                                             sidebarLayout(
                                               sidebarPanel(
                                                 tags$b("Data:"),
                                                 tags$h5("The top 50 most listened songs in the world from Spotify. This dataset has several variables about the songs."),
                                                 hr(),
                                                 tags$b("Plot:"),
                                                 checkboxInput("se", "Add confidence interval around the regression line", TRUE),
                                                 hr(),
                                                 
                                                 radioButtons("format", "Download report:", c("HTML", "PDF", "Word"),
                                                              inline = TRUE
                                                 ),
                                                 checkboxInput("echo", "Show code in report?", FALSE),
                                                 downloadButton("downloadReport"),
                                                 #link my github account and the code
                                                 hr(),
                                                 HTML('<p>Report a <a href="https://github.com/sreebitbos/SpotifyR/issues">bug</a> or view the <a href="https://github.com/sreebitbos/SpotifyR/blob/master/ShinyCode">code</a>'),
                                                 
                                               ),           
                                               mainPanel(
                                                 tags$h4("2019 Spotify data:"),
                                                 DT::dataTableOutput("top50"),
                                                 br(),
                                                 uiOutput("data"),
                                                 br(),
                                                 tags$h4("Compute parameters in R:"),
                                                 verbatimTextOutput("summary"),
                                                 br(),
                                                 tags$h4("Regression plot:"),
                                                 uiOutput("results"),
                                                 plotlyOutput("plot"),
                                                 br(),
                                                 tags$h4("Interpretation:"),
                                                 uiOutput("interpretation"),
                                                 br(),
                                                 br()
                                               )
                                             ),
                                             
                                    ),
                           ),
                           tabPanel("Hit or Miss?",
                                    titlePanel("Hit or Miss?"),
                                    withMathJax(),
                                    
                                    #Sidebar with slider input
                                    sidebarLayout(
                                      sidebarPanel(
                                        tags$b("Song Characteristics:"),
                                        hr(),
                                        sliderInput(inputId = "num",
                                                    label = "Choose amount of Beats per Minute",
                                                    value = 88, min = 50, max = 300),
                                        sliderInput(inputId = "num2",
                                                    label = "Choose Energy Level",
                                                    value = 88, min = 1, max = 100),
                                        sliderInput(inputId = "num3",
                                                    label = "Choose Danceability Level",
                                                    value = 88, min = 1, max = 100),
                                        sliderInput(inputId = "num4",
                                                    label = "Choose Loudness Level",
                                                    value = 88, min = -20, max = 10),
                                        sliderInput(inputId = "num5",
                                                    label = "Choose Liveness Level",
                                                    value = 88, min = 1, max = 75),
                                        sliderInput(inputId = "num6",
                                                    label = "Choose Length",
                                                    value = 88, min = 100, max = 315),
                                        sliderInput(inputId = "num7",
                                                    label = "Choose Valence",
                                                    value = 88, min = 1, max = 100),
                                        sliderInput(inputId = "num8",
                                                    label = "Choose Acousticness",
                                                    value = 88, min = 1, max = 100),
                                        sliderInput(inputId = "num9",
                                                    label = "Choose Speechiness",
                                                    value = 88, min = 1, max = 50),
                                      ),
                                      mainPanel(
                                        tags$h4("Song Popularity Determination Metric"),
                                        br(),
                                        textOutput("popularity") ,
                                        br(),
                                        tags$h4("Hit or Miss?"),
                                        uiOutput("HoM"),
                                        imageOutput("hit"),
                                        imageOutput("miss"),
                                        br()
                                        
                                      )
                                    )
                           )
                           
                )
)

server <- function(input, output) {
  
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  
  # Data output
  output$top50 = DT::renderDataTable({
    top50
    DT::datatable(data = top50,
                  extensions = "Buttons",
                  options = list(
                    lengthChange = FALSE,
                    dom = "Blfrtip",
                    buttons = c("copy", "csv", "excel", "pdf", "print")
                  )
    )
  })      
  
  output$summary <- renderPrint({
    multRegModel = lm(top50$Popularity~top50$Beats.Per.Minute+top50$Energy+top50$Danceability+top50$Loudness..dB..+top50$Liveness+top50$Valence+top50$Length+top50$Acousticness+top50$Speechiness,data=top50)
    summary(multRegModel)
  })
  
  output$results <- renderUI({
    multRegModel = lm(top50$Popularity~top50$Beats.Per.Minute+top50$Energy+top50$Danceability+top50$Loudness..dB..+top50$Liveness+top50$Valence+top50$Length+top50$Acousticness+top50$Speechiness,data=top50)
    
    withMathJax(
      paste0(
        "Adj. \\( R^2 = \\) ", round(summary(multRegModel)$adj.r.squared, 3),
        ", \\( \\beta_0 = \\) ", round(multRegModel$coef[[1]], 3),
        ", \\( \\beta_1 = \\) ", round(multRegModel$coef[[2]], 3),
        ", \\( \\beta_2 = \\) ", round(multRegModel$coef[[3]], 3),
        ", \\( \\beta_3 = \\) ", round(multRegModel$coef[[4]], 3),
        ", \\( \\beta_4 = \\) ", round(multRegModel$coef[[5]], 3),
        ", \\( \\beta_5 = \\) ", round(multRegModel$coef[[6]], 3),
        ", \\( \\beta_6 = \\) ", round(multRegModel$coef[[7]], 3),
        ", \\( \\beta_7 = \\) ", round(multRegModel$coef[[8]], 3),
        ", \\( \\beta_8 = \\) ", round(multRegModel$coef[[9]], 3),
        ", \\( \\beta_9 = \\) ", round(multRegModel$coef[[10]], 3)
        #", P-value ", "\\( = \\) ", signif(summary(multRegModel)$coef[2, 4], 3)
      )
    )
  })
  
  output$interpretation <- renderUI({multRegModel = lm(top50$Popularity~top50$Beats.Per.Minute+top50$Energy+top50$Danceability+top50$Loudness..dB..+top50$Liveness+top50$Valence+top50$Length+top50$Acousticness+top50$Speechiness,data=top50)
  
  if (summary(multRegModel)$coefficients[1, 4] < 0.05 & summary(multRegModel)$coefficients[2, 4] < 0.05) {
    withMathJax(
      paste0("Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"))
    br()
  } else if (summary(multRegModel)$coefficients[1, 4] < 0.05 & summary(multRegModel)$coefficients[2, 4] >= 0.05) {
    withMathJax(
      paste0("Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients."),
      br(),
      paste0("\\( \\beta_1 \\)", " has a p-value = ", round(summary(multRegModel)$coefficients[2, 4], 3), ", so there is no significant relationship between Beats Per Minute and Popularity."),
      br(),
      paste0("\\( \\beta_2 \\)", " has a p-value = ", round(summary(multRegModel)$coefficients[3, 4], 3), ", so there is no significant relationship between Energy and Popularity."),
      br(),
      paste0("\\( \\beta_3 \\)", " has a p-value = ", round(summary(multRegModel)$coefficients[4, 4], 3), ", so there is no significant relationship between Danceability and Popularity."),
      br(),
      paste0("\\( \\beta_4 \\)", " has a p-value = ", round(summary(multRegModel)$coefficients[5, 4], 3), ", so there is no significant relationship between Loudness and Popularity."),
      br(),
      paste0("\\( \\beta_5 \\)", " has a p-value = ", round(summary(multRegModel)$coefficients[6, 4], 3), ", so there is no significant relationship between Liveness and Popularity."),
      br(),
      tags$b(paste0("\\( \\beta_6 \\)", " has a p-value = ", round(summary(multRegModel)$coefficients[7, 4], 3), ", describing a significant relationship between Valence and Popularity.")),
      br(),
      paste0("\\( \\beta_7 \\)", " has a p-value = ", round(summary(multRegModel)$coefficients[8, 4], 3), ", so there is no significant relationship between Length and Popularity."),
      br(),
      paste0("\\( \\beta_8 \\)", " has a p-value = ", round(summary(multRegModel)$coefficients[9, 4], 3), ", so there is no significant relationship between Acousticness and Popularity."),
      br(),
      paste0("\\( \\beta_9 \\)", " has a p-value = ", round(summary(multRegModel)$coefficients[10, 4], 3), ", so there is no significant relationship between Speechiness and Popularity.")
    )
  } else if (summary(multRegModel)$coefficients[1, 4] >= 0.05 & summary(multRegModel)$coefficients[2, 4] < 0.05) {
    withMathJax(
      paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
      br(),
      paste0("\\( \\beta_0 \\)", " is not significantly different from 0 (p-value = ", round(summary(multRegModel)$coefficients[1, 4], 3), ") so when ", input$xlab, " = 0, the mean of ", input$ylab, " is not significantly different from 0."),
      br(),
    )
  } else {
    withMathJax(
      paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
      br(),
      paste0("\\( \\beta_0 \\)", " and ", "\\( \\beta_1 \\)", " are not significantly different from 0 (p-values = ", round(summary(multRegModel)$coefficients[1, 4], 3), " and ", round(summary(multRegModel)$coefficients[2, 4], 3), ", respectively) so the mean of ", input$ylab, " is not significantly different from 0.")
    )
    
  }
  })
  
  output$plot <- renderPlotly({
    y <- top50$Popularity
    x <- top50$Valence
    multRegModel = lm(top50$Popularity~top50$Beats.Per.Minute+top50$Energy+top50$Danceability+top50$Loudness..dB..+top50$Liveness+top50$Valence+top50$Length+top50$Acousticness+top50$Speechiness,data=top50)
    
    dat <- data.frame(x, y)
    p <- ggplot(dat, aes(x = x , y = y)) +
      geom_point() +
      stat_smooth(method = "lm", se = input$se) +
      ylab("Popularity") +
      xlab("Valence") +
      theme_minimal()
    ggplotly(p)    
    
  })
  output$popularity = renderText({ 
    
    
    paste0("Popularity = ", (90.52 + 0.01*(input$num) + 0.01 *(input$num2) + 0.01 *(input$num3) + 0.13 *(input$num4) + 0.06 *(input$num5) + -0.07 *(input$num7) + -0.02 *(input$num6)+ -0.01 *(input$num8)+ 0.09 *(input$num9))
           
    )}
  )
  
  output$HoM <- renderUI({
    
    PopMetric = reactive({90.52 + 0.01*(input$num) + 0.01 *(input$num2) + 0.01 *(input$num3) + 0.13 *(input$num4) + 0.06 *(input$num5) + -0.07 *(input$num7) + -0.02 *(input$num6)+ -0.01 *(input$num8)+ 0.09 *(input$num9)})
    
    if (PopMetric() < 70) {
      withMathJax(
        paste0("Miss!"))
      br()
    } else {
      withMathJax(
        paste0("Hit!"),
        br() )
      
    }
  })
  
  # ReturnedValue = if (output$HoM == "Miss!") {
  #   withMathJax(
  #     paste0("No"))
  #   br()
  # } else {
  #   withMathJax(
  #     paste0("Yes"),
  #     br() )
  #   
  output$hit = renderImage({
    #conditionalPanel( condition = ReturnedValue == "Yes", 
    filename <- normalizePath(file.path('/cloud/project/CaseStudy3/images',
                                        paste('hit', '.jpeg', sep='')))
    list(src = filename)
    
    #conditionalPanel(condition = ReturnedValue == "No",
    #filename2 <- normalizePath(file.path('/cloud/project/CaseStudy3/images',
    #paste('miss', '.jpeg', sep=''))),
    #list(src = filename2)
  })
  br()
  
  deleteFile = FALSE      
  
}
shinyApp(ui = ui, server = server)

