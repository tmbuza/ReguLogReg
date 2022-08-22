library(shiny)
library(shinydashboard)
library(shinythemes)
library(caret)
library(tidyverse)



ui <- dashboardPage(skin="black",
                    dashboardHeader(title=tags$em("Shiny Machine Learning App", style="text-align:center;color:#006600;font-size:100%"),titleWidth = 800),
                    
                    dashboardSidebar(width = 250,
                                     sidebarMenu(
                                       br(),
                                       menuItem(tags$em("Upload Test Data",style="font-size:120%"),icon=icon("upload"),tabName="data"),
                                       menuItem(tags$em("Download Predictions",style="font-size:120%"),icon=icon("download"),tabName="download")
                                       
                                       
                                     )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName="data",
                                
                                
                                br(),
                                
                                tags$h4("This shiny app allows you to upload your data, then do prediction and download results.
                                  This exampe demonstrate a Regularized Logistic Regression. 
                                  You can predict whether, for example, an individual is healthy or unhealthy. 
                                  So, by evaluating the results, you can make decision if the model predicts correctly of poorly.", style="font-size:150%"),
                                br(),
                                
                                tags$h4("Start by uploading test data, preferably in `csv format`", style="font-size:150%"),
                                
                                tags$h4("Then, go to the", tags$span("Download Predictions",style="color:red"),
                                        tags$span("section in the sidebar to  download the predictions."), style="font-size:150%"),
                                
                                br(),
                                br(),
                                br(),
                                column(width = 4,
                                       fileInput('file1', em('Upload test data in csv format ',style="text-align:center;color:blue;font-size:150%"),multiple = FALSE,
                                                 accept=c('.csv')),
                                       
                                       uiOutput("sample_input_data_heading"),
                                       tableOutput("sample_input_data"),
                                       
                                       
                                       br(),
                                       br(),
                                       br(),
                                       br()
                                ),
                                br()
                                
                        ),
                        
                        
                        tabItem(tabName="download",
                                fluidRow(
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  column(width = 8,
                                         tags$h4("After you upload a test dataset, you can download the predictions in csv format by
                                    clicking the button below.", 
                                                 style="font-size:200%"),
                                         br(),
                                         br()
                                  )),
                                fluidRow(
                                  
                                  column(width = 7,
                                         downloadButton("downloadData", em('Download Predictions',style="text-align:center;color:blue;font-size:150%")),
                                         plotOutput('plot_predictions')
                                  ),
                                  column(width = 4,
                                         uiOutput("sample_prediction_heading"),
                                         tableOutput("sample_predictions")
                                  )
                                  
                                ))
                      )))



load("rlreg_model.rda")    # Load saved model

source("featureMapping.R")                         #  a function for feature engineering. 
#  You can include data imputation, data manipulation, data cleaning,
#  feature transformation, etc.,  functions


server <- function(input, output) {
  
  options(shiny.maxRequestSize = 800*1024^2)   # This is a number which specifies the maximum web request size, 
  # which serves as a size limit for file uploads. 
  # If unset, the maximum request size defaults to 5MB.
  # The value I have put here is 80MB
  
  
  output$sample_input_data_heading = renderUI({   # show only if data has been uploaded
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Sample input data')
    }
  })
  
  output$sample_input_data = renderTable({    # show sample of uploaded data
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
      
      colnames(input_data) = c("Label","Test1", "Test2")
      
      input_data$Label = as.factor(input_data$Label )
      
      levels(input_data$Label) <- c("Healthy", "Unhealthy")
      head(input_data)
    }
  })
  
  
  
  predictions<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      withProgress(message = 'Predictions in progress. Please wait ...', {
        input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
        
        colnames(input_data) = c("Label","Test1", "Test2")
        
        input_data$Label = as.factor(input_data$Label )
        
        levels(input_data$Label) <- c("Healthy", "Unhealthy")
        
        mapped = feature_mapping(input_data)
        
        df_final = cbind(input_data, mapped)
        prediction = predict(rlreg_model, df_final)
        
        input_data_with_prediction = cbind(input_data,prediction )
        input_data_with_prediction
        
      })
    }
  })
  
  
  output$sample_prediction_heading = renderUI({  # show only if data has been uploaded
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Input with Predictions')
    }
  })
  
  output$sample_predictions = renderTable({   # the last 6 rows to show
    pred = predictions()
    head(pred)
    
  })
  
  
  output$plot_predictions = renderPlot({   # the last 6 rows to show
    pred = predictions()
    cols <- c("Healthy" = "green4", "Unhealthy" = "red")
    pred %>% 
      ggplot(aes(x = Test1, y = Test2, color = factor(prediction))) + 
      geom_point(size = 4, shape = 19, alpha = 0.6) +
      scale_colour_manual(values = cols, 
                          labels = c("Healthy", "Unhealthy"), 
                          name="Test Result") +
      theme_bw()
  })
  
  
  # Downloadable csv of predictions ----
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("input_data_with_predictions", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(predictions(), file, row.names = FALSE)
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("input_data_with_predictions", ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(predictions(), file)
    })
  
}

shiny::shinyApp(ui = ui, server = server)
