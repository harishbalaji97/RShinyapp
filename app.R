library(shiny)
library(DT)
library(dplyr)
library(shinythemes)

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
      .col-sm-4 {
        width: 20%;
      }")
    )
  ),
  # Create the Navigation for the app
  navbarPage(
    theme = shinytheme("flatly"), 
    "Statistical Analysis Application"),
  navlistPanel(
    "Statistical Analysis",
    tabPanel("Home",
             h3("About the Application"),
             h5("The purpose of this Shiny application is to provide users with a user-friendly interface for performing statistical analysis on their datasets. It allows users to upload CSV files, perform descriptive statistics (for both continuous and categorical variables), and conduct various statistical tests such as t-tests and chi-square tests.")
    ),
    # Create tabs for each functionality
    tabPanel("Upload file",
             h3("Upload File"),
             fileInput("file", "Choose CSV File", accept = ".csv"),
             h4("Data Preview", style = "font-weight:bold;"),
             DTOutput("summary_df_1"),
             br(),
             actionButton("print_summary", "Print Summary Statistics", class = "btn-primary"),
             conditionalPanel(
               condition = "input.print_summary > 0",
               h4("Descriptive Statistics", style = "font-weight:bold;"),
               DTOutput("summary_2"),
               br())
    ),
    
    # Create tab for Descriptive Analysis
    tabPanel("Descriptive Analysis",
             radioButtons("variable_type", "Choose Variable Type for Analysis:",choices = c("Continuous", "Categorical")),
             conditionalPanel(
               condition = "input.variable_type == 'Continuous'",
               
               fluidRow(
                 column(6,uiOutput("variable_select")),column(6,actionButton("analyze_btn", "Analyze the Variable Selected",class = "btn-primary"))),
               
               conditionalPanel(
                 condition = "input.analyze_btn > 0",
                 DTOutput("summary"),
                 plotOutput("histogram" ,height = "300px"),
                 h5("Shapiro-Wilk Test for Normality:", style = "font-weight:bold;"),
                 DTOutput("shapiro_test_result"),
                 br())
               
             ),
             conditionalPanel(
               condition = "input.variable_type == 'Categorical'",
               
               fluidRow(
                 column(6,uiOutput("categorical_variable_select")),column(6,actionButton("analyze_categorical_btn", "Analyze Categorical Data",class = "btn-primary"))
               ),
               
               conditionalPanel(condition = "input.analyze_categorical_btn > 0",
                                h4("Frequency of Categorical Values", style = "font-weight:bold;"),
                                plotOutput("barplot_categorical" ,height = "300px"),
                                h4("Summarised Table ", style = "font-weight:bold;"),
                                DTOutput("table_categorical")
               )
               
             )
    ),
    # Create tab for Statistical Analysis
    tabPanel("Statistical Analysis",
             radioButtons("variable_type_2", "Choose Variable Type for Analysis:",choices = c("Continuous", "Categorical")),
             conditionalPanel(
               condition = "input.variable_type_2 == 'Continuous'",
               numericInput("test_value", "Enter the test statistics value for One Sample T-Test ", value = 0),
               fluidRow(
                 column(6,uiOutput("variable_select_1")),column(6,uiOutput("variable_select_two"))
               ),
               fluidRow(
                 column(6,actionButton("t_test_btn", "Run One Sample T-Test",class = "btn-primary")),column(6,actionButton("t_test_two_btn", "Run Two Sample Independent T-Test",class = "btn-primary"))
               ),
               conditionalPanel(
                 condition = "input.t_test_btn > 0",
                 h4("Result of One Sample T-Test", style = "font-weight:bold;"),
                 DTOutput("t_test_result"),
               ),
               conditionalPanel(
                 condition = "input.t_test_two_btn > 0",
                 h4("Result of Two Sample Independent T-Test", style = "font-weight:bold;"),
                 DTOutput("t_test_two_result"),
                 plotOutput("scatter_plot" ,height = "300px"),
                 verbatimTextOutput("correlation_output")
               )
               
             ),
             conditionalPanel(
               condition = "input.variable_type_2 == 'Categorical'",
               
               fluidRow(
                 column(6,uiOutput("categorical_variable_select_1")),column(6,uiOutput("categorical_variable_select_2"))
               ),
               fluidRow(
                 column(6,actionButton("categorical_btn", "Run Chi-square Test of Homogeneity",class = "btn-primary")),column(6,actionButton("chi_square_btn", "Run Chi-square Test of Independence",class = "btn-primary"))
               ),
               
               conditionalPanel(condition = "input.categorical_btn > 0",
                                h4("Chi-Square Test of Homogeneity Result:", style = "font-weight:bold;"),
                                DTOutput("homogeneity_result")),
               conditionalPanel(condition = "input.chi_square_btn > 0 ",
                                h4("Chi-Square Test of Independence Result:", style = "font-weight:bold;"),
                                DTOutput("chi_square_result"),
                                plotOutput("stacked_bar" ,height = "300px")
               )
               
             )
    )
    
  )
)

server <- function(input, output, session) {
  
  # Get the input Data
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = TRUE)
  })
  # Get the list of all numeric columns
  numeric_columns <- reactive({
    req(data())
    names(data())[sapply(data(), is.numeric)]
  })
  # Get the list of all categorical columns
  categorical_columns <- reactive({
    req(data())
    names(data())[sapply(data(), function(x) is.factor(x) | is.character(x))]
  })
  
  # Get top 5 rows of Data
  output$summary_df_1 <- renderDT({
    datatable(head(data()), options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE))
  })
  
  # Get the variable for which analysis needs to be done 
  output$variable_select <- renderUI({
    selectInput("variable", "Choose a variable for Analysis", choices = numeric_columns())
  })
  
  # Get the variable for which analysis needs to be done 
  output$variable_select_1 <- renderUI({
    selectInput("variable_1", "Choose a variable for Analysis", choices = numeric_columns())
  })
  
  # Get the variable for which analysis needs to be done 
  output$variable_select_two <- renderUI({
    selectInput("variable_two", "Choose another variable for comparison:", choices = numeric_columns())
  })
  
  # Dynamically generate categorical variable selection
  output$categorical_variable_select <- renderUI({
    selectInput("categorical_variable", "Choose a categorical variable for Analysis", choices = categorical_columns())
  })
  
  # Get the variable for which analysis needs to be done 
  output$categorical_variable_select_1 <- renderUI({
    selectInput("categorical_variable_1", "Choose a categorical variable for Analysis", choices = categorical_columns())
  })
  
  # Get the variable for which analysis needs to be done 
  output$categorical_variable_select_2 <- renderUI({
    selectInput("categorical_variable_2", "Choose another categorical variable for comparison:", choices = categorical_columns())
  })
  
  # Create Event to print summary statistics of data
  observeEvent(input$print_summary,{
    summary_table <- t(sapply(data()[, numeric_columns()], function(col) {
      c(
        Mean = round(mean(col, na.rm = TRUE), 2),
        Median = round(median(col, na.rm = TRUE), 2),
        Min = round(min(col, na.rm = TRUE), 2),
        Max = round(max(col, na.rm = TRUE), 2),
        N = length(col)
      )
    }))
    
    colnames(summary_table) <- c("Mean", "Median", "Min", "Max", "N")
    
    # Create a data frame for summary table with row names
    summary_df <- data.frame(summary_table)
    #rownames(summary_df) <- c("Mean", "Median", "Min", "Max", "N")
    
    output$summary_2 <- renderDT({
      datatable(summary_df, options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE))
    })
  })
  
  # Create Event to print summary statistics of selected variable
  observeEvent(input$analyze_btn, {
    req(input$variable)
    
    calculate_summary_stats <- function(column) {
      mean_val <- round(mean(column, na.rm = TRUE), digits = 2)
      median_val <- median(column, na.rm = TRUE)
      quantile_25 <- quantile(column, probs = 0.25, na.rm = TRUE)
      quantile_75 <- quantile(column, probs = 0.75, na.rm = TRUE)
      lowest <- min(column, na.rm = TRUE)
      highest <- max(column, na.rm = TRUE)
      
      summary_stats <- data.frame(
        Mean = mean_val,
        Median = median_val,
        "First Quantile" = quantile_25,
        "Third Quantile" = quantile_75,
        Lowest = lowest,
        Highest = highest
      )
      
      return(summary_stats)
    }
    
    column_name <- data()[[input$variable]]
    summary_df <- calculate_summary_stats(column_name)
    rownames(summary_df) <- c("Descriptive Statistics")
    
    output$summary <- renderDT({
      datatable(summary_df, options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE)) 
    })
    
    # Plot histogram and boxplot simultaneously
    output$histogram <- renderPlot({
      par(mfrow = c(1, 2))  
      hist(data()[[input$variable]], col = "lightblue", main = "Histogram", xlab = "Values", ylab = "Frequency")
      boxplot(data()[[input$variable]], col = "lightblue", main = "Boxplot", xlab = "Columns", ylab = "Values")
      par(mfrow = c(1, 1))
    })
    
    # Perform Shapiro-Wilk test for normality
    column_data <- data()[[input$variable]]
    shapiro_test_result <- shapiro.test(column_data[1:min(length(column_data), 5000)])  # Limit to the first 5000 samples
    shapiro_res <- data.frame(
      "Statistic Value" = round(shapiro_test_result$statistic,2),
      "P value" = shapiro_test_result$p.value
    )
    rownames(shapiro_res) <- "Test Result"
    output$shapiro_test_result <- renderDT({
      datatable(shapiro_res, options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE))
    })
  })
  
  # Create Event to print  statistics of selected categorical variable
  observeEvent(input$analyze_categorical_btn, {
    req(input$categorical_variable)
    
    categorical_data <- table(data()[[input$categorical_variable]])
    categorical_data_sf <- data.frame(Category = names(categorical_data), Frequency = as.numeric(categorical_data))
    categorical_data_sf <- arrange(categorical_data_sf, desc(Frequency))
    categorical_data_percentage <- prop.table(categorical_data) * 100 
    
    output$barplot_categorical <- renderPlot({
      barplot(categorical_data, col = "lightblue",xlab = input$categorical_variable, ylab = "Frequency")
    })
    
    output$table_categorical <- renderDT({
      datatable(categorical_data_sf, options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE))
    })
    
  })
  
  # Create Event to print Chi-square test result
  observeEvent(input$chi_square_btn, {
    req(input$categorical_variable_1, input$categorical_variable_2)
    
    contingency_table <- table(data()[[input$categorical_variable_1]], data()[[input$categorical_variable_2]])
    chi_square_result <- chisq.test(contingency_table)
    
    summary_res <- data.frame(
      "Chi-Square Statistic" = round(chi_square_result$statistic,2),
      "Degrees of Freedom" = chi_square_result$parameter,
      "P-value" = chi_square_result$p.value
    )
    
    rownames(summary_res) <- "Test Result"
    
    output$chi_square_result <- renderDT(summary_res ,options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE)) 
    
    # Stacked bar chart for two categorical variables
    output$stacked_bar <- renderPlot({
      barplot(contingency_table, beside = TRUE, col = rainbow(nrow(contingency_table)), legend.text = rownames(contingency_table),
              args.legend = list(x = "topright", bty = "n", cex = 0.8))
    })
  })
  
  
  # Create Event to print t-test result
  observeEvent(input$t_test_btn,{
    req(input$variable_1,input$test_value)
    t_test_result <- t.test(data()[[input$variable_1]], mu = input$test_value)
    summary_res=data.frame(
      "Statistics Value" = round(t_test_result$statistic,2),
      "P-value" = t_test_result$p.value
      #,"Confidence Interval" = paste(t_test_result$conf.int[1],t_test_result$conf.int[2])
    )
    rownames(summary_res)=paste("Test Result")
    output$t_test_result <- renderDT({ datatable (summary_res ,options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE))
    })
  })
  
  # Create Event to print t-test result
  
  observeEvent(input$t_test_two_btn,{
    req(input$variable_1, input$variable_two)
    t_test_two_result <- t.test(data()[[input$variable_1]], data()[[input$variable_two]])
    summary_res=data.frame(
      "Statistics Value" = round(t_test_two_result$statistic,2),
      "P-value" = t_test_two_result$p.value
    )
    rownames(summary_res)=paste("Test Result")
    output$t_test_two_result <- renderDT({ datatable (summary_res ,options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE))
    })
    
    output$scatter_plot <- renderPlot({
      # Create a scatter plot between the two selected variables
      plot(data()[[input$variable_1]], data()[[input$variable_two]],
           xlab = input$variable_1, ylab = input$variable_two,
           main = "Scatter Plot")  # Add x-label and y-label here
      
      # Fit a linear regression line
      lm_model <- lm(data()[[input$variable_two]] ~ data()[[input$variable_1]])
      abline(lm_model, col = "red")
      
      # Calculate correlation coefficient
      correlation <- cor(data()[[input$variable_1]], data()[[input$variable_two]])
      
      # Print correlation coefficient inside the plot
      text(x = min(data()[[input$variable_1]]), 
           y = min(data()[[input$variable_two]]), 
           labels = paste("Correlation:", round(correlation, 2)), 
           pos = 4,
           cex = 1,  # Adjust text size
           font = 1)
    })
    
    output$correlation_output <- renderPrint({
      correlation <- cor(data()[[input$variable_1]], data()[[input$variable_two]] ,use = "complete.obs")
      paste("Correlation:", round(correlation, 2))
    })
    
    
  })
  

  # Create Event to print Chi-Square result
  observeEvent(input$chi_square_btn, {
    req(input$categorical_variable_1, input$categorical_variable_2)
    
    contingency_table <- table(data()[[input$categorical_variable_1]], data()[[input$categorical_variable_2]])
    chi_square_result <- chisq.test(contingency_table)
    
    summary_res_cat_2 <- data.frame(
      "Chi-Square Statistic" = round(chi_square_result$statistic,2),
      "Degrees of Freedom" = chi_square_result$parameter,
      "P-value" = chi_square_result$p.value
    )
    
    rownames(summary_res_cat_2) <- "Test Result"
    
    output$chi_square_result <- renderDT(summary_res_cat_2 ,options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE)) 
  })
  
  # Create Event to print Chi-Square result
  observeEvent(input$categorical_btn,{
    req(input$categorical_variable_1)
    contingency_table <- table(data()[[input$categorical_variable_1]])
    chi_square_result <- chisq.test(contingency_table)
    
    summary_res_cat <- data.frame(
      "Chi-Square Statistic" = round(chi_square_result$statistic,2),
      "Degrees of Freedom" = chi_square_result$parameter,
      "P-value" = chi_square_result$p.value
    )
    rownames(summary_res_cat) <- "Test Result"
    output$homogeneity_result <- renderDT(summary_res_cat ,options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE)) 
  })
  
}
# Run the application
shinyApp(ui = ui, server = server)
