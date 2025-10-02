##########
# Shiny App RUME-NEEMSIS
# October 2025
# Arnaud NATAL
# arnaud.natal@ifpindia.org
##########


########## Initialisation
setwd("C:/Users/Arnaud/Documents/GitHub/Shiny_NEEMSIS")


########## Install
install.packages("shiny")
install.packages("DT")
install.packages("ggplot2") 
install.packages("readxl")
install.packages("dplyr")



########## Load
library(shiny)
library(DT)
library(ggplot2)
library(readxl)
library(dplyr)



#
rm(list = ls())


# Load your data (replace with your actual file path)
data <- read_excel("households.xlsx")

# Define UI
ui <- fluidPage(
  titlePanel("Household Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var1", "Choose a variable:", choices = names(data)),
      
      uiOutput("group1_ui"),   # first grouping variable
      uiOutput("group2_ui"),   # second grouping variable
      uiOutput("stats_ui"),    # stats selection for numeric
      uiOutput("boxplot_ui"),  # outlier toggle
      uiOutput("cat2_ui")      # second categorical variable for categorical case
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Table", DTOutput("summaryTable")),
        tabPanel("Plot", plotOutput("plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # UI: First grouping variable (for numeric var)
  output$group1_ui <- renderUI({
    if(!is.null(input$var1) && is.numeric(data[[input$var1]])) {
      selectInput("group1", "Group by 1 (optional):", 
                  choices = c("None", names(Filter(function(x) is.factor(x) || is.character(x), data))))
    }
  })
  
  # UI: Second grouping variable (for numeric var)
  output$group2_ui <- renderUI({
    if(!is.null(input$var1) && is.numeric(data[[input$var1]])) {
      selectInput("group2", "Group by 2 (optional):", 
                  choices = c("None", names(Filter(function(x) is.factor(x) || is.character(x), data))))
    }
  })
  
  # UI: Show statistics options for numeric variables
  output$stats_ui <- renderUI({
    if(!is.null(input$var1) && is.numeric(data[[input$var1]])) {
      checkboxGroupInput("stats", "Choose statistics:", 
                         choices = c("Mean" = "mean", "Median" = "median", "Std. Dev." = "sd"),
                         selected = c("mean", "median", "sd"))
    }
  })
  
  # UI: Option to hide extreme values in boxplot
  output$boxplot_ui <- renderUI({
    if(!is.null(input$var1) && is.numeric(data[[input$var1]]) && 
       (!is.null(input$group1) && input$group1 != "None")) {
      checkboxInput("remove_outliers", "Remove extreme values from boxplot", value = FALSE)
    }
  })
  
  # UI: Second categorical variable for cross-tab (if var1 is categorical)
  output$cat2_ui <- renderUI({
    if(!is.null(input$var1) && !is.numeric(data[[input$var1]])) {
      selectInput("cat2", "Cross with (optional):", 
                  choices = c("None", names(Filter(function(x) is.factor(x) || is.character(x), data))))
    }
  })
  
  # Summary table
  output$summaryTable <- renderDT({
    var <- input$var1
    if(is.null(var)) return(NULL)
    
    if(is.numeric(data[[var]])) {
      stats_selected <- input$stats
      if(length(stats_selected) == 0) return(NULL)
      
      g1 <- if(!is.null(input$group1) && input$group1 != "None") input$group1 else NULL
      g2 <- if(!is.null(input$group2) && input$group2 != "None") input$group2 else NULL
      
      if(!is.null(g1) && !is.null(g2)) {
        # Two grouping variables
        df <- data %>%
          group_by(.data[[g1]], .data[[g2]]) %>%
          summarise(
            Mean = if("mean" %in% stats_selected) mean(.data[[var]], na.rm=TRUE) else NA,
            Median = if("median" %in% stats_selected) median(.data[[var]], na.rm=TRUE) else NA,
            SD = if("sd" %in% stats_selected) sd(.data[[var]], na.rm=TRUE) else NA,
            .groups = "drop"
          )
      } else if(!is.null(g1)) {
        # One grouping variable
        df <- data %>%
          group_by(.data[[g1]]) %>%
          summarise(
            Mean = if("mean" %in% stats_selected) mean(.data[[var]], na.rm=TRUE) else NA,
            Median = if("median" %in% stats_selected) median(.data[[var]], na.rm=TRUE) else NA,
            SD = if("sd" %in% stats_selected) sd(.data[[var]], na.rm=TRUE) else NA,
            .groups = "drop"
          )
      } else {
        # No grouping
        df <- data.frame(
          Mean = if("mean" %in% stats_selected) mean(data[[var]], na.rm=TRUE) else NA,
          Median = if("median" %in% stats_selected) median(data[[var]], na.rm=TRUE) else NA,
          SD = if("sd" %in% stats_selected) sd(data[[var]], na.rm=TRUE) else NA
        )
      }
      datatable(df, options = list(pageLength = 10))
      
    } else {
      # Categorical summary
      if(!is.null(input$cat2) && input$cat2 != "None") {
        df <- as.data.frame(table(data[[var]], data[[input$cat2]]))
        colnames(df) <- c("Category1", "Category2", "Count")
      } else {
        df <- as.data.frame(table(data[[var]]))
        colnames(df) <- c("Category", "Count")
      }
      datatable(df, options = list(pageLength = 10))
    }
  })
  
  # Plot
  output$plot <- renderPlot({
    var <- input$var1
    if(is.null(var)) return(NULL)
    
    if(is.numeric(data[[var]])) {
      g1 <- if(!is.null(input$group1) && input$group1 != "None") input$group1 else NULL
      g2 <- if(!is.null(input$group2) && input$group2 != "None") input$group2 else NULL
      
      if(!is.null(g1) && !is.null(g2)) {
        ggplot(data, aes(x = .data[[g1]], y = .data[[var]], fill = .data[[g2]])) +
          geom_boxplot(outlier.shape = if(!is.null(input$remove_outliers) && input$remove_outliers) NA else 16) +
          theme_minimal()
      } else if(!is.null(g1)) {
        ggplot(data, aes(x = .data[[g1]], y = .data[[var]])) +
          geom_boxplot(fill="lightblue", outlier.shape = if(!is.null(input$remove_outliers) && input$remove_outliers) NA else 16) +
          theme_minimal()
      } else {
        ggplot(data, aes(x = .data[[var]])) +
          geom_histogram(bins=30, fill="lightblue", color="black") +
          theme_minimal()
      }
    } else {
      if(!is.null(input$cat2) && input$cat2 != "None") {
        ggplot(data, aes(x = .data[[var]], fill = .data[[input$cat2]])) +
          geom_bar(position="dodge") +
          theme_minimal()
      } else {
        ggplot(data, aes(x = .data[[var]])) +
          geom_bar(fill="lightgreen") +
          theme_minimal()
      }
    }
  })
}

# Run the app
shinyApp(ui, server)