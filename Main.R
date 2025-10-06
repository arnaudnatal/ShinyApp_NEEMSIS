##########
# Shiny App RUME-NEEMSIS
# October 2025
# Arnaud NATAL
# arnaud.natal@ifpindia.org
##########


########## Initialisation
setwd("C:/Users/Arnaud/Documents/GitHub/ShinyApp_NEEMSIS")


########## Install
# install.packages("shiny")
# install.packages("DT")
# install.packages("ggplot2") 
# install.packages("readxl")
# install.packages("dplyr")

########## shinyapps.io

rsconnect::setAccountInfo(name='neemsis',
                          token='AC1A329E00B679CFD4DEE045F7F39D82',
                          secret='95GeXLOKKutUjIdkf2xLaJAsfNFAewsRBMUY7XIT')



########## Load
library(shiny)
library(DT)
library(ggplot2)
library(readxl)
library(dplyr)


#
rm(list = ls())


# app.R
library(shiny)
library(DT)
library(ggplot2)
library(readxl)
library(dplyr)



# ------------------- Load data -------------------
data <- read_excel("households.xlsx")
data <- data %>% mutate(across(where(is.character), as.factor))

factor_cols_all <- names(Filter(is.factor, data))





# ------------------- UI -------------------
ui <- fluidPage(
  titlePanel("NEEMSIS Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var1", "Variable to analyse:",
                  choices = names(data)),
      uiOutput("group1_ui"),
      uiOutput("group2_ui"),
      uiOutput("plot_options_ui")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Table", DTOutput("summaryTable")),
        tabPanel("Plot", plotOutput("plot", height="600px"))
      )
    )
  )
)

# ------------------- Server -------------------
server <- function(input, output, session) {
  
  # --- Group UI ---
  output$group1_ui <- renderUI({
    choices <- factor_cols_all
    if (!is.null(input$var1) && input$var1 %in% choices) choices <- setdiff(choices, input$var1)
    selectInput("group1", "Group by (optional)", choices=c("None", choices), selected="None")
  })
  
  output$group2_ui <- renderUI({
    choices <- factor_cols_all
    g1 <- input$group1
    if (!is.null(input$var1) && input$var1 %in% choices) choices <- setdiff(choices, input$var1)
    if (!is.null(g1) && g1 != "None") choices <- setdiff(choices, g1)
    selectInput("group2", "Group by (optional)", choices=c("None", choices), selected="None")
  })
  
  # --- Plot options for numeric var1 ---
  output$plot_options_ui <- renderUI({
    if (!is.null(input$var1) && is.numeric(data[[input$var1]])) {
      tagList(
        radioButtons("plot_type","Plot type:",choices=c("Boxplot"="box","Density curve"="density"),selected="box"),
        checkboxInput("remove_outliers","Remove extreme values (rescale y-axis)",value=FALSE)
      )
    } else NULL
  })
  
  # --- Summary Table ---
  output$summaryTable <- renderDT({
    var1 <- input$var1
    if (is.null(var1)) return(NULL)
    g1 <- if (!is.null(input$group1) && input$group1!="None") input$group1 else NULL
    g2 <- if (!is.null(input$group2) && input$group2!="None") input$group2 else NULL
    
    # --- Numeric variable ---
    if (is.numeric(data[[var1]])) {
      if (!is.null(g1) && !is.null(g2)) {
        df <- data %>%
          group_by(.data[[g1]], .data[[g2]]) %>%
          summarise(
            Mean = mean(.data[[var1]], na.rm=TRUE),
            Median = median(.data[[var1]], na.rm=TRUE),
            SD = sd(.data[[var1]], na.rm=TRUE),
            .groups="drop"
          )
      } else if (!is.null(g1)) {
        df <- data %>%
          group_by(.data[[g1]]) %>%
          summarise(
            Mean = mean(.data[[var1]], na.rm=TRUE),
            Median = median(.data[[var1]], na.rm=TRUE),
            SD = sd(.data[[var1]], na.rm=TRUE),
            .groups="drop"
          )
      } else {
        df <- data.frame(
          Mean = mean(data[[var1]], na.rm=TRUE),
          Median = median(data[[var1]], na.rm=TRUE),
          SD = sd(data[[var1]], na.rm=TRUE)
        )
      }
      datatable(df, options=list(pageLength=10))
      
      # --- Qualitative variable ---
    } else {
      df <- NULL
      if (!is.null(g1) && !is.null(g2)) {
        df <- data %>%
          group_by(.data[[var1]], .data[[g1]], .data[[g2]]) %>%
          summarise(Count=n(), .groups="drop") %>%
          group_by(.data[[g1]], .data[[g2]]) %>%
          mutate(Percentage=round(Count/sum(Count)*100,1)) %>% ungroup()
      } else if (!is.null(g1)) {
        df <- data %>%
          group_by(.data[[var1]], .data[[g1]]) %>%
          summarise(Count=n(), .groups="drop") %>%
          group_by(.data[[g1]]) %>%
          mutate(Percentage=round(Count/sum(Count)*100,1)) %>% ungroup()
      } else {
        df <- data %>%
          group_by(.data[[var1]]) %>%
          summarise(Count=n(), .groups="drop") %>%
          mutate(Percentage=round(Count/sum(Count)*100,1))
      }
      datatable(df, options=list(pageLength=10))
    }
  })
  
  # --- Plot ---
  output$plot <- renderPlot({
    var1 <- input$var1
    if (is.null(var1)) return(NULL)
    g1 <- if (!is.null(input$group1) && input$group1!="None") input$group1 else NULL
    g2 <- if (!is.null(input$group2) && input$group2!="None") input$group2 else NULL
    
    # --- Numeric var1 ---
    if (is.numeric(data[[var1]])) {
      ptype <- input$plot_type %||% "box"
      remove_out <- isTRUE(input$remove_outliers)
      
      if (ptype=="box") {
        if (!is.null(g1) && !is.null(g2)) {
          p <- ggplot(data, aes(x=.data[[g1]], y=.data[[var1]], fill=.data[[g2]])) +
            geom_boxplot(outlier.shape = if(remove_out) NA else 16)
        } else if (!is.null(g1)) {
          p <- ggplot(data, aes(x=.data[[g1]], y=.data[[var1]])) +
            geom_boxplot(fill="lightblue", outlier.shape = if(remove_out) NA else 16)
        } else {
          p <- ggplot(data, aes(x="", y=.data[[var1]])) +
            geom_boxplot(fill="lightblue", outlier.shape = if(remove_out) NA else 16) + xlab("")
        }
        
        if (remove_out) {
          grouping_vars <- c(g1, g2)
          grouping_vars <- grouping_vars[!sapply(grouping_vars, is.null)]
          if (length(grouping_vars)>0) {
            whisk <- data %>%
              group_by(across(all_of(grouping_vars))) %>%
              summarise(q1=quantile(.data[[var1]],0.25,na.rm=TRUE),
                        q3=quantile(.data[[var1]],0.75,na.rm=TRUE),
                        upper=q3+1.5*IQR(.data[[var1]],na.rm=TRUE),
                        lower=q1-1.5*IQR(.data[[var1]],na.rm=TRUE),
                        .groups="drop")
            p <- p + coord_cartesian(ylim=c(min(whisk$lower,na.rm=TRUE), max(whisk$upper,na.rm=TRUE)))
          } else {
            q1 <- quantile(data[[var1]],0.25,na.rm=TRUE)
            q3 <- quantile(data[[var1]],0.75,na.rm=TRUE)
            p <- p + coord_cartesian(ylim=c(q1-1.5*IQR(data[[var1]],na.rm=TRUE), q3+1.5*IQR(data[[var1]],na.rm=TRUE)))
          }
        }
        p + theme_minimal()
        
      } else { # density
        if (!is.null(g1) && !is.null(g2)) {
          ggplot(data, aes(x=.data[[var1]], fill=interaction(.data[[g1]],.data[[g2]]))) +
            geom_density(alpha=0.5) + labs(fill=paste(g1,"&",g2)) + theme_minimal()
        } else if (!is.null(g1)) {
          ggplot(data, aes(x=.data[[var1]], fill=.data[[g1]])) +
            geom_density(alpha=0.5) + theme_minimal()
        } else {
          ggplot(data, aes(x=.data[[var1]])) +
            geom_density(fill="lightblue", alpha=0.6) + theme_minimal()
        }
      }
      
      # --- Qualitative var1 ---
    } else {
      if (!is.null(g1) && !is.null(g2)) {
        df_plot <- data %>%
          group_by(.data[[var1]],.data[[g1]],.data[[g2]]) %>%
          summarise(Count=n(), .groups="drop") %>%
          group_by(.data[[g1]],.data[[g2]]) %>%
          mutate(Percentage=round(Count/sum(Count)*100,1)) %>% ungroup()
        ggplot(df_plot, aes(x=.data[[var1]], y=Count, fill=interaction(.data[[g1]],.data[[g2]]))) +
          geom_bar(stat="identity", position="dodge") +
          geom_text(aes(label=paste0(Percentage,"%")), position=position_dodge(width=0.9), vjust=-0.5, size=3) +
          labs(fill=paste(g1,"&",g2)) + theme_minimal()
      } else if (!is.null(g1)) {
        df_plot <- data %>%
          group_by(.data[[var1]],.data[[g1]]) %>%
          summarise(Count=n(), .groups="drop") %>%
          group_by(.data[[g1]]) %>%
          mutate(Percentage=round(Count/sum(Count)*100,1)) %>% ungroup()
        ggplot(df_plot, aes(x=.data[[var1]], y=Count, fill=.data[[g1]])) +
          geom_bar(stat="identity", position="dodge") +
          geom_text(aes(label=paste0(Percentage,"%")), position=position_dodge(width=0.9), vjust=-0.5, size=3) +
          theme_minimal()
      } else {
        df_plot <- data %>%
          group_by(.data[[var1]]) %>%
          summarise(Count=n(), .groups="drop") %>%
          mutate(Percentage=round(Count/sum(Count)*100,1))
        ggplot(df_plot, aes(x=.data[[var1]], y=Count)) +
          geom_bar(stat="identity", fill="lightgreen") +
          geom_text(aes(label=paste0(Percentage,"%")), vjust=-0.5, size=3) +
          theme_minimal()
      }
    }
  }, res=96)
  
}

# ------------------- Run App -------------------
shinyApp(ui, server)
