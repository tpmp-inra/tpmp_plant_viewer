#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggExtra)
library(ggcorrplot)
library(tidyverse)
library(gridExtra)
library(reshape2)
library(data.table)
library(RColorBrewer)
library(shinyWidgets)
library(gtools)

source('../shinyCommon/R/shiny_common_all.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  options(shiny.maxRequestSize=30*1024^2) # Still not sure it's a good idea
   
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    load_experience_csv(input)
  })
  
  # Populate plants selector
  output$cbPlantSelection <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    fill_plant_selection(df, preseselct_all=F)
  })
  
  #The following set of functions populate the x axis selectors
  output$xAxisCombo <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    new_df <- df[sapply(df,is.numeric)]
    dsnames <- names(df)
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    selectInput("xAxisCombo", "X Axis:", choices = cb_options, selected = "day_after_start")
  })
  
  #The following set of functions populate the x axis selectors
  output$yAxisCombo <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    new_df <- df[sapply(df,is.numeric)]
    dsnames <- names(new_df)
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    
    pickerInput(
      inputId = "yAxisCombo", 
      label = "Y axis (multiple allowed):", 
      choices = cb_options,
      options = list(
        `selected-text-format` = "count > 5",
        `count-selected-text` = "{0} attributes selelcted",
        `actions-box` = TRUE,
        `deselect-all-text` = "Select none",
        `select-all-text` = "Select all"
      ), 
      selected = "area",
      multiple = TRUE
    )
  })
  
  output$cbNormalizationMethod <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    fill_normalization_cb()
  })
  
  output$chkShowOutliers <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if ("outlier" %in% colnames(df)) {
      checkboxInput("chkShowOutliers", paste('Show outliers (', length(which(df$outlier==1)), ')', sep=''), TRUE)
    } else {
      checkboxInput("chkShowOutliers", 'No outliers detected, option ignored', FALSE)
    }
  })
  
  output$smoothingModel <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    selectInput("smoothingModel", 
                "Smoothing model (regression):", 
                c("None" = "none",
                  "Linear Regression" = "lm", 
                  "Generalized Linear Regression" = "glm", 
                  "Generalized Additive Model" = "gam", 
                  "Local Regression" = "loess", 
                  "Robust Linear Models" = "rlm",
                  "Auto" = "auto"
                ))
    
  })
  
  output$timeSliceSelector <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    tags$hr()
    min_time <- trunc(min(df$trunc_day_after_start))
    max_time <- trunc(max(df$trunc_day_after_start))
    sliderInput(inputId =  "timeSliceSelector", 
                label = "Time slice: ", 
                min = min_time, 
                max = max_time, 
                value = c(min_time, max_time),
                step = 1)
  })
  
  # Here it renders
  output$plant_plots = renderPlot({
    req(input$xAxisCombo, input$yAxisCombo)
    
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    xv <- input$xAxisCombo
    if (is.null(xv)) return(NULL)
    yv <- input$yAxisCombo
    if (is.null(yv)) return(NULL)
    
    if (sum(xv %in% names(df))>0) { # supress error when changing files
      
      # Filter by selected plants
      plants_to_plot <- df %>% filter(plant %in% input$cbPlantSelection)
      
      if (!input$chkShowOutliers & ("outlier" %in% colnames(df))) {
        plants_to_plot <- plants_to_plot %>% filter(outlier == 0)
      }
      
      # Normalize
      if (input$cbNormalizationMethod == "normalization") {
        normalize <- function(x) {
          return((x-min(x)) / (max(x)-min(x)))
        }
        plants_to_plot <- plants_to_plot %>% mutate_at(vars(yv), funs(normalize(.) %>% as.vector))
      } else {
        if (input$cbNormalizationMethod == "scale") {
          plants_to_plot <- plants_to_plot %>% mutate_at(vars(yv), funs(scale(.) %>% as.vector))
        }
      }
      
      # Filter by time
      if (input$xAxisCombo == "day_after_start") {
        plants_to_plot <- filter(plants_to_plot, 
                                 trunc_day_after_start >= input$timeSliceSelector[1] & 
                                   trunc_day_after_start <= input$timeSliceSelector[2])
      }
      
      if ("treatment" %in% colnames(df)) {
        plants_to_plot <- mutate(plants_to_plot, plant= sprintf("%s - %s", plant, treatment))
      } else {
        if ("genotype" %in% colnames(df)) {
          plants_to_plot <- mutate(plants_to_plot, plant= sprintf("%s - %s", plant, genotype))
        }
      }
        
      if ("treatment" %in% colnames(df)) {
        mdf <- melt(plants_to_plot,id.vars=c(xv, "plant", "treatment"),measure.vars=yv)
      } else {
        mdf <- melt(plants_to_plot,id.vars=c(xv, "plant"),measure.vars=yv)
      }
      
      # Plot the dots
      gg <- ggplot(data=mdf, aes_string(x=xv,y="value",color="variable"))
      
      # Use different colour for each treatment
      if ("treatment" %in% colnames(df)) {
        gg <- gg + geom_rect(aes_string(linetype = "treatment"), 
                             colour = "black", 
                             fill = "white", 
                             size = 2,
                             xmin = -Inf,
                             xmax = Inf, 
                             ymin = -Inf,
                             ymax = Inf)
      }
      
      # Select display mode
      gg <- gg + geom_line(size = 2)
      
      # Smoothy
      if (input$smoothingModel != "none") {
        gg <- gg + geom_smooth(method = input$smoothingModel)
      }
      
      # # Split the plots
      gg <- gg + facet_wrap("plant")
      
      # gg <- gg + theme(legend.position="none")
      # 
      # gg <- gg + theme(legend.title = element_text(size=32, face = "bold"),
      #                  legend.text=element_text(size=30),
      #                  axis.text=element_text(size=20),
      #                  axis.title=element_text(size=22,face="bold"),
      #                  title = element_text(size=20))
      
      gg
    }  
  })
  
  #This previews the CSV data file
  output$filetable <- renderTable({
    filedata()
  })
  
})
