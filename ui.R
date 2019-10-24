#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggExtra)
library(ggcorrplot)
library(dplyr)
library(gridExtra)
library(reshape2)
library(data.table)
library(RColorBrewer)
library(shinyWidgets)

source('../shinyCommon/R/shiny_common_all.R')


# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Plant viewer v2"),
  
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    fileInput('datafile', 
              'Choose CSV file',
              accept=c("text/csv", 
                       "text/comma-separated-values,text/plain",
                       ".csv")),
    
    uiOutput("cbPlantSelection"),
    
    uiOutput("chkShowOutliers"),
    
    fluidRow(
      column(6, uiOutput("xAxisCombo")),
      column(6, uiOutput("yAxisCombo"))
    ),
    
    uiOutput("cbNormalizationMethod"),
    
    uiOutput("smoothingModel"),
    
    uiOutput("timeSliceSelector"),
    
    tags$head(tags$style("#plant_plots{height:80vh !important;}"))
  ),
    
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Plots", 
               plotOutput(outputId = "plant_plots",
                          inline = FALSE, 
                          height = "800")),
      tabPanel("CSV File", tableOutput("filetable"))
    )
  )
))
