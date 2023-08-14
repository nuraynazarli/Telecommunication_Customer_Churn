library(tidyverse)
library(inspectdf)
library(timetk)
library(lubridate)
library(highcharter)
library(h2o)
library(shiny)
library(DT)
library(plotly)
library(modeltime)
library(caTools)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)   
library(car)
library(caret)
library(glue)
library(scorecard)
library(corrmorant)
library(correlationfunnel)
library(explore)
library(knitr)

df <- read.csv('telco-customer-churn.csv')

na_counts <- colSums(is.na(df))
columns_with_na <- names(na_counts[na_counts > 0])
total_nas <- sum(is.na(df))
unique_counts <- sapply(df, function(x) length(unique(x)))

data <- na.omit(df)
data$Churn <- data$Churn %>% 
  recode("'Yes' = 1; 'No' = 0 ") %>% 
  as.factor()
data$SeniorCitizen <- data$SeniorCitizen %>% as.character()

target <- "Churn"
features <- data %>% select(gender,SeniorCitizen,Partner,Dependents,PhoneService,PaperlessBilling,Churn) %>% names()
exclude <- c("customerID")

columns_to_exclude <- c("customerID")
data_column_names <- setdiff(colnames(data), columns_to_exclude)

column_types <- sapply(data, class)

numeric_columns <- data %>%
  select_if(is.numeric) %>%
  names()

categorical_columns <- data %>%
  select_if(is.character) %>%
  names()

prepare_data <- function(data) {
  return(data)
}

h2o.init()

ui <- dashboardPage(
  dashboardHeader(
    title = "Telecommunication",
    titleWidth = 250
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      # https://fontawesome.com/icons/
      menuItem("Database", tabName = "database", icon = icon("database", lib = "font-awesome")),
      menuItem("Data Info", tabName = "analysis", icon = icon("question", lib = "font-awesome"), selected = TRUE),
      menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("chart-line", lib = "font-awesome"), selected = TRUE),
      menuItem("Logistic Regression",tabName = "lr",icon = icon("chart-bar", lib = "font-awesome"),
               menuSubItem("Bins",tabName = "bins"),
               menuSubItem("ROC Curve",tabName = "rc"),
               menuSubItem("Evolution Matrix",tabName = "cm")),
      menuItem("AutoML",tabName = "ml",icon = icon("pie-chart", lib = "font-awesome"),
               menuSubItem("AUC and Gini",tabName = "ag"),
               menuSubItem("Evolution Matrix",tabName = "cm1"),
               menuSubItem("Prediction",tabName = "pred")),
      menuItem("Prediction", tabName = "prediction", icon = icon("magic", lib = "font-awesome"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    # HTML codes ----
    tags$style(HTML("
    .custom-box-green { background-color: #00C957; }
    .custom-box-blue { background-color: #007bff; }
    .custom-box-purple { background-color: #6f42c1; }
    .custom-box-red {background-color: #dc3545; }
    .custom-box-gold {background-color: #ffc107; }
    .custom-box-cyan {background-color: #17a2b8; }
    .custom-box-indigo {background-color: #6610f2; }
    .custom-box-magenta {background-color: #e83e8c; }
    .custom-box-orange {background-color: #fd7e14; }
    .custom-box-teal {background-color: #20c997; }
    .custom-box-gray {background-color: #6c757d; }
    .custom-box-mo {background-color: #ba55d3; }
    .custom-box-sb {background-color: #4682b4; }
    .custom-box-do {background-color: #9932cc; }
    .custom-box-o {background-color: #ffa500; }
    .custom-box-dt {background-color: #00ced1; }
    .custom-box-bv {background-color: #8a2be2; }
    .custom-box-orchid {background-color: #da70d6; }
    .custom-box-darko {background-color: #ff8c00; }
    .custom-box-teal1 {background-color: #008080; }
    .custom-box-dp {background-color: #ff1493; }
    .custom-box-yo {background-color: #F5A623; }
    .custom-box-skyblue {background-color: #00AEEF; }
    .custom-box-brightyellow {background-color: #FFC300; }
    .custom-box-green { height: 100px !important; }
    .custom-box-blue { height: 100px !important; }
    .custom-box-purple { height: 100px !important; }
    .custom-box-red {height: 100px !important; }
    .custom-box-gold {height: 100px !important; }
    .custom-box-cyan {height: 100px !important; }
    .custom-box-indigo {height: 100px !important; }
    .custom-box-magenta {height: 100px !important; }
    .custom-box-orange {height: 100px !important; }
    .custom-box-teal {height: 100px !important; }
    .custom-box-gray {height: 100px !important; }
    .custom-box-mo {height: 100px !important; }
    .custom-box-sb {height: 100px !important; }
    .custom-box-do {height: 100px !important; }
    .custom-box-o {height: 100px !important; }
    .custom-box-dt {height: 100px !important; }
    .custom-box-bv {height: 100px !important; }
    .custom-box-orchid {height: 100px !important; }
    .custom-box-darko {height: 100px !important; }
    .custom-box-teal1 {height: 100px !important; }
    .custom-box-dp {height: 100px !important; }
    .custom-box-yo {height: 100px !important; }
    .custom-box-skyblue {height: 100px !important; }
    .custom-box-brightyellow {height: 100px !important; }
    .slider-input {
        padding: 20px;
        background-color: #56caf0;
        border: 1px solid #ddd;
        border-radius: 5px;
      }
    .input-spacing {
        margin-bottom: 10px;
      }
  ")),  
    # end of HTML codes ----
    
    tabItems(   
      tabItem(tabName = "database",
              h2("Database"),
              DTOutput("data_table")
      ),
      
      tabItem(tabName = "analysis",      # Anaylsis data ----
              h2("Analysis of Data"),
              
              fluidRow(
                column(width = 4,
                       box(id = "box1",
                           title = tags$b("Number of NAs"), width = NULL, solidHeader = T,
                           h4(paste("Number of missing values in each column:", paste(na_counts, collapse = ", "))),
                           class = "custom-box-green",
                           style = "color: white;"
                       )
                ),
                column(width = 4,
                       box(id = "box2",
                           title = tags$b("Column with NAs"), width = NULL, solidHeader = T,
                           h4(paste("The name of column with missing values:", paste(columns_with_na, collapse = ", "))),
                           class = "custom-box-blue",
                           style = "color: white;"
                       )
                ),
                column(width = 4,
                       box(id = "box3",
                           title = tags$b("Total NAs"), width = NULL, solidHeader = T,
                           h4(paste("Total number of missing values:", total_nas)),
                           class = "custom-box-purple",
                           style = "color: white;"
                       )
                )
              ),
              
              fluidRow(
                column(width = 4,
                       box(
                         id = "box4",
                         title = tags$b("customerID"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["customerID"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "customerID"),
                             NULL,
                             choices = unique(df[["customerID"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-red",
                         style = "color: white;"
                       )
                ),
                column(width = 4,
                       box(
                         id = "box5",
                         title = tags$b("gender"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["gender"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "gender"),
                             NULL,
                             choices = unique(df[["gender"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-gold",
                         style = "color: white;"
                       )
                )
                ,
                column(width = 4,
                       box(
                         id = "box6",
                         title = tags$b("SeniorCitizen"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["SeniorCitizen"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "SeniorCitizen"),
                             NULL,
                             choices = unique(df[["SeniorCitizen"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-cyan",
                         style = "color: white;"
                       )
                )
              ),
              fluidRow(
                column(width = 4,
                       box(
                         id = "box7",
                         title = tags$b("Partner"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["Partner"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "Partner"),
                             NULL,
                             choices = unique(df[["Partner"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-indigo",
                         style = "color: white;"
                       )
                ),
                column(width = 4,
                       box(
                         id = "box8",
                         title = tags$b("Dependents"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["Dependents"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "Dependents"),
                             NULL,
                             choices = unique(df[["Dependents"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-magenta",
                         style = "color: white;"
                       )
                )
                ,
                column(width = 4,
                       box(
                         id = "box9",
                         title = tags$b("tenure"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["tenure"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "tenure"),
                             NULL,
                             choices = unique(df[["tenure"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-orange",
                         style = "color: white;"
                       )
                )
              ),
              fluidRow(
                column(width = 4,
                       box(
                         id = "box10",
                         title = tags$b("PhoneService"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["PhoneService"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "PhoneService"),
                             NULL,
                             choices = unique(df[["PhoneService"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-teal",
                         style = "color: white;"
                       )
                ),
                column(width = 4,
                       box(
                         id = "box11",
                         title = tags$b("MultipleLines"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["MultipleLines"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "MultipleLines"),
                             NULL,
                             choices = unique(df[["MultipleLines"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-gray",
                         style = "color: white;"
                       )
                )
                ,
                column(width = 4,
                       box(
                         id = "box12",
                         title = tags$b("InternetService"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["InternetService"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "InternetService"),
                             NULL,
                             choices = unique(df[["InternetService"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-mo",
                         style = "color: white;"
                       )
                )
              ),
              fluidRow(
                column(width = 4,
                       box(
                         id = "box13",
                         title = tags$b("OnlineSecurity"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["OnlineSecurity"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "OnlineSecurity"),
                             NULL,
                             choices = unique(df[["OnlineSecurity"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-sb",
                         style = "color: white;"
                       )
                ),
                column(width = 4,
                       box(
                         id = "box14",
                         title = tags$b("OnlineBackup"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["OnlineBackup"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "OnlineBackup"),
                             NULL,
                             choices = unique(df[["OnlineBackup"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-do",
                         style = "color: white;"
                       )
                )
                ,
                column(width = 4,
                       box(
                         id = "box15",
                         title = tags$b("DeviceProtection"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["DeviceProtection"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "DeviceProtection"),
                             NULL,
                             choices = unique(df[["DeviceProtection"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-o",
                         style = "color: white;"
                       )
                )
              ),
              fluidRow(
                column(width = 4,
                       box(
                         id = "box16",
                         title = tags$b("TechSupport"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["TechSupport"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "TechSupport"),
                             NULL,
                             choices = unique(df[["TechSupport"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-dt",
                         style = "color: white;"
                       )
                ),
                column(width = 4,
                       box(
                         id = "box17",
                         title = tags$b("StreamingTV"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["StreamingTV"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "StreamingTV"),
                             NULL,
                             choices = unique(df[["StreamingTV"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-bv",
                         style = "color: white;"
                       )
                )
                ,
                column(width = 4,
                       box(
                         id = "box18",
                         title = tags$b("StreamingMovies"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["StreamingMovies"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "StreamingMovies"),
                             NULL,
                             choices = unique(df[["StreamingMovies"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-orchid",
                         style = "color: white;"
                       )
                )
              ),
              fluidRow(
                column(width = 4,
                       box(
                         id = "box19",
                         title = tags$b("Contract"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["Contract"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "Contract"),
                             NULL,
                             choices = unique(df[["Contract"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-darko",
                         style = "color: white;"
                       )
                ),
                column(width = 4,
                       box(
                         id = "box20",
                         title = tags$b("PaperlessBilling"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["PaperlessBilling"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "PaperlessBilling"),
                             NULL,
                             choices = unique(df[["PaperlessBilling"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-teal1",
                         style = "color: white;"
                       )
                )
                ,
                column(width = 4,
                       box(
                         id = "box21",
                         title = tags$b("PaymentMethod"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["PaymentMethod"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "PaymentMethod"),
                             NULL,
                             choices = unique(df[["PaymentMethod"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-dp",
                         style = "color: white;"
                       )
                )
              ),
              fluidRow(
                column(width = 4,
                       box(
                         id = "box22",
                         title = tags$b("MonthlyCharges"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["MonthlyCharges"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "MonthlyCharges"),
                             NULL,
                             choices = unique(df[["MonthlyCharges"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-yo",
                         style = "color: white;"
                       )
                ),
                column(width = 4,
                       box(
                         id = "box23",
                         title = tags$b("TotalCharges"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["TotalCharges"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "TotalCharges"),
                             NULL,
                             choices = unique(df[["TotalCharges"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-skyblue",
                         style = "color: white;"
                       )
                )
                ,
                column(width = 4,
                       box(
                         id = "box24",
                         title = tags$b("Churn"), width = NULL, solidHeader = T,
                         # Display column name
                         h5(paste("Number of Unique Values:", length(unique(df[["Churn"]])))),  # Number of unique values
                         tags$div(
                           h5("Unique Values:"),  # Unique values header
                           pickerInput(
                             paste0("unique_values_", "Churn"),
                             NULL,
                             choices = unique(df[["Churn"]]),  # Display unique values
                             options = list(
                               'actions-box' = TRUE,
                               dropdownArrow = TRUE,  # Show dropdown arrow
                               width = "200px"  # Make the dropdown occupy full width of the box
                             ),
                             multiple = TRUE
                           ),
                           style = "display: flex; flex-direction: right; align-items: flex-start; "
                         ),
                         class = "custom-box-brightyellow",
                         style = "color: white;"
                       )
                )
              )
      ),
      # end of rows ----
      tabItem(tabName = "eda",
              h2("Exploratory Data Analysis"),
              fluidRow(
                tabBox(
                  tabPanel("Inspect Missing Values", plotOutput("inspect_na_plot"))
                ),
                tabBox(
                  tabPanel("Correlation Matrix", plotOutput("correlation_plot"))
                )
              ),
              fluidRow(
                tabBox(
                  tabPanel("Correlation Plot", plotOutput("inspect_cor_plot"))
                ),
                tabBox(
                  tabPanel("Explore", plotOutput("explore_plot"))
                )
              )),
      
      tabItem(tabName = "bins",
              h2("Logistic Regression"),
              h3("Plots of Bins"),
              fluidRow(
                tabBox(
                  tabPanel("Bins Chart-Tenure", plotOutput("bins_tenure_chart"))
                ),
                tabBox(
                  tabPanel("Bins Chart-InternetService", plotOutput("bins_InternetService_chart"))
                )
              ),
              fluidRow(
                tabBox(
                  tabPanel("Bins Chart-Contract", plotOutput("bins_Contract_chart"))
                ),
                tabBox(
                  tabPanel("Bins Chart-PaymentMethod", plotOutput("bins_PaymentMethod_chart"))
                )
              )),
      
      tabItem(tabName = "rc",
              h2("Logistic Regression"),
              h3("Pie-Chart of Variable Importance and ROC Curve"),
              fluidRow(
                tabBox(
                  tabPanel("Pie-Chart of Variable Importance", highchartOutput("var_importance_chart"))
                ),
                tabBox(
                  tabPanel("ROC Curver", highchartOutput("roc_curve"))
                )
              )),
      
      tabItem(tabName = "cm",
              h2("Logistic Regression"),
              h3("Confusion Matrix"),
              fluidRow(
                valueBoxOutput("precision"),
                valueBoxOutput("recall_sensitivity"),
                valueBoxOutput("specificity"),
                valueBoxOutput("accuracy"),
                valueBoxOutput("f1_score"),
                valueBoxOutput("balanced_accuracy")
              ),
              fluidRow(
                mainPanel(
                  DTOutput("confusion_matrix_table")
                )
                
              )),
      
      tabItem(tabName = "ag",
              h2("Auto ML"),
              h3("AUC and Gini, Pie-Chart of Variable Importance"),
              fluidRow(
                box(
                  title = "Model AUC and Gini",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  highchartOutput("model_auc_gini")
                ),
                tabBox(
                  tabPanel("Pie-Chart of Variable Importances", highchartOutput("variable_importances"))
                )
              )),
      
      tabItem(tabName = "cm1",
              h2("Auto ML"),
              mainPanel(
                DTOutput("confusion_matrix_table1")
              ),
              textOutput("threshold_summary")
      ),
      
      tabItem(tabName = "pred",
              h2("Auto ML"),
              h3("Predicted Data"),
              fluidRow(
                box(
                  title = "Classification Model Results",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  dataTableOutput("model_results")
                ))),
      
      tabItem(tabName = "prediction",
              h2("Auto ML"),
              fluidPage(
                column(width = 4,
                       selectInput("gender","Select gender:",
                                   choices = unique(data[["gender"]]) 
                       ),
                       selectInput("SeniorCitizen","Select SeniorCitizen:",
                                   choices = unique(data[["SeniorCitizen"]]) 
                       ),
                       selectInput("Partner","Select Partner:",
                                   choices = unique(data[["Partner"]]) 
                       ),
                       selectInput("Dependents","Select Dependents:",
                                   choices = unique(data[["Dependents"]]) 
                       ),
                       selectInput("PhoneService","Select PhoneService:",
                                   choices = unique(data[["PhoneService"]]) 
                       ),
                       selectInput("MultipleLines","Select MultipleLines:",
                                   choices = unique(data[["MultipleLines"]]) 
                       ),
                       selectInput("InternetService","Select InternetService:",
                                   choices = unique(data[["InternetService"]]) 
                       ),
                       selectInput("OnlineSecurity","Select OnlineSecurity:",
                                   choices = unique(data[["OnlineSecurity"]]) 
                       )
                ),
                column(width = 4,
                       selectInput("OnlineBackup","Select OnlineBackup:",
                                   choices = unique(data[["OnlineBackup"]]) 
                       ),
                       selectInput("DeviceProtection","Select DeviceProtection:",
                                   choices = unique(data[["DeviceProtection"]]) 
                       ),
                       selectInput("TechSupport","Select TechSupport:",
                                   choices = unique(data[["TechSupport"]]) 
                       ),
                       selectInput("StreamingTV","Select StreamingTV:",
                                   choices = unique(data[["StreamingTV"]]) 
                       ),
                       selectInput("StreamingMovies","Select StreamingMovies:",
                                   choices = unique(data[["StreamingMovies"]]) 
                       ),
                       selectInput("Contract","Select Contract:",
                                   choices = unique(data[["Contract"]]) 
                       ),
                       selectInput("PaperlessBilling","Select PaperlessBilling:",
                                   choices = unique(data[["PaperlessBilling"]]) 
                       ),
                       selectInput("PaymentMethod","Select PaymentMethod:",
                                   choices = unique(data[["PaymentMethod"]]) 
                       )
                ),
                
                column(width = 4,
                       sliderInput("tenure", "Select tenure:",
                                   min = min(data$tenure, na.rm = TRUE),
                                   max = max(data$tenure, na.rm = TRUE),
                                   value = min(data$tenure, na.rm = TRUE)
                       ),
                       sliderInput("MonthlyCharges", "Select MonthlyCharges:",
                                   min = min(data$MonthlyCharges, na.rm = TRUE),
                                   max = max(data$MonthlyCharges, na.rm = TRUE),
                                   value = min(data$MonthlyCharges, na.rm = TRUE)
                       ),
                       sliderInput("TotalCharges", "Select TotalCharges:",
                                   min = min(data$TotalCharges, na.rm = TRUE),
                                   max = max(data$TotalCharges, na.rm = TRUE),
                                   value = min(data$TotalCharges, na.rm = TRUE)
                       ),
                       
                       mainPanel(
                         tableOutput("predicted_result")
                       ),
                       actionButton("predict_button", "Predict")
                )
                
              )
      )
    )
  ))

server <- function(input, output, session) {
  
  df1 <- reactive({
    df1 <- read.csv('telco-customer-churn.csv')
    
    df1
  })
  
  # 2. Compute NA counts using reactive expression
  na_counts1 <- reactive({
    na_counts1 <- colSums(is.na(df1()))
    
    na_counts1
  })
  
  # 3. Identify columns with NA values using reactive expression
  columns_with_na1 <- reactive({
    columns_with_na1 <- names(na_counts1()[na_counts1() > 0])
    
    columns_with_na1
  })
  
  # 4. Compute total NA count using reactive expression
  total_nas1 <- reactive({
    total_nas1 <- sum(is.na(df1()))
    
    total_nas1
  })
  
  # 5. Compute unique counts for each column using reactive expression
  unique_counts1 <- reactive({
    unique_counts1 <- sapply(df1(), function(x) length(unique(x)))
    
    unique_counts1
  })
  
  # 6. Create a new data frame without NAs using reactive expression
  data_with_factors1 <- reactive({
    data_temp <- na.omit(df1())
    data_temp$Churn <- recode(data_temp$Churn, "'Yes' = 1; 'No' = 0") %>% as.factor()
    data_temp$SeniorCitizen <- as.character(data_temp$SeniorCitizen)
    
    data_with_factors1 <- data_temp
    
    data_with_factors1
  })
  
  # Use data_with_factors1 to define data1
  data1 <- reactive({
    data1 <- data_with_factors1()
    
    data1
  })
  
  target <- reactive({
    target <- "Churn"
    
    target
  })
  
  # Select the features for modeling
  features <- reactive({
    features <- data1() %>%
      select(gender, SeniorCitizen, Partner, Dependents, PhoneService, PaperlessBilling, Churn) %>%
      names()
  })
  
  columns_to_exclude <- reactive({
    columns_to_exclude <- c("customerID")
    
    columns_to_exclude
    })
  
  # Get data column names excluding excluded columns
  data_column_names <- reactive({
    data_column_names <- setdiff(colnames(data1()), columns_to_exclude())
    
    data_column_names
  })
  
  # Get column types
  column_types <- reactive({
    column_types <- sapply(data1(), class)
    
    column_types
  })
  
  # Get numeric columns
  numeric_columns <- reactive({
    numeric_columns <- data1() %>%
      select_if(is.numeric) %>%
      names()
    
    numeric_columns
  })
  
  # Get categorical columns
  categorical_columns <- reactive({
    categorical_columns <- data1() %>%
      select_if(is.character) %>%
      names()
    
    categorical_columns
  })
  
  prepared_data <- reactive({
    prepare_data <- function(data) {
      return(data)
    }
    
    prepared_data <- prepare_data(data1())
    
    prepared_data
  })
  
  train_test_splits <- reactive({
    data <- prepared_data()
    n <- nrow(data)
    train_indices <- sample(1:n, 0.8 * n)  
    train_data <- data[train_indices, ]
    test_data <- data[-train_indices, ]
    
    train_test_splits <- list(train = train_data, test = test_data)
    
    train_test_splits
  })
  
  output$data_table <- renderDT({
    datatable(prepared_data(), options = list(scrollX = TRUE))
  })
  
  output$na_counts_output <- renderText({
    paste("Number of missing values in each column:", paste(na_counts1(), collapse = ", "))
  })
  
  output$columns_with_na_output <- renderText({
    paste("Columns with missing values:", paste(columns_with_na1(), collapse = ", "))
  })
  
  output$total_nas_output <- renderText({
    paste("Total number of missing values in the dataset:", total_nas1())
  })
  
  # EDA
  output$inspect_na_plot <- renderPlot({
    df1() %>% 
      inspect_na() %>% 
      show_plot()
  })
  
  # Correlation Matrix
  output$correlation_plot <- renderPlot({
    df1() %>% 
      ggcorrm() +
      lotri(geom_point(alpha=0.5))+
      lotri(geom_smooth())+
      utri_heatmap()+
      utri_corrtext()+
      dia_names()+
      dia_histogram()+
      scale_fill_corr()+
      labs(title = "Correlation")
  })
  
  # Correlation Plot
  output$inspect_cor_plot <- renderPlot({
    df1() %>% 
      inspect_cor() %>% 
      show_plot()
  })
  
  # Explore
  output$explore_plot <- renderPlot({
    df1() %>% explore(TotalCharges, target = InternetService)
  })
  
  iv_vars <- reactive({
    iv_vars <- data1() %>% 
      iv(y = target()) %>% 
      as_tibble() %>% 
      mutate(info_value = round(info_value, 3)) %>% 
      arrange(desc(info_value))
    
    iv_vars
  })
  
  ivars <- reactive({
    ivars <- iv_vars() %>% filter(info_value > 0.02) %>% pull(variable)
    
    ivars
  })
  
  df.iv <- reactive({
    df.iv <- data1() %>% select(all_of(target()), all_of(ivars()))
    
    df.iv
  })
  
  dt_list <- reactive({
    dt_list <- df.iv() %>% 
      split_df(target(), ratios = c(0.8, 0.2), seed = 123)
    
    dt_list
  })
  
  bins <- reactive({
    data <- dt_list()$train
    bins <- woebin(data, target())
    
    bins
  })
  
  output$bins_tenure_chart <- renderPlot({
    bins()$tenure %>% woebin_plot()
  })
  
  output$bins_InternetService_chart <- renderPlot({
    bins()$InternetService %>% woebin_plot()
  })
  
  output$bins_Contract_chart <- renderPlot({
    bins()$Contract %>% woebin_plot()
  })
  
  output$bins_PaymentMethod_chart <- renderPlot({
    bins()$PaymentMethod %>% woebin_plot()
  })
  
  train_woe_recursive <- reactive({
    data <- dt_list()$train

    # Your code to calculate woe for train data using woebin_ply function
    data_woe <- data %>%
      woebin_ply(bins())

    # Set names for the woe columns
    names(data_woe) <- names(data_woe) %>% str_replace_all("_woe", "")

    train_woe_recursive <- data_woe

    train_woe_recursive
  })

  # Recursive function to calculate woe for test data
  test_woe_recursive <- reactive({
    data <- dt_list()$test

    # Your code to calculate woe for test data using woebin_ply function
    data_woe <- data %>%
      woebin_ply(bins())

    # Set names for the woe columns
    names(data_woe) <- names(data_woe) %>% str_replace_all("_woe", "")

    test_woe_recursive <- data_woe

    test_woe_recursive
  })

  features <- reactive({
    solve_multicollinearity <- function(data, target) {
      features <- data %>% select(-all_of(target)) %>% names()
      f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
      glm <- glm(f, data = data, family = "binomial")

      coef_na <- attributes(alias(glm)$Complete)$dimnames[[1]]
      features <- features[!features %in% coef_na]

      f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
      glm <- glm(f, data = data, family = "binomial")

      while (glm %>% scorecard::vif() %>% arrange(desc(gvif)) %>% .[1, 2] >= 2) {
        afterVIF <- glm %>% scorecard::vif() %>% arrange(desc(gvif)) %>% pull(variable) %>% .[-1]
        f <- as.formula(paste(target, paste(afterVIF, collapse = " + "), sep = " ~ "))
        glm <- glm(f, data = data, family = "binomial")
      }
      return(glm %>% scorecard::vif() %>% pull(variable))
    }

    features <- train_woe_recursive() %>% solve_multicollinearity(target())

    features
  })

  # Continue with the remaining code

  train_h2o <- reactive({
    train_h2o <- train_woe_recursive() %>% select(all_of(target()), all_of(features())) %>% as.h2o()
    
    train_h2o
  })
  
  test_h2o <- reactive({
    test_h2o <- test_woe_recursive() %>% select(all_of(target()), all_of(features())) %>% as.h2o()
    
    test_h2o
  })
  
  model <- reactive({
    h2o.init()
    
    model <- h2o.glm(
      x = features(),
      y = "Churn",
      training_frame = train_h2o(),
      validation_frame = test_h2o(),
      family = "binomial",
      nfolds = 10,
      seed = 123,
      remove_collinear_columns = TRUE,
      balance_classes = TRUE,
      lambda = 0,
      compute_p_values = TRUE
    )
    
    model
  })
  
  # while(model()@model$coefficients_table %>%
  #        as.data.frame() %>%
  #        select(names, p_value) %>%
  #        mutate(p_value = round(p_value, 3)) %>%
  #        .[-1, ] %>%
  #        arrange(desc(p_value)) %>%
  #        .[1, 2] >= 0.05) {
  #   model()@model$coefficients_table %>%
  #     as.data.frame() %>%
  #     select(names, p_value) %>%
  #     mutate(p_value = round(p_value, 3)) %>%
  #     filter(!is.nan(p_value)) %>%
  #     .[-1, ] %>%
  #     arrange(desc(p_value)) %>%
  #     .[1, 1] -> v
  #
  #   features <- reactive({
  #     features() %>% .[features() != v]
  #   })
  #
  #   train_h2o <- reactive({
  #     train_woe() %>%
  #       select(target(), all_of(as.character(features()))) %>%
  #       as.h2o()
  #   })
  #
  #   test_h2o <- reactive({
  #     test_woe() %>%
  #       select(target(), all_of(as.character(features()))) %>%
  #       as.h2o()
  #   })
  # }

  # Output coefficients table
  output$coefficients_table <- renderDT({
    model()@model$coefficients_table %>%
      as.data.frame() %>%
      select(names, p_value) %>%
      mutate(p_value = round(p_value, 3))
  })

  # Output coefficients tibble
  output$coefficients_tibble <- renderTable({
    model()@model$coefficients %>%
      as.data.frame() %>%
      mutate(names = rownames(model()@model$coefficients %>% as.data.frame())) %>%
      'colnames<-'(c("coefficients", "names")) %>%
      select(names, coefficients) %>%
      as_tibble()
  })

  # Output Variable Importance Chart (Pie Chart)
  output$var_importance_chart <- renderHighchart({
    h2o.varimp(model()) %>%
      as.data.frame() %>%
      filter(percentage != 0) %>%
      dplyr::select(variable, percentage) %>%
      hchart("pie", hcaes(x = variable, y = percentage)) %>%
      hc_colors(colors = "green") %>%
      hc_xAxis(visible = TRUE) %>%
      hc_yAxis(visible = TRUE)
  })

  # Output predicted probabilities
  output$pred_prob <- renderText({
    pred <- model() %>%
      h2o.predict(test_h2o()) %>%
      as.data.frame() %>%
      select(p1, predict)

    paste("Maximum probability for class 0:", max(pred[pred$predict == 0, "p1"]), "\n",
          "Minimum probability for class 1:", min(pred[pred$predict == 1, "p1"]))
  })

  # Output model performance metrics
  output$precision <- renderValueBox({
    pred <- model() %>%
      h2o.predict(test_h2o()) %>%
      as.data.frame() %>%
      select(p1, predict)

    paste("Maximum probability for class 0:", max(pred[pred$predict == 0, "p1"]), "\n",
          "Minimum probability for class 1:", min(pred[pred$predict == 1, "p1"]))

    actuals <- dt_list()$test %>% pull(target())
    predictions <- pred$predict

    cm <- table(actuals, predictions)

    tp <- cm[2, 2]
    tn <- cm[1, 1]
    fp <- cm[1, 2]
    fn <- cm[2, 1]

    precision <- tp / (tp + fp)

    valueBox(
      value = round(precision, 3),
      subtitle = "Precision",
      icon = icon("check-circle"),
      color = "green"
    )
  })

  output$recall_sensitivity <- renderValueBox({
    pred <- model() %>%
      h2o.predict(test_h2o()) %>%
      as.data.frame() %>%
      select(p1, predict)

    paste("Maximum probability for class 0:", max(pred[pred$predict == 0, "p1"]), "\n",
          "Minimum probability for class 1:", min(pred[pred$predict == 1, "p1"]))

    actuals <- dt_list()$test %>% pull(target())
    predictions <- pred$predict

    cm <- table(actuals, predictions)

    tp <- cm[2, 2]
    tn <- cm[1, 1]
    fp <- cm[1, 2]
    fn <- cm[2, 1]

    recall_sensitivity <- tp / (tp + fn)

    valueBox(
      value = round(recall_sensitivity, 3),
      subtitle = "Recall Sensitivity",
      icon = icon("check-circle"),
      color = "green"
    )
  })

  pred_df <- reactive({
    model() %>%
      h2o.predict(test_h2o()) %>%
      as.data.frame() %>%
      select(p1, predict)
  })

  output$prediction_data <- renderDataTable({
    pred_df()
  })

  output$specificity <- renderValueBox({
    pred <- model() %>%
      h2o.predict(test_h2o()) %>%
      as.data.frame() %>%
      select(p1, predict)

    paste("Maximum probability for class 0:", max(pred[pred$predict == 0, "p1"]), "\n",
          "Minimum probability for class 1:", min(pred[pred$predict == 1, "p1"]))

    actuals <- dt_list()$test %>% pull(target())
    predictions <- pred$predict

    cm <- table(actuals, predictions)

    tp <- cm[2, 2]
    tn <- cm[1, 1]
    fp <- cm[1, 2]
    fn <- cm[2, 1]

    specificity <- tn / (tn + fn)

    valueBox(
      value = round(specificity, 3),
      subtitle = "Specificity",
      icon = icon("check-circle"),
      color = "green"
    )
  })

  output$accuracy <- renderValueBox({
    pred <- model() %>%
      h2o.predict(test_h2o()) %>%
      as.data.frame() %>%
      select(p1, predict)

    paste("Maximum probability for class 0:", max(pred[pred$predict == 0, "p1"]), "\n",
          "Minimum probability for class 1:", min(pred[pred$predict == 1, "p1"]))

    actuals <- dt_list()$test %>% pull(target())
    predictions <- pred$predict

    cm <- table(actuals, predictions)

    tp <- cm[2, 2]
    tn <- cm[1, 1]
    fp <- cm[1, 2]
    fn <- cm[2, 1]

    accuracy <- (tp + tn) / (tp + tn + fp + fn)

    valueBox(
      value = round(accuracy, 3),
      subtitle = "Accuracy",
      icon = icon("check-circle"),
      color = "green"
    )
  })

  output$f1_score <- renderValueBox({
    pred <- model() %>%
      h2o.predict(test_h2o()) %>%
      as.data.frame() %>%
      select(p1, predict)

    paste("Maximum probability for class 0:", max(pred[pred$predict == 0, "p1"]), "\n",
          "Minimum probability for class 1:", min(pred[pred$predict == 1, "p1"]))

    actuals <- dt_list()$test %>% pull(target())
    predictions <- pred$predict

    cm <- table(actuals, predictions)

    tp <- cm[2, 2]
    tn <- cm[1, 1]
    fp <- cm[1, 2]
    fn <- cm[2, 1]

    precision <- tp / (tp + fp)
    recall_sensitivity <- tp / (tp + fn)

    f1_score <- 2 * precision * recall_sensitivity / (precision + recall_sensitivity)

    valueBox(
      value = round(f1_score, 3),
      subtitle = "F1 Score",
      icon = icon("check-circle"),
      color = "green"
    )
  })


  output$balanced_accuracy <- renderValueBox({
    pred <- model() %>%
      h2o.predict(test_h2o()) %>%
      as.data.frame() %>%
      select(p1, predict)

    paste("Maximum probability for class 0:", max(pred[pred$predict == 0, "p1"]), "\n",
          "Minimum probability for class 1:", min(pred[pred$predict == 1, "p1"]))

    actuals <- dt_list()$test %>% pull(target())
    predictions <- pred$predict

    cm <- table(actuals, predictions)

    tp <- cm[2, 2]
    tn <- cm[1, 1]
    fp <- cm[1, 2]
    fn <- cm[2, 1]

    recall_sensitivity <- tp / (tp + fn)
    specificity <- tn / (tn + fn)

    balanced_accuracy <- (recall_sensitivity + specificity) / 2

    valueBox(
      value = round(balanced_accuracy, 3),
      subtitle = "Balanced_accuracy",
      icon = icon("check-circle"),
      color = "green"
    )
  })

  output$pred_prob <- renderText({
    pred <- model() %>%
      h2o.predict(test_h2o()) %>%
      as.data.frame() %>%
      select(p1, predict)

    paste("Maximum probability for class 0:", max(pred[pred$predict == 0, "p1"]), "\n",
          "Minimum probability for class 1:", min(pred[pred$predict == 1, "p1"]))
  })

  prediction_table_data <- reactive({
    selected_column <- input$feature_column
    pred_data <- data1()
    pred_data$Temp <- ifelse(data1()[, selected_column] == unique(data1()[, selected_column])[1], 0, 1)
    model <- glm("Churn ~ Temp", data = pred_data, family = binomial)
    predictions <- predict(model, newdata = pred_data, type = "response")
    pred_data$Prob_Class0 <- 1 - predictions
    pred_data$Prob_Class1 <- predictions
    pred_data <- pred_data[, c("Prob_Class0", "Prob_Class1"), drop = FALSE]
    pred_data
  })

  output$prediction_table <- renderDT({
    prediction_table_data()
  })
  # Output ROC curve
  output$roc_curve <- renderHighchart({
    metrices <- model() %>%
      h2o.performance(test_h2o()) %>%
      h2o.metric() %>%
      select(threshold, precision, recall, tpr, fpr) %>%
      add_column(random_tpr = runif(nrow(.), min = 0.001, max = 1)) %>%
      mutate(random_fpr = random_tpr) %>%
      arrange(random_tpr, random_fpr)

    auc <- model() %>%
      h2o.performance(test_h2o()) %>%
      h2o.auc() %>%
      round(2)

    highchart() %>%
      hc_add_series(metrices, "scatter", hcaes(y = tpr, x = fpr), color = "green", name = "TPR") %>%
      hc_add_series(metrices, "line", hcaes(y = random_tpr, x = random_fpr), color = "orange", name = "Random guess") %>%
      hc_add_annotation(
        labels = list(
          point = list(xAxis = 0, yAxis = 0, x = 0.3, y = 0.6),
          text = glue("AUC = {enexpr(auc)}"))
      ) %>%
      hc_subtitle(text = "The model performs better than a random guess")
  })

  # Output AUC values
  output$auc_values <- renderTable({
    model() %>%
      h2o.auc(train = T, valid = T, xval = T) %>%
      as_tibble() %>%
      round(2) %>%
      mutate(data = c("train", "test", "cross_val")) %>%
      mutate(gini = 2 * value - 1) %>%
      select(data, auc = value, gini)
  })

  output$confusion_matrix_table <- renderDataTable({
    pred <- model() %>%
      h2o.predict(test_h2o()) %>%
      as.data.frame() %>%
      select(p1, predict)

    actuals <- dt_list()$test %>% pull(target())
    predictions <- pred$predict

    cm <- table(actuals, predictions)

    cm_table <- data.frame(
      " " = c("Actual 0", "Actual 1"),
      "Predicted 0" = c(cm[1, 1], cm[2, 1]),
      "Predicted 1" = c(cm[1, 2], cm[2, 2])
    )

    colnames(cm_table) <- c("Variables","Predicted 0", "Predicted 1")

    rownames(cm_table) <- NULL

    datatable(cm_table, options = list(paging = FALSE, searching = FALSE, lengthChange = FALSE))
  })
  # AutoML
  saved_model <- reactive({
    saved_model <- h2o.loadModel("GBM_3_AutoML_1_20230730_182706")

    saved_model
  })

  # Reactive expression for converting data to H2O format
  h2o_data1 <- reactive({
    h2o_data1 <- as.h2o(data1())

    h2o_data1
  })

  # Reactive expression for splitting H2O data into train and test sets
  h2o_data1_split <- reactive({
    h2o_data1_split <- h2o.splitFrame(h2o_data1(), ratios = 0.8, seed = 123)

    h2o_data1_split
  })

  # Get the train and test H2O frames
  train1 <- reactive({
    train1 <- h2o_data1_split()[[1]]

    train1
  })

  test1 <- reactive({
    test1 <- h2o_data1_split()[[2]]

    test1
  })

  # Reactive expression for predicting on the test set
  pred1 <- reactive({
    pred1 <- saved_model() %>%
      h2o.predict(test1()) %>%
      as.data.frame()

    pred1
  })

  auc_gini <- reactive({
    auc_gini <- saved_model() %>%
      h2o.auc(train = TRUE, valid = TRUE, xval = TRUE) %>%
      as_tibble() %>%
      round(2) %>%
      mutate(data = c("train", "test", "cross_val")) %>%
      mutate(gini = 2 * value - 1) %>%
      select(data, auc = value, gini)

    auc_gini
  })

  var_importance <- reactive({
    var_importance <- saved_model() %>%
      h2o.varimp() %>%
      as.data.frame() %>%
      filter(percentage != 0) %>%
      select(variable, percentage)

    var_importance
  })

  output$model_results <- renderDataTable({
    pred1()
  })

  # Render a bar chart for AUC and Gini values
  output$model_auc_gini <- renderHighchart({
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Model AUC and Gini") %>%
      hc_xAxis(categories = auc_gini()$data) %>%
      hc_add_series(name = "AUC", data = auc_gini()$auc) %>%
      hc_add_series(name = "Gini", data = auc_gini()$gini)
  })

  # Render a pie chart for variable importances
  output$variable_importances <- renderHighchart({
    var_importance() %>%
      hchart("pie", hcaes(x = variable, y = percentage)) %>%
      hc_colors(colors = "green") %>%
      hc_xAxis(visible = TRUE) %>%
      hc_yAxis(visible = TRUE)
  })

  h2o_data1 <- reactive({
    h2o_data1 <- as.h2o(data1())

    h2o_data1 <- h2o_data1 %>% h2o.splitFrame(ratios = 0.8, seed = 123)

    h2o_data1
  })

  train1 <- reactive({
    train1 <- h2o_data1()[[1]]

    train1
  })

  test1 <- reactive({
    test1 <- h2o_data1()[[2]]

    test1
  })

  pred1 <- reactive({
    target1 <- "Churn"
    features1 <- setdiff(colnames(data1()), target1)

    pred1 <- as.data.frame(h2o.predict(saved_model(), test1()))

    pred1$predict <- as.character(pred1$predict)

    pred1
  })

  actuals1 <- reactive({
    actuals1 <- as.data.frame(test1()) %>% pull(Churn)

    actuals1 <- as.character(actuals1)

    actuals1
  })

  output$confusion_matrix_table1 <- renderDT({
    conf_matrix1 <- table(Actual = actuals1(),
                          Predicted = pred1()$predict)

    conf_matrix1

  })

  observeEvent(input$predict_button, {
    input_data <- data.frame(
      gender = input$gender,
      SeniorCitizen = input$SeniorCitizen,
      Partner = input$Partner,
      Dependents = input$Dependents,
      PhoneService = input$PhoneService,
      MultipleLines = input$MultipleLines,
      InternetService = input$InternetService,
      OnlineSecurity = input$OnlineSecurity,
      OnlineBackup = input$OnlineBackup,
      DeviceProtection = input$DeviceProtection,
      TechSupport = input$TechSupport,
      StreamingTV = input$StreamingTV,
      StreamingMovies = input$StreamingMovies,
      Contract = input$Contract,
      PaperlessBilling = input$PaperlessBilling,
      PaymentMethod = input$PaymentMethod,
      tenure = input$tenure,
      MonthlyCharges = input$MonthlyCharges,
      TotalCharges = input$TotalCharges
    )

    h2o_input_data <- as.h2o(input_data)

    predictions <-  h2o.predict(saved_model(), h2o_input_data)

    predictions_df <- predictions %>%
      as.data.frame() %>%
      mutate(p0 = round(p0, 2),
             p1 = round(p1, 2),
             predict = ifelse(predict == 0, "Churn_No", "Churn_Yes"))
    
    output$predicted_result <- renderTable({
      if (!all(sapply(predictions_df, is.character))) {
        predictions_df[] <- lapply(predictions_df, as.character)
      }
      predictions_df
    })
 
  })
  
  max_p1 <- reactive({
    pred_data <- pred1()
    pred_data[pred_data$predict == "Churn_No", "p1"] %>% max()
  })
  
  min_p1 <- reactive({
    pred_data <- pred1()
    pred_data[pred_data$predict == "Churn_Yes", "p1"] %>% min()
  })
  
  threshold_by_max_metric <- reactive({
    model <- saved_model()
    h2o.performance(model, test1()) %>%
      h2o.find_threshold_by_max_metric("f1")
  })
  
  output$threshold_summary <- renderText({
    paste("Threshold by max metric (f1):", threshold_by_max_metric(), "\n")
  })
  
}

shinyApp(ui = ui, server = server)

