###
library(dplyr)
library(ggplot2)
library(leaflet)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
options(spinner.color="#128870")
library(stringr)
library(highcharter)
###

## Ładowanie funkcji pomocniczych
source('pomocne_funkcje.R')

## Budowaniee UI --------------------------------
## 1. Header -----------------------------------

header <- dashboardHeader(title = div(
          id = 'title',
          img(src='logo1.png', style="height: 40px; width: 50px;"),
          span("Medi", style = "color: #333333;"),
          span("Flow", style = "color: #FFFFFF;")
          ),
          
          dropdownMenuCustom(
                             customSentence = customSentence_share,
                             icon = icon('share-alt'),
                             messageItem(
                               from = 'Twitter',
                               message = "",
                               icon = icon("twitter"),
                               href = "https://www.x.com"
                             ),
                             messageItem(
                               from = 'Facebook',
                               message = "",
                               icon = icon("facebook"),
                               href = "https://www.Facebook.com"
                             ),
                             messageItem(
                               from = 'Google+',
                               message = "",
                               icon = icon("google-plus"),
                               href = "https://www.google.com"
                             ),
                             messageItem(
                               from = 'Pinterest',
                               message = "",
                               icon = icon("pinterest-p"),
                               href = "http://www.Pinterest.com"
                             ),
                             messageItem(
                               from = 'LinkedIn',
                               message = "",
                               icon = icon("linkedin"),
                               href = "http://www.LinkedIn.com"
                             ),
                             messageItem(
                               from = 'Tumblr',
                               message = "",
                               icon = icon("tumblr"),
                               href = "http://www.Tumblr.com"
                             )
          )
)

## 2. Sidebar ------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id="sidebar",
    style = "position: relative; overflow: visible; overflow-y: hidden",
    
    ## 1 kafelek w Sidebarze ---------------
    menuItem(text = "Główny dashboard",
             tabName = "dashboard",
             icon = icon("dashboard"),
             badgeLabel = format(Sys.Date(), "%d %b"),
             badgeColor = "black"),
    
    ## Sekcja z filtrami, warunek - musi być otwarty "Główny dashboard"
  div(id="filter_panel",
      conditionalPanel(
        condition = "input.sidebar === 'dashboard'",
        selectizeInput("filter_var", "Wybierz zmienną", choices = choices),
        checkboxInput("gender_split", "Czy ma być podział na płeć?", FALSE)
      )),
    
    ## 2 kafelek w Sidebarze -----------
    menuItem(text = "Szczegółowe podsumowanie",
             tabName = "summary",
             icon = icon("list"))
  )
)
## 3. Body -----------------------------------------

body <- dashboardBody(
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Josefin+Sans:ital,wght@0,100..700;1,100..700&display=swap", rel = "stylesheet")
  ),
  
  ## Dołączanie pliku js
  tags$script(src = "script.js"),
  
  ## Dołączanie pliku CSS 
  includeCSS("www/styles.css"),
  
  tags$div(class = "scrollup", 
           actionButton("go_up", label = " ", icon = icon("arrow-up"))
  ),
  
  # Spinner na środku ekranu
  tags$div(id = "loading-spinner", class = "spinner-wrapper",
           tags$div(class = "spinner")
  ),
  
  ## 3.1 Dashboard body
  tabItems(
    tabItem(
      tabName = "dashboard",

      fluidRow(
        tags$h3("Główne statystyki:"),
        tags$div(
          tags$div(
            tags$p("Wskaźnik rotacji"),
            textOutput(outputId = "attrRate") 
          ) %>% tagAppendAttributes(class = "stat-card", id="attrRateCard"),
          tags$div(
            tags$p("Średnie dochody miesięczne"),
            textOutput(outputId = "averageIncome")
          ) %>% tagAppendAttributes(class = "stat-card"),
          tags$div(
            tags$p("Średni wiek"),
            textOutput(outputId = "averageAge")
          ) %>% tagAppendAttributes(class = "stat-card"),
          tags$div(
            tags$p("Pozycja"),
            textOutput(outputId = "rotationPosition")
          ) %>% tagAppendAttributes(class = "stat-card")
        ) %>% tagAppendAttributes(class = "stat-card-container"),
        width = 9
      ) %>% tagAppendAttributes(class = "main-container"),
      
      
      # Kontant na Głównym dashboardzie
      fluidRow(
        column(6, highchartOutput("attrRatePlot")),
        column(6, highchartOutput("attritionCountPlot"))
      ),
      
      fluidRow(
        column(6, highchartOutput("monthlyIncomeYesPlot")),
        column(6, highchartOutput("monthlyIncomeNoPlot"))
      ),
      
      fluidRow(
        conditionalPanel(
          condition = "input.gender_split == false",
            column(12, highchartOutput("overTimePlot"))
        ),
        
        conditionalPanel(
          condition = "input.gender_split == true",
            column(6, highchartOutput("femalePlot")),
            column(6, highchartOutput("malePlot"))
        )
      ),
      
      fluidRow(
        column(6, highchartOutput("satisfactionNoRotationPlot")),
        column(6, highchartOutput("satisfactionRotationPlot"))
      ),
      
      fluidRow(
        column(6, highchartOutput("genderAttritionPlot")),
        column(6, highchartOutput("worklifeBalanceHeatmapPlot"))
      )
    ),
    
    tabItem( 
      tabName = "summary",
      
          h2("Szczegółowe podsumowanie", style="text-align: center; margin-bottom: 20px; 
             text-transform: uppercase; letter-spacing: 1px; font-weight: 600; 
             border-bottom: 3px solid #128870; background-color: #ecf0f5;
             font-family: 'Roboto' sans-serif; color: #333333;"),
          
          fluidRow(
            valueBoxOutput("attritionRate", width = 3) %>% withSpinner(type=4),
            valueBoxOutput("numberOfAttrPp", width = 3),
            valueBoxOutput("femaleAttrition", width = 3),
            valueBoxOutput("maleAttrition", width = 3)
          ),
          fluidRow(
            valueBoxOutput("avgAge", width = 3),
            valueBoxOutput("avgIncome", width = 3),
            valueBoxOutput("avgYearsAtCompany", width = 3),
            valueBoxOutput("avgDistanceFromHome", width = 3)
          ),
          fluidRow(
            valueBoxOutput("avgJobRate", width = 3),
            valueBoxOutput("avgWorkLifeBalance", width = 3),
            valueBoxOutput("leftAndOverWorked", width = 3),
            valueBoxOutput("jobRole", width = 3)
          )
    )
  )
)


ui <- dashboardPage(header, sidebar, body)  # Łączenie elementów ui w całość