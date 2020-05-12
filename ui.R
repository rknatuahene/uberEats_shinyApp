library(DT)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tibble)
library(wordcloud2)

shinyUI(dashboardPage(
    dashboardHeader(title = span("Price my menu",style ="font-size:24px")),
                                 
    dashboardSidebar(
        
        sidebarUserPanel("Robert Atuahene",
                         image = "Robert_Atuahene_linkedin.jpeg"),
        sidebarMenu(
            menuItem("All Cali", tabName = "main", icon = icon("map")),
            menuItem("Top cities", tabName = "city", icon = icon("map")),
            menuItem("Are prices different?", tabName = "regressions", icon = icon("question")),
            menuItem("What are ratings worth?", tabName = "ratings", icon = icon("question")),
            menuItem("Key words and prices", tabName = "wcloud", icon = icon("puzzle-piece")),
            menuItem("Price my menu", tabName = "pricing", icon = icon("check")),
            menuItem("About Project", tabName = "about", icon = icon("info"))
        )
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems( ##all side tabs are instantiated here
            tabItem(tabName = "main",
                    fluidRow(infoBoxOutput("total_dishes"),
                             infoBoxOutput("rest_total"),
                             infoBoxOutput("total_submenus"),
                             infoBoxOutput("city_cnt"),
                             infoBoxOutput("state_cnt"),
                             infoBoxOutput("top_city")
                            
                             ),
                    fluidRow(selectizeInput("selectedState",
                                   "State",
                                   choices = unique(filtered_set$state))),
                             
                    fluidRow(box(plotOutput("bar_rest")),
                             box(plotOutput("bar_submenu")))),
            
            tabItem(tabName = "city",
                    fluidRow(selectizeInput("selectedCity",
                                            "City",
                                            choices = c("",sort(city_rest_cnt$local_city)),
                                            selected = "los angeles")),
                    
                    fluidRow(box(plotOutput("bar_city_submenu")),
                             box(plotOutput("violin_city_prices")))),  
            
            tabItem(tabName = "regressions",
                    fluidRow(selectizeInput("selectedCity_reg",
                                            "City",
                                            choices = c("ALL",sort(city_rest_cnt$local_city)),
                                            selected = "los angeles"),
                             selectizeInput("selectedConfi",
                                            "Confidence Level",
                                            choices = c(0.95,0.99,0.997),
                                            selected = 0.95)
                    ),
                    fluidRow(box(plotOutput("regressOut") , height = 300, width = 300))),
            
            tabItem(tabName = "ratings",
                    fluidRow(selectizeInput("selectedCity_ratings",
                                            "City",
                                            choices = c("ALL",sort(city_rest_cnt$local_city)),
                                            selected = "los angeles")
                    ),
                    fluidRow(box(plotOutput("ratingsOut") , height = 500),
                             box(DT::dataTableOutput("betaTable")))),
            
            tabItem(tabName = "wcloud",
                    fluidRow(selectizeInput("selectedCity_wcloud",
                                            "City",
                                            choices = c("ALL",sort(city_rest_cnt$local_city)),
                                            selected = "los angeles")
                    ),
                    fluidRow(box(wordcloud2Output("wcloudOut") , height = 500))), #wordcloud requires it's own special output call
            tabItem(tabName = "pricing",
                    fluidRow(box(DT::dataTableOutput("recommendedTable")),
                             box(DT::dataTableOutput("customerInput"))),
                    fluidRow(box(plotOutput("recommendedPlot"), height = 500, width = 500))
                    ),
            tabItem(tabName = "about",
                    fluidRow(htmlOutput("aboutProject"))
            )
            )
        )
    )
)