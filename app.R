library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(DT)
library(ggrepel)
library(openxlsx)
library(plotly)

shiny::shinyApp(
  ui = dashboardPage(
    skin = "black",
    dashboardHeader(title = 'Jader Hinweis'),
    dashboardSidebar(
      collapsed = TRUE,
      sidebarMenu(
        menuItem(
          text = "Page1",
          tabName = "page1",
          icon = icon("chart-line")
        ),
        menuItem(
          text = "Help",
          tabName = "help",
          icon = icon("house")
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "page1",
          tags$link(
            rel = "stylesheet",
            type = "text/css",
            href = "style.css"
          ),
          fluidRow(
            box(
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              title = "有害事象マップから症状を選択",
              plotly::plotlyOutput("plot01am"),
              div(class = "space"),
              uiOutput("clpointinfo01am"),
              uiOutput("cldownload01am"),
              uiOutput("cldbt01am")
            )
          )
        ),
        tabItem(
          tabName = "help",
          tags$link(
            rel = "stylesheet",
            type = "text/css",
            href = "pagestyle.css"
          ),
          htmlTemplate("page.html")
        )
      )
    )
  ),
  server = function(input, output) {
  }
)
