library("needs")
needs(c(shiny, shinydashboard, ggplot2, tidyverse, DT, ggrepel, openxlsx, plotly))


shiny::shinyApp(
  ui = dashboardPage(
    skin = "black",
    dashboardHeader(title = 'Jader Hinweis'),
    dashboardSidebar(
      collapsed = TRUE,
      sidebarMenu(
        menuItem(text = "Page1", tabName = "page1", icon = icon("chart-line")),
        menuItem(text = "Help", tabName = "help", icon = icon("house"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "page1",
          tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
          fluidRow(
            box(
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              title = "有害事象マップ",
              DTOutput("mydatatable"),
              div(class = "space"),
              uiOutput("cldownload01"),
              uiOutput("cldbt01")
            )
          )
        ),
        tabItem(
          tabName = "help",
          tags$link(rel = "stylesheet", type = "text/css", href = "pagestyle.css"),
          htmlTemplate("page.html")
        )
      )
    )
  ),
  server = function(input, output) {  
    tbl200 <- readRDS("data/tbl_200.obj")
    tblall <- readRDS("data/tbl_all.obj")
    # tbl200 <- reactive({readRDS("data/tbl_200.obj")})
    # tblall <- reactive({readRDS("data/tbl_all.obj")})
    output$mydatatable <- renderDT({
        tbl200 %>%
          dplyr::select(性別, 順位, クラス, 有害事象, 件数, 高齢発症ROR, 多剤発症ROR) %>%
          dplyr::mutate(
            高齢発症ROR = round(高齢発症ROR, 2),
            多剤発症ROR = round(多剤発症ROR, 2)
          ) %>%
          DT::datatable(
            class = "cell-border stripe",
            style = 'bootstrap',
            selection = "single",
            filter = "top",
            option = list(
              autoFill = TRUE, responsive = TRUE, scrollX = TRUE, autoWidth = TRUE,
              searchBuilder = list(enterSearch = TRUE),
              searchPanes = list(il8n = list(loadMessage = "loading...")),
              select = list(style = "multi", selector = "td:first-child")
            )
          )
      })
  }
)
