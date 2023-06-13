library("needs")
needs("shiny", "shinydashboard", "ggplot2", "tidyverse", "DT", "ggrepel",
  "openxlsx", "plotly", "pipeR")

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
            shinydashboard::box(
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              title = "1. リストから有害事象を選ぶ",
              DTOutput("dt_top200"),
              # div(class = "space"),
              uiOutput("cldownload01"),
              uiOutput("cldbt01"),
              collapsible	= TRUE
            ),
            shinydashboard::box(
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              title = "2. 有害事象マップの確認",
              collapsible	= TRUE,
              plotOutput("plt_map")
            ),
            shinydashboard::box(
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              title = "3. 有害事象の詳細データを抽出",
              collapsible	= TRUE
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
    tbl200 <- readRDS("tbl_200.obj")
    figlist <- readRDS("fig_list.obj")
    tblall <- readRDS("tbl_all.obj")
    # tbl200 <- reactive({readRDS("tbl_200.obj")})
    # tblall <- reactive({readRDS("tbl_all.obj")})
    output$dt_top200 <- renderDT({
      DT::datatable(
        data = tbl200,
        filter = "top",
        rownames = FALSE,
        selection = "single",
        option = list(
          responsive = TRUE,
          autoWidth = TRUE,
          columnDefs = list(
            list(
              visible = FALSE,
              targets = c(1, 5:12, 14, 15, 17:18)
            )
          )
        )
      ) %>%
      DT::formatStyle(columns = colnames(.), fontSize = "25%")
    })
    observeEvent(input$dt_top200_rows_selected, {
      thisrow <- input$dt_top200_rows_selected
      tmp <- tbl200 %>%
        dplyr::slice(thisrow)
      outfig <- figlist[[tmp$性別[1]]] +
        geom_point(data = tmp, color = "#e15f41", shape = 19, alpha = 0.8) +
        ggrepel::geom_label_repel(
          data = tmp, mapping = aes(label = 有害事象), color = "#e15f41",
          size = 4, family = "HiraKakuProN-W6", show.legend = FALSE
        )
      output$plt_map = renderPlot({
        plot(outfig)
      })
    })
  }
)
