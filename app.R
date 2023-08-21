library("shiny")
library("shinydashboard")
library("tidyverse")
library("ggrepel")
library("DT")
library("fst")
# test test test
shiny::shinyApp(
  ui = shinydashboard::dashboardPage(
    skin = "black",
    dashboardHeader(title = 'Jader HiYnweis'),
    dashboardSidebar(
      collapsed = TRUE,
      sidebarMenu(
        menuItem(text = "有害事象マップ", tabName = "page1", icon = icon("chart-line")),
        menuItem(text = "全データ", tabName = "page2", icon = icon("chart-line")),
        menuItem(text = "Help", tabName = "help", icon = icon("house"))
      )
    ),
    shinydashboard::dashboardBody(
      tabItems(
        tabItem(
          tabName = "page1",
          tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
          fluidRow(
            shinydashboard::box(
              width = 12, solidHeader = TRUE, collapsible	= TRUE,
              status = "primary", title = "1. リストから有害事象を選ぶ",
              DTOutput("dt_top200")
            ),
            shinydashboard::box(
              width = 12, solidHeader = TRUE, collapsible	= TRUE,
              status = "primary", title = "2. 有害事象マップの確認",
              plotOutput("plt_map")
            ),
            shinydashboard::box(
              width = 12, solidHeader = TRUE, collapsible	= TRUE,
              status = "primary", title = "3. 有害事象の詳細データを抽出",
              DTOutput("dt_detail")
            )
          )
        ),
        tabItem(
          tabName = "page2",
          tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
          fluidRow(
            shinydashboard::box(
              width = 12, solidHeader = TRUE, collapsible	= TRUE,
              status = "primary", title = "1. JADERデータ全体から抽出する",
              DTOutput("dt_all")
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
  server = function(input, output, session) {
    tbl200 <- fst::read.fst(path = "tbl_200.obj") %>%
       dplyr::arrange(性別, 件数)
    tbl_dtl <- fst::read.fst(path = "tbl_all.obj")
    figlist <- readRDS("fig_list.obj")
    output$dt_top200 <- renderDT({
      DT::datatable(
        data = tbl200, filter = "top", rownames = FALSE, selection = "single",
        option = list(
          scrollX = TRUE, responsive = TRUE, autoWidth = TRUE,
          columnDefs = list(
            list(
              visible = FALSE,
              targets = c(1, 5:12, 14, 15, 17:18)
            )
          )
        )
      )
    })
    output$dt_all <- renderDT({
      DT::datatable(
        data = tbl_dtl,
        filter = "top", rownames = FALSE,
        selection = "single", extensions = "Buttons",
        option = list(
          scrollX = TRUE, responsive = TRUE, autoWidth = TRUE,
          dom = "Blfrtip", buttons = c("csv", "excel")
        )
      )
    })
    observeEvent(input$dt_top200_rows_selected, {
      thisrow <- input$dt_top200_rows_selected
      tmpdata <- tbl200 %>%
        dplyr::slice(thisrow)
      outfig <- figlist[[tmpdata$性別[1]]] +
        geom_point(data = tmpdata, color = "#e15f41", shape = 19, alpha = 0.8) +
        ggrepel::geom_label_repel(
          data = tmpdata, mapping = aes(label = 有害事象), color = "#e15f41",
          size = 4, family = "HiraKakuProN-W6", show.legend = FALSE
        )
      output$plt_map <- renderPlot({plot(outfig)})
      tbl_sub <- tbl_dtl %>%
            dplyr::filter(有害事象 == as.character(tmpdata$有害事象[1]))
      output$dt_detail <- renderDT(server = FALSE,{
        DT::datatable(
          data = tbl_sub,
          filter = "top", rownames = FALSE,
          selection = "single", extensions = "Buttons",
          option = list(
            scrollX = TRUE, responsive = TRUE, autoWidth = TRUE,
            dom = "Blfrtip", buttons = c("csv", "excel")
          )
        )
      })
    })
  }
)
