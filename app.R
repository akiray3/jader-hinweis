library("shiny")
library("shinydashboard")
library("tidyverse")
library("ggrepel")
library("DT")
library("fst")
library("data.table")


figlist <- readRDS("fig_list.obj")
tbl200 <- fst::read.fst(path = "tbl_200.obj") %>%
  dplyr::arrange(性別, 件数)
tbl_dtl <- fst::read.fst(path = "tbl_all.obj")
demo2 <- dplyr::distinct(.data = tbl_dtl, 識別番号,性別)
##gg2の作成できた
gg3 <- tbl_dtl %>%
  distinct(識別番号, 一般名) %>%
  rename("薬剤名" = "一般名") %>%
  merge.data.table(., demo2, by = "識別番号", all = FALSE)
#drug3の作成できた
drug3 <- gg3 %>%
  dplyr::group_by(薬剤名) %>%
  dplyr::summarise(件数 = n()) %>%
  dplyr::arrange(-件数) %>% 
  dplyr::filter(件数 >= 10) %>%
  select(-件数) 
  #dplyr::pull(-件数)

#reac2の作成できた
reac2 <-  dplyr::distinct(.data = tbl_dtl, 識別番号,有害事象,性別)
#reac3の作成
reac3 <- tbl_dtl %>%
  dplyr::group_by(有害事象) %>%
  dplyr::summarise(件数 = n()) %>%
  dplyr::arrange(-件数) %>% 
  dplyr::filter(件数 >= 10) %>%
  dplyr::pull(-件数) %>%
  as.character()




shiny::shinyApp(
  ui = shinydashboard::dashboardPage(
    skin = "black",
    dashboardHeader(title = 'Jader Hinweis'),
    dashboardSidebar(
      collapsed = TRUE,
      sidebarMenu(
        menuItem(text = "有害事象マップ", tabName = "page1", icon = icon("chart-line")),
        # menuItem(text = "全データ", tabName = "page2", icon = icon("chart-line")),
        menuItem(text = "オッズ比計算", tabName = "page2", icon = icon("chart-line")),
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
          fluidPage(
            titlePanel("オッズ比計算"),
            sidebarLayout(
              sidebarPanel(
<<<<<<< HEAD
                selectizeInput("drug3", "薬剤を選択", choices = unique(drug3$薬剤名), multiple = F, options = list(placeholder = "薬剤名を入力してください")),
                selectizeInput("reac3", "有害事象を選択", choices = unique(reac3), multiple = T, options = list(placeholder = "有害事象を入力してください")),
=======
                selectizeInput("drug3",   "薬剤を選択"  , choices = unique(drug3), multiple = TRUE , options = list(placeholder = "薬剤名を入力してください")),
                selectizeInput("reac3", "有害事象を選択", choices = unique(reac3), multiple = TRUE , options = list(placeholder = "有害事象を入力してください")),
>>>>>>> 63c320294e951d5ad2aef62853ffaf6c6c471f9a
                actionButton("calculate", "計算")
              ),
              mainPanel(
                fluidRow(
                  column(8,
                         verbatimTextOutput("selectedDE")
                  )
                ),
                fluidRow(
                  column(4,h4("男性：クロス表"),tableOutput("crossTableMale")),
                  column(4,h4("女性：クロス表"),tableOutput("crossTableFemale")),
                  column(4,h4("全体：クロス表"),tableOutput("crossTableAll"))),
                fluidRow(
                  column(4,h4("男性：オッズ比"),verbatimTextOutput("oddsRatioMale")),
                  column(4,h4("女性：オッズ比"),verbatimTextOutput("oddsRatioFemale")),
                  column(4,h4("全体：オッズ比"),verbatimTextOutput("oddsRatioAll"))),
                fluidRow(
                  column(4,h4("男性：95%信頼区間"),verbatimTextOutput("CI_Male")),
                  column(4,h4("女性：95%信頼区間"),verbatimTextOutput("CI_Female")),
                  column(4,h4("全体：95%信頼区間"),verbatimTextOutput("CI_All"))
                )
              )
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
    observeEvent(input$dt_top200_rows_selected, {
      thisrow <- input$dt_top200_rows_selected
      tmpdata <- tbl200 %>%
        dplyr::slice(thisrow)
#      outfig <- figlist[[tmpdata$性別[1]]] +
#        geom_point(data = tmpdata, color = "#e15f41", shape = 19, alpha = 0.8) +
#        ggrepel::geom_label_repel(
#          data = tmpdata, mapping = aes(label = 有害事象), color = "#e15f41",
#          size = 4, family = "HiraKakuProN-W6", show.legend = FALSE
#        )
#      output$plt_map <- renderPlot({plot(outfig)})
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
    observeEvent(input$calculate, {
      selected_drug <- input$drug3
      selected_event <- input$reac3
      #selected_drug <- "プレドニゾロン"
      #selected_event <- c("薬物相互作用","増強的薬物相互作用")
      drugM <- gg3$識別番号[which(gg3$薬剤名 == selected_drug & gg3$性別 == "男性")]
      drugF <- gg3$識別番号[which(gg3$薬剤名 == selected_drug & gg3$性別 == "女性")]
      drugA <- union(drugM ,drugF)
      reacM <- reac2$識別番号[which(reac2$有害事象 %in% selected_event & reac2$性別 == "男性")]
      reacF <- reac2$識別番号[which(reac2$有害事象 %in% selected_event & reac2$性別 == "女性")]
      reacA <- union(reacM, reacF)
      
      # 男ークロス表を作成・表示
      cross_Male <- matrix(c(length(intersect(reacM, drugM)), length(drugM) - length(intersect(reacM, drugM)), length(reacM) - length(intersect(reacM, drugM)), sum(demo2$性別 == "男性") - length(drugM) - length(reacM) + length(intersect(reacM, drugM))), nrow = 2, byrow = TRUE)
      rownames(cross_Male) <- c("服用◯", "服用❌")
      colnames(cross_Male) <- c("有害事象◯", "有害事象❌")
      output$crossTableMale <- renderTable(cross_Male, rownames = TRUE)
      # 女ークロス表を作成・表示　
      cross_Female <- matrix(c(length(intersect(reacF, drugF)), length(drugF) - length(intersect(reacF, drugF)), length(reacF) - length(intersect(reacF, drugF)), sum(demo2$性別 == "女性") - length(drugF) - length(reacF) + length(intersect(reacF, drugF))), nrow = 2, byrow = TRUE)
      rownames(cross_Female) <- c("服用◯", "服用❌")
      colnames(cross_Female) <- c("有害事象◯", "有害事象❌")
      output$crossTableFemale <- renderTable(cross_Female, rownames = TRUE)
      # 全体ークロス表を作成・表示　
      cross_All <- matrix(c(length(intersect(reacA, drugA)), length(drugA) - length(intersect(reacA, drugA)), length(reacA) - length(intersect(reacA, drugA)), {sum(demo2$性別 == "男性") + sum(demo2$性別 == "女性")} - length(drugA) - length(reacA) + length(intersect(reacA, drugA))), nrow = 2, byrow = TRUE)
      rownames(cross_All) <- c("服用◯", "服用❌")
      colnames(cross_All) <- c("有害事象◯", "有害事象❌")
      output$crossTableAll <- renderTable(cross_All, rownames = TRUE)
      
      # 男ーオッズ比を計算・表示
      OR_Male <- ((cross_Male[1,1]) /(cross_Male[1,2]))/((cross_Male[2,1]) /(cross_Male[2,2]))
      output$oddsRatioMale <- renderText(paste("オッズ比:", sprintf("%.3f",OR_Male)))
      # 女ーオッズ比を計算・表示
      OR_Female <- ((cross_Female[1,1]) /(cross_Female[1,2]))/((cross_Female[2,1]) /(cross_Female[2,2]))
      output$oddsRatioFemale <- renderText(paste("オッズ比:", sprintf("%.3f",OR_Female)))
      # 全体ーオッズ比を計算・表示
      OR_ALL <- ((cross_All[1,1]) /(cross_All[1,2]))/((cross_All[2,1]) /(cross_All[2,2]))
      output$oddsRatioAll <- renderText(paste("オッズ比:", sprintf("%.3f",OR_ALL)))
      
      #男ー95%信頼区間の計算・表示
      CI.Low_Male  <- exp(log(OR_Male) - (1.96 * sqrt((1/cross_Male[1,1]) + (1/(cross_Male[2,2])) + (1/(cross_Male[1,2])) + (1/cross_Male[2,1]))))
      CI.High_Male <- exp(log(OR_Male) + (1.96 * sqrt((1/cross_Male[1,1]) + (1/(cross_Male[2,2])) + (1/(cross_Male[1,2])) + (1/cross_Male[2,1]))))
      output$CI_Male <- renderText({paste("下限:", sprintf("%.3f",CI.Low_Male)," "  , "上限:", sprintf("%.3f",CI.High_Male))})
      #女ー95%信頼区間の計算・表示
      CI.Low_Female  <- exp(log(OR_Female)  -  (1.96 * sqrt((1/cross_Female[1,1]) + (1/cross_Female[2,2]) + (1/cross_Female[1,2]) + (1/cross_Female[2,1]))))
      CI.High_Female <- exp(log(OR_Female)  +  (1.96 * sqrt((1/cross_Female[1,1]) + (1/cross_Female[2,2]) + (1/cross_Female[1,2]) + (1/cross_Female[2,1]))))
      output$CI_Female <- renderText({paste("下限:", sprintf("%.3f",CI.Low_Female)," " , "上限:", sprintf("%.3f",CI.High_Female))})
      #全体ー95%信頼区間の計算・表示
      CI.Low_All  <- exp(log(OR_ALL)  - (1.96 * sqrt((1/cross_All[1,1]) + (1/(cross_All[2,2])) + (1/(cross_All[1,2])) + (1/cross_All[2,1]) )))
      CI.High_All <- exp(log(OR_ALL)  + (1.96 * sqrt((1/cross_All[1,1]) + (1/(cross_All[2,2])) + (1/(cross_All[1,2])) + (1/cross_All[2,1]) )))
      output$CI_All <- renderText({paste("下限:", sprintf("%.3f",CI.Low_All)," ","上限:", sprintf("%.3f",CI.High_All))})

      
      # 選択された薬剤名・有害事象を表示
      if (!is.null(selected_drug) && length(selected_event) > 0) {
        output$selectedDE <- renderText({
          paste("＜", paste(selected_drug,  collapse = " , "), "＞", "　✖️　", 
                "＜", paste(selected_event, collapse = " , "), "＞")
        })
      } else {
        output$selectedDE <- renderText({ NULL })
      }
      
      
    })
    
  }
)