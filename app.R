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
tbl_dtl2 <- tbl_dtl %>%
  select(-医薬品の関与, -年齢, -年代, -多剤併用, -転帰, -薬剤数)
demo2 <- tbl_dtl2 %>%
  select(-一般名, -有害事象) %>%
  distinct(識別番号,性別)
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
ae <- as.character(gg3$薬剤名)
ad <- as.character(reac2$有害事象)

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
            titlePanel("オッズ比計算アプリ"),
            sidebarLayout(
              sidebarPanel(
                selectizeInput("drug3", "薬剤を選択", choices = unique(drug3$薬剤名), multiple = FALSE, options = list(placeholder = "薬剤名を入力してください")),
                selectizeInput("reac3", "有害事象を選択", choices = unique(reac3), multiple = T, options = list(placeholder = "有害事象を入力してください")),
                actionButton("calculate", "オッズ比計算")
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
                  column(4,h4("男性：95%信頼区間"),verbatimTextOutput("confidenceIntervalMale")),
                  column(4,h4("女性：95%信頼区間"),verbatimTextOutput("confidenceIntervalFemale")),
                  column(4,h4("全体：95%信頼区間"),verbatimTextOutput("confidenceIntervalAll"))
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
      drugM <- gg3$識別番号[which(gg3$薬剤名 == selected_drug & gg3$性別 == "男性")]
      drugW <- gg3$識別番号[which(gg3$薬剤名 == selected_drug & gg3$性別 == "女性")]
      drugA <- union(drugM ,drugW)
      reacM <- reac2$識別番号[which(reac2$有害事象 %in% selected_event & reac2$性別 == "男性")]
      reacW <- reac2$識別番号[which(reac2$有害事象 %in% selected_event & reac2$性別 == "女性")]
      reacA <- union(reacM, reacW)

#      drug_amazon <- gg3 %>%
#        filter(薬剤名 == selected_drug)
#      drugM <- drug_amazon %>%
#        filter(性別 == "男性") %>%
#        pull(識別番号)
#      drugW <- drug_amazon %>%
#        filter(性別 == "女性") %>%
#        pull(識別番号)
#      drugA <- c(drugM, drugW)
     
#      reac_amazon <- reac2[which(reac2$有害事象 %in% selected_event), ]
#      reacM <- reac_amazon$識別番号[which(reac2$性別 == "男性")]       
#      reacW <- reac_amazon$識別番号[which(reac2$性別 == "女性")]
#      reacA <- c(reacM, reacW)
     
     
     # selected_eventが複数選択されている場合
#      if (length(selected_event) > 1) {
#        reacA <- reacM <- reacW <- as.numeric(0)  
#        
#        for (event in selected_event) {
#          ad <- as.character(reac2$有害事象)
#          reac_amazon <- reac2 %>%
#            filter(ad == event)
#          reacM_event <- reac_amazon %>%
#            filter(性別 == "男性") %>%
#            pull(識別番号)
#          reacW_event <- reac_amazon %>%
#            filter(性別 == "女性") %>%
#            pull(識別番号)
#          reacM <- union(reacM, reacM_event)  
#          reacW <- union(reacW, reacW_event)  
#          reacA <- union(reacA, c(reacM_event, reacW_event))  
#        }
#      } else {
#        ad <- as.character(reac2$有害事象)
#        reac_amazon <- reac2 %>%
#          filter(ad == selected_event)
#        reacM <- reac_amazon %>%
#          filter(性別 == "男性") %>%
#          pull(識別番号)
#        reacW <- reac_amazon %>%
#          filter(性別 == "女性") %>%
#          pull(識別番号)
#        reacA <- c(reacM, reacW)
#      }

      
      # 男ークロス表を作成・表示
      cross_tableMale <- matrix(c(sum(reacM %in% drugM), length(drugM) - sum(reacM %in% drugM), length(reacM) - sum(reacM %in% drugM), sum(demo2$性別 == "男性") - length(drugM) - length(reacM) + sum(reacM %in% drugM)), nrow = 2, byrow = TRUE)
      rownames(cross_tableMale) <- c("服用◯", "服用❌")
      colnames(cross_tableMale) <- c("有害事象◯", "有害事象❌")
      output$crossTableMale <- renderTable(cross_tableMale, rownames = TRUE)
      # 女ークロス表を作成・表示　
      cross_tableFemale <- matrix(c(sum(reacW %in% drugW), length(drugW) - sum(reacW %in% drugW), length(reacW) - sum(reacW %in% drugW), sum(demo2$性別 == "女性") - length(drugW) - length(reacW) + sum(reacW %in% drugW)), nrow = 2, byrow = TRUE)
      rownames(cross_tableFemale) <- c("服用◯", "服用❌")
      colnames(cross_tableFemale) <- c("有害事象◯", "有害事象❌")
      output$crossTableFemale <- renderTable(cross_tableFemale, rownames = TRUE)
      # 全体ークロス表を作成・表示　
      cross_tableAll <- matrix(c(sum(reacA %in% drugA), length(drugA) - sum(reacA %in% drugA), length(reacA) - sum(reacA %in% drugA), {sum(demo2$性別 == "男性") + sum(demo2$性別 == "女性")} - length(drugA) - length(reacA) + sum(reacA %in% drugA)), nrow = 2, byrow = TRUE)
      rownames(cross_tableAll) <- c("服用◯", "服用❌")
      colnames(cross_tableAll) <- c("有害事象◯", "有害事象❌")
      output$crossTableAll <- renderTable(cross_tableAll, rownames = TRUE)
      
      # 男ーオッズ比を計算・表示
      odds_ratioMale <- (sum(reacM %in% drugM) * (sum(demo2$性別 == "男性")-length(drugM)-length(reacM) + sum(reacM %in% drugM))) / ((length(drugM) - sum(reacM %in% drugM)) * (length(reacM) - sum(reacM %in% drugM)))
      output$oddsRatioMale <- renderText(paste("オッズ比:", sprintf("%.3f",odds_ratioMale)))
      # 女ーオッズ比を計算・表示
      odds_ratioFemale <- (sum(reacW %in% drugW) * (sum(demo2$性別 == "女性") - length(drugW) - length(reacW) + sum(reacW %in% drugW))) / ((length(drugW) - sum(reacW %in% drugW)) * (length(reacW) - sum(reacW %in% drugW)))
      output$oddsRatioFemale <- renderText(paste("オッズ比:", sprintf("%.3f",odds_ratioFemale)))
      # 全体ーオッズ比を計算・表示
      odds_ratioAll <- (sum(reacA %in% drugA) * ({sum(demo2$性別 == "男性") + sum(demo2$性別 == "女性")} - length(drugA) - length(reacA) + sum(reacA %in% drugA))) / ((length(drugA) - sum(reacA %in% drugA)) * (length(reacA) - sum(reacA %in% drugA)))
      output$oddsRatioAll <- renderText(paste("オッズ比:", sprintf("%.3f",odds_ratioAll)))
      
      #男ー95%信頼区間の計算・表示
      conf.lowMale  <- odds_ratioMale  -  exp(1.96 * sqrt(1/sum(reacM %in% drugM) + 1/(sum(demo2$性別 == "男性") - length(drugM) - length(reacM) + sum(reacM %in% drugM)) + 1/(length(drugM) - sum(reacM %in% drugM)) + 1/(length(reacM) - sum(reacM %in% drugM))))
      conf.highMale <- odds_ratioMale  +  exp(1.96 * sqrt(1/sum(reacM %in% drugM) + 1/(sum(demo2$性別 == "男性") - length(drugM) - length(reacM) + sum(reacM %in% drugM)) + 1/(length(drugM) - sum(reacM %in% drugM)) + 1/(length(reacM) - sum(reacM %in% drugM))))
      output$confidenceIntervalMale <- renderText({
        paste("下限:", sprintf("%.3f",conf.lowMale)," "  , "上限:", sprintf("%.3f",conf.highMale))})
      #女ー95%信頼区間の計算・表示
      conf.lowFemale  <- odds_ratioMale  -  exp(1.96 * sqrt(1/sum(reacW %in% drugW) + 1/(sum(demo2$性別 == "女性") - length(drugW) - length(reacW) + sum(reacW %in% drugW)) + 1/(length(drugW) - sum(reacW %in% drugW)) + 1/(length(reacW) - sum(reacW %in% drugW))))
      conf.highFemale <- odds_ratioMale  +  exp(1.96 * sqrt(1/sum(reacW %in% drugW) + 1/(sum(demo2$性別 == "女性") - length(drugW) - length(reacW) + sum(reacW %in% drugW)) + 1/(length(drugW) - sum(reacW %in% drugW)) + 1/(length(reacW) - sum(reacW %in% drugW))))
      output$confidenceIntervalFemale <- renderText({
        paste("下限:", sprintf("%.3f",conf.lowFemale)," " , "上限:", sprintf("%.3f",conf.highFemale))})
      #全体ー95%信頼区間の計算・表示
      conf.lowAll  <- odds_ratioAll  -  exp(1.96 * sqrt(1/sum(reacA %in% drugA) + 1/({sum(demo2$性別 == "男性") + sum(demo2$性別 == "女性")} - length(drugA) - length(reacA) + sum(reacA %in% drugA)) + 1/(length(drugA) - sum(reacA %in% drugA)) + 1/(length(reacA) - sum(reacA %in% drugA))))
      conf.highAll <- odds_ratioAll  +  exp(1.96 * sqrt(1/sum(reacA %in% drugA) + 1/({sum(demo2$性別 == "男性") + sum(demo2$性別 == "女性")} - length(drugA) - length(reacA) + sum(reacA %in% drugA)) + 1/(length(drugA) - sum(reacA %in% drugA)) + 1/(length(reacA) - sum(reacA %in% drugA))))
      output$confidenceIntervalAll <- renderText({
        paste("下限:", sprintf("%.3f",conf.lowAll)," ","上限:", sprintf("%.3f",conf.highAll))})
      
      # 選択された薬剤名・有害事象を表示
      output$selectedDE <- renderText({
        paste("＜",selected_drug ,"＞", "　✖️　" ,"＜", 
              if (length(input$reac3) > 0) {
                selected_events <- paste("＜", input$drug3, "＞", "　✖️　", "＜", paste(input$reac3, collapse = " , "), "＞")
                return(selected_events)
              } else {
                return(NULL)
              }
              
              ,"＞")
      })
      
      
    })
    
  }
)

