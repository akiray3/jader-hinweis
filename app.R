#library
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(DT)
library(ggrepel)
library(openxlsx)
library(plotly)

################################################################################
####ui
ui <- dashboardPage(
  skin = "black",
    dashboardHeader(title = 'Jader Hinweis'),
    dashboardSidebar(
      collapsed = TRUE,
      sidebarMenu(
        menuItem("Home",tabName = "home",icon = icon("house")),
        menuItem("Page1",tabName = "page1",icon = icon("chart-line")),
        menuItem("Page2",tabName = "page2",icon = icon("magnifying-glass-chart"))
      )
    ),
    dashboardBody(
      tabItems(
                tabItem(tabName = "home",
                        tags$link(rel = "stylesheet",type = "text/css",href = "pagestyle.css"),
                        htmlTemplate("page.html")
                ),
                tabItem(tabName = "page1",
                        tags$link(rel = "stylesheet",type = "text/css",href = "style.css"),
                        tabsetPanel(
                          tabPanel(
                            "figa_M",
                            class = "tab",
                            fluidRow(tags$div(
                              div(class = "spacetop"))),
                            fluidRow(
                              box(width = 12,solidHeader = TRUE,status ="primary",title = "Map",
                                  plotly::plotlyOutput("plot01am"),
                                  downloadButton(outputId = "downloadplot01am",label = "Download",icon = icon("download"),class = "downloadbutton"),
                                  div(class = "space"),
                                  h2("click info",class = "title"),
                                  uiOutput("clpointinfo01am"),
                                  uiOutput("cldownload01am"),
                                  uiOutput("cldbt01am")
                                  )
                              ),
                            fluidRow(tags$div(
                              div(class = "space")
                            )),
                            fluidRow(
                              box(width = 12,solidHeader = TRUE,status = "primary",title = "Map info",
                                  DT::dataTableOutput(outputId = "datatableoutput01am",width = "auto"),
                                  selectInput(inputId = "select01am",label = "ファイル形式",choices = list(".xlsx",".csv"),selected = 1),
                                  uiOutput(outputId = "download01am")
                                  )
                              )
                          ),
                          tabPanel(
                            "figa_F",
                            class = "tab",
                            fluidRow(tags$div(
                              div(class = "spacetop"))),
                            fluidRow(
                              box(width = 12,solidHeader = TRUE,status = "primary",title = "Map",
                                  plotly::plotlyOutput("plot01af"),
                                  downloadButton(outputId = "downloadplot01af",label = "Download",icon = icon("download"),class = "downloadbutton"),
                                  div(class = "space"),
                                  h2("click info",class = "title"),
                                  uiOutput("clpointinfo01af"),
                                  uiOutput("cldownload01af"),
                                  uiOutput("cldbt01af")
                                  )
                              ),
                            fluidRow(tags$div(
                              div(class = "space")
                            )),
                            fluidRow(
                              box(width = NULL,solidHeader = TRUE,status = "primary",title = "Map info",
                                  column(width = 12,div(DT::dataTableOutput(outputId = "datatableoutput01af"))),
                                  selectInput(inputId = "select01af",label = "ファイル形式",choices = list(".xlsx",".csv"),selected = 1),
                                  uiOutput(outputId = "download01af")
                              ) 
                              )
                          ),
                          tabPanel(
                            "figb_M",
                            class = "tab",
                            fluidRow(tags$div(
                              div(class = "spacetop"))),
                            fluidRow(
                              box(width = 12,solidHeader = TRUE,status = "primary",title = "Map",
                                  plotly::plotlyOutput("plot01bm"),
                                  downloadButton(outputId = "downloadplot01bm",label = "Download",icon = icon("download"),class = "downloadbutton"),
                                  div(class = "space"),
                                  h2("click info",class = "title"),
                                  uiOutput("clpointinfo01bm"),
                                  uiOutput("cldownload01bm"),
                                  uiOutput("cldbt01bm")
                                  )
                              ),
                            fluidRow(tags$div(
                              div(class = "space")
                            )),
                            fluidRow(tags$div(
                              box(width = NULL,solidHeader = TRUE,status = "primary",title = "Map info",
                                  column(width = 12,div(DT::dataTableOutput(outputId = "datatableoutput01bm"))),
                                  selectInput(inputId = "select01bm",label = "ファイル形式",choices = list(".xlsx",".csv"),selected = 1),
                                  uiOutput(outputId = "download01bm")
                                  )
                              ))
                          ),
                          tabPanel(
                            "figb_F",
                            class = "tab",
                            fluidRow(tags$div(
                              div(class = "spacetop"))),
                            fluidRow(
                              box(width = 12,solidHeader = TRUE,status = "primary",title = "Map",
                                  plotly::plotlyOutput("plot01bf"),
                                  downloadButton(outputId = "downloadplot01bf",label = "Download",icon = icon("download"),class = "downloadbutton"),
                                  div(class = "space"),
                                  h2("click info",class = "title"),
                                  uiOutput("clpointinfo01bf"),
                                  uiOutput("cldownload01bf"),
                                  uiOutput("cldbt01bf")
                              )
                              ),
                            fluidRow(tags$div(
                              div(class = "space")
                            )),
                            fluidRow(
                              box(width = NULL,solidHeader = TRUE,status = "primary",title = "Map info",
                                  column(width = 12,div(DT::dataTableOutput(outputId = "datatableoutput01bf"))),
                              selectInput(inputId = "select01bf",label = "ファイル形式",choices = list(".xlsx" ,".csv"),selected = 1),
                              uiOutput(outputId = "download01bf") 
                              )
                              )
                              )
                        )),
                tabItem(tabName = "page2",
                        tags$link(rel = "stylesheet",type = "text/css",href = "style.css"),
                        fluidRow(
                              div(
                                class = "pg2txt",
                                p("全てのデータを確認することができます。",class = "page2t"),
                                tags$br(),
                                p("列名下の検索ボックスで絞り込み、ダウンロードが可能です。",class = "page2t")
                              ),
                              tags$div(column(width = 12,div(DT::dataTableOutput(outputId = "detailoutput")))),
                              selectInput(inputId = "selectdetail",label = "ファイル形式",choices = list(".xlsx" ,".csv"),selected = 1),
                              uiOutput(outputId = "downloaddetail")
                            )
                        )
              ))
      
    )




################################################################################
####server
server <- function(input, output) {
  
  aesum <- reactive({readRDS("data/20230304_all")})
  detail <- reactive({readRDS("data/20230304_detail")})

  ##tab1 
  #Map
  #有害事象の分類マップその１　男性
  output$plot01am <- plotly::renderPlotly({
     plotfig01_am <- source(file = file.path("server_scripts/plot_fig01a_m.R"),local = TRUE)$value
     p <- plotly::ggplotly(plotfig01_am,source = "A")
     event_register(p,event = 'plotly_click')
     p%>%config(scrollZoom = TRUE,displaylogo = FALSE)
  })
  #click info download
  #有害事象分類マップ　クリックしたときに、対象のデータが絞られる。
  #ページごとに男女で分かれているので、男女別かつクリックした有害事象のデータ
  #その時のダウンロード機構
  

  observeEvent(event_data("plotly_click",source = "A"),
               { 
                 edata <- event_data("plotly_click",source = "A")
               key <- aesum()%>%filter(多剤併用OR %in% edata$x)%>%filter(高齢OR %in% edata$y)
               output$clpointinfo01am <- renderUI({DT::renderDataTable(
                 datatable(detail()%>% filter(性別 == "男性")%>%filter(有害事象 %in% key$VAL),
                           option = list(responsive = TRUE,scrollX = TRUE,autoWidth = TRUE)) 
                 )})
               }) 
  
  output$cldownload01am <- renderUI({
    req(event_data("plotly_click",source = "A"))
    selectInput(inputId = "clplot01am",label = "ファイル形式",choices = list(".xlsx" ,".csv"),selected = 1)
  })
  output$cldbt01am <- renderUI({
    req(event_data("plotly_click",source = "A"))
    switch(
      input$clplot01am,
      ".xlsx" = downloadButton(".xlsxam",label = "Download",icon = icon("download"),class = "downloadbutton"),
      ".csv" = downloadButton(".csvam", label = "Download",icon = icon("download"),class = "downloadbutton")
    )
  })
  observeEvent(event_data("plotly_click",source = "A"),{
  edata <- event_data("plotly_click",source = "A")
  key <- aesum()%>%filter(多剤併用OR %in% edata$x)%>%filter(高齢OR %in% edata$y)
  Sys.sleep(1)
    cldetail <- detail()%>% filter(性別 == "男性")%>%filter(有害事象 %in% key$VAL)
    output$.xlsxam <- downloadHandler(
      filename = function(){
        paste("cldata01am",Sys.Date(),".xlsx",sep = "")
      },
      content = function(file){
        write.xlsx(cldetail,file)
      }
    )
    output$.csvam <- downloadHandler(
      filename = function(){
        paste("cldata01am",Sys.Date(),".csv",sep = "")
      },
      content = function(file){
        write.csv(cldetail,file,
                  fileEncoding = "CP932")
      }
    )
  })
  #Map
  #有害事象分類マップ download
  output$downloadplot01am <- downloadHandler(
    filename = function() {
      paste("plot01am", Sys.Date(), ".png", sep="")
    },
    content = function(file){
      ggplot2::ggsave(
        plot = source(file = file.path("server_scripts/plot_fig01a_m.R"),local = TRUE)$value, 
        filename = file, 
        width = 10 * sqrt(2), height = 10,
        device = "png"
      )
    }
  )
  
  #Map info
  #有害事象分類マップを構成するデータ download
  #selectボタンでexcelか、csvかを選択できる
  output$datatableoutput01am <- DT::renderDataTable(datatable(aesum()%>%filter(性別 == "男性")%>%select(-"性別"),
                                                              option = list(responsive = TRUE,scrollX = TRUE,autoWidth = TRUE)))
  
  output$download01am <- renderUI({
    switch(
      input$select01am,
    ".xlsx" = downloadButton(".xlsxam2",label = "Download",icon = icon("download"),class = "downloadbutton"),
    ".csv" = downloadButton(".csvam2", label = "Download",icon = icon("download"),class = "downloadbutton")
    )
  })
  
  output$.xlsxam2 <- downloadHandler(
    filename = function(){
    paste("table01am",Sys.Date(),".xlsx",sep = "")
      },
  content = function(file){
    write.xlsx(aesum()%>%filter(性別 == "男性")%>%select(-"性別"),file)
  }
  )
  
  output$.csvam2 <- downloadHandler(
    filename = function(){
      paste("table01am",Sys.Date(),".csv",sep = "")
    },
    content = function(file){
      write.csv(aesum()%>%filter(性別 == "男性")%>%select(-"性別"),file,
                fileEncoding = "CP932")
    }
  )
  
  #tab2 女性
  output$plot01af <- plotly::renderPlotly({
    plotfig01_af <- source(file = file.path("server_scripts/plot_fig01a_f.R"),local = TRUE)$value
    paf <- plotly::ggplotly(plotfig01_af,source = "B") 
    event_register(paf,event = 'plotly_click')
    paf%>%config(scrollZoom = TRUE,displaylogo = FALSE)
  })
  
  #
  
  observeEvent(event_data("plotly_click",source = "B"),{
    output$clpointinfo01af <- renderUI({
      edataaf <- event_data("plotly_click",source = "B")
      keyaf <- aesum()%>%filter(多剤併用OR %in% edataaf$x)%>%filter(高齢OR %in% edataaf$y)
      DT::renderDataTable(
        datatable(detail()%>% filter(性別 == "女性")%>%filter(有害事象 %in% keyaf$VAL),
                  option = list(responsive = TRUE,scrollX = TRUE,autoWidth = TRUE)) 
      )})
  })
   
              
  
  output$cldownload01af <- renderUI({
    req(event_data("plotly_click",source = "B"))
    selectInput(inputId = "clplot01af",label = "ファイル形式",choices = list(".xlsx" ,".csv"),selected = 1)
  })
  output$cldbt01af <- renderUI({
    req(event_data("plotly_click",source = "B"))
    switch(
      input$clplot01af,
      ".xlsx" = downloadButton(".xlsxaf",label = "Download",icon = icon("download"),class = "downloadbutton"),
      ".csv" = downloadButton(".csvaf", label = "Download",icon = icon("download"),class = "downloadbutton")
    )
  })

  observeEvent(event_data("plotly_click",source = "B"),{
    edataaf <- event_data("plotly_click",source = "B")
    keyaf <- aesum()%>%filter(多剤併用OR %in% edataaf$x)%>%filter(高齢OR %in% edataaf$y)
    cldetailaf <-detail() %>% filter(性別 == "女性") %>% filter(有害事象 %in% keyaf$VAL)
    output$.xlsxaf <- downloadHandler(
      filename = function(){
        paste("cldata01af",Sys.Date(),".xlsx",sep = "")
      },
      content = function(file){
        write.xlsx(cldetailaf,file)
      }
      )
    output$.csvaf <- downloadHandler(
      filename = function(){
        paste("cldata01af",Sys.Date(),".csv",sep = "")
      },
      content = function(file){
        write.csv(cldetailaf,file,
                  fileEncoding = "CP932")
      }
      )
  })


  #  
  output$downloadplot01af <- downloadHandler(
    filename = function() {
      paste("plot01af", Sys.Date(), ".png", sep="")
    },
    content = function(file){
      ggplot2::ggsave(
        plot = source(file = file.path("server_scripts/plot_fig01a_f.R"),local = TRUE)$value, 
        filename = file, 
        width = 10 * sqrt(2), height = 10,
        device = "png"
      )
    }
  )
  
  output$datatableoutput01af <- DT::renderDataTable(datatable(aesum()%>%filter(性別 == "女性")%>%select(-"性別"),
                                                              option = list(responsive = TRUE,scrollX = TRUE,autoWidth = TRUE)))
  
  output$download01af<- renderUI({
    switch(
      input$select01af,
      ".xlsx" = downloadButton(".xlsxaf2",label = "Download",icon = icon("download"),class = "downloadbutton"),
      ".csv" = downloadButton(".csvaf2", label = "Download",icon = icon("download"),class = "downloadbutton")
    )
  })
  
  output$.xlsxaf2 <- downloadHandler(
    filename = function(){
      paste("table01af",Sys.Date(),".xlsx",sep = "")
    },
    content = function(file){
      write.xlsx(aesum()%>%filter(性別 == "女性")%>%select(-"性別"),file)
    }
  )
  
  output$.csvaf2 <- downloadHandler(
    filename = function(){
      paste("table01af",Sys.Date(),".csv",sep = "")
    },
    content = function(file){
      write.csv(aesum()%>%filter(性別 == "女性")%>%select(-"性別"),file,
                fileEncoding = "CP932")
    }
  )
  
  
  #tab3 男性
  #
  output$plot01bm <-plotly::renderPlotly({
    plotfig01_bm <- source(file = file.path("server_scripts/plot_fig01b_m.R"),local = TRUE)$value
    pbm <- plotly::ggplotly(plotfig01_bm,source = "C") 
    event_register(pbm,event = 'plotly_click')
    pbm%>%config(scrollZoom = TRUE,displaylogo = FALSE)
  })
 #
  
  observeEvent(event_data("plotly_click",source = "C"),
              {
                edatabm <- event_data("plotly_click",source = "C")
                keybm <-aesum()%>%filter(平均併用薬剤 %in% edatabm$x)%>%filter(平均発症年齢 %in% edatabm$y)
                output$clpointinfo01bm <- renderUI({DT::renderDataTable(
                  datatable(detail()%>% filter(性別 == "男性")%>%filter(有害事象 %in% keybm$VAL),
                            option = list(responsive = TRUE,scrollX = TRUE,autoWidth = TRUE)) 
                )})
              })
 
  output$cldownload01bm <- renderUI({
    req(event_data("plotly_click",source = "C"))
    selectInput(inputId = "clplot01bm",label = "ファイル形式",choices = list(".xlsx" ,".csv"),selected = 1)
  })
  output$cldbt01bm <- renderUI({
    req(event_data("plotly_click",source = "C"))
    switch(
      input$clplot01bm,
      ".xlsx" = downloadButton(".xlsxbm",label = "Download",icon = icon("download"),class = "downloadbutton"),
      ".csv" = downloadButton(".csvbm", label = "Download",icon = icon("download"),class = "downloadbutton")
    )
  })
 
 observeEvent(input$clplot01bm,{
   edatabm <- event_data("plotly_click",source = "C")
   keybm <- aesum()%>%filter(平均併用薬剤 %in% edatabm$x)%>%filter(平均発症年齢 %in% edatabm$y)
   cldetailbm <- detail()%>% filter(性別 == "男性")%>%filter(有害事象 %in% keybm$VAL)
   output$.xlsxbm <- downloadHandler(
     filename = function(){
       paste("cldata01bm",Sys.Date(),".xlsx",sep = "")
     },
     content = function(file){
       write.xlsx(cldetailbm,file)
     }
   )
   output$.csvbm <- downloadHandler(
     filename = function(){
       paste("cldata01bm",Sys.Date(),".csv",sep = "")
     },
     content = function(file){
       write.csv(cldetailbm,file,
                 fileEncoding = "CP932")
     }
   )
  })
 
  #
  output$downloadplot01bm <- downloadHandler(
    filename = function() {
      paste("plot01bm", Sys.Date(), ".png", sep="")
    },
    content = function(file){
      ggplot2::ggsave(
        plot = source(file = file.path("server_scripts/plot_fig01b_m.R"),local = TRUE)$value, 
        filename = file, 
        width = 10 * sqrt(2), height = 10,
        device = "png"
      )
    }
  )
 
  
  output$datatableoutput01bm <- DT::renderDataTable(datatable(aesum()%>%filter(性別 == "男性")%>%select(-"性別"),
                                                              option = list(responsive = TRUE,scrollX = TRUE,autoWidth = TRUE)))
  output$download01bm<- renderUI({
    switch(
      input$select01bm,
      ".xlsx" = downloadButton(".xlsxbm2",label = "Download",icon = icon("download"),class = "downloadbutton"),
      ".csv" = downloadButton(".csvbm2", label = "Download",icon = icon("download"),class = "downloadbutton")
    )
  })
  
  output$.xlsxbm2 <- downloadHandler(
    filename = function(){
      paste("table01bm",Sys.Date(),".xlsx",sep = "")
    },
    content = function(file){
      write.xlsx(aesum()%>%filter(性別 == "男性")%>%select(-"性別"),file)
    }
  )
  
  output$.csvbm2 <- downloadHandler(
    filename = function(){
      paste("table01bm",Sys.Date(),".csv",sep = "")
    },
    content = function(file){
      write.csv(aesum()%>%filter(性別 == "男性")%>%select(-"性別"),file,
                fileEncoding = "CP932")
    }
  )
  
  
  #tab4 女性
  output$plot01bf <- plotly::renderPlotly({
    plotfig01_bf <- source(file = file.path("server_scripts/plot_fig01b_f.R"),local = TRUE)$value
    pbf <- plotly::ggplotly(plotfig01_bf,source = "D") 
    event_register(pbf,'plotly_click')
    pbf%>%config(scrollZoom = TRUE,displaylogo = FALSE)
  }) 
  
  observeEvent(event_data("plotly_click",source = "D"),
               {edata <- event_data("plotly_click",source = "D")
               key <- aesum()%>%filter(平均併用薬剤 %in% edata$x)%>%filter(平均発症年齢 %in% edata$y)
               output$clpointinfo01bf <- renderUI({DT::renderDataTable(
                 datatable(detail()%>% filter(性別 == "女性")%>%filter(有害事象 %in% key$VAL),
                           option = list(responsive = TRUE,scrollX = TRUE,autoWidth = TRUE)) 
               )})
               })
  
  
 output$cldownload01bf <- renderUI({
   req(event_data("plotly_click",source = "D"))
   selectInput(inputId = "clplot01bf",label = "ファイル形式",choices = list(".xlsx" ,".csv"),selected = 1)
 })
 output$cldbt01bf <- renderUI({
   req(event_data("plotly_click",source = "D"))
   switch(
     input$clplot01bf,
     ".xlsx" = downloadButton(".xlsxbf",label = "Download",icon = icon("download"),class = "downloadbutton"),
     ".csv" = downloadButton(".csvbf", label = "Download",icon = icon("download"),class = "downloadbutton")
   )
 })

  
  observeEvent(event_data("plotly_click",source = "D"),{
    edatabf <- event_data("plotly_click",source = "D")
    keybf <- aesum()%>%filter(平均併用薬剤 %in% edatabf$x)%>%filter(平均発症年齢 %in% edatabf$y)
    cldetailbf <- detail()%>% filter(性別 == "女性")%>%filter(有害事象 %in% keybf$VAL)
    output$.xlsxbf <- downloadHandler(
      filename = function(){
        paste("cldata01bf",Sys.Date(),".xlsx",sep = "")
      },
      content = function(file){
        write.xlsx(cldetailbf,file)
      }
    )
    
    output$.csvbf <- downloadHandler(
      filename = function(){
        paste("cldata01bf",Sys.Date(),".csv",sep = "")
      },
      content = function(file){
        write.csv(cldetailbf,file,
                  fileEncoding = "CP932")
      }
    )
  })
  #
  output$downloadplot01bf <- downloadHandler(
    filename = function() {
      paste("plot01bf", Sys.Date(), ".png", sep="")
    },
    content = function(file){
      ggplot2::ggsave(
        plot = source(file = file.path("server_scripts/plot_fig01b_f.R"),local = TRUE)$value, 
        filename = file, 
        width = 10 * sqrt(2), height = 10,
        device = "png"
      )
    }
  )
  
  output$datatableoutput01bf <- DT::renderDataTable(datatable(aesum()%>%filter(性別 == "女性")%>%select(-"性別"),
                                                              option = list(responsive = TRUE,scrollX = TRUE,autoWidth = TRUE)))
  output$download01bf<- renderUI({
    switch(
      input$select01bf,
      ".xlsx" = downloadButton(".xlsxbf2",label = "Download",icon = icon("download"),class = "downloadbutton"),
      ".csv" = downloadButton(".csvbf2", label = "Download",icon = icon("download"),class = "downloadbutton")
    )
  })
  
  output$.xlsxbf2 <- downloadHandler(
    filename = function(){
      paste("table01bf",Sys.Date(),".xlsx",sep = "")
    },
    content = function(file){
      write.xlsx(aesum()%>%filter(性別 == "女性")%>%select(-"性別"),file)
    }
  )
  
  output$.csvbf2 <- downloadHandler(
    filename = function(){
      paste("table01bf",Sys.Date(),".csv",sep = "")
    },
    content = function(file){
      write.csv(aesum()%>%filter(性別 == "女性")%>%select(-"性別"),file,
                fileEncoding = "CP932")
    }
  )
  
  #######
  #dashbpard page2
  output$detailoutput <- DT::renderDataTable(datatable(detail(), filter = 'top',
                                                       option = list(autoFill = TRUE,
                                                                     responsive = TRUE,scrollX = TRUE,autoWidth = TRUE,
                                                                     searchBuilder = list(enterSearch = TRUE),
                                                                     searchPanes = list(il8n = list(loadMessage = "loading...")))))

  #data download
  output$downloaddetail<- renderUI({
    switch(
      input$selectdetail,
      ".xlsx" = downloadButton(".xlsxdt",label = "Download",icon = icon("download"),class = "downloadbutton"),
      ".csv" = downloadButton(".csvdt", label = "Download",icon = icon("download"),class = "downloadbutton")
    )
  })
  
  output$.xlsxdt <- downloadHandler(
    filename = function(){
      paste("detail",Sys.Date(),".xlsx",sep = "")
    },
    content = function(file){
      r <- input$detailoutput_rows_all
      write.xlsx(detail()[r,],file)
    }
  )
  
  output$.csvdt <- downloadHandler(
    filename = function(){
      paste("detail",Sys.Date(),".csv",sep = "")
    },
    content = function(file){
      r <- input$detailoutput_rows_all
      write.csv(detail()[r,],file,
                fileEncoding = "CP932")
    }
  )
}

shinyApp(ui = ui, server = server)
# runApp("/Users/akira/OneDrive - 城西大学/apiwebjader")