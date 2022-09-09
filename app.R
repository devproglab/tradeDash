####
library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(zoo)
library(dbplyr)
library(DBI)
library(rlang)
library(lubridate)
library(DT)
library(data.table)
library(shinyjs)
library(shinycssloaders)
library(glue)

options(scipen=999)
shinyOptions(cache = cachem::cache_disk("./cache/"))
upd <- file.info('df-10-znak.sqlite')$mtime


options(spinner.color = "#006272")
country_list <- readRDS('countryList.RDS')
goods_list_2 <- readRDS('goods_list_2.RDS')
goods_list_4 <- readRDS('goods_list_4.RDS')
goods_list_6 <- readRDS('goods_list_6.RDS')
tradehistplot <- readRDS('tradehistplot.RDS')
dataAvailIm <- readRDS('dataAvailIm.RDS')
dataAvailEx <- readRDS('dataAvailEx.RDS')

compare21 <- readRDS('tradeShares.RDS')
ratioIm <- round(compare21$ratio[1], 1)
ratioEx <- round(compare21$ratio[2], 1)


img_urls <- paste0(
  '<img src="https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/',
  tolower(dataAvailIm$STRANA), '.svg" height=15>'
)


dataAvailIm$FLAG <- img_urls
dataAvailEx$FLAG <- img_urls
dataAvailIm <- dataAvailIm %>%
  relocate(FLAG, .after = STRANA)
dataAvailEx <- dataAvailEx %>%
  relocate(FLAG, .after = STRANA)



header <- dashboardHeader(title = "Header")

sidebar <- dashboardSidebar(
  div(airMonthpickerInput(
    inputId = "chosenMonth",
    label = "Выбранный месяц:",
    value = as.Date('2022-05-01')
  )),
  
  sidebarMenu(
    id = 'sidebar',
    
    # Главная страница
    menuItem(
      "Общие сведения",
      tabName = 'dashboard',
      icon = icon("dashboard")
    ),
    
    # Страница с выбором стран
    menuItem(
      "Данные по странам",
      tabName = 'country_intel',
      icon = icon('globe')
    ),
    div(
      conditionalPanel("input.sidebar === 'country_intel'",
                       selectizeInput(
                         "select_country",
                         "Выберите необходимые страны",
                         choices =  country_list,
                         selected = NULL,
                         width = "200px",
                         multiple = T
                       ),
                       actionButton(
                         'btn_build_country_report',
                         paste0('Получить данные'),
                         icon = icon('wrench')
                       )
      )
    ),
    useShinyjs(),
    
    # Страница с выбором товаров
    menuItem(
      "Данные по товарам",
      tabName = 'com_intel',
      icon = icon("th")
    ),
    div(
      conditionalPanel("input.sidebar === 'com_intel'",
                       radioButtons("tnvedCheckbox", "Выберите уровень группировки:",
                                    c("2 знака", "4 знака", "6 знаков")),
                       selectizeInput(
                         "select_commodity",
                         "Выберите необходимые коды товаров",
                         choices =  goods_list_2,
                         selected = NULL,
                         width = "200px",
                         multiple = T
                       ),
                       actionButton(
                         'btn_build_comm_report',
                         paste0('Получить данные'),
                         icon = icon('wrench')
                       )
      )
    ),
    
    # Страница экспорта данных
    menuItem(
      "Aгрегирование данных",
      tabName = 'aggregate_data',
      icon = icon('globe')
    ),
    div(
      conditionalPanel("input.sidebar === 'aggregate_data'",
                       selectizeInput(
                         "strana_on_off",
                         "Выберите страны:",
                         choices =  country_list,
                         selected = NULL,
                         width = "200px",
                         multiple = T
                       ),
                       radioButtons("aggstrCheckbox", "Aгрегировать данные по странам",
                                      c("да", "нет")),
                       radioButtons("codeCheckbox", "Выберите уровень группировки:",
                                    c("2 знака", "4 знака", "6 знаков")),
                       selectizeInput(
                         "tnved_on_off",
                         "Выберите товары:",
                         choices =  goods_list_2,
                         selected = NULL,
                         width = "200px",
                         multiple = T
                       ),
                       radioButtons("aggcomCheckbox", "Aгрегировать данные по товарам",
                                    c("да", "нет")),
                       # radioButtons("addftsCheckbox", "Добавить ФТС данные к зеркальным",
                       #              c("да", "нет")),
                       actionButton(
                         'btn_build_agg_report',
                         paste0('Получить данные'),
                         icon = icon('wrench')
                       )
                       
      )
    ),
    menuItem("Диагностика",
             tabName = 'diag',
             icon=icon('th')),
    useShinyjs()
    
  )
)

body <- dashboardBody(tabItems(
  tabItem(
    tabName = "dashboard",
    
    ## Главная страница
    # Организация body
    h2("Внешняя торговля России") ,
    fluidRow(
      column(3,
             valueBox(
               paste0(ratioEx, '%'),
               'Покрытие объемов торговли, экспорт',
               icon = icon('info-sign', lib = 'glyphicon'),
               color = 'blue',
               width=12
             ),
             valueBox(
               paste0(ratioIm, '%'),
               'Покрытие объемов торговли, импорт',
               icon = icon('info-sign', lib = 'glyphicon'),
               color = 'blue',
               width=12
             )
      ),
      column(3,
             valueBoxOutput("currentMonthEx", width = 12) %>% withSpinner(),
             valueBoxOutput("currentMonthIm", width = 12),
      ),
      column(3,
             valueBoxOutput("monthgrowthEx", width = 12) %>% withSpinner(),
             valueBoxOutput("monthgrowthIm", width = 12)      
      ),
      column(3,
             valueBoxOutput("yeargrowthEx", width = 12) %>% withSpinner(),
             valueBoxOutput("yeargrowthIm", width = 12)
      )
    ),
    
    
    h2("Динамика внешней торговли") ,
    fluidRow(
      # tradehistplot
      plotlyOutput('tradeHistoryPlots')
      ) %>% withSpinner()
  ),
  
  
  
  tabItem(
    tabName = "country_intel",
    
    ## Страница с выбором стран
    # Организация body
    h2("Внешняя торговля России в разрезе по странами") ,
    
    fluidRow(
      column(4,
             valueBoxOutput("exactcurrentMonthEx", width = 12) %>% withSpinner(),
             valueBoxOutput("exactcurrentMonthIm", width = 12),
      ),
      column(4,
             valueBoxOutput("exactmonthgrowthEx", width = 12) %>% withSpinner(),
             valueBoxOutput("exactmonthgrowthIm", width = 12)      
      ),
      #yeargrowthEx
      column(4,
             valueBoxOutput("exactyeargrowthEx", width = 12) %>% withSpinner(),
             valueBoxOutput("exactyeargrowthIm", width = 12)
      )
      
    ),

    h2("Динамика внешней торговли") ,
    fluidRow(plotlyOutput('exacttradeHistoryPlots')) %>% withSpinner()
    
  ),
  
  ## Страница с выбором товаров
  # Организация body
  tabItem(
    tabName = "com_intel",
    
    h2("Внешняя торговля России в разрезе по товарам") ,
    
    fluidRow(
      column(4,
             valueBoxOutput("tnvedcurrentMonthEx", width = 12) %>% withSpinner(),
             valueBoxOutput("tnvedcurrentMonthIm", width = 12),
      ),
      column(4,
             valueBoxOutput("tnvedmonthgrowthEx", width = 12) %>% withSpinner(),
             valueBoxOutput("tnvedmonthgrowthIm", width = 12)      
      ),
      #yeargrowthEx
      column(4,
             valueBoxOutput("tnvedyeargrowthEx", width = 12) %>% withSpinner(),
             valueBoxOutput("tnvedyeargrowthIm", width = 12)
      )
      
    ),
    h2("Динамика внешней торговли") ,
    tabsetPanel(type='tabs',
                  tabPanel("Стоимостные объемы", plotlyOutput('stoim_tnvedtradeHistoryPlots')),
                  tabPanel("Физические объемы (основная ед. изм.)", plotlyOutput('netto_tnvedtradeHistoryPlots')),
                  tabPanel("Физические объемы (дополнительная ед. изм.)", plotlyOutput('kol_tnvedtradeHistoryPlots'))

      )
      # tabBox(
      #   #title = "First tabBox",
      #   # The id lets us use input$tabset1 on the server to find the current tab
      #   id = "tabset1", width = "250px",
      #   tabPanel(
      #     "Стоимостные объемы",
      #     plotlyOutput('stoim_tnvedtradeHistoryPlots')
      #   ),
      #   tabPanel(
      #     "Физические объемы (основная ед. изм.)",
      #      plotlyOutput('netto_tnvedtradeHistoryPlots')
      #   ),
      #   tabPanel(
      #     "Физические объемы (дополнительная ед. изм.)",
      #      plotlyOutput('kol_tnvedtradeHistoryPlots')
      #   )
      # )
    
    # %>% withSpinner()
  ),
  
  ## Страница с экспортом данных
  # Организация body
  tabItem(
    tabName = "aggregate_data",
    
    h2("Таблица, полученная из выбранных данных:"),
    fluidRow(
      DT::dataTableOutput("aggTable")
    )
    
  ),
  
  tabItem(
    tabName = 'diag',
    height=12,
    h2("Доступность данных"),
    h4(glue("Для каждой страны и года отражается число месяцев, данные по которым загружены в базу.
       В норме для всех годов, кроме {year(today())}, должно отображаться значение 12.
       ")),
    tabsetPanel(type = "tabs",
                         tabPanel("Импорт", 
                                  datatable(dataAvailIm, fillContainer = T, 
                                            options = list(dom='t', pageLength = 10000, scrollY = T, scrollX = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all")))
                                            , escape = FALSE) %>%
                                    formatStyle(
                                      names(dataAvailIm)[-c(1:2, ncol(dataAvailIm))],
                                      backgroundColor = styleInterval(11, c('red', 'green'))
                                    ) %>%
                                    formatStyle(names(dataAvailIm)[-c(1:2, ncol(dataAvailIm))], color="white")
                                  ),
                         tabPanel("Экспорт", 
                                  datatable(dataAvailEx, fillContainer = T, 
                                            options = list(dom='t', pageLength = 10000, scrollY = T, scrollX = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all")))
                                            , escape = FALSE) %>%
                                    formatStyle(
                                      names(dataAvailIm)[-c(1:2, ncol(dataAvailIm))],
                                      backgroundColor = styleInterval(11, c('red', 'green'))
                                    ) %>%
                                    formatStyle(names(dataAvailIm)[-c(1:2, ncol(dataAvailIm))], color="white")
                                  )
             ), style='height:100%'
             
    #          tabBox(width=12, height="100%",
    #   id='tabsetdiag',
    #   tabPanel("Импорт", ),
    #   tabPanel("Экспорт", ) 
    # )
    
  )
  
  
))


## ui.R ##
ui <- dashboardPage(dashboardHeader(),
                    dashboardSidebar(sidebar),
                    dashboardBody(
                      tags$head(
                        # tags$script('
                        #   // Define function to set height of "map" and "map_container"
                        #   setHeight = function() {
                        #     var window_height = $(window).height();
                        #     var header_height = $(".main-header").height();
                        #   
                        #     var boxHeight = window_height - header_height - 30;
                        #   
                        #     $("#diagcont").height(boxHeight);
                        #   };
                        #   
                        #   // Set input$box_height when the connection is established
                        #   $(document).on("shiny:connected", function(event) {
                        #     setHeight();
                        #   });
                        #   
                        #    // Refresh the box height on every window resize event    
                        #   $(window).on("resize", function(){
                        #     setHeight();
                        #   });'),
                        tags$style(HTML('
                                        .air-datepicker-global-container {z-index:99999}
                                        .content-wrapper {margin-left: 115px!important;} 
                                        .sidebar-toggle {display:none;} 
                                        .fa-calendar {display:none;}
                                        ' ))
                      ),
                      
                      body))


server <- function(input, output) {
  joinTables <- function(df) {
    df <- df %>%
      inner_join(napr,
                 by = c('NAPR' = 'id'),
                 suffix = c(".x", "")) %>%
      inner_join(strana,
                 by = c('STRANA' = 'id'),
                 suffix = c(".x", "")) %>%
      inner_join(tnved,
                 by = c('TNVED' = 'id'),
                 suffix = c(".x", "")) %>%
      inner_join(edizm,
                 by = c('EDIZM' = 'id'),
                 suffix = c(".x", "")) %>%
      inner_join(tnved4,
                 by = c('TNVED4' = 'id'),
                 suffix = c(".x", "")) %>%
      inner_join(tnved6,
                 by = c('TNVED6' = 'id'),
                 suffix = c(".x", "")) %>%
      inner_join(tnved2,
                 by = c('TNVED2' = 'id'),
                 suffix = c(".x", "")) %>%
      select(-ends_with(".x"))
    return(df)
  }
  
  con <-
    dbConnect(RSQLite::SQLite(), "df-10-znak.sqlite")
  df_FTS <- tbl(con, "fts")
  df <- tbl(con, "mirr")
  napr <- tbl(con, "napr")
  strana <- tbl(con, "strana")
  tnved <- tbl(con, "tnved")
  edizm <- tbl(con, "edizm")
  tnved4 <- tbl(con, "tnved4")
  tnved6 <- tbl(con, "tnved6")
  tnved2 <- tbl(con, "tnved2")
  
  
  print('Ready')
  
  
  # 0.2 Make some queries in advance
  qq <- function(p) {
    return(paste0(
      mapply(paste0, "'", p, "'")
      , collapse=','))
  }
  ex_id <- dbGetQuery(con, 'SELECT id FROM napr WHERE NAPR = (?)', params = 'ЭК')$id
  im_id <- dbGetQuery(con, 'SELECT id FROM napr WHERE NAPR = (?)', params = 'ИМ')$id
  
  
  
  
  # 1. Начало Главной страницы ------------------------------------------------
  # 1.1 Организация Valuebox'ов
  
  output$currentMonthEx <- renderValueBox({
    
    currentMonthEx <- df %>%
      filter(PERIOD == local(as.numeric(input$chosenMonth))) %>%
      filter(NAPR == ex_id) %>%
      group_by(PERIOD) %>%
      summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
      collect() %>%
      mutate(PERIOD = as.Date(PERIOD)) %>%
      select(STOIM) %>%
      as.numeric
    
    if (format(currentMonthEx / 10 ^ 6, big.mark = ',') < 0) {
      cl <- "red"
    }
    if (format(currentMonthEx / 10 ^ 6, big.mark = ',') > 0) {
      cl <- "green"
    }
    if  (is.na(currentMonthEx)) {
      cl <- "yellow"
    }
    
    valueBox(
      paste0('$', format(round(currentMonthEx / 10 ^ 6), big.mark = ','), " млн"),
      paste0('Экспорт, ', input$chosenMonth),
      icon = icon('export', lib = 'glyphicon'),
      color = cl
    )
  }) %>% bindCache(input$chosenMonth, 'currentMonthEx', upd)
  
  output$currentMonthIm <- renderValueBox({
    
    currentMonthIm <- df %>%
      filter(PERIOD == local(as.numeric(input$chosenMonth))) %>%
      filter(NAPR == im_id) %>%
      group_by(PERIOD) %>%
      summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
      collect() %>%
      mutate(PERIOD = as.Date(PERIOD)) %>%
      select(STOIM) %>%
      as.numeric

    
    if (format(currentMonthIm / 10 ^ 6, big.mark = ',') < 0) {
      cl <- "red"
    }
    if (format(currentMonthIm / 10 ^ 6, big.mark = ',') > 0) {
      cl <- "green"
    }
    if  (is.na(currentMonthIm)) {
      cl <- "yellow"
    }
    
    valueBox(
      paste0('$', format(round(currentMonthIm / 10 ^ 6), big.mark = ','), " млн"),
      paste0('Импорт, ', input$chosenMonth),
      icon = icon('import', lib = 'glyphicon'),
      color = cl
    )
  }) %>% bindCache(input$chosenMonth, 'currentMonthIm', upd)
  
  output$monthgrowthEx <- renderValueBox({
    monthgrowthEx <- df %>%
      filter(NAPR == ex_id) %>%
      filter(PERIOD == local(as.numeric(input$chosenMonth)) |
               PERIOD == local(as.numeric(input$chosenMonth - months(1)))) %>%
      group_by(PERIOD) %>%
      summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
      mutate(STOIM = STOIM / lag(STOIM, 1) - 1) %>%
      ungroup() %>%
      collect() %>%
      mutate(PERIOD = as.Date(PERIOD)) %>%
      filter(PERIOD == input$chosenMonth) %>%
      select(STOIM) %>%
      as.numeric
    
    if (format(monthgrowthEx / 10 ^ 6, big.mark = ',') < 0) {
      cl <- "red"
    }
    if (format(monthgrowthEx / 10 ^ 6, big.mark = ',') > 0) {
      cl <- "green"
    }
    if  (is.na(monthgrowthEx)) {
      cl <- "yellow"
    }
    
    valueBox(
      paste0(format(
        round(monthgrowthEx * 100, 2), big.mark = ','
      ), " % м/м"),
      paste0('Экспорт, ',
        input$chosenMonth,
        '/',
        input$chosenMonth - months(1)
      ),
      icon = icon('export', lib = 'glyphicon'),
      color = cl
    )
  }) %>% bindCache(input$chosenMonth, 'monthgrowthEx', upd)
  
  output$monthgrowthIm <- renderValueBox({
    monthgrowthIm <- df %>%
      filter(NAPR == im_id) %>%
      filter(PERIOD == local(as.numeric(input$chosenMonth)) |
               PERIOD == local(as.numeric(input$chosenMonth - months(1)))) %>%
      group_by(PERIOD) %>%
      summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
      mutate(STOIM = STOIM / lag(STOIM, 1) - 1) %>%
      ungroup() %>%
      collect() %>%
      mutate(PERIOD = as.Date(PERIOD)) %>%
      filter(PERIOD == input$chosenMonth) %>%
      select(STOIM) %>%
      as.numeric
    
    if (format(monthgrowthIm / 10 ^ 6, big.mark = ',') < 0) {
      cl <- "red"
    }
    if (format(monthgrowthIm / 10 ^ 6, big.mark = ',') > 0) {
      cl <- "green"
    }
    if  (is.na(monthgrowthIm)) {
      cl <- "yellow"
    }
    
    valueBox(
      paste0(format(
        round(monthgrowthIm * 100, 2), big.mark = ','
      ), " % м/м"),
      paste0('Импорт, ',
        input$chosenMonth,
        '/',
        input$chosenMonth - months(1)
      ),
      icon = icon('import', lib = 'glyphicon'),
      color = cl
    )
  }) %>% bindCache(input$chosenMonth, 'monthgrowthIm', upd)
  
  
  output$yeargrowthEx <- renderValueBox({
    yeargrowthEx <- df %>%
      filter(NAPR == ex_id) %>%
      filter(PERIOD == local(as.numeric(input$chosenMonth)) |
               PERIOD == local(as.numeric(input$chosenMonth - months(12)))) %>%
      group_by(PERIOD) %>%
      summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
      mutate(STOIM = STOIM / lag(STOIM, 1) - 1) %>%
      ungroup() %>%
      collect() %>%
      mutate(PERIOD = as.Date(PERIOD)) %>%
      filter(PERIOD == input$chosenMonth) %>%
      select(STOIM) %>%
      as.numeric
    
    if (format(yeargrowthEx / 10 ^ 6, big.mark = ',') < 0) {
      cl <- "red"
    }
    if (format(yeargrowthEx / 10 ^ 6, big.mark = ',') > 0) {
      cl <- "green"
    }
    if  (is.na(yeargrowthEx)) {
      cl <- "yellow"
    }
    
    valueBox(
      paste0(format(
        round(yeargrowthEx * 100, 2), big.mark = ','
      ), " % г/г"),
      paste0('Экспорт, ',
        input$chosenMonth,
        '/',
        input$chosenMonth - years(1)
      ),
      icon = icon('export', lib = 'glyphicon'),
      color = cl
    )
  }) %>% bindCache(input$chosenMonth, 'yeargrowthEx', upd)
  
  output$yeargrowthIm <- renderValueBox({
    yeargrowthIm <- df %>%
      filter(NAPR == im_id) %>%
      filter(PERIOD == local(as.numeric(input$chosenMonth)) |
               PERIOD == local(as.numeric(input$chosenMonth - months(12)))) %>%
      group_by(PERIOD) %>%
      summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
      mutate(STOIM = STOIM / lag(STOIM, 1) - 1) %>%
      ungroup() %>%
      collect() %>%
      mutate(PERIOD = as.Date(PERIOD)) %>%
      filter(PERIOD == input$chosenMonth) %>%
      select(STOIM) %>%
      as.numeric
    
    if (format(yeargrowthIm / 10 ^ 6, big.mark = ',') < 0) {
      cl <- "red"
    }
    if (format(yeargrowthIm / 10 ^ 6, big.mark = ',') > 0) {
      cl <- "green"
    }
    if  (is.na(yeargrowthIm)) {
      cl <- "yellow"
    }
    
    valueBox(
      paste0(format(
        round(yeargrowthIm * 100, 2), big.mark = ','
      ), " % г/г"),
      paste0('Импорт, ',
        input$chosenMonth,
        '/',
        input$chosenMonth - years(1)
      ),
      icon = icon('import', lib = 'glyphicon'),
      color = cl
    )
  }) %>% bindCache(input$chosenMonth, 'yeargrowthIm', upd)
  
  print('ValueBoxes ready')
  
  # 1.2 Организация графиков
  # 1.2.1 экспорт
  
  output$tradeHistoryPlots <- renderPlotly({

    tradeHistory <- df %>%
      group_by(PERIOD, NAPR) %>%
      summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
      collect() %>%
      mutate(PERIOD = as.Date(PERIOD))

    FTStradeHistory <- df_FTS %>%
      group_by(PERIOD, NAPR) %>%
      summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
      collect() %>%
      mutate(PERIOD = as.Date(PERIOD))

    tradeHistoryPlotEx <-
      tradeHistory %>%
      filter(NAPR == ex_id) %>%
      ggplot() +
      geom_line(aes(
        x = PERIOD,
        y = STOIM / 10 ^ 6,
        text = paste0(
          'Суммарный экспорт, зеркальные данные, ',
          PERIOD,
          ': ',
          round(STOIM / 10 ^ 6, 1),
          ' млн'
        ),
        linetype = 'Зеркальные данные',
        group=1),
        col = 'blue'
      ) +
      geom_line(
        data = FTStradeHistory %>%
          filter(NAPR == ex_id) %>%
          group_by(PERIOD) %>%
          summarise(TOTAL = sum(STOIM, na.rm = TRUE)),
        aes(
          x = PERIOD,
          y = TOTAL / 10 ^ 6,
          group = 2,
          linetype = 'Данные ФТС',
          text = paste0(
            'Суммарный экспорт, данные ФТС, ',
            PERIOD,
            ': ',
            round(TOTAL / 10 ^ 6, 1),
            ' млн'
          )
        ), col = 'blue',
      ) +
      xlab('') +
      ylab('млн $') +
      scale_linetype_manual(name = '', values = c('Зеркальные данные' = 'solid', 'Данные ФТС' = 'dashed'))


    tradeHistoryPlotEx <-
      ggplotly(tradeHistoryPlotEx, tooltip = 'text') %>%
      config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
      layout(legend = list(
        orientation = "h",
        x = 0.3,
        y = -0.1
      ))

    # 1.2.2 импорт

    tradeHistoryPlotIm <-
      tradeHistory %>%
      filter(NAPR == im_id) %>%
      ggplot() +
      geom_line(aes(
        x = PERIOD,
        y = STOIM / 10 ^ 6,
        text = paste0(
          'Суммарный импорт, зеркальные данные, ',
          PERIOD,
          ': ',
          round(STOIM / 10 ^ 6, 1),
          ' млн'
        ),
        linetype = 'Зеркальные данные',
        group=1),
        col = 'blue'
      ) +
      geom_line(
        data = FTStradeHistory %>%
          filter(NAPR == im_id) %>%
          group_by(PERIOD) %>%
          summarise(TOTAL = sum(STOIM, na.rm = TRUE)),
        aes(
          x = PERIOD,
          y = TOTAL / 10 ^ 6,
          group = 2,
          linetype = 'Данные ФТС',
          text = paste0(
            'Суммарный импорт, данные ФТС, ',
            PERIOD,
            ': ',
            round(TOTAL / 10 ^ 6, 1),
            ' млн'
          )
        ), col = 'blue',
      ) +
      xlab('') +
      ylab('млн $') +
      scale_linetype_manual(name = '', values = c('Зеркальные данные' = 'solid', 'Данные ФТС' = 'dashed'))


    tradeHistoryPlotIm <-
      ggplotly(tradeHistoryPlotIm, tooltip = 'text') %>%
      config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
      layout(legend = list(
        orientation = "h",
        x = 0.3,
        y = -0.1
      ))

    # 1.2.3 суммарное преобразование получившихся графиков

    fig <- subplot(style(tradeHistoryPlotEx, showlegend=F), tradeHistoryPlotIm, nrows=2, margin = 0.1) %>% layout(annotations = list(
      list(x = 0.1 , y = 1.05, text = "Экспорт", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.1 , y = 0.5, text = "Импорт", showarrow = F, xref='paper', yref='paper')))
    fig
  })  %>% bindCache(upd)
  # 
  # 
  # print('Global plot ready')
  
  
  
  # 2. Начало страницы с выбором стран ----------------------------------------
  # 2.1 Организация Valuebox'ов
  
  observeEvent(input$btn_build_country_report,
               {
                 
                 quotedLst <- qq(input$select_country) 
                 query <- paste0("SELECT id FROM strana WHERE STRANA IN (", quotedLst, ")", sep = '')
                 country_id <- dbGetQuery(con, query)$id
                 
                 
                 
                 output$exactcurrentMonthEx <- renderValueBox({
                   exactcurrentMonthEx <- df %>%
                     filter(STRANA %in% country_id) %>%
                     filter(PERIOD == local(as.numeric(input$chosenMonth))) %>%
                     filter(NAPR == ex_id) %>%
                     group_by(PERIOD) %>%
                     summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
                     collect() %>%
                     mutate(PERIOD = as.Date(PERIOD)) %>%
                     select(STOIM) %>%
                     as.numeric
                   
                   if (format(exactcurrentMonthEx / 10 ^ 6, big.mark = ',') < 0) {
                     cl <- "red"
                   }
                   if (format(exactcurrentMonthEx / 10 ^ 6, big.mark = ',') > 0) {
                     cl <- "green"
                   }
                   if  (is.na(exactcurrentMonthEx)) {
                     cl <- "yellow"
                   }
                   
                   valueBox(
                     paste0(
                       '$',
                       format(round(exactcurrentMonthEx / 10 ^ 6), big.mark = ','),
                       " млн"
                     ),
                     paste0('Экспорт, ', input$chosenMonth),
                     icon = icon('export', lib = 'glyphicon'),
                     color = cl
                   )
                 })
                 
                 output$exactcurrentMonthIm <- renderValueBox({
                   exactcurrentMonthIm <- df %>%
                     filter(STRANA %in% country_id) %>%
                     filter(PERIOD == local(as.numeric(input$chosenMonth))) %>%
                     filter(NAPR == im_id) %>%
                     group_by(PERIOD) %>%
                     summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
                     collect() %>%
                     mutate(PERIOD = as.Date(PERIOD)) %>%
                     select(STOIM) %>%
                     as.numeric
                   
                   if (format(exactcurrentMonthIm / 10 ^ 6, big.mark = ',') < 0) {
                     cl <- "red"
                   }
                   if (format(exactcurrentMonthIm / 10 ^ 6, big.mark = ',') > 0) {
                     cl <- "green"
                   }
                   if  (is.na(exactcurrentMonthIm)) {
                     cl <- "yellow"
                   }
                   
                   valueBox(
                     paste0(
                       '$',
                       format(round(exactcurrentMonthIm / 10 ^ 6), big.mark = ','),
                       " млн"
                     ),
                     paste0('Импорт, ', input$chosenMonth),
                     icon = icon('import', lib = 'glyphicon'),
                     color = cl
                   )
                 })
                 
                 
                 output$exactmonthgrowthEx <- renderValueBox({
                   exactmonthgrowthEx <- df %>%
                     filter(STRANA %in% country_id) %>%
                     filter(NAPR == ex_id) %>%
                     filter(PERIOD == local(as.numeric(input$chosenMonth)) |
                              PERIOD == local(as.numeric(input$chosenMonth - months(1)))) %>%
                     group_by(PERIOD) %>%
                     summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
                     mutate(STOIM = STOIM / lag(STOIM, 1) - 1) %>%
                     ungroup() %>%
                     collect() %>%
                     mutate(PERIOD = as.Date(PERIOD)) %>%
                     filter(PERIOD == input$chosenMonth) %>%
                     select(STOIM) %>%
                     as.numeric
                   
                   if (format(exactmonthgrowthEx / 10 ^ 6, big.mark = ',') < 0) {
                     cl <- "red"
                   }
                   if (format(exactmonthgrowthEx / 10 ^ 6, big.mark = ',') > 0) {
                     cl <- "green"
                   }
                   if  (is.na(exactmonthgrowthEx)) {
                     cl <- "yellow"
                   }
                   
                   valueBox(
                     paste0(format(
                       round(exactmonthgrowthEx * 100, 2), big.mark = ','
                     ), " % м/м"),
                     paste0('"Экспорт, ',
                       input$chosenMonth,
                       '/',
                       input$chosenMonth - months(1)
                     ),
                     icon = icon('export', lib = 'glyphicon'),
                     color = cl
                   )
                 })
                 
                 output$exactmonthgrowthIm <- renderValueBox({
                   exactmonthgrowthIm <- df %>%
                     filter(STRANA %in% country_id) %>%
                     filter(NAPR == im_id) %>%
                     filter(PERIOD == local(as.numeric(input$chosenMonth)) |
                              PERIOD == local(as.numeric(input$chosenMonth - months(1)))) %>%
                     group_by(PERIOD) %>%
                     summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
                     mutate(STOIM = STOIM / lag(STOIM, 1) - 1) %>%
                     ungroup() %>%
                     collect() %>%
                     mutate(PERIOD = as.Date(PERIOD)) %>%
                     filter(PERIOD == input$chosenMonth) %>%
                     select(STOIM) %>%
                     as.numeric
                   
                   if (format(exactmonthgrowthIm / 10 ^ 6, big.mark = ',') < 0) {
                     cl <- "red"
                   }
                   if (format(exactmonthgrowthIm / 10 ^ 6, big.mark = ',') > 0) {
                     cl <- "green"
                   }
                   if  (is.na(exactmonthgrowthIm)) {
                     cl <- "yellow"
                   }
                   
                   
                   valueBox(
                     paste0(format(
                       round(exactmonthgrowthIm * 100, 2), big.mark = ','
                     ), " % м/м"),
                     paste0('Импорт, ',
                       input$chosenMonth,
                       '/',
                       input$chosenMonth - months(1)
                     ),
                     icon = icon('import', lib = 'glyphicon'),
                     color = cl
                   )
                 })
                 
                 
                 output$exactyeargrowthEx <- renderValueBox({
                   exactyeargrowthEx <- df %>%
                     filter(STRANA %in% country_id) %>%
                     filter(NAPR == ex_id) %>%
                     filter(PERIOD == local(as.numeric(input$chosenMonth)) |
                              PERIOD == local(as.numeric(input$chosenMonth - months(12)))) %>%
                     group_by(PERIOD) %>%
                     summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
                     mutate(STOIM = STOIM / lag(STOIM, 1) - 1) %>%
                     ungroup() %>%
                     collect() %>%
                     mutate(PERIOD = as.Date(PERIOD)) %>%
                     filter(PERIOD == input$chosenMonth) %>%
                     select(STOIM) %>%
                     as.numeric
                   
                   if (format(exactyeargrowthEx / 10 ^ 6, big.mark = ',') < 0) {
                     cl <- "red"
                   }
                   if (format(exactyeargrowthEx / 10 ^ 6, big.mark = ',') > 0) {
                     cl <- "green"
                   }
                   if  (is.na(exactyeargrowthEx)) {
                     cl <- "yellow"
                   }
                   
                   valueBox(
                     paste0(format(
                       round(exactyeargrowthEx * 100, 2), big.mark = ','
                     ), " % г/г"),
                     paste0('Экспорт, ',
                       input$chosenMonth,
                       '/',
                       input$chosenMonth - years(1)
                     ),
                     icon = icon('export', lib = 'glyphicon'),
                     color = cl
                   )
                 })
                 
                 output$exactyeargrowthIm <- renderValueBox({
                   exactyeargrowthIm <- df %>%
                     filter(STRANA %in% country_id) %>%
                     filter(NAPR == im_id) %>%
                     filter(PERIOD == local(as.numeric(input$chosenMonth)) |
                              PERIOD == local(as.numeric(input$chosenMonth - months(12)))) %>%
                     group_by(PERIOD) %>%
                     summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
                     mutate(STOIM = STOIM / lag(STOIM, 1) - 1) %>%
                     ungroup() %>%
                     collect() %>%
                     mutate(PERIOD = as.Date(PERIOD)) %>%
                     filter(PERIOD == input$chosenMonth) %>%
                     select(STOIM) %>%
                     as.numeric
                   
                   if (format(exactyeargrowthIm / 10 ^ 6, big.mark = ',') < 0) {
                     cl <- "red"
                   }
                   if (format(exactyeargrowthIm / 10 ^ 6, big.mark = ',') > 0) {
                     cl <- "green"
                   }
                   if  (is.na(exactyeargrowthIm)) {
                     cl <- "yellow"
                   }
                   
                   valueBox(
                     paste0(format(
                       round(exactyeargrowthIm * 100, 2), big.mark = ','
                     ), " % г/г"),
                     paste0('Импорт, ',
                       input$chosenMonth,
                       '/',
                       input$chosenMonth - years(1)
                     ),
                     icon = icon('import', lib = 'glyphicon'),
                     color = cl
                   )
                 })
                 
                 print('ValueBoxes ready')
                 
                 
                 # 2.2 Организация графиков
                 # 2.2.1 экспорт
                 
                 output$exacttradeHistoryPlots <- renderPlotly({
                   
                   exacttradeHistory <- df %>%
                     filter(STRANA %in% country_id) %>%
                     group_by(PERIOD, STRANA, NAPR) %>%
                     summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
                     inner_join(strana,
                                by = c('STRANA' = 'id'), suffix=c('.x', '')) %>%
                     collect() %>%
                     mutate(PERIOD = as.Date(PERIOD)) %>%
                     select(-ends_with('.x'))
                   
                   
                   exacttradeHistoryPlotEx <-
                     exacttradeHistory %>% 
                     filter(NAPR == ex_id) %>%
                     ggplot() +
                     geom_line(aes(
                       x = PERIOD,
                       y = STOIM / 10 ^ 6,
                       group = STRANA,
                       col = STRANA,
                       text = paste0(
                         'Экспорт, ',
                         STRANA,
                         ', ',
                         PERIOD,
                         ': ',
                         round(STOIM / 10 ^ 6, 1),
                         ' млн'
                       )
                     )) +
                     scale_color_discrete(name = '') +
                     geom_line(
                       data = exacttradeHistory %>%
                         filter(NAPR == ex_id) %>%
                         group_by(PERIOD) %>%
                         summarise(TOTAL = sum(STOIM, na.rm = TRUE)),
                       aes(
                         x = PERIOD,
                         y = TOTAL / 10 ^ 6,
                         group = 1,
                         col = 'blue',
                         linetype = 'total',
                         text = paste0(
                           'Экспорт, по выбранным странам, ',
                           PERIOD,
                           ': ',
                           round(TOTAL / 10 ^ 6, 1),
                           ' млн'
                         )
                       )
                     ) +
                     scale_linetype_manual(name = '', values = c('total' = "dashed")) +
                     xlab('') +
                     ylab('млн $') +
                     theme(legend.position = 'best')
                   
                   exacttradeHistoryPlotEx <-
                     ggplotly(exacttradeHistoryPlotEx, tooltip = 'text') %>%
                     config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
                     layout(legend = list(
                       orientation = "h",
                       x = 0.3,
                       y = -0.1
                     ))
                   
                   
                   # 2.2.2 импорт
                   
                   exacttradeHistoryPlotIm <-
                     exacttradeHistory %>% 
                     filter(NAPR == im_id) %>%
                     ggplot() +
                     geom_line(aes(
                       x = PERIOD,
                       y = STOIM / 10 ^ 6,
                       group = STRANA,
                       col = STRANA,
                       text = paste0(
                         'Импорт, ',
                         STRANA,
                         ', ',
                         PERIOD,
                         ': ',
                         round(STOIM / 10 ^ 6, 1),
                         ' млн'
                       )
                     )) +
                     scale_color_discrete(name = '') +
                     geom_line(
                       data = exacttradeHistory %>%
                         filter(NAPR == im_id) %>%
                         group_by(PERIOD) %>%
                         summarise(TOTAL = sum(STOIM, na.rm = TRUE)),
                       aes(
                         x = PERIOD,
                         y = TOTAL / 10 ^ 6,
                         group = 1,
                         col = 'blue',
                         linetype = 'total',
                         text = paste0(
                           'Импорт, по выбранным странам, ',
                           PERIOD,
                           ': ',
                           round(TOTAL / 10 ^ 6, 1),
                           ' млн'
                         )
                       )
                     ) +
                     scale_linetype_manual(name = '', values = c('total' = "dashed")) +
                     xlab('') +
                     ylab('млн $') +
                     theme(legend.position = 'best')                   
                   
                   exacttradeHistoryPlotIm <-
                     ggplotly(exacttradeHistoryPlotIm, tooltip = 'text') %>%
                     config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
                     layout(legend = list(
                       orientation = "h",
                       x = 0.3,
                       y = -0.1
                     ))
                   
                   # 2.2.3 суммарное преобразование получившихся графиков
                   
                   fig <- subplot(exacttradeHistoryPlotEx, exacttradeHistoryPlotIm, nrows=2, margin = 0.1) %>% layout(annotations = list(
                     list(x = 0.1 , y = 1.05, text = "Экспорт", showarrow = F, xref='paper', yref='paper'),
                     list(x = 0.1 , y = 0.5, text = "Импорт", showarrow = F, xref='paper', yref='paper')))
                   fig
                 })
                 
                 print('Country plots ready')
               })
  
  # 3. Начало страницы с выбором товаров --------------------------------------
  # 3.1 Проверка выбора желаемой длины кода
  observe({
  
    x <- substr(input$tnvedCheckbox, 1, 1)
    if (x==2) {
      updateSelectizeInput(session = getDefaultReactiveDomain()
                           , inputId = "select_commodity"
                           , choices = goods_list_2
                           , selected= NULL
      )
    }
    if (x==4) {
      updateSelectizeInput(session = getDefaultReactiveDomain()
                           , inputId = "select_commodity"
                           , choices = goods_list_4
                           , selected= NULL
      )
    }
    if (x==6) {
      updateSelectizeInput(session = getDefaultReactiveDomain()
                           , inputId = "select_commodity"
                           , choices = goods_list_6
                           , selected= NULL
      )
    }
    
  })
  

  # 3.2 Организация Valuebox'ов
  observeEvent(input$btn_build_comm_report,
               {
                 
                 y <- as.numeric(substr(input$tnvedCheckbox, 1, 1))
                 
                 
                 
                 
                 if (y==2) {
                   quotedLst <- qq(input$select_commodity) 
                   query <- paste0("SELECT id FROM tnved2 WHERE TNVED2 IN (", quotedLst, ")", sep = '')
                   commodity_id <- dbGetQuery(con, query)$id
                 }
                 if (y==4) {
                   quotedLst <- qq(input$select_commodity) 
                   query <- paste0("SELECT id FROM tnved4 WHERE TNVED4 IN (", quotedLst, ")", sep = '')
                   commodity_id <- dbGetQuery(con, query)$id
                 }
                 if (y==6) {
                   quotedLst <- qq(input$select_commodity) 
                   query <- paste0("SELECT id FROM tnved6 WHERE TNVED6 IN (", quotedLst, ")", sep = '')
                   commodity_id <- dbGetQuery(con, query)$id
                 }
                 
                 
                 
                 print(y)
                 
                 output$tnvedcurrentMonthEx <- renderValueBox({
                   tnvedcurrentMonthEx <- df %>%
                     filter(rlang::sym(paste0("TNVED", y)) %in% commodity_id) %>%
                     filter(PERIOD == local(as.numeric(input$chosenMonth))) %>%
                     filter(NAPR == ex_id) %>%
                     group_by(PERIOD) %>%
                     summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
                     collect() %>%
                     mutate(PERIOD = as.Date(PERIOD)) %>%
                     select(STOIM) %>%
                     as.numeric
                   
                   if (format(tnvedcurrentMonthEx / 10 ^ 6, big.mark = ',') < 0) {
                     cl <- "red"
                   }
                   if (format(tnvedcurrentMonthEx / 10 ^ 6, big.mark = ',') > 0) {
                     cl <- "green"
                   }
                   if  (is.na(tnvedcurrentMonthEx)) {
                     cl <- "yellow"
                   }
                   
                   valueBox(
                     paste0(
                       '$',
                       format(round(tnvedcurrentMonthEx / 10 ^ 6), big.mark = ','),
                       " млн"
                     ),
                     paste0('Экспорт, ', input$chosenMonth),
                     icon = icon('export', lib = 'glyphicon'),
                     color = cl
                   )
                 })
                 
                 output$tnvedcurrentMonthIm <- renderValueBox({
                   tnvedcurrentMonthIm <- df %>%
                     filter(rlang::sym(paste0("TNVED", y)) %in% commodity_id) %>%
                     filter(PERIOD == local(as.numeric(input$chosenMonth))) %>%
                     filter(NAPR == im_id) %>%
                     group_by(PERIOD) %>%
                     summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
                     collect() %>%
                     mutate(PERIOD = as.Date(PERIOD)) %>%
                     select(STOIM) %>%
                     as.numeric
                   
                   if (format(tnvedcurrentMonthIm / 10 ^ 6, big.mark = ',') < 0) {
                     cl <- "red"
                   }
                   if (format(tnvedcurrentMonthIm / 10 ^ 6, big.mark = ',') > 0) {
                     cl <- "green"
                   }
                   if  (is.na(tnvedcurrentMonthIm)) {
                     cl <- "yellow"
                   }
                   
                   valueBox(
                     paste0(
                       '$',
                       format(round(tnvedcurrentMonthIm / 10 ^ 6), big.mark = ','),
                       " млн"
                     ),
                     paste0('Импорт, ', input$chosenMonth),
                     icon = icon('import', lib = 'glyphicon'),
                     color = cl
                   )
                 })
                 
                 
                 output$tnvedmonthgrowthEx <- renderValueBox({
                   tnvedmonthgrowthEx <- df %>%
                     filter(rlang::sym(paste0("TNVED", y)) %in% commodity_id) %>%
                     filter(NAPR == ex_id) %>%
                     filter(PERIOD == local(as.numeric(input$chosenMonth)) |
                              PERIOD == local(as.numeric(input$chosenMonth - months(1)))) %>%
                     group_by(PERIOD) %>%
                     summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
                     mutate(STOIM = STOIM / lag(STOIM, 1) - 1) %>%
                     ungroup() %>%
                     collect() %>%
                     mutate(PERIOD = as.Date(PERIOD)) %>%
                     filter(PERIOD == input$chosenMonth) %>%
                     select(STOIM) %>%
                     as.numeric
                   
                   if (format(tnvedmonthgrowthEx / 10 ^ 6, big.mark = ',') < 0) {
                     cl <- "red"
                   }
                   if (format(tnvedmonthgrowthEx / 10 ^ 6, big.mark = ',') > 0) {
                     cl <- "green"
                   }
                   if  (is.na(tnvedmonthgrowthEx)) {
                     cl <- "yellow"
                   }
                   
                   valueBox(
                     paste0(format(
                       round(tnvedmonthgrowthEx * 100, 2), big.mark = ','
                     ), " % м/м"),
                     paste0('Экспорт, ',
                       input$chosenMonth,
                       '/',
                       input$chosenMonth - months(1)
                     ),
                     icon = icon('export', lib = 'glyphicon'),
                     color = cl
                   )
                 })
                 
                 output$tnvedmonthgrowthIm <- renderValueBox({
                   tnvedmonthgrowthIm <- df %>%
                     filter(rlang::sym(paste0("TNVED", y)) %in% commodity_id) %>%
                     filter(NAPR == im_id) %>%
                     filter(PERIOD == local(as.numeric(input$chosenMonth)) |
                              PERIOD == local(as.numeric(input$chosenMonth - months(1)))) %>%
                     group_by(PERIOD) %>%
                     summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
                     mutate(STOIM = STOIM / lag(STOIM, 1) - 1) %>%
                     ungroup() %>%
                     collect() %>%
                     mutate(PERIOD = as.Date(PERIOD)) %>%
                     filter(PERIOD == input$chosenMonth) %>%
                     select(STOIM) %>%
                     as.numeric
                   
                   if (format(tnvedmonthgrowthIm / 10 ^ 6, big.mark = ',') < 0) {
                     cl <- "red"
                   }
                   if (format(tnvedmonthgrowthIm / 10 ^ 6, big.mark = ',') > 0) {
                     cl <- "green"
                   }
                   if  (is.na(tnvedmonthgrowthIm)) {
                     cl <- "yellow"
                   }
                   
                   
                   valueBox(
                     paste0(format(
                       round(tnvedmonthgrowthIm * 100, 2), big.mark = ','
                     ), " % м/м"),
                     paste0('Импорт, ',
                       input$chosenMonth,
                       '/',
                       input$chosenMonth - months(1)
                     ),
                     icon = icon('import', lib = 'glyphicon'),
                     color = cl
                   )
                 })
                 
                 
                 output$tnvedyeargrowthEx <- renderValueBox({
                   tnvedyeargrowthEx <- df %>%
                     filter(rlang::sym(paste0("TNVED", y)) %in% commodity_id) %>%
                     filter(NAPR == ex_id) %>%
                     filter(PERIOD == local(as.numeric(input$chosenMonth)) |
                              PERIOD == local(as.numeric(input$chosenMonth - months(12)))) %>%
                     group_by(PERIOD) %>%
                     summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
                     mutate(STOIM = STOIM / lag(STOIM, 1) - 1) %>%
                     ungroup() %>%
                     collect() %>%
                     mutate(PERIOD = as.Date(PERIOD)) %>%
                     filter(PERIOD == input$chosenMonth) %>%
                     select(STOIM) %>%
                     as.numeric
                   
                   if (format(tnvedyeargrowthEx / 10 ^ 6, big.mark = ',') < 0) {
                     cl <- "red"
                   }
                   if (format(tnvedyeargrowthEx / 10 ^ 6, big.mark = ',') > 0) {
                     cl <- "green"
                   }
                   if  (is.na(tnvedyeargrowthEx)) {
                     cl <- "yellow"
                   }
                   
                   valueBox(
                     paste0(format(
                       round(tnvedyeargrowthEx * 100, 2), big.mark = ','
                     ), " % г/г"),
                     paste0('Экспорт, ',
                       input$chosenMonth,
                       '/',
                       input$chosenMonth - years(1)
                     ),
                     icon = icon('export', lib = 'glyphicon'),
                     color = cl
                   )
                 })
                 
                 output$tnvedyeargrowthIm <- renderValueBox({
                   tnvedyeargrowthIm <- df %>%
                     filter(rlang::sym(paste0("TNVED", y)) %in% commodity_id) %>%
                     filter(NAPR == im_id) %>%
                     filter(PERIOD == local(as.numeric(input$chosenMonth)) |
                              PERIOD == local(as.numeric(input$chosenMonth - months(12)))) %>%
                     group_by(PERIOD) %>%
                     summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
                     mutate(STOIM = STOIM / lag(STOIM, 1) - 1) %>%
                     ungroup() %>%
                     collect() %>%
                     mutate(PERIOD = as.Date(PERIOD)) %>%
                     filter(PERIOD == input$chosenMonth) %>%
                     select(STOIM) %>%
                     as.numeric
                   
                   if (format(tnvedyeargrowthIm / 10 ^ 6, big.mark = ',') < 0) {
                     cl <- "red"
                   }
                   if (format(tnvedyeargrowthIm / 10 ^ 6, big.mark = ',') > 0) {
                     cl <- "green"
                   }
                   if  (is.na(tnvedyeargrowthIm)) {
                     cl <- "yellow"
                   }
                   
                   valueBox(
                     paste0(format(
                       round(tnvedyeargrowthIm * 100, 2), big.mark = ','
                     ), " % г/г"),
                     paste0('Импорт, ',
                       input$chosenMonth,
                       '/',
                       input$chosenMonth - years(1)
                     ),
                     icon = icon('import', lib = 'glyphicon'),
                     color = cl
                   )
                 })
                 
                 print('ValueBoxes ready')
                 
                 
                 # 3.3 Организация графиков
                 # 3.3.1 экспорт
                 
                 tnvedDF <- df %>%
                   filter(rlang::sym(paste0("TNVED", y)) %in%  commodity_id) %>%
                   group_by(PERIOD, rlang::sym(paste0("TNVED", y)), NAPR) %>%
                   collect() %>%
                   ungroup() %>%
                   mutate(PERIOD = as.Date(PERIOD)) %>%
                   mutate(across(starts_with("TNVED"), ~as.character(.)))
                 
      
                   output$stoim_tnvedtradeHistoryPlots <- renderPlotly({
                   
                     # code_col = rlang::sym(paste0("TNVED", y)) 
                     
                     tnvedtradeHistory <- tnvedDF %>%
                       group_by(PERIOD, !!!syms(paste0("TNVED", y)), NAPR) %>%
                       summarise(STOIM = sum(STOIM, na.rm = TRUE))
                     
                     
                     tnvedtradeHistoryPlotEx <-
                       tnvedtradeHistory %>% 
                       filter(NAPR == ex_id) %>%
                       ggplot() +
                       geom_line(aes_string(
                         x = "PERIOD",
                         y = "STOIM/ 10 ^ 6" ,
                         group = paste0("TNVED", y) ,
                         col = paste0("TNVED", y) 
                         # ,text = paste0(
                         #   'Экспорт, по выбранным товарам, ',
                         #   paste0("TNVED", y),
                         #   ', ',
                         #   "PERIOD",
                         #   ': ',
                         #   "STOIM / 10 ^ 6",
                         #   ' млн'
                         # )
                       )) +
                       scale_color_discrete(name = '') +
                       geom_line(
                         data = tnvedtradeHistory %>%
                           filter(NAPR == ex_id) %>%
                           group_by(PERIOD) %>%
                           summarise(TOTAL = sum(STOIM, na.rm = TRUE)),
                         aes(
                           x = PERIOD,
                           y = TOTAL / 10 ^ 6,
                           group = 1,
                           col = 'blue',
                           linetype = 'total',
                           text = paste0(
                             'Суммарный экспорт, ',
                             PERIOD,
                             ': ',
                             round(TOTAL / 10 ^ 6, 1),
                             ' млн'
                           )
                         )
                       ) +
                       scale_linetype_manual(name = '', values = c('total' = "dashed")) +
                       xlab('') +
                       ylab('млн $') +
                       theme(legend.position = 'none')
                     
                     tnvedtradeHistoryPlotEx <-
                       ggplotly(tnvedtradeHistoryPlotEx, tooltip = 'text') %>%
                       config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
                       layout(legend = list(
                         orientation = "h",
                         x = 0.3,
                         y = -0.1
                       ))
                     
                     
                     # 3.3.2 импорт
                     
                     tnvedtradeHistoryPlotIm <-
                       tnvedtradeHistory %>% 
                       filter(NAPR == im_id) %>%
                       ggplot() +
                       geom_line(aes_string(
                         x = "PERIOD",
                         y = "STOIM / 10 ^ 6",
                         group = paste0("TNVED", y),
                         col = paste0("TNVED", y)
                         # ,text = paste0(
                         #   'Импорт, по выбранным товарам, ',
                         #   rlang::sym(paste0("TNVED", y)),
                         #   ', ',
                         #   PERIOD,
                         #   ': ',
                         #   round(STOIM / 10 ^ 6, 1),
                         #   ' млн'
                         # )
                       )) +
                       scale_color_discrete(name = '') +
                       geom_line(
                         data = tnvedtradeHistory %>%
                           filter(NAPR == im_id) %>%
                           group_by(PERIOD) %>%
                           summarise(TOTAL = sum(STOIM, na.rm = TRUE)),
                         aes(
                           x = PERIOD,
                           y = TOTAL / 10 ^ 6,
                           group = 1,
                           col = 'blue',
                           linetype = 'total',
                           text = paste0(
                             'Суммарный импорт, ',
                             PERIOD,
                             ': ',
                             round(TOTAL / 10 ^ 6, 1),
                             ' млн'
                           )
                         )
                       ) +
                       scale_linetype_manual(name = '', values = c('total' = "dashed")) +
                       xlab('') +
                       ylab('млн $') +
                       theme(legend.position = 'best')                   
                     
                     tnvedtradeHistoryPlotIm <-
                       ggplotly(tnvedtradeHistoryPlotIm, tooltip = 'text') %>%
                       config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
                       layout(legend = list(
                         orientation = "h",
                         x = 0.3,
                         y = -0.1
                       ))
                     
                     
                     # 3.3.3 суммарное преобразование получившихся графиков
                     
                     fig <- subplot(tnvedtradeHistoryPlotEx, tnvedtradeHistoryPlotIm, nrows=2, margin = 0.1) %>% layout(annotations = list(
                       list(x = 0.1 , y = 1.05, text = "Экспорт", showarrow = F, xref='paper', yref='paper'),
                       list(x = 0.1 , y = 0.5, text = "Импорт", showarrow = F, xref='paper', yref='paper')))
                     fig
                   })
                   
                   
                   output$netto_tnvedtradeHistoryPlots <- renderPlotly({
                     
                     # code_col = rlang::sym(paste0("TNVED", y)) 
                     
                     tnvedtradeHistory <- tnvedDF %>%
                       group_by(PERIOD, !!!syms(paste0("TNVED", y)), NAPR) %>%
                       summarise(NETTO = sum(NETTO, na.rm = TRUE))
                     
                     
                     tnvedtradeHistoryPlotEx <-
                       tnvedtradeHistory %>% 
                       filter(NAPR == ex_id) %>%
                       ggplot() +
                       geom_line(aes_string(
                         x = "PERIOD",
                         y = "NETTO/ 10 ^ 6" ,
                         group = paste0("TNVED", y) ,
                         col = paste0("TNVED", y) 
                         # ,text = paste0(
                         #   'Экспорт, по выбранным товарам, ',
                         #   paste0("TNVED", y),
                         #   ', ',
                         #   "PERIOD",
                         #   ': ',
                         #   "NETTO / 10 ^ 6",
                         #   ' млн'
                         # )
                       )) +
                       scale_color_discrete(name = '') +
                       geom_line(
                         data = tnvedtradeHistory %>%
                           filter(NAPR == ex_id) %>%
                           group_by(PERIOD) %>%
                           summarise(TOTAL = sum(NETTO, na.rm = TRUE)),
                         aes(
                           x = PERIOD,
                           y = TOTAL / 10 ^ 6,
                           group = 1,
                           col = 'blue',
                           linetype = 'total',
                           text = paste0(
                             'Суммарный экспорт, ',
                             PERIOD,
                             ': ',
                             round(TOTAL / 10 ^ 6, 1),
                             ' млн'
                           )
                         )
                       ) +
                       scale_linetype_manual(name = '', values = c('total' = "dashed")) +
                       xlab('') +
                       ylab('млн $') +
                       theme(legend.position = 'none')
                     
                     tnvedtradeHistoryPlotEx <-
                       ggplotly(tnvedtradeHistoryPlotEx, tooltip = 'text') %>%
                       config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
                       layout(legend = list(
                         orientation = "h",
                         x = 0.3,
                         y = -0.1
                       ))
                     
                     
                     # 3.3.2 импорт
                     
                     tnvedtradeHistoryPlotIm <-
                       tnvedtradeHistory %>% 
                       filter(NAPR == im_id) %>%
                       ggplot() +
                       geom_line(aes_string(
                         x = "PERIOD",
                         y = "NETTO / 10 ^ 6",
                         group = paste0("TNVED", y),
                         col = paste0("TNVED", y)
                         # ,text = paste0(
                         #   'Импорт, по выбранным товарам, ',
                         #   rlang::sym(paste0("TNVED", y)),
                         #   ', ',
                         #   PERIOD,
                         #   ': ',
                         #   round(NETTO / 10 ^ 6, 1),
                         #   ' млн'
                         # )
                       )) +
                       scale_color_discrete(name = '') +
                       geom_line(
                         data = tnvedtradeHistory %>%
                           filter(NAPR == im_id) %>%
                           group_by(PERIOD) %>%
                           summarise(TOTAL = sum(NETTO, na.rm = TRUE)),
                         aes(
                           x = PERIOD,
                           y = TOTAL / 10 ^ 6,
                           group = 1,
                           col = 'blue',
                           linetype = 'total',
                           text = paste0(
                             'Суммарный импорт, ',
                             PERIOD,
                             ': ',
                             round(TOTAL / 10 ^ 6, 1),
                             ' млн'
                           )
                         )
                       ) +
                       scale_linetype_manual(name = '', values = c('total' = "dashed")) +
                       xlab('') +
                       ylab('млн $') +
                       theme(legend.position = 'best')                   
                     
                     tnvedtradeHistoryPlotIm <-
                       ggplotly(tnvedtradeHistoryPlotIm, tooltip = 'text') %>%
                       config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
                       layout(legend = list(
                         orientation = "h",
                         x = 0.3,
                         y = -0.1
                       ))
                     
                     
                     # 3.3.3 суммарное преобразование получившихся графиков
                     
                     fig <- subplot(tnvedtradeHistoryPlotEx, tnvedtradeHistoryPlotIm, nrows=2, margin = 0.1) %>% layout(annotations = list(
                       list(x = 0.1 , y = 1.05, text = "Экспорт", showarrow = F, xref='paper', yref='paper'),
                       list(x = 0.1 , y = 0.5, text = "Импорт", showarrow = F, xref='paper', yref='paper')))
                     fig
                   })
                 
                   output$kol_tnvedtradeHistoryPlots <- renderPlotly({
                     
                     # code_col = rlang::sym(paste0("TNVED", y)) 
                     
                     tnvedtradeHistory <- tnvedDF %>%
                       group_by(PERIOD, !!!syms(paste0("TNVED", y)), NAPR) %>%
                       summarise(KOL = sum(KOL, na.rm = TRUE))
                     
                     
                     tnvedtradeHistoryPlotEx <-
                       tnvedtradeHistory %>% 
                       filter(NAPR == ex_id) %>%
                       ggplot() +
                       geom_line(aes_string(
                         x = "PERIOD",
                         y = "KOL/ 10 ^ 6" ,
                         group = paste0("TNVED", y) ,
                         col = paste0("TNVED", y) 
                         # ,text = paste0(
                         #   'Экспорт, по выбранным товарам, ',
                         #   paste0("TNVED", y),
                         #   ', ',
                         #   "PERIOD",
                         #   ': ',
                         #   "KOL / 10 ^ 6",
                         #   ' млн'
                         # )
                       )) +
                       scale_color_discrete(name = '') +
                       geom_line(
                         data = tnvedtradeHistory %>%
                           filter(NAPR == ex_id) %>%
                           group_by(PERIOD) %>%
                           summarise(TOTAL = sum(KOL, na.rm = TRUE)),
                         aes(
                           x = PERIOD,
                           y = TOTAL / 10 ^ 6,
                           group = 1,
                           col = 'blue',
                           linetype = 'total',
                           text = paste0(
                             'Суммарный экспорт, ',
                             PERIOD,
                             ': ',
                             round(TOTAL / 10 ^ 6, 1),
                             ' млн'
                           )
                         )
                       ) +
                       scale_linetype_manual(name = '', values = c('total' = "dashed")) +
                       xlab('') +
                       ylab('млн $') +
                       theme(legend.position = 'none')
                     
                     tnvedtradeHistoryPlotEx <-
                       ggplotly(tnvedtradeHistoryPlotEx, tooltip = 'text') %>%
                       config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
                       layout(legend = list(
                         orientation = "h",
                         x = 0.3,
                         y = -0.1
                       ))
                     
                     
                     # 3.3.2 импорт
                     
                     tnvedtradeHistoryPlotIm <-
                       tnvedtradeHistory %>% 
                       filter(NAPR == im_id) %>%
                       ggplot() +
                       geom_line(aes_string(
                         x = "PERIOD",
                         y = "KOL / 10 ^ 6",
                         group = paste0("TNVED", y),
                         col = paste0("TNVED", y)
                         # ,text = paste0(
                         #   'Импорт, по выбранным товарам, ',
                         #   rlang::sym(paste0("TNVED", y)),
                         #   ', ',
                         #   PERIOD,
                         #   ': ',
                         #   round(KOL / 10 ^ 6, 1),
                         #   ' млн'
                         # )
                       )) +
                       scale_color_discrete(name = '') +
                       geom_line(
                         data = tnvedtradeHistory %>%
                           filter(NAPR == im_id) %>%
                           group_by(PERIOD) %>%
                           summarise(TOTAL = sum(KOL, na.rm = TRUE)),
                         aes(
                           x = PERIOD,
                           y = TOTAL / 10 ^ 6,
                           group = 1,
                           col = 'blue',
                           linetype = 'total',
                           text = paste0(
                             'Суммарный импорт, ',
                             PERIOD,
                             ': ',
                             round(TOTAL / 10 ^ 6, 1),
                             ' млн'
                           )
                         )
                       ) +
                       scale_linetype_manual(name = '', values = c('total' = "dashed")) +
                       xlab('') +
                       ylab('млн $') +
                       theme(legend.position = 'best')                   
                     
                     tnvedtradeHistoryPlotIm <-
                       ggplotly(tnvedtradeHistoryPlotIm, tooltip = 'text') %>%
                       config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
                       layout(legend = list(
                         orientation = "h",
                         x = 0.3,
                         y = -0.1
                       ))
                     
                     
                     # 3.3.3 суммарное преобразование получившихся графиков
                     
                     fig <- subplot(tnvedtradeHistoryPlotEx, tnvedtradeHistoryPlotIm, nrows=2, margin = 0.1) %>% layout(annotations = list(
                       list(x = 0.1 , y = 1.05, text = "Экспорт", showarrow = F, xref='paper', yref='paper'),
                       list(x = 0.1 , y = 0.5, text = "Импорт", showarrow = F, xref='paper', yref='paper')))
                     fig
                   })
                 
                 
                 print('Global plot ready')
               })
  
  
  # 4. Начало страницы с экспортом данных -------------------------------------
  # 4.1 Проверка выбора желаемой длины кода
  
  observe({
    x <- substr(input$codeCheckbox, 1, 1)
    if (x==2) {
      updateSelectizeInput(session = getDefaultReactiveDomain()
                           , inputId = "tnved_on_off"
                           , choices = goods_list_2
                           , selected= NULL
      )
    }
    if (x==4) {
      updateSelectizeInput(session = getDefaultReactiveDomain()
                           , inputId = "tnved_on_off"
                           , choices = goods_list_4
                           , selected= NULL
      )
    }
    if (x==6) {
      updateSelectizeInput(session = getDefaultReactiveDomain()
                           , inputId = "tnved_on_off"
                           , choices = goods_list_6
                           , selected= NULL
      )
    }
    
  })
  
  
  # 4.2 Проверка выбранных стран + добавления данных ФТС + сортировка данных

  observeEvent(input$btn_build_agg_report,
               {
                 
                 # aggregate countries?
                 y <- input$aggstrCheckbox 
                 
                 # get country ids
                 quotedLstCNTR <- qq(input$strana_on_off) 
                 query <- paste0("SELECT id FROM strana WHERE STRANA IN (", quotedLstCNTR, ")", sep = '')
                 country_id <- dbGetQuery(con, query)$id
                 
                 # tnved level
                 z <- as.numeric(substr(input$codeCheckbox, 1, 1))
                 
                 # get tnved ids 
                 quotedLstTN <- qq(input$tnved_on_off) 
                 query <- paste0("SELECT id FROM tnved", z, " WHERE TNVED", z, " IN (", quotedLstTN, ")", sep = '')
                 commodity_id <- dbGetQuery(con, query)$id
                 
                 # aggregate tnveds?
                 w <- input$aggcomCheckbox 
                 
                 # add fts?
                 # k <- input$addftsCheckbox
                 
                 result <- df %>%
                   filter(STRANA %in% country_id) %>%
                   filter(rlang::sym(paste0("TNVED", z)) %in% commodity_id) %>%
                   select(-TNVED4, -TNVED6, -TNVED2)
                 
                 if (w=='да') {
                   result <- result %>%
                     group_by(PERIOD, STRANA, NAPR) %>%
                     summarize(STOIM = sum(STOIM, na.rm=TRUE), 
                               NETTO = sum(NETTO, na.rm=TRUE),
                               KOL = sum(as.numeric(KOL), na.rm=TRUE), TNVED = quotedLstTN, EDIZM = EDIZM)
                     
                 }
                 
                 if (y=='да') {
                   result <- result %>%
                     group_by(PERIOD, NAPR, TNVED) %>%
                     summarize(STOIM = sum(STOIM, na.rm=TRUE), 
                               NETTO = sum(NETTO, na.rm=TRUE),
                               KOL = sum(as.numeric(KOL), na.rm=TRUE), STRANA = quotedLstCNTR, EDIZM = EDIZM)
                 }

                countryIds <- strana %>%
                  collect()
                naprIds <- napr %>%
                  collect()
                tnvedIds <- tnved %>%
                  collect()
                edizmIds <- edizm %>%
                  collect()
                
                result <- result %>%
                  collect() %>%
                  select(PERIOD, STOIM, NETTO, KOL, NAPR, STRANA, TNVED, EDIZM) %>%
                  left_join(naprIds, by=c("NAPR" = "id"), suffix=c("", ".x")) %>%
                  ungroup() %>%
                  select(-NAPR) %>%
                  mutate(PERIOD = as.Date(PERIOD)) %>%
                  rename("NAPR" = "NAPR.x")
                
                if (w=='нет') {
                  result <- result %>%
                    left_join(tnvedIds, by=c("TNVED" = "id"), suffix=c("", ".x")) %>%
                    select(-TNVED) %>%
                    rename("TNVED" = "TNVED.x") %>%
                    left_join(edizmIds, by=c("EDIZM" = "id"), suffix=c("", ".x")) %>%
                    select(-EDIZM) %>%
                    rename("EDIZM" = "EDIZM.x")
                }
                
                if (y=='нет') {
                  result <- result %>%
                    left_join(countryIds, by=c("STRANA" = "id"), suffix=c("", ".x")) %>%
                    select(-STRANA) %>%
                    rename("STRANA" = "STRANA.x")
                  flags <- data.frame(
                    STRANA = unique(result$STRANA),
                    FLAG = paste0(
                      '<img src="https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/',
                      tolower(unique(result$STRANA)), '.svg" height=15>')
                  )
                  result <- result %>%
                    left_join(flags)
                  result <- result %>%
                    select(STRANA, FLAG, PERIOD, NAPR, TNVED, STOIM, NETTO, KOL, EDIZM)
                } else {
                  result <- result %>%
                    select(STRANA, PERIOD, NAPR, TNVED, STOIM, NETTO, KOL, EDIZM)
                }
                

                
                
                  
                output$aggTable = DT::renderDataTable(
                  result, extensions = 'Buttons', escape = FALSE,
                            options = list(scrollX = TRUE,
                                           scrollY = FALSE
                                           , pageLength = 20
                                           , dom = 'Blfrtip'
                                           , buttons = 'excel'
                                           , columnDefs = list(list(className = 'dt-center', targets = "_all"))
                            ), server = F
                )

               })

}

shinyApp(ui, server)