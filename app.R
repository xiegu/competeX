library(shiny)
library(shinydashboard)
library(magrittr)
library(RMySQL)
library(DT)
library(scales)
library(jiebaR)
library(dplyr)
library(highcharter)
library(wordcloud2)
library(purrr)

#source('config.R', local = TRUE)
#source('data.R', local = TRUE)
load('data.RData')

ui <- dashboardPage(skin = 'red',
                    dashboardHeader(title = 'CompeteX'),
                    dashboardSidebar(
                      sidebarMenu(
                        br(),
                        menuItem('竞品全景', tabName = 'Competitor', icon = icon('th')),
                        menuItem('热卖商品分析', tabName = 'HotSales', icon = icon('th')),
                        menuItem('热门搜索词分析', tabName = 'HotSearch', icon = icon('th')),
                        menuItem('评论抓取监视', tabName = "CommentCrawl", icon = icon("dashboard")),
                        menuItem('用户偏好画像', tabName = 'CommentScore', icon = icon('th')),
                        menuItem('用户感知画像', tabName = 'CommentSentiment', icon = icon('th'))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = 'Competitor',
                                fluidRow(
                                  column(
                                    offset = 10,
                                    width = 2,
                                    valueBox(width = NULL, color = 'red', icon = icon('refresh', class = 'fa-spin fa-inverse'), subtitle = '数据更新时间: ', value = h4(strong(com$update_time[1])))
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 2,
                                    checkboxInput('Outlier', '是否包括价格极高/极低的商品？', value = TRUE)
                                  )
                                ),
                                fluidRow(
                                  box(status = 'danger',
                                      title = '冰箱最新价格全景',
                                      width = 5,
                                      highchartOutput('PriceRef', height = '400px')
                                  ),
                                  box(status = 'success',
                                      title = '冰箱最新在售商品数',
                                      width = 3,
                                      highchartOutput('TopRef', height = '400px')
                                  ),
                                  box(status = 'primary',
                                      title = '冰箱最新市场容量',
                                      width = 4,
                                      highchartOutput('SaleRef', height = '400px')
                                  )
                                ),
                                fluidRow(
                                  box(status = 'danger',
                                      title = '空调最新价格全景',
                                      width = 5,
                                      highchartOutput('PriceAir', height = '400px')
                                  ),
                                  box(status = 'success',
                                      title = '空调最新在售商品数',
                                      width = 3,
                                      highchartOutput('TopAir', height = '400px')
                                  ),
                                  box(status = 'primary',
                                      title = '空调最新市场容量',
                                      width = 4,
                                      highchartOutput('SaleAir', height = '400px')
                                  )
                                ),
                                fluidRow(
                                  box(status = 'danger',
                                      title ='洗衣机最新价格全景',
                                      width = 5,
                                      highchartOutput('PriceWash', height = '400px')
                                  ),
                                  box(status = 'success',
                                      title = '洗衣机最新在售商品数',
                                      width = 3,
                                      highchartOutput('TopWash', height = '400px')
                                  ),
                                  box(status = 'primary',
                                      title = '洗衣机最新市场容量',
                                      width = 4,
                                      highchartOutput('SaleWash', height = '400px')
                                  )
                                ),
                                fluidRow(
                                  box(status = 'danger',
                                      title = '电视最新价格全景',
                                      width = 5,
                                      highchartOutput('PriceTV', height = '400px')
                                  ),
                                  box(status = 'success',
                                      title = '电视最新在售商品数',
                                      width = 3,
                                      highchartOutput('TopTV', height = '400px')
                                  ),
                                  box(status = 'primary',
                                      title = '电视最新市场容量',
                                      width = 4,
                                      highchartOutput('SaleTV', height = '400px')
                                  )
                                )
                                
                        ),
                        tabItem(tabName = "CommentCrawl",
                                fluidRow(
                                  valueBox(
                                    subtitle = 'No of comments',
                                    value = textOutput('Comments'),
                                    color = 'teal',
                                    icon = icon('money')
                                  ),
                                  valueBox(
                                    subtitle = 'No of item ID',
                                    value = textOutput('Ids'),
                                    color = 'teal',
                                    icon = icon('money')
                                  )
                                ),
                                fluidRow(
                                  column(width =12,
                                         dataTableOutput('Details')
                                  )
                                )
                        ),
                        tabItem(tabName = 'HotSales',
                                fluidRow(
                                  column(
                                    offset = 10,
                                    width = 2,
                                    valueBox(width = NULL, color = 'red', icon = icon('refresh', class = 'fa-spin fa-inverse'), subtitle = '数据更新时间: ', value = h4(strong(sale$update_time[1])))
                                  )
                                ),
                                # fluidRow(icon = icon('refresh', class = 'fa-spin fa-inverse'),
                                #   column(width = 2,
                                #          selectInput('HotCategory', label = '类别', choices = unique(sale$category))
                                #   ),
                                #   column(width = 2,
                                #          uiOutput('HotBrand')
                                #   )
                                # ),
                                fluidRow(
                                  box(status = 'primary',
                                      title = '各品牌热卖商品排行榜', 
                                      width = 6,
                                      height = '700px',
                                      fluidRow(
                                        column(width = 2,
                                               selectInput('HotCategory', label = '类别', choices = unique(sale$category))
                                        ),
                                        column(width = 2,
                                               uiOutput('HotBrand')
                                        )
                                      ),
                                      dataTableOutput('HotName', height = 'auto')
                                  ),
                                  tabBox(
                                    title = '热卖商品关键字',
                                    side = 'right',
                                    width = 6,
                                    tabPanel('冰箱',
                                             wordcloud2Output('RefWord', height = '400px')
                                    ),
                                    tabPanel('空调',
                                             wordcloud2Output('AirWord', height = '400px')
                                    ),
                                    tabPanel('洗衣机',
                                             wordcloud2Output('WashWord', height = '400px')
                                    ),
                                    tabPanel('电视',
                                             wordcloud2Output('TVWord', height = '400px')
                                    )
                                  )
                                ),
                                fluidRow(
                                  tabBox(title = NULL,
                                         side = 'right',
                                         width = 6,
                                         tabPanel('最新热卖冰箱排行榜',
                                                  #p('')
                                                  dataTableOutput('SaleRefTop')
                                         ),
                                         tabPanel(
                                           title = '最新热卖冰箱品牌占比',
                                           highchartOutput('SaleBrandRef')
                                         )
                                  ),
                                  tabBox(title = NULL,
                                         side = 'right',
                                         width = 6,
                                         tabPanel('最新热卖空调排行榜',
                                                  #p('')
                                                  dataTableOutput('SaleAirTop')
                                         ),
                                         tabPanel(
                                           title = '最新热卖空调品牌占比',
                                           highchartOutput('SaleBrandAir')
                                         )
                                  )
                                ),
                                fluidRow(
                                  tabBox(title = NULL,
                                         side = 'right',
                                         width = 6,
                                         tabPanel('最新热卖洗衣机排行榜',
                                                  #p('')
                                                  dataTableOutput('SaleWashTop')
                                         ),
                                         tabPanel(
                                           title = '最新热卖洗衣机品牌占比',
                                           highchartOutput('SaleBrandWash')
                                         )
                                  ),
                                  tabBox(title = NULL,
                                         side = 'right',
                                         width = 6,
                                         tabPanel('最新热卖电视排行榜',
                                                  #p('')
                                                  dataTableOutput('SaleTVTop')
                                         ),
                                         tabPanel(
                                           title = '最新热卖电视品牌占比',
                                           highchartOutput('SaleBrandTV')
                                         )
                                  )
                                )
                        ),
                        tabItem(tabName = 'HotSearch',
                                fluidRow(
                                  column(
                                    offset = 10,
                                    width = 2,
                                    valueBox(width = NULL, color = 'red', icon = icon('refresh', class = 'fa-spin fa-inverse'), subtitle = '数据更新时间: ', value = h4(strong(search$update_time[1])))
                                  )
                                ),
                                fluidRow(
                                  box(status = 'primary',
                                      title = '热门搜索词分布',
                                      width = 6,
                                      highchartOutput('HotSearchBar', height = '800px')
                                  ),
                                  box(status = 'primary',
                                      title = '热门搜索词类占比',
                                      width = 6,
                                      highchartOutput('HotSearchPie', height = '600px'))
                                )
                        ),
                        tabItem(tabName = 'CommentScore',
                                fluidRow(
                                  column(
                                    offset = 10,
                                    width = 2,
                                    valueBox(width = NULL, color = 'red', icon = icon('refresh', class = 'fa-spin fa-inverse'), subtitle = '数据更新时间: ', value = h4(strong(score_full$update_time[1])))
                                  )
                                ),
                                fluidRow(
                                  box(status = 'info',
                                      title = '冰箱最新客户品牌偏好度',
                                      width = 4,
                                      highchartOutput('ScoreRef', height = '600px')
                                  ),
                                  tabBox(
                                    title = NULL,
                                    width = 8,
                                    side = 'right',
                                    height = 'auto',
                                    tabPanel(
                                      title = '冰箱各品牌商品得分',
                                      fluidRow(
                                        column(width = 2,
                                               selectInput('RefBrandSelector', label = '品牌', choices = unique(filter(score_full, category == 'ref')$brand))
                                        )
                                      ),
                                      dataTableOutput('ScoreRefBrand')
                                    ),
                                    tabPanel(
                                      title = '冰箱商品用户偏好得分排行榜',
                                      dataTableOutput('ScoreRefTop')
                                    )
                                  )
                                  
                                ),
                                fluidRow(
                                  box(status = 'info',
                                      title = '空调最新客户品牌偏好度',
                                      width = 4,
                                      highchartOutput('ScoreAir', height = '600px')
                                  ),
                                  tabBox(
                                    title = NULL,
                                    width = 8,
                                    side = 'right',
                                    tabPanel(
                                      title = '空调各品牌商品得分',
                                      fluidRow(
                                        column(width = 2,
                                               selectInput('AirBrandSelector', label = '品牌', choices = unique(filter(score_full, category == 'air')$brand))
                                        )),
                                      dataTableOutput('ScoreAirBrand')
                                    ),
                                    tabPanel(
                                      title = '空调商品用户偏好排行榜',
                                      dataTableOutput('ScoreAirTop')
                                    )
                                  )
                                  
                                ),
                                fluidRow(
                                  box(status = 'info',
                                      title = '洗衣机最新客户品牌偏好度',
                                      width = 4,
                                      highchartOutput('ScoreWash', height = '600px')
                                  ),
                                  tabBox(
                                    title = NULL,
                                    side = 'right',
                                    width = 8,
                                    tabPanel(
                                      title = '洗衣机各品牌商品得分',
                                      fluidRow(
                                        column(width = 2, 
                                               selectInput('WashBrandSelector', label = '品牌', choices = unique(filter(score_full, category == 'wash')$brand))
                                        )
                                      ),
                                      dataTableOutput('ScoreWashBrand')
                                    ),
                                    tabPanel(
                                      title = '洗衣机商品用户偏好排行榜',
                                      dataTableOutput('ScoreWashTop')
                                    )
                                  )
                                  
                                ),
                                fluidRow(
                                  box(status = 'info',
                                      title = '电视最新客户品牌偏好度',
                                      width = 4,
                                      highchartOutput('ScoreTV', height = '600px')
                                  ),
                                  tabBox(
                                    title = NULL,
                                    side = 'right',
                                    width = 8,
                                    tabPanel(
                                      title = '电视各品牌商品得分',
                                      fluidRow(
                                        column(width = 2,
                                               selectInput('TVBrandSelector', label = '品牌', choices = unique(filter(score_full, category == 'tv')$brand))
                                        )
                                      ),
                                      dataTableOutput('ScoreTVBrand')
                                      #dataTableOutput('ScoreTVBrand', height = '400px')
                                    ),
                                    tabPanel(
                                      title = '电视商品用户偏好排行榜',
                                      dataTableOutput('ScoreTVTop')
                                    )
                                  )
                                  
                                )
                        ),
                        tabItem(tabName = 'CommentSentiment',
                                fluidRow(
                                  column(
                                    offset = 10,
                                    width = 2,
                                    valueBox(width = NULL, color = 'red', icon = icon('refresh', class = 'fa-spin fa-inverse'), subtitle = '数据更新时间: ', value = h4(strong(score_full$update_time[1]))) # to udapte
                                  )
                                ),
                                fluidRow(
                                  box(status = 'danger',
                                      title = '用户感知度',
                                      width = 12,
                                      fluidRow(
                                        column(width = 2,
                                               selectInput('CommentGoP', label = '客户感知度', choices = c('好评' = 'good', '差评' = 'poor'))
                                        ),
                                        column(width = 2,
                                               selectInput('CommentCat', label = '类别', choices = c(
                                                                                                  '冰箱' = 'ref', 
                                                                                                  '空调' = 'air',
                                                                                                  '洗衣机' = 'wash', 
                                                                                                  '电视' = 'tv'))
                                        ),
                                        column(width = 2,
                                               selectInput('CommentDirection', label = '方向', choices = c(
                                                                                                    '纵向' = 'vertical',
                                                                                                    '横向' = 'horizontal'
                                               ))
                                        )
                                      ),
                                      uiOutput('CommentBrand')
                                      )
                                )
                        )
                        )
                      )
                    )

server <- shinyServer(function(input, output, session){
  
  output$TopRef <- renderHighchart({
    highchart()%>%
      #hc_title(text = '冰箱在售商品数')%>%
      hc_chart(type ='bar')%>%
      hc_xAxis(categories = head(top_ref,10)$brand)%>%
      hc_add_series(data = head(top_ref,10)$num, color = '#d62d20', name = '商品数')%>%
      hc_legend(enabled = FALSE)
  })
  
  output$TopAir <- renderHighchart({
    highchart()%>%
      #hc_title(text = '空调在售商品数')%>%
      hc_chart(type ='bar')%>%
      hc_xAxis(categories = head(top_air,10)$brand)%>%
      hc_add_series(data = head(top_air,10)$num, color = '#0057e7', name = '商品数')%>%
      hc_legend(enabled = FALSE)
  })
  
  output$TopWash <- renderHighchart({
    highchart()%>%
      #hc_title(text = '洗衣机在售商品数')%>%
      hc_chart(type ='bar')%>%
      hc_xAxis(categories = head(top_wash,10)$brand)%>%
      hc_add_series(data = head(top_wash,10)$num, color = '#008744', name = '商品数')%>%
      hc_legend(enabled = FALSE)
  })
  
  output$TopTV <- renderHighchart({
    highchart()%>%
      #hc_title(text = '电视在售商品数')%>%
      hc_chart(type ='bar')%>%
      hc_xAxis(categories = head(top_tv,10)$brand)%>%
      hc_add_series(data = head(top_tv,10)$num, color = '#ffa700', name = '商品数')%>%
      hc_legend(enabled = FALSE)
  })
  
  
  
  output$PriceRef <- renderHighchart({
    highchart()%>%
      #hc_title(text = '冰箱最新价格全景')%>%
      hc_add_series_boxplot(x = com_ref$p, by = com_ref$brand, name = 'Price', color = '#d62d20', outliers = input$Outlier)%>%
      hc_legend(enabled = FALSE)
  })
  
  output$PriceAir <- renderHighchart({
    highchart()%>%
      #hc_title(text = '空调最新价格全景')%>%
      hc_add_series_boxplot(x = com_air$p, by =com_air$brand, name = 'Price', color = '#0057e7', outliers = input$Outlier)%>%
      hc_legend(enabled = FALSE)
  })
  
  output$PriceWash <- renderHighchart({
    highchart()%>%
      #hc_title(text = '洗衣机最新价格全景')%>%
      hc_add_series_boxplot(x = com_wash$p, by = com_wash$brand, name = 'Price', color = '#008744', outliers = input$Outlier)%>%
      hc_legend(enabled = FALSE)
  })
  
  output$PriceTV <- renderHighchart({
    highchart()%>%
      #hc_title(text = '电视最新价格全景')%>%
      hc_add_series_boxplot(x = com_tv$p, by = com_tv$brand, name = 'Price', color = '#ffa700', outliers = input$Outlier)%>%
      hc_legend(enabled = FALSE)
  })

  output$SaleRef <- renderHighchart({
    hchart(top_ref, 'treemap', hcaes(x = brand, value = sale, color = sale))%>%
      hc_colorAxis(minColor= '#FFFFFF',maxColor= '#d62d20') %>%
      hc_legend(enabled = FALSE)
  })
  
  output$SaleAir <- renderHighchart({
    hchart(top_air, 'treemap', hcaes(x = brand, value = sale, color = sale))%>%
      hc_colorAxis(minColor= '#FFFFFF',maxColor= '#0057e7') %>%
      hc_legend(enabled = FALSE)
  })
  
  output$SaleWash <- renderHighchart({
    hchart(top_wash, 'treemap', hcaes(x = brand, value = sale, color = sale)) %>%
      hc_colorAxis(minColor= '#FFFFFF',maxColor= '#008744') %>%
      hc_legend(enabled = FALSE)
  })
  
  output$SaleTV <- renderHighchart({
    hchart(top_tv, 'treemap', hcaes(x = brand, value = sale, color = sale))%>%
      hc_colorAxis(minColor= '#FFFFFF',maxColor= '#ffa700') %>%
      hc_legend(enabled = FALSE)
  })
  
  output$RefWord <- renderWordcloud2({
    word <- table(hot_sale_cat_key$ref)%>%as.data.frame%>%subset(., !Var1%in%stop_words$ref)%>%arrange(-Freq)%>%head(50)
    wordcloud2(word, color= ifelse(word$Freq > 25, '#d62d20', 'grey'))
  })
  
  output$AirWord <- renderWordcloud2({
    word <- table(hot_sale_cat_key$air)%>%as.data.frame%>%subset(., !Var1%in%stop_words$air)%>%arrange(-Freq)%>%head(50)
    wordcloud2(word, color = ifelse(word$Freq> 25, '#0057e7', 'grey'))
  })
  
  output$WashWord <- renderWordcloud2({
    word <- table(hot_sale_cat_key$wash)%>%as.data.frame%>%subset(., !Var1%in%stop_words$wash)%>%arrange(-Freq)%>%head(50)
    wordcloud2(word, color = ifelse(word$Freq > 25 , '#008744', 'grey'))
  })
  
  output$TVWord <- renderWordcloud2({
    word <- table(hot_sale_cat_key$tv)%>%as.data.frame%>%subset(., !Var1%in%stop_words$tv)%>%arrange(-Freq)%>%head(50)
    wordcloud2(word, color = ifelse(word$Freq > 25, '#ffa700', 'grey'))
  })
  
  output$HotBrand <- renderUI({
    table <- subset(sale, category == input$HotCategory)
    brand <- unique(table$brand)
    selectInput('HotBrand', '品牌', choices = brand)
  })
  
  output$HotName <- renderDataTable({
    table <- subset(sale, (category == input$HotCategory) & (brand == input$HotBrand))%>%select(category, brand, id, name)
    datatable(table, rownames = FALSE)
  })
  
  output$SaleBrandRef <- renderHighchart({
    table <- filter(sale_brand, category == 'ref') %>%ungroup() %>% mutate(pct = n/sum(n)*100) %>% select(brand, pct) %>% rename(name = brand, y = pct)
    table2 <- list_parse(table)
    highchart()%>%
      hc_add_series(type = 'pie', data = table2, innerSize = '40%', name = '占比') %>%
      hc_plotOptions(pie = list(allowPointSelect = TRUE,
                                showInLegend = FALSE,
                                cursor = 'pointer',
                                dataLabels = list(
                                  enabled = TRUE,
                                  format = '<b>{point.name}</b>: {point.percentage:.1f} %'),
                                connectorColor = 'silver')) %>%
      hc_tooltip(pointFormat= '{series.name}: <b>{point.percentage:.1f}%</b>')
  })
  
  output$SaleBrandAir <- renderHighchart({
    table <- filter(sale_brand, category == 'air') %>%ungroup() %>% mutate(pct = n/sum(n)*100) %>% select(brand, pct) %>% rename(name = brand, y = pct)
    table2 <- list_parse(table)
    highchart()%>%
      hc_add_series(type = 'pie', data = table2, innerSize = '40%', name = '占比') %>%
      hc_plotOptions(pie = list(allowPointSelect = TRUE,
                                showInLegend = FALSE,
                                cursor = 'pointer',
                                dataLabels = list(
                                  enabled = TRUE,
                                  format = '<b>{point.name}</b>: {point.percentage:.1f} %'),
                                connectorColor = 'silver')) %>%
      hc_tooltip(pointFormat= '{series.name}: <b>{point.percentage:.1f}%</b>')
  })
  
  output$SaleBrandWash <- renderHighchart({
    table <- filter(sale_brand, category == 'wash') %>%ungroup() %>% mutate(pct = n/sum(n)*100) %>% select(brand, pct) %>% rename(name = brand, y = pct)
    table2 <- list_parse(table)
    highchart()%>%
      hc_add_series(type = 'pie', data = table2, innerSize = '40%', name = '占比') %>%
      hc_plotOptions(pie = list(allowPointSelect = TRUE,
                                showInLegend = FALSE,
                                cursor = 'pointer',
                                dataLabels = list(
                                  enabled = TRUE,
                                  format = '<b>{point.name}</b>: {point.percentage:.1f} %'),
                                connectorColor = 'silver')) %>%
      hc_tooltip(pointFormat= '{series.name}: <b>{point.percentage:.1f}%</b>')
  })
  
  output$SaleBrandTV <- renderHighchart({
    table <- filter(sale_brand, category == 'tv') %>%ungroup() %>% mutate(pct = n/sum(n)*100) %>% select(brand, pct) %>% rename(name = brand, y = pct)
    table2 <- list_parse(table)
    highchart()%>%
      hc_add_series(type = 'pie', data = table2, innerSize = '40%', name = '占比') %>%
      hc_plotOptions(pie = list(allowPointSelect = TRUE,
                                showInLegend = FALSE,
                                cursor = 'pointer',
                                dataLabels = list(
                                  enabled = TRUE,
                                  format = '<b>{point.name}</b>: {point.percentage:.1f} %'),
                                connectorColor = 'silver')) %>%
      hc_tooltip(pointFormat= '{series.name}: <b>{point.percentage:.1f}%</b>')
  })
  
  output$SaleRefTop <- renderDataTable({
    table <- select(sale_ref, brand, id, name)
    datatable(table) %>%
      formatStyle(
        'brand',
        color = 'white',
        backgroundColor = styleEqual(
          unique(sale_ref$brand), color_brand[1:length(unique(sale_ref$brand))])
        )
  })
  
  output$SaleAirTop <- renderDataTable({
    table <- select(sale_air, brand, id, name)
    datatable(table)%>%
      formatStyle(
        'brand',
        color = 'white',
        backgroundColor = styleEqual(
          unique(sale_air$brand), color_brand[1:length(unique(sale_air$brand))])
        )
  })
  
  output$SaleWashTop <- renderDataTable({
    table <- select(sale_wash, brand, id, name)
    datatable(table)%>%
      formatStyle(
        'brand',
        color = 'white',
        backgroundColor = styleEqual(
          unique(sale_wash$brand), color_brand[1:length(unique(sale_wash$brand))])
        )
  })
  
  output$SaleTVTop <- renderDataTable({
    table <- select(sale_tv, brand, id, name)
    datatable(table) %>%
      formatStyle(
        'brand',
        color = 'white',
        backgroundColor = styleEqual(
          unique(sale_tv$brand), color_brand[1:length(unique(sale_tv$brand))])
        )
  })
  
  output$HotSearchBar <- renderHighchart({
   table <- filter(search_table, category != 'mobile') %>% select(word, color_2, count) %>% rename(name = word, color = color_2, y = count)
   table2 <- list_parse(table)
    highchart() %>% 
      hc_xAxis(categories = table$name) %>% 
      #hc_yAxis(title = list(text = NULL)) %>% 
      hc_add_series(data = table2, type = "bar", showInLegend = FALSE,
                    name = '搜索量')
  })
  
  output$HotSearchPie <- renderHighchart({
    table <- filter(search_table, category != 'mobile') %>%group_by(group, color_2)%>%summarize(n = sum(count))%>%ungroup()%>%mutate(pct = n/sum(n)*100)%>%rename(name = group, y =pct, color = color_2)%>%select(name, y, color)
    table2 <- list_parse(table)
    highchart()%>%
      hc_add_series(type = 'pie', data = table2, name = '占比')%>%
      hc_plotOptions(pie = list(allowPointSelect = TRUE,
                                showInLegend = TRUE,
                                cursor = 'pointer',
                                dataLabels = list(
                                  enabled = TRUE,
                                  format = '<b>{point.name}</b>: {point.percentage:.1f} %'),
                                connectorColor = 'silver'))%>%
      hc_tooltip(pointFormat= '{series.name}: <b>{point.percentage:.1f}%</b>') 
  })
  
  s <- reactive({
    invalidateLater(50000, session)
    comment_summary()})
  
  output$Comments <- renderText({
    com <- s()$comments
    prettyNum(com, big.mark = ',')
  })
  
  output$Ids <- renderText({
    id <- s()$ids
    prettyNum(id, big.mark = ',')
  })
  
  output$Details <- renderDataTable({
    invalidateLater(50000, session)
    d <- comment_detail()
    datatable(d, rownames = FALSE)
  })
  
  output$ScoreRef <- renderHighchart({
    table <- filter(score_full, category == 'ref')%>% group_by(brand) %>% summarize(comment_num = sum(comment_count), good = sum(good_count), poor = sum(poor_count), good_rate = good/comment_num, poor_rate = poor/comment_num)%>%arrange(-comment_num)%>%head(10)
    table <- mutate(table, adj_good_rate = good_rate/max(good_rate), adj_poor_rate = poor_rate/max(poor_rate))
    table <- arrange(table, adj_good_rate)
    highchart()%>%
      #hc_title(text = '电视在售商品数')%>%
      hc_chart(type ='bar')%>%
     #hc_yAxis(labels= list(format='{function() {return Math.abs(value)}}')) %>%
      hc_xAxis(categories = table$brand, reversed = FALSE)%>%
      hc_xAxis(categories = table$brand, opposite = TRUE, reversed = FALSE) %>%
      hc_add_series(data = table$adj_good_rate, name = '相对好评率', color = '#d62d20', label = list(step = 1))%>%
      hc_add_series(data = -table$adj_poor_rate, name = '相对差评率', color = 'black', label = list(step = 1)) %>%
      hc_plotOptions(series = list(stacking = 'normal'))%>%
      hc_tooltip(pointFormat = '{series.name}: {point.y: .3f}') %>% 
      hc_legend(enabled = TRUE) %>%
      hc_add_theme(hc_theme_google())
      #'{series.name}: <b>{point.percentage:.1f}%</b>'
  })
  
  output$ScoreAir <- renderHighchart({
    table <- filter(score_full, category == 'air')%>% group_by(brand) %>% summarize(comment_num = sum(comment_count), good = sum(good_count), poor = sum(poor_count), good_rate = good/comment_num, poor_rate = poor/comment_num)%>%arrange(-comment_num)%>%head(10)
    table <- mutate(table, adj_good_rate = good_rate/max(good_rate), adj_poor_rate = poor_rate/max(poor_rate))
    table <- arrange(table, adj_good_rate)
    highchart()%>%
      #hc_title(text = '电视在售商品数')%>%
      hc_chart(type ='bar')%>%
      #hc_yAxis(labels= list(format='{function() {return Math.abs(value)}}')) %>%
      hc_xAxis(categories = table$brand, reversed = FALSE)%>%
      hc_xAxis(categories = table$brand, opposite = TRUE, reversed = FALSE) %>%
      hc_add_series(data = table$adj_good_rate, name = '相对好评率', color = '#0057e7', label = list(step = 1))%>%
      hc_add_series(data = -table$adj_poor_rate, name = '相对差评率', color = 'black', label = list(step = 1)) %>%
      hc_plotOptions(series = list(stacking = 'normal'))%>%
      hc_tooltip(pointFormat = '{series.name}: {point.y: .3f}') %>% 
      hc_legend(enabled = TRUE) %>%
      hc_add_theme(hc_theme_google())
    #'{series.name}: <b>{point.percentage:.1f}%</b>'
  })
  
  output$ScoreWash <- renderHighchart({
    table <- filter(score_full, category == 'wash')%>% group_by(brand) %>% summarize(comment_num = sum(comment_count), good = sum(good_count), poor = sum(poor_count), good_rate = good/comment_num, poor_rate = poor/comment_num)%>%arrange(-comment_num)%>%head(10)
    table <- mutate(table, adj_good_rate = good_rate/max(good_rate), adj_poor_rate = poor_rate/max(poor_rate))
    table <- arrange(table, adj_good_rate)
    highchart()%>%
      #hc_title(text = '电视在售商品数')%>%
      hc_chart(type ='bar')%>%
      #hc_yAxis(labels= list(format='{function() {return Math.abs(value)}}')) %>%
      hc_xAxis(categories = table$brand, reversed = FALSE)%>%
      hc_xAxis(categories = table$brand, opposite = TRUE, reversed = FALSE) %>%
      hc_add_series(data = table$adj_good_rate, name = '相对好评率', color = '#008744', label = list(step = 1))%>%
      hc_add_series(data = -table$adj_poor_rate, name = '相对差评率', color = 'black', label = list(step = 1)) %>%
      hc_plotOptions(series = list(stacking = 'normal'))%>%
      hc_tooltip(pointFormat = '{series.name}: {point.y: .3f}') %>% 
      hc_legend(enabled = TRUE) %>%
      hc_add_theme(hc_theme_google())
    #'{series.name}: <b>{point.percentage:.1f}%</b>'
  })
  
  output$ScoreTV <- renderHighchart({
    table <- filter(score_full, category == 'tv')%>% group_by(brand) %>% summarize(comment_num = sum(comment_count), good = sum(good_count), poor = sum(poor_count), good_rate = good/comment_num, poor_rate = poor/comment_num)%>%arrange(-comment_num)%>%head(10)
    table <- mutate(table, adj_good_rate = good_rate/max(good_rate), adj_poor_rate = poor_rate/max(poor_rate))
    table <- arrange(table, adj_good_rate)
    highchart()%>%
      #hc_title(text = '电视在售商品数')%>%
      hc_chart(type ='bar')%>%
      #hc_yAxis(labels= list(format='{function() {return Math.abs(value)}}')) %>%
      hc_xAxis(categories = table$brand, reversed = FALSE)%>%
      hc_xAxis(categories = table$brand, opposite = TRUE, reversed = FALSE) %>%
      hc_add_series(data = table$adj_good_rate, name = '相对好评率',  color = '#ffa700', label = list(step = 1))%>%
      hc_add_series(data = -table$adj_poor_rate, name = '相对差评率', color = 'black', label = list(step = 1)) %>%
      hc_plotOptions(series = list(stacking = 'normal'))%>%
      hc_tooltip(pointFormat = '{series.name}: {point.y: .3f}') %>% 
      hc_legend(enabled = TRUE) #%>%
      #hc_add_theme(hc_theme_google())
    #'{series.name}: <b>{point.percentage:.1f}%</b>'
  })
  
  output$ScoreRefBrand <- renderDataTable({
    table <- filter(score_full, (category == 'ref') & (brand == input$RefBrandSelector)) %>% select(id, name, comment_count, good_rate, poor_rate) %>% arrange(-comment_count)
    datatable(table, rownames = FALSE)
    })
  
  output$ScoreAirBrand <- renderDataTable({
    table <- filter(score_full, (category == 'air') & (brand == input$AirBrandSelector)) %>% select(id, name, comment_count, good_rate, poor_rate) %>% arrange(-comment_count)
    datatable(table, rownames = FALSE)
  })
  
  output$ScoreWashBrand <- renderDataTable({
    table <- filter(score_full, (category == 'wash') & (brand == input$WashBrandSelector)) %>% select(id, name, comment_count, good_rate, poor_rate) %>% arrange(-comment_count)
    datatable(table, rownames = FALSE)
  })
  
  output$ScoreTVBrand <- renderDataTable({
    table <- filter(score_full, (category == 'tv') & (brand == input$TVBrandSelector)) %>% select(id, name, comment_count, good_rate, poor_rate) %>% arrange(-comment_count)
    datatable(table, rownames = FALSE)
  })
  
  output$ScoreRefTop <- renderDataTable({
    table <- filter(score_full, category == 'ref') %>% select(id, name, brand, score) %>% top_n(20, score)
    datatable(table, rownames = FALSE)%>%
      formatStyle(
        'score',
        background = styleColorBar(table$score, '#d62d20'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  output$ScoreAirTop <- renderDataTable({
    table <- filter(score_full, category == 'air') %>% select(id, name, brand, score) %>% top_n(20, score)
    datatable(table, rownames = FALSE)%>%
      formatStyle(
        'score',
        background = styleColorBar(table$score, '#0057e7'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  output$ScoreWashTop <- renderDataTable({
    table <- filter(score_full, category == 'wash') %>% select(id, name, brand, score) %>% top_n(20, score)
    datatable(table, rownames = FALSE)%>%
      formatStyle(
        'score',
        background = styleColorBar(table$score, '#008744'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  output$ScoreTVTop <- renderDataTable({
    table <- filter(score_full, category == 'tv') %>% select(id, name, brand, score) %>% top_n(20, score)
    datatable(table, rownames = FALSE) %>%
      formatStyle(
        'score',
        background = styleColorBar(table$score, '#ffa700'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  output$CommentBrand <- renderUI({
    withProgress(message = '正在加载...', value = 0,
                 {
                   if(input$CommentGoP == 'good'){
                     match_df <- switch(input$CommentCat,
                                        ref = match_ref_df_good,
                                        air = match_air_df_good,
                                        wash = match_wash_df_good,
                                        tv = match_tv_df_good)
                     n_df <- switch(input$CommentCat,
                                    ref = commentC_ref_good_n,
                                    air = commentC_air_good_n,
                                    wash = commentC_wash_good_n,
                                    tv = commentC_tv_good_n)
                   }else{
                     match_df <- switch(input$CommentCat,
                                        ref = match_ref_df_poor,
                                        air = match_air_df_poor,
                                        wash = match_wash_df_poor,
                                        tv = match_tv_df_poor)
                     n_df <- switch(input$CommentCat,
                                    ref = commentC_ref_poor_n,
                                    air = commentC_air_poor_n,
                                    wash = commentC_wash_poor_n,
                                    tv = commentC_tv_poor_n)
                   }
                   creat_hc <- function(t){
                     table <- filter(match_df, brand == t)
                     nt <- filter(n_df, brand == t)$n
                     table <- data.frame(name = names(table)[-1], y = as.numeric(table[1,-1])) %>% mutate(color = head(color_brand, ncol(match_df)-1)) %>%mutate(y = y/nt*100)
                     table2 <- list_parse(table)
                     highchart() %>% 
                       hc_title(text = t) %>%
                       hc_xAxis(categories = table$name) %>% 
                       hc_yAxis(labels = list(format = "{value}%")) %>% 
                       hc_add_series(data = table2, type = "bar", showInLegend = FALSE,
                                     name = '占比') %>%
                       hc_tooltip(pointFormat= '{series.name}: <b>{point.y:.1f}%</b>') 
                   }
                   
                   creat_hc2 <- function(t){
                     col_col <- head(color_brand, ncol(match_df)-1)
                     brands <- head(match_df, 10)$brand %>%as.character
                     names(col_col) <- names(match_df)[-1]
                     table <- select(match_df, 'brand', t) %>% inner_join(n_df, by = c('brand' = 'brand')) %>% mutate(y = get(t)/n * 100, color = col_col[t], name = brand) %>% select(name, y, color) %>%
                       filter(name %in% brands)
                     table2 <- list_parse(table)
                     highchart() %>%
                       hc_title(text = t) %>%
                       hc_xAxis(categories = table$name) %>% 
                       hc_yAxis(labels = list(format = "{value}%")) %>% 
                       hc_add_series(data = table2, type = "column", showInLegend = FALSE,
                                     name = '占比') %>%
                       hc_tooltip(pointFormat= '{series.name}: <b>{point.y:.1f}%</b>') 
                   }
                 })
    if(input$CommentDirection == 'vertical'){
      brands <- head(match_df, 10)$brand %>%as.character
      map(brands, creat_hc)%>% hw_grid(ncol = 5)
    }else if(input$CommentDirection == 'horizontal'){
      map(names(match_df)[-1], creat_hc2)%>% hw_grid(ncol = 5)
    }else{
      stop('Error!')
    }
  })
  
})




shinyApp(ui=ui,server=server)
