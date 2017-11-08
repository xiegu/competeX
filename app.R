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
library(jsonlite)

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
                        menuItem('用户感知画像', tabName = 'CommentSentiment', icon = icon('th')),

                        column(
                          width =12,
                          hr(),
                          h5(icon('refresh', class = 'fa-spin'), '数据更新时间: '),
                          strong(com$update_time[1])
                        )
                      )
                    ),
                    dashboardBody(
                      # Include IntroJS styling
                      includeCSS("www/introjs.css"),
                      
                      # Include styling for the app
                      #includeCSS("app.css"),
                      
                      # Include IntroJS library
                      includeScript("www/intro.js"),
                      
                      # Include javascript code to make shiny communicate with introJS
                      includeScript("www/app.js"),
#                       tags$style(HTML("
# 
# 
# .box.box-danger>.box-body {
#   color:#fff;
#   background:#666666
#                     }
# 
# .box.box-solid.box-primary{
# border-bottom-color:#666666;
# border-left-color:#666666;
# border-right-color:#666666;
# border-top-color:#666666;
# } ")),
                      tabItems(
                        tabItem(tabName = 'Competitor',
                                fluidRow(
                                  column(
                                    width = 1,
                                    actionButton(inputId="startHelp1", label="Help", class="btn-success")
                                  ),
                                  column(
                                    width = 2,
                                    checkboxInput('Outlier', '是否包括价格极高/极低的商品？', value = TRUE)
                                  ),
                                  
                                  column(
                                    width = 4,
                                    selectizeInput('MultiCat', label = NULL,  multiple = TRUE,  choices = c(
                                      '冰箱' = 'ref', 
                                      '空调' = 'air',
                                      '洗衣机' = 'wash', 
                                      '电视' = 'tv',
                                      '燃气灶' = 'gas',
                                      '油烟机' = 'hood',
                                      '洗碗机'= 'dish'), selected = c('ref', 'air', 'wash'), options = list(maxItems = 4) )
                                  )
                                ),
                                
                                uiOutput('ComProfile')
                                
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
                                  column(width = 1,
                                         actionButton(inputId="startHelp2", label="Help", class="btn-success")
                                  )
                                ),
                                br(),
                                fluidRow(
                                  box(id = 'step2_1',
                                      status = 'primary',
                                      title = '各品牌热卖商品排行榜', 
                                      width = 6,
                                      fluidRow(
                                        column(width = 2,
                                               selectInput('HotCategory', label = '类别', choices = c(
                                                 '冰箱' = 'ref', 
                                                 '空调' = 'air',
                                                 '洗衣机' = 'wash', 
                                                 '电视' = 'tv',
                                                 '燃气灶' = 'gas',
                                                 '油烟机' = 'hood',
                                                 '洗碗机'= 'dish'))
                                        ),
                                        column(width = 2,
                                               uiOutput('HotBrand')
                                        )
                                      ),
                                      dataTableOutput('HotName')
                                  ),
                                  tabBox(id = 'step2_2',
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
                                    ),
                                    tabPanel('燃气灶',
                                             wordcloud2Output('GasWord', height = '400px')
                                    ),
                                    tabPanel('油烟机',
                                             wordcloud2Output('HoodWord', height = '400px')
                                    ),
                                    tabPanel('洗碗机',
                                             wordcloud2Output('DishWord', height = '400px')
                                    )
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(
                                    width = 4,
                                    selectizeInput('MultiCat2', label = NULL, multiple = TRUE,  choices = c(
                                      '冰箱' = 'ref',
                                      '空调' = 'air',
                                      '洗衣机' = 'wash', 
                                      '电视' = 'tv',
                                      '燃气灶' = 'gas',
                                      '油烟机' = 'hood',
                                      '洗碗机'= 'dish'), selected = c('ref', 'air', 'wash'), options = list(maxItems = 4))
                                  )
                                ),
                                fluidRow(
                                uiOutput('HotSaleList')
                                )
                        ),
                        tabItem(tabName = 'HotSearch',
                                fluidRow(
                                  column(width = 2,
                                         actionButton(inputId="startHelp3", label="Help", class="btn-success")
                                  )
                                ),
                                br(),
                                fluidRow(
                                  box(id = 'step3_1',
                                      status = 'primary',
                                      title = '热门搜索词分布',
                                      width = 6,
                                      highchartOutput('HotSearchBar', height = '800px')
                                  ),
                                  box(id = 'step3_2',
                                      status = 'primary',
                                      title = '热门搜索词类占比',
                                      width = 6,
                                      highchartOutput('HotSearchPie', height = '600px'))
                                )
                        ),
                        tabItem(tabName = 'CommentScore',
                                fluidRow(
                                  column(width = 1,
                                         actionButton(inputId="startHelp4", label="Help", class="btn-success")
                                  ),
                                  column(width = 4,
                                         selectizeInput('MultiCat3', label = NULL, multiple = TRUE, choices = c(
                                           '冰箱' = 'ref', 
                                           '空调' = 'air',
                                           '洗衣机' = 'wash', 
                                           '电视' = 'tv',
                                           '燃气灶' = 'gas',
                                           '油烟机' = 'hood',
                                           '洗碗机'= 'dish'), selected = c('ref', 'air','wash'), options = list(maxItems = 4))
                                         
                                  )
                                ),
                               
                                uiOutput('UserPref')
                        ),
                        tabItem(tabName = 'CommentSentiment',
                                fluidRow(
                                  column(width = 1,
                                         actionButton(inputId="startHelp5", label="Help", class="btn-success")
                                  )
                                ),
                                br(),
                                fluidRow(
                                  box(id = 'step5_1',
                                      status = 'danger',
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
                                                                                                  '电视' = 'tv',
                                                                                                  '燃气灶' = 'gas',
                                                                                                  '油烟机' = 'hood',
                                                                                                  '洗碗机'= 'dish'))
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
  
  
  observeEvent(input$startHelp1,{
    # set help content
    session$sendCustomMessage(type = 'setHelpContent1', message = list(steps_1 = toJSON(steps_1)))
    
    # on click, send custom message to start help
    session$sendCustomMessage(type = 'startHelp1', message = list(""))
    
  })
  
  observeEvent(input$startHelp2,{
    session$sendCustomMessage(type = 'setHelpContent2', message = list(steps_2 = toJSON(steps_2)))
    # on click, send custom message to start help
    session$sendCustomMessage(type = 'startHelp2', message = list(""))
    
  })
  
  observeEvent(input$startHelp3,{
    session$sendCustomMessage(type = 'setHelpContent3', message = list(steps_3 = toJSON(steps_3)))
    # on click, send custom message to start help
    session$sendCustomMessage(type = 'startHelp3', message = list(""))
    
  })
  
  observeEvent(input$startHelp4,{
    session$sendCustomMessage(type = 'setHelpContent4', message = list(steps_4 = toJSON(steps_4)))
    # on click, send custom message to start help
    session$sendCustomMessage(type = 'startHelp4', message = list(""))
    
  })
  
  observeEvent(input$startHelp5,{
    session$sendCustomMessage(type = 'setHelpContent5', message = list(steps_5 = toJSON(steps_5)))
    # on click, send custom message to start help
    session$sendCustomMessage(type = 'startHelp5', message = list(""))
    
  })
  
  output$TopRef <- renderHighchart({
    highchart()%>%
      #hc_title(text = '冰箱在售商品数')%>%
      hc_chart(type ='bar')%>%
      hc_xAxis(categories = head(top_ref,10)$brand)%>%
      hc_add_series(data = head(top_ref,10)$num, color = '#fd8282', name = '商品数')%>%
      hc_legend(enabled = FALSE)
  })
  
  output$TopAir <- renderHighchart({
    highchart()%>%
      #hc_title(text = '空调在售商品数')%>%
      hc_chart(type ='bar')%>%
      hc_xAxis(categories = head(top_air,10)$brand)%>%
      hc_add_series(data = head(top_air,10)$num, color = '#1b75bb', name = '商品数')%>%
      hc_legend(enabled = FALSE)
  })
  
  output$TopWash <- renderHighchart({
    highchart()%>%
      #hc_title(text = '洗衣机在售商品数')%>%
      hc_chart(type ='bar')%>%
      hc_xAxis(categories = head(top_wash,10)$brand)%>%
      hc_add_series(data = head(top_wash,10)$num, color = '#43e8d8', name = '商品数')%>%
      hc_legend(enabled = FALSE)
  })
  
  output$TopTV <- renderHighchart({
    highchart()%>%
      #hc_title(text = '电视在售商品数')%>%
      hc_chart(type ='bar')%>%
      hc_xAxis(categories = head(top_tv,10)$brand)%>%
      hc_add_series(data = head(top_tv,10)$num, color = '#ffa800', name = '商品数')%>%
      hc_legend(enabled = FALSE)
  })
  
  output$TopGas <- renderHighchart({
    highchart()%>%
      #hc_title(text = '电视在售商品数')%>%
      hc_chart(type ='bar')%>%
      hc_xAxis(categories = head(top_gas,10)$brand)%>%
      hc_add_series(data = head(top_gas,10)$num, color = '#abc7b9', name = '商品数')%>%
      hc_legend(enabled = FALSE)
  })
  
  output$TopHood <- renderHighchart({
    highchart()%>%
      #hc_title(text = '电视在售商品数')%>%
      hc_chart(type ='bar')%>%
      hc_xAxis(categories = head(top_hood,10)$brand)%>%
      hc_add_series(data = head(top_hood,10)$num, color = '#ab7ac1', name = '商品数')%>%
      hc_legend(enabled = FALSE)
  })
  
  output$TopDish <- renderHighchart({
    highchart()%>%
      #hc_title(text = '电视在售商品数')%>%
      hc_chart(type ='bar')%>%
      hc_xAxis(categories = head(top_dish,10)$brand)%>%
      hc_add_series(data = head(top_dish,10)$num, color = '#ff7c4c', name = '商品数')%>%
      hc_legend(enabled = FALSE)
  })
  
  output$PriceRef <- renderHighchart({
    highchart()%>%
      #hc_title(text = '冰箱最新价格全景')%>%
      hc_add_series_boxplot(x = com_ref$p, by = com_ref$brand, name = 'Price', color = '#fd8282', outliers = input$Outlier)%>%
      hc_legend(enabled = FALSE)
  })
  
  output$PriceAir <- renderHighchart({
    highchart()%>%
      #hc_title(text = '空调最新价格全景')%>%
      hc_add_series_boxplot(x = com_air$p, by =com_air$brand, name = 'Price', color = '#1b75bb', outliers = input$Outlier)%>%
      hc_legend(enabled = FALSE)
  })
  
  output$PriceWash <- renderHighchart({
    highchart()%>%
      #hc_title(text = '洗衣机最新价格全景')%>%
      hc_add_series_boxplot(x = com_wash$p, by = com_wash$brand, name = 'Price', color = '#43e8d8', outliers = input$Outlier)%>%
      hc_legend(enabled = FALSE)
  })
  
  output$PriceTV <- renderHighchart({
    highchart()%>%
      #hc_title(text = '电视最新价格全景')%>%
      hc_add_series_boxplot(x = com_tv$p, by = com_tv$brand, name = 'Price', color = '#ffa800', outliers = input$Outlier)%>%
      hc_legend(enabled = FALSE)
  })
  
  output$PriceGas <- renderHighchart({
    highchart()%>%
      #hc_title(text = '洗衣机最新价格全景')%>%
      hc_add_series_boxplot(x = com_gas$p, by = com_gas$brand, name = 'Price', color = '#abc7b9', outliers = input$Outlier)%>%
      hc_legend(enabled = FALSE)
  })
  
  output$PriceHood <- renderHighchart({
    highchart()%>%
      #hc_title(text = '洗衣机最新价格全景')%>%
      hc_add_series_boxplot(x = com_hood$p, by = com_hood$brand, name = 'Price', color = '#ab7ac1', outliers = input$Outlier)%>%
      hc_legend(enabled = FALSE)
  })
  
  output$PriceDish <- renderHighchart({
    highchart()%>%
      #hc_title(text = '洗衣机最新价格全景')%>%
      hc_add_series_boxplot(x = com_dish$p, by = com_dish$brand, name = 'Price', color = '#ff7c4c', outliers = input$Outlier)%>%
      hc_legend(enabled = FALSE)
  })

  output$SaleRef <- renderHighchart({
    hchart(top_ref, 'treemap', hcaes(x = brand, value = sale, color = sale))%>%
      hc_colorAxis(minColor= '#FFFFFF',maxColor= '#fd8282') %>%
      hc_legend(enabled = FALSE)
  })
  
  output$SaleAir <- renderHighchart({
    hchart(top_air, 'treemap', hcaes(x = brand, value = sale, color = sale))%>%
      hc_colorAxis(minColor= '#FFFFFF',maxColor= '#1b75bb') %>%
      hc_legend(enabled = FALSE)
  })
  
  output$SaleWash <- renderHighchart({
    hchart(top_wash, 'treemap', hcaes(x = brand, value = sale, color = sale)) %>%
      hc_colorAxis(minColor= '#FFFFFF',maxColor= '#43e8d8') %>%
      hc_legend(enabled = FALSE)
  })
  
  output$SaleTV <- renderHighchart({
    hchart(top_tv, 'treemap', hcaes(x = brand, value = sale, color = sale))%>%
      hc_colorAxis(minColor= '#FFFFFF',maxColor= '#ffa800') %>%
      hc_legend(enabled = FALSE)
  })
  
  output$SaleHood <- renderHighchart({
    hchart(top_hood, 'treemap', hcaes(x = brand, value = sale, color = sale))%>%
      hc_colorAxis(minColor= '#FFFFFF',maxColor= '#ab7ac1') %>%
      hc_legend(enabled = FALSE)
  })
  
  output$SaleDish <- renderHighchart({
    hchart(top_dish, 'treemap', hcaes(x = brand, value = sale, color = sale))%>%
      hc_colorAxis(minColor= '#FFFFFF',maxColor= '#ff7c4c') %>%
      hc_legend(enabled = FALSE)
  })
  
  output$SaleGas <- renderHighchart({
    hchart(top_gas, 'treemap', hcaes(x = brand, value = sale, color = sale))%>%
      hc_colorAxis(minColor= '#FFFFFF',maxColor= '#abc7b9') %>%
      hc_legend(enabled = FALSE)
  })
  
  output$ComProfile <- renderUI({
    ui <- list(
    ref = fluidRow(
      box(id = 'step1_1',
          status = 'danger',
          title = '冰箱最新价格全景',
          width = 5,
          highchartOutput('PriceRef', height = '400px')
      ),
      box(id = 'step1_2',
          status = 'success',
          title = '冰箱最新在售商品数',
          width = 3,
          highchartOutput('TopRef', height = '400px')
      ),
      box(id = 'step1_3',
          status = 'primary',
          title = '冰箱最新市场容量',
          width = 4,
          highchartOutput('SaleRef', height = '400px')
      )
    ),
    air = fluidRow(
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
    wash = fluidRow(
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
    tv = fluidRow(
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
    ),
    gas = fluidRow(
      box(status = 'danger',
          title = '燃气灶最新价格全景',
          width = 5,
          highchartOutput('PriceGas', height = '400px')
      ),
      box(status = 'success',
          title = '燃气灶最新在售商品数',
          width = 3,
          highchartOutput('TopGas', height = '400px')
      ),
      box(status = 'primary',
          title = '燃气灶最新市场容量',
          width = 4,
          highchartOutput('SaleGas', height = '400px')
      )
    ),
    hood = fluidRow(
      box(status = 'danger',
          title = '油烟机最新价格全景',
          width = 5,
          highchartOutput('PriceHood', height = '400px')
      ),
      box(status = 'success',
          title = '油烟机最新在售商品数',
          width = 3,
          highchartOutput('TopHood', height = '400px')
      ),
      box(status = 'primary',
          title = '油烟机最新市场容量',
          width = 4,
          highchartOutput('SaleHood', height = '400px')
      )
    ),
    dish = fluidRow(
      box(status = 'danger',
          title = '洗碗机最新价格全景',
          width = 5,
          highchartOutput('PriceDish', height = '400px')
      ),
      box(status = 'success',
          title = '洗碗机最新在售商品数',
          width = 3,
          highchartOutput('TopDish', height = '400px')
      ),
      box(status = 'primary',
          title = '洗碗机最新市场容量',
          width = 4,
          highchartOutput('SaleDish', height = '400px')
      )
    )
    )
    
    tagList(ui[input$MultiCat])
    
    
  })
  
  output$RefWord <- renderWordcloud2({
    word <- table(hot_sale_cat_key$ref)%>%as.data.frame%>%subset(., !Var1%in%stop_words$ref)%>%arrange(-Freq)%>%head(50)
    wordcloud2(word, rotateRatio = 0, color= ifelse(word$Freq > 25, '#fd8282', 'grey'))
  })
  
  output$AirWord <- renderWordcloud2({
    word <- table(hot_sale_cat_key$air)%>%as.data.frame%>%subset(., !Var1%in%stop_words$air)%>%arrange(-Freq)%>%head(50)
    wordcloud2(word, rotateRatio = 0, color = ifelse(word$Freq> 25, '#1b75bb', 'grey'))
  })
  
  output$WashWord <- renderWordcloud2({
    word <- table(hot_sale_cat_key$wash)%>%as.data.frame%>%subset(., !Var1%in%stop_words$wash)%>%arrange(-Freq)%>%head(50)
    wordcloud2(word, rotateRatio = 0, color = ifelse(word$Freq > 25 , '#43e8d8', 'grey'))
  })
  
  output$TVWord <- renderWordcloud2({
    word <- table(hot_sale_cat_key$tv)%>%as.data.frame%>%subset(., !Var1%in%stop_words$tv)%>%arrange(-Freq)%>%head(50)
    wordcloud2(word, rotateRatio = 0, color = ifelse(word$Freq > 25, '#ffa800', 'grey'))
  })
  
  output$HoodWord <- renderWordcloud2({
    word <- table(hot_sale_cat_key$hood)%>%as.data.frame%>%subset(., !Var1%in%stop_words$hood)%>%arrange(-Freq)%>%head(50)
    wordcloud2(word, rotateRatio = 0, color = ifelse(word$Freq > 25, '#ab7ac1', 'grey'))
  })
  
  output$GasWord <- renderWordcloud2({
    word <- table(hot_sale_cat_key$gas)%>%as.data.frame%>%subset(., !Var1%in%stop_words$gas)%>%arrange(-Freq)%>%head(50)
    wordcloud2(word, rotateRatio = 0, color = ifelse(word$Freq > 25, '#abc7b9', 'grey'))
  })
  
  output$DishWord <- renderWordcloud2({
    word <- table(hot_sale_cat_key$dish)%>%as.data.frame%>%subset(., !Var1%in%stop_words$dish)%>%arrange(-Freq)%>%head(50)
    wordcloud2(word, rotateRatio = 0, color = ifelse(word$Freq > 15, '#ff7c4c', 'grey'))
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
  
  output$SaleBrandGas <- renderHighchart({
    table <- filter(sale_brand, category == 'gas') %>%ungroup() %>% mutate(pct = n/sum(n)*100) %>% select(brand, pct) %>% rename(name = brand, y = pct)
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
  
  output$SaleBrandHood <- renderHighchart({
    table <- filter(sale_brand, category == 'hood') %>%ungroup() %>% mutate(pct = n/sum(n)*100) %>% select(brand, pct) %>% rename(name = brand, y = pct)
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
  
  output$SaleBrandDish <- renderHighchart({
    table <- filter(sale_brand, category == 'dish') %>%ungroup() %>% mutate(pct = n/sum(n)*100) %>% select(brand, pct) %>% rename(name = brand, y = pct)
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
  
  output$SaleGasTop <- renderDataTable({
    table <- select(sale_gas, brand, id, name)
    datatable(table) %>%
      formatStyle(
        'brand',
        color = 'white',
        backgroundColor = styleEqual(
          unique(sale_gas$brand), color_brand[1:length(unique(sale_gas$brand))])
      )
  })
  
  output$SaleHoodTop <- renderDataTable({
    table <- select(sale_hood, brand, id, name)
    datatable(table) %>%
      formatStyle(
        'brand',
        color = 'white',
        backgroundColor = styleEqual(
          unique(sale_hood$brand), color_brand[1:length(unique(sale_hood$brand))])
      )
  })
  
  output$SaleDishTop <- renderDataTable({
    table <- select(sale_dish, brand, id, name)
    datatable(table) %>%
      formatStyle(
        'brand',
        color = 'white',
        backgroundColor = styleEqual(
          unique(sale_dish$brand), color_brand[1:length(unique(sale_dish$brand))])
      )
  })
  
  output$HotSaleList <- renderUI({
    ui <- list(
        ref = tabBox(id = 'step2_3',
               title = NULL,
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
        air = tabBox(title = NULL,
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
      ),
        wash = tabBox(title = NULL,
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
        tv = tabBox(title = NULL,
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
      ),
        gas = tabBox(title = NULL,
               side = 'right',
               width = 6,
               tabPanel('最新热卖燃气灶排行榜',
                        #p('')
                        dataTableOutput('SaleGasTop')
               ),
               tabPanel(
                 title = '最新热卖燃气灶品牌占比',
                 highchartOutput('SaleBrandGas')
               )
        ),
        hood = tabBox(title = NULL,
               side = 'right',
               width = 6,
               tabPanel('最新热卖油烟机排行榜',
                        #p('')
                        dataTableOutput('SaleHoodTop')
               ),
               tabPanel(
                 title = '最新热卖油烟机品牌占比',
                 highchartOutput('SaleBrandHood')
               )
      ),
        dish = tabBox(title = NULL,
               side = 'right',
               width = 6,
               tabPanel('最新热卖洗碗机排行榜',
                        #p('')
                        dataTableOutput('SaleDishTop')
               ),
               tabPanel(
                 title = '最新热卖洗碗机品牌占比',
                 highchartOutput('SaleBrandDish')
               )
        )
    )
    tagList(ui[input$MultiCat2])
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
      hc_chart(type ='bar')%>%
     #hc_yAxis(labels= list(format='{function() {return Math.abs(value)}}')) %>%
      hc_xAxis(categories = table$brand, reversed = FALSE)%>%
      hc_xAxis(categories = table$brand, opposite = TRUE, reversed = FALSE) %>%
      hc_add_series(data = table$adj_good_rate, name = '相对好评率', color = '#fd8282', label = list(step = 1))%>%
      hc_add_series(data = -table$adj_poor_rate, name = '相对差评率', color = '#727272', label = list(step = 1)) %>%
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
      hc_chart(type ='bar')%>%
      #hc_yAxis(labels= list(format='{function() {return Math.abs(value)}}')) %>%
      hc_xAxis(categories = table$brand, reversed = FALSE)%>%
      hc_xAxis(categories = table$brand, opposite = TRUE, reversed = FALSE) %>%
      hc_add_series(data = table$adj_good_rate, name = '相对好评率', color = '#1b75bb', label = list(step = 1))%>%
      hc_add_series(data = -table$adj_poor_rate, name = '相对差评率', color = '#727272', label = list(step = 1)) %>%
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
      hc_chart(type ='bar')%>%
      #hc_yAxis(labels= list(format='{function() {return Math.abs(value)}}')) %>%
      hc_xAxis(categories = table$brand, reversed = FALSE)%>%
      hc_xAxis(categories = table$brand, opposite = TRUE, reversed = FALSE) %>%
      hc_add_series(data = table$adj_good_rate, name = '相对好评率', color = '#43e8d8', label = list(step = 1))%>%
      hc_add_series(data = -table$adj_poor_rate, name = '相对差评率', color = '#727272', label = list(step = 1)) %>%
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
      hc_add_series(data = table$adj_good_rate, name = '相对好评率',  color = '#ffa800', label = list(step = 1))%>%
      hc_add_series(data = -table$adj_poor_rate, name = '相对差评率', color = '#727272', label = list(step = 1)) %>%
      hc_plotOptions(series = list(stacking = 'normal'))%>%
      hc_tooltip(pointFormat = '{series.name}: {point.y: .3f}') %>% 
      hc_legend(enabled = TRUE) #%>%
      #hc_add_theme(hc_theme_google())
    #'{series.name}: <b>{point.percentage:.1f}%</b>'
  })
  
  output$ScoreGas <- renderHighchart({
    table <- filter(score_full, category == 'gas')%>% group_by(brand) %>% summarize(comment_num = sum(comment_count), good = sum(good_count), poor = sum(poor_count), good_rate = good/comment_num, poor_rate = poor/comment_num)%>%arrange(-comment_num)%>%head(10)
    table <- mutate(table, adj_good_rate = good_rate/max(good_rate), adj_poor_rate = poor_rate/max(poor_rate))
    table <- arrange(table, adj_good_rate)
    highchart()%>%
      hc_chart(type ='bar')%>%
      #hc_yAxis(labels= list(format='{function() {return Math.abs(value)}}')) %>%
      hc_xAxis(categories = table$brand, reversed = FALSE)%>%
      hc_xAxis(categories = table$brand, opposite = TRUE, reversed = FALSE) %>%
      hc_add_series(data = table$adj_good_rate, name = '相对好评率',  color = '#abc7b9', label = list(step = 1))%>%
      hc_add_series(data = -table$adj_poor_rate, name = '相对差评率', color = '#727272', label = list(step = 1)) %>%
      hc_plotOptions(series = list(stacking = 'normal'))%>%
      hc_tooltip(pointFormat = '{series.name}: {point.y: .3f}') %>% 
      hc_legend(enabled = TRUE) #%>%
  })
  
  output$ScoreHood <- renderHighchart({
    table <- filter(score_full, category == 'hood')%>% group_by(brand) %>% summarize(comment_num = sum(comment_count), good = sum(good_count), poor = sum(poor_count), good_rate = good/comment_num, poor_rate = poor/comment_num)%>%arrange(-comment_num)%>%head(10)
    table <- mutate(table, adj_good_rate = good_rate/max(good_rate), adj_poor_rate = poor_rate/max(poor_rate))
    table <- arrange(table, adj_good_rate)
    highchart()%>%
      hc_chart(type ='bar')%>%
      #hc_yAxis(labels= list(format='{function() {return Math.abs(value)}}')) %>%
      hc_xAxis(categories = table$brand, reversed = FALSE)%>%
      hc_xAxis(categories = table$brand, opposite = TRUE, reversed = FALSE) %>%
      hc_add_series(data = table$adj_good_rate, name = '相对好评率',  color = '#ab7ac1', label = list(step = 1))%>%
      hc_add_series(data = -table$adj_poor_rate, name = '相对差评率', color = '#727272', label = list(step = 1)) %>%
      hc_plotOptions(series = list(stacking = 'normal'))%>%
      hc_tooltip(pointFormat = '{series.name}: {point.y: .3f}') %>% 
      hc_legend(enabled = TRUE) #%>%
  })
  
  output$ScoreDish <- renderHighchart({
    table <- filter(score_full, category == 'dish')%>% group_by(brand) %>% summarize(comment_num = sum(comment_count), good = sum(good_count), poor = sum(poor_count), good_rate = good/comment_num, poor_rate = poor/comment_num)%>%arrange(-comment_num)%>%head(10)
    table <- mutate(table, adj_good_rate = good_rate/max(good_rate), adj_poor_rate = poor_rate/max(poor_rate))
    table <- arrange(table, adj_good_rate)
    highchart()%>%
      hc_chart(type ='bar')%>%
      #hc_yAxis(labels= list(format='{function() {return Math.abs(value)}}')) %>%
      hc_xAxis(categories = table$brand, reversed = FALSE)%>%
      hc_xAxis(categories = table$brand, opposite = TRUE, reversed = FALSE) %>%
      hc_add_series(data = table$adj_good_rate, name = '相对好评率',  color = '#ff7c4c', label = list(step = 1))%>%
      hc_add_series(data = -table$adj_poor_rate, name = '相对差评率', color = '#727272', label = list(step = 1)) %>%
      hc_plotOptions(series = list(stacking = 'normal'))%>%
      hc_tooltip(pointFormat = '{series.name}: {point.y: .3f}') %>% 
      hc_legend(enabled = TRUE) #%>%
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
  
  output$ScoreGasBrand <- renderDataTable({
    table <- filter(score_full, (category == 'gas') & (brand == input$GasBrandSelector)) %>% select(id, name, comment_count, good_rate, poor_rate) %>% arrange(-comment_count)
    datatable(table, rownames = FALSE)
  })
  
  output$ScoreHoodBrand <- renderDataTable({
    table <- filter(score_full, (category == 'hood') & (brand == input$HoodBrandSelector)) %>% select(id, name, comment_count, good_rate, poor_rate) %>% arrange(-comment_count)
    datatable(table, rownames = FALSE)
  })
  
  output$ScoreDishBrand <- renderDataTable({
    table <- filter(score_full, (category == 'dish') & (brand == input$DishBrandSelector)) %>% select(id, name, comment_count, good_rate, poor_rate) %>% arrange(-comment_count)
    datatable(table, rownames = FALSE)
  })
  
  output$ScoreRefTop <- renderDataTable({
    table <- filter(score_full, category == 'ref') %>% select(id, name, brand, score) %>% top_n(20, score)
    datatable(table, rownames = FALSE)%>%
      formatStyle(
        'score',
        background = styleColorBar(table$score, '#fd8282'),
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
        background = styleColorBar(table$score, '#1b75bb'),
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
        background = styleColorBar(table$score, '#43e8d8'),
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
        background = styleColorBar(table$score, '#ffa800'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  output$ScoreGasTop <- renderDataTable({
    table <- filter(score_full, category == 'gas') %>% select(id, name, brand, score) %>% top_n(20, score)
    datatable(table, rownames = FALSE) %>%
      formatStyle(
        'score',
        background = styleColorBar(table$score, '#abc7b9'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  output$ScoreHoodTop <- renderDataTable({
    table <- filter(score_full, category == 'hood') %>% select(id, name, brand, score) %>% top_n(20, score)
    datatable(table, rownames = FALSE) %>%
      formatStyle(
        'score',
        background = styleColorBar(table$score, '#ab7ac1'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  output$ScoreDishTop <- renderDataTable({
    table <- filter(score_full, category == 'dish') %>% select(id, name, brand, score) %>% top_n(20, score)
    datatable(table, rownames = FALSE) %>%
      formatStyle(
        'score',
        background = styleColorBar(table$score, '#ff7c4c'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  output$UserPref <- renderUI({
    ui <- list(
      ref = fluidRow(
        box(id = 'step4_1',
            status = 'info',
            title = '冰箱最新客户品牌偏好度',
            width = 4,
            highchartOutput('ScoreRef', height = '600px')
        ),
        tabBox(
          id = 'step4_2',
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
      air = fluidRow(
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
      wash = fluidRow(
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
      tv = fluidRow(
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
        
      ),
      gas = fluidRow(
        box(status = 'info',
            title = '燃气灶最新客户品牌偏好度',
            width = 4,
            highchartOutput('ScoreGas', height = '600px')
        ),
        tabBox(
          title = NULL,
          width = 8,
          side = 'right',
          tabPanel(
            title = '燃气灶各品牌商品得分',
            fluidRow(
              column(width = 2,
                     selectInput('GasBrandSelector', label = '品牌', choices = unique(filter(score_full, category == 'gas')$brand))
              )),
            dataTableOutput('ScoreGasBrand')
          ),
          tabPanel(
            title = '燃气灶商品用户偏好排行榜',
            dataTableOutput('ScoreGasTop')
          )
        )
        
      ),
      hood = fluidRow(
        box(status = 'info',
            title = '油烟机最新客户品牌偏好度',
            width = 4,
            highchartOutput('ScoreHood', height = '600px')
        ),
        tabBox(
          title = NULL,
          width = 8,
          side = 'right',
          tabPanel(
            title = '油烟机各品牌商品得分',
            fluidRow(
              column(width = 2,
                     selectInput('HoodBrandSelector', label = '品牌', choices = unique(filter(score_full, category == 'hood')$brand))
              )),
            dataTableOutput('ScoreHoodBrand')
          ),
          tabPanel(
            title = '油烟机商品用户偏好排行榜',
            dataTableOutput('ScoreHoodTop')
          )
        )
        
      ),
      dish = fluidRow(
        box(status = 'info',
            title = '洗碗机最新客户品牌偏好度',
            width = 4,
            highchartOutput('ScoreDish', height = '600px')
        ),
        tabBox(
          title = NULL,
          width = 8,
          side = 'right',
          tabPanel(
            title = '洗碗机各品牌商品得分',
            fluidRow(
              column(width = 2,
                     selectInput('DishBrandSelector', label = '品牌', choices = unique(filter(score_full, category == 'dish')$brand))
              )),
            dataTableOutput('ScoreDishBrand')
          ),
          tabPanel(
            title = '洗碗机商品用户偏好排行榜',
            dataTableOutput('ScoreDishTop')
          )
        )
      )
    )
    tagList(ui[input$MultiCat3])
  })
  output$CommentBrand <- renderUI({
    withProgress(message = '正在加载...', value = 0,
                 {
                   if(input$CommentGoP == 'good'){
                     match_df <- switch(input$CommentCat,
                                        ref = match_ref_df_good,
                                        air = match_air_df_good,
                                        wash = match_wash_df_good,
                                        tv = match_tv_df_good,
                                        gas = match_gas_df_good,
                                        hood = match_hood_df_good,
                                        dish = match_dish_df_good)
                     n_df <- switch(input$CommentCat,
                                    ref = commentC_ref_good_n,
                                    air = commentC_air_good_n,
                                    wash = commentC_wash_good_n,
                                    tv = commentC_tv_good_n,
                                    gas = commentC_gas_good_n,
                                    hood = commentC_hood_good_n,
                                    dish = commentC_dish_good_n)
                   }else{
                     match_df <- switch(input$CommentCat,
                                        ref = match_ref_df_poor,
                                        air = match_air_df_poor,
                                        wash = match_wash_df_poor,
                                        tv = match_tv_df_poor,
                                        gas = match_gas_df_poor,
                                        hood = match_hood_df_poor,
                                        dish = match_dish_df_poor)
                     n_df <- switch(input$CommentCat,
                                    ref = commentC_ref_poor_n,
                                    air = commentC_air_poor_n,
                                    wash = commentC_wash_poor_n,
                                    tv = commentC_tv_poor_n,
                                    gas = commentC_gas_poor_n,
                                    hood = commentC_hood_poor_n,
                                    dish = commentC_dish_poor_n)
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
