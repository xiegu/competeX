library(jiebaR)
library(dplyr)
library(magrittr)
library(data.table)
library(RMySQL)
library(pool)

# Connect to database 'jd'
#con <- dbConnect(MySQL(), dbname = db$dbname, username = db$username, password = db$password)

pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = db$dbname,
  username = db$username,
  password = db$password
)

dbGetQuery(pool,'SET NAMES utf8') 

competitor_info <- function(){
  res <- dbGetQuery(pool, 'select id, category, name, link, p, update_time from JD_cat_item_info;')
}

score_info <- function(){
  res <- dbGetQuery(pool, 'select sku_id, comment_count from JD_item_score_summary;')
}

score_full_info <- function(){
  res <- dbGetQuery(pool, 'select * from JD_item_score_summary;')
}

detail_info <- function(){
  res <- dbGetQuery(pool, 'select * from JD_item_detail;')  
  }

hot_sale_info <- function(){
  res <- dbGetQuery(pool, 'select * from JD_hot_sale_info where category <> "mobile";')
}

hot_search_info <- function(){
  res <- dbGetQuery(pool, 'select * from JD_hot_search_info;')
}

comment_info <- function(){
  res <- dbGetQuery(pool, 'select * from JD_item_comment;')
}

com <- competitor_info()
score <- score_info()
score_full <- score_full_info()
detail <- detail_info()
sale <- hot_sale_info()
search <- hot_search_info()
comment <- comment_info()

detail <- mutate(detail, brand = toupper(brand))
com <- inner_join(com, score, by = c('id'= 'sku_id'))%>%unique
com <- left_join(com, unique(select(detail, id, brand)), by = c('id' = 'id')) %>% unique

sale <- left_join(sale, unique(select(detail, id, brand)), by = c('id' = 'id')) %>% unique
# Text segmentation
wk <- worker()

# brandC <- character()
# for (i in com$brand){
#    brandC <- append(brandC, wk[i][1])
#  }

hot_sale_name_brand <- character()
for (i in sale$name){
  hot_sale_name_brand <- append(hot_sale_name_brand, wk[i][1])
}

hot_sale_name_seg <- function(by = c('category', 'brand')){
  if(by =='category'){
    category <- unique(sale$category)
    name_seg <- list()
    for (i in category){
      sale_cat <- subset(sale, category == i)
      ch <- character()
      for (j in sale_cat$name){
        ch <- append(ch, wk[j][-(1:2)])
      }
      name_seg[[i]] <- ch
    }
    return(name_seg)
  }else if(by == 'brand'){
    category <- unique(sale$category)
    name_seg <- list()
    for (c in category){
      name_seg2 <- list()
      brand <- unique(subset(sale, category == c)$brand)
      for (i in brand){
        sale_brand <- subset(sale, (category == c) & (brand == i))
        ch <- character()
        for (j in sale_brand$name){
          ch <- append(ch, wk[j][-(1:2)])
        }
        name_seg2[[i]] <- ch 
      }
      name_seg[[c]] <- name_seg2
    }
    return(name_seg)
  }else{
    stop('Check "by" argument:')
  }
}

# Data manupulation
#com <- mutate(com, brand = brandC)
sale <- mutate(sale, brand = hot_sale_name_brand)

sale_ref <- filter(sale, category == 'ref') %>% select(id, name, category, update_time, brand)
sale_air <- filter(sale, category == 'air') %>% select(id, name, category, update_time, brand)
sale_wash <- filter(sale, category == 'wash') %>% select(id, name, category, update_time, brand)
sale_tv <- filter(sale, category == 'tv') %>% select(id, name, category, update_time, brand)
sale_dish <- filter(sale, category == 'dish') %>% select(id, name, category, update_time, brand)
sale_gas <- filter(sale, category == 'gas') %>% select(id, name, category, update_time, brand)
sale_hood <- filter(sale, category == 'hood') %>% select(id, name, category, update_time, brand)




top_ref <- group_by(subset(com, category == 'ref'), brand)%>%summarize(., num = length(p), sale = sum(comment_count, na.rm = TRUE))%>%arrange(., -num)
top_air <- group_by(subset(com, category == 'air'), brand)%>%summarize(., num = length(p), sale = sum(comment_count, na.rm = TRUE))%>%arrange(., -num)
top_wash <- group_by(subset(com, category == 'wash'), brand)%>%summarize(., num = length(p), sale = sum(comment_count, na.rm = TRUE))%>%arrange(., -num)
top_tv <- group_by(subset(com, category == 'tv'), brand)%>%summarize(., num = length(p), sale = sum(comment_count, na.rm = TRUE))%>%arrange(., -num)
top_dish <- group_by(subset(com, category == 'dish'), brand)%>%summarize(., num = length(p), sale = sum(comment_count, na.rm = TRUE))%>%arrange(., -num)
top_hood <- group_by(subset(com, category == 'hood'), brand)%>%summarize(., num = length(p), sale = sum(comment_count, na.rm = TRUE))%>%arrange(., -num)
top_gas <- group_by(subset(com, category == 'gas'), brand)%>%summarize(., num = length(p), sale = sum(comment_count, na.rm = TRUE))%>%arrange(., -num)

other_ref <- data.frame(brand = '其他', num = sum(top_ref$num, na.rm = TRUE) - sum(head(top_ref,10)$num, na.rm = TRUE), sale = sum(top_ref$sale, na.rm = TRUE) - sum(head(top_ref,10)$sale, na.rm = TRUE))
other_air <- data.frame(brand = '其他', num = sum(top_air$num, na.rm = TRUE) - sum(head(top_air,10)$num, na.rm = TRUE), sale = sum(top_air$sale, na.rm = TRUE) - sum(head(top_air,10)$sale, na.rm = TRUE))
other_wash <- data.frame(brand = '其他', num = sum(top_wash$num, na.rm = TRUE) - sum(head(top_wash,10)$num, na.rm = TRUE), sale = sum(top_wash$sale, na.rm = TRUE) - sum(head(top_wash,10)$sale, na.rm = TRUE))
other_tv <- data.frame(brand = '其他', num = sum(top_tv$num, na.rm = TRUE) - sum(head(top_tv,10)$num, na.rm = TRUE), sale = sum(top_tv$sale, na.rm = TRUE) - sum(head(top_tv,10)$sale, na.rm = TRUE))
other_dish <- data.frame(brand = '其他', num = sum(top_dish$num, na.rm = TRUE) - sum(head(top_dish,10)$num, na.rm = TRUE), sale = sum(top_dish$sale, na.rm = TRUE) - sum(head(top_dish,10)$sale, na.rm = TRUE))
other_gas <- data.frame(brand = '其他', num = sum(top_gas$num, na.rm = TRUE) - sum(head(top_gas,10)$num, na.rm = TRUE), sale = sum(top_gas$sale, na.rm = TRUE) - sum(head(top_gas,10)$sale, na.rm = TRUE))
other_hood <- data.frame(brand = '其他', num = sum(top_hood$num, na.rm = TRUE) - sum(head(top_hood,10)$num, na.rm = TRUE), sale = sum(top_hood$sale, na.rm = TRUE) - sum(head(top_hood,10)$sale, na.rm = TRUE))


top_10_ref <- rbind(head(top_ref,10), other_ref)
top_10_air <- rbind(head(top_air,10), other_air)
top_10_wash <- rbind(head(top_wash,10), other_wash)
top_10_tv <- rbind(head(top_tv,10), other_tv)
top_10_dish <- rbind(head(top_dish,10), other_dish)
top_10_gas <- rbind(head(top_gas,10), other_gas)
top_10_hood <- rbind(head(top_hood,10), other_hood)


com_ref <- subset(com, category == 'ref')%>%select(category, brand, p)%>%subset(brand%in%head(top_ref,10)$brand)
com_air <- subset(com, category == 'air')%>%select(category, brand, p)%>%subset(brand%in%head(top_air,10)$brand)
com_wash <- subset(com, category == 'wash')%>%select(category, brand, p)%>%subset(brand%in%head(top_wash,10)$brand)
com_tv <- subset(com, category == 'tv')%>%select(category, brand, p)%>%subset(brand%in%head(top_tv,10)$brand)
com_dish <- subset(com, category == 'dish')%>%select(category, brand, p)%>%subset(brand%in%head(top_dish,10)$brand)
com_gas <- subset(com, category == 'gas')%>%select(category, brand, p)%>%subset(brand%in%head(top_gas,10)$brand)
com_hood <- subset(com, category == 'hood')%>%select(category, brand, p)%>%subset(brand%in%head(top_hood,10)$brand)

hot_sale_cat_key <- hot_sale_name_seg('category')
hot_sale_brand_key <- hot_sale_name_seg('brand')

sale_brand <- count(sale, category, brand)

search_table <- select(search, category, word, count)%>%
  mutate(color_1 = ifelse(search$category == 'mobile', 'black', 'grey'))%>%
    mutate(color_2 = ifelse(grepl('冰箱', word), '#d62d20', ifelse(grepl('空调', word), '#0057e7', ifelse(grepl('洗衣机', word), '#008744', ifelse(grepl('电视', word), '#ffa700', ifelse(grepl('油烟机', word), '#A6CEE3', ifelse(grepl('洗碗机', word), '#1F78B4', ifelse(grepl('燃气灶', word), '#B2DF8A', 'skyblue'))))))),
           group =  ifelse(grepl('冰箱', word), '冰箱', ifelse(grepl('空调', word), '空调', ifelse(grepl('洗衣机', word), '洗衣机', ifelse(grepl('电视', word), '电视', ifelse(grepl('油烟机', word), '油烟机', ifelse(grepl('洗碗机', word), '洗碗机', ifelse(grepl('燃气灶', word), '燃气灶',  '其他'))))))))

comment_summary <- function(){
  res <- dbSendQuery(con, 'select count(1) as comments, count(distinct id) as ids from JD_item_comment;')
  data <- dbFetch(res)
  return(data)
}

comment_detail <- function(){
  res <- dbSendQuery(con, 'select third_category, count(distinct id) as ids, count(1) as comments from JD_item_comment group by third_category;')
  data <- dbFetch(res)
  return(data)
}

score_full <- select(com, id, name, brand)%>%inner_join(score_full, by = c('id'='sku_id'))

# caclcuate 'best' item by weighted-average score

score_full <- mutate(score_full, score = round((good_count - poor_count)/(score2_count + score3_count + mean(score2_count + score3_count)),2)) %>% arrange(-score)

commentC <- filter(comment, third_category %in%c(878, 870,880, 798, 1300, 13298, 13117)) %>% left_join(select(detail, id, brand), by = c('id' = 'id')) %>% unique
rm(comment)
gc()
commentC <- mutate(commentC, category = ifelse(third_category == 878, 'ref', ifelse(third_category == 870, 'air', ifelse(third_category == 880, 'wash', ifelse(third_category == 1300, 'hood', ifelse(third_category == 13298, 'gas', ifelse(third_category == 798, 'tv', 'dish')))))))
# comment_ref_seg <- list()
# for(i in 1:length(comment_ref$content)){
#   if(comment_ref$content[i] %in% c('.', '~', '/////')){
#     next
#   }else{comment_ref_seg[[i]] <- wk[comment_ref$content[i]]
#   }
# }

good_score_index <- which(commentC$score == 5)
poor_score_index <- which(commentC$score %in% c(1,2))

#commentC_brand_good <- commentC$brand[good_score_index]
#commentC_brand_poor <- commentC$brand[poor_score_index]

tag_ref_name <- attributes(tag_words$ref$pos$product)$names
tag_air_name <- attributes(tag_words$air$pos$product)$names
tag_wash_name <- attributes(tag_words$wash$pos$product)$names
tag_tv_name <- attributes(tag_words$tv$pos$product)$names
tag_dish_name <- attributes(tag_words$dish$pos$product)$names
tag_gas_name <- attributes(tag_words$gas$pos$product)$names
tag_hood_name <- attributes(tag_words$hood$pos$product)$names

commentC_ref_good <- commentC[good_score_index, ] %>% filter(category == 'ref')
commentC_ref_poor <- commentC[poor_score_index, ] %>% filter(category == 'ref')
commentC_air_good <- commentC[good_score_index, ] %>% filter(category == 'air')
commentC_air_poor <- commentC[poor_score_index, ] %>% filter(category == 'air')
commentC_wash_good <- commentC[good_score_index, ] %>% filter(category == 'wash')
commentC_wash_poor <- commentC[poor_score_index, ] %>% filter(category == 'wash')
commentC_tv_good <- commentC[good_score_index, ] %>% filter(category == 'tv')
commentC_tv_poor <- commentC[poor_score_index, ] %>% filter(category == 'tv')
commentC_gas_good <- commentC[good_score_index, ] %>% filter(category == 'gas')
commentC_gas_poor <- commentC[poor_score_index, ] %>% filter(category == 'gas')
commentC_hood_good <- commentC[good_score_index, ] %>% filter(category == 'hood')
commentC_hood_poor <- commentC[poor_score_index, ] %>% filter(category == 'hood')
commentC_dish_good <- commentC[good_score_index, ] %>% filter(category == 'dish')
commentC_dish_poor <- commentC[poor_score_index, ] %>% filter(category == 'dish')

match_ref_good_list <- list()
for(i in tag_ref_name){
  match <- numeric()
 #for(j in good_score_index){ #length(comment_ref_seg_good)
    #match <- append(match, intersect(tag_words$ref$pos$product[i], comment_ref_seg_good[[j]]) %>% length)
  pattern <- tag_words$ref$pos$product[i]%>%unlist %>% as.character %>% paste(collapse = '|')
  match <- grepl(pattern, commentC_ref_good$content)
  #match <- sapply(good_score_index, function(y) sapply(tag_words$ref$pos$product[i]%>%unlist, function(x) grepl(x, comment_ref$content[y]))%>%any%>%as.numeric%>%append(match))
  #match <-
    #match <- sapply(tag_words$ref$pos$product[i]%>%unlist, function(x) grepl(x, comment_ref$content[j]))%>%any%>%as.numeric%>%append(match)
  #}
  match_ref_good_list[[i]] <- match
}

match_ref_poor_list <- list()
for(i in tag_ref_name){
  match <- numeric()
  pattern <- tag_words$ref$neg$product[i]%>%unlist %>% as.character %>% paste(collapse = '|')
  match <- grepl(pattern, commentC_ref_poor$content)
  match_ref_poor_list[[i]] <- match
}

match_air_good_list <- list()
for(i in tag_air_name){
  match <- numeric()
  pattern <- tag_words$air$pos$product[i]%>%unlist %>% as.character %>% paste(collapse = '|')
  match <- grepl(pattern, commentC_air_good$content)
  match_air_good_list[[i]] <- match
}

match_air_poor_list <- list()
for(i in tag_air_name){
  match <- numeric()
  pattern <- tag_words$air$neg$product[i]%>%unlist %>% as.character %>% paste(collapse = '|')
  match <- grepl(pattern, commentC_air_poor$content)
  match_air_poor_list[[i]] <- match
}

match_wash_good_list <- list()
for(i in tag_wash_name){
  match <- numeric()
  pattern <- tag_words$wash$pos$product[i]%>%unlist %>% as.character %>% paste(collapse = '|')
  match <- grepl(pattern, commentC_wash_good$content)
  match_wash_good_list[[i]] <- match
}

match_wash_poor_list <- list()
for(i in tag_wash_name){
  match <- numeric()
  pattern <- tag_words$wash$neg$product[i]%>%unlist %>% as.character %>% paste(collapse = '|')
  match <- grepl(pattern, commentC_wash_poor$content)
  match_wash_poor_list[[i]] <- match
}

match_tv_good_list <- list()
for(i in tag_tv_name){
  match <- numeric()
  pattern <- tag_words$tv$pos$product[i]%>%unlist %>% as.character %>% paste(collapse = '|')
  match <- grepl(pattern, commentC_tv_good$content)
  match_tv_good_list[[i]] <- match
}

match_tv_poor_list <- list()
for(i in tag_tv_name){
  match <- numeric()
  pattern <- tag_words$tv$neg$product[i]%>%unlist %>% as.character %>% paste(collapse = '|')
  match <- grepl(pattern, commentC_tv_poor$content)
  match_tv_poor_list[[i]] <- match
}

match_gas_good_list <- list()
for(i in tag_gas_name){
  match <- numeric()
  pattern <- tag_words$gas$pos$product[i]%>%unlist %>% as.character %>% paste(collapse = '|')
  match <- grepl(pattern, commentC_gas_good$content)
  match_gas_good_list[[i]] <- match
}

match_gas_poor_list <- list()
for(i in tag_gas_name){
  match <- numeric()
  pattern <- tag_words$gas$neg$product[i]%>%unlist %>% as.character %>% paste(collapse = '|')
  match <- grepl(pattern, commentC_gas_poor$content)
  match_gas_poor_list[[i]] <- match
}

match_dish_good_list <- list()
for(i in tag_dish_name){
  match <- numeric()
  pattern <- tag_words$dish$pos$product[i]%>%unlist %>% as.character %>% paste(collapse = '|')
  match <- grepl(pattern, commentC_dish_good$content)
  match_dish_good_list[[i]] <- match
}

match_dish_poor_list <- list()
for(i in tag_dish_name){
  match <- numeric()
  pattern <- tag_words$dish$neg$product[i]%>%unlist %>% as.character %>% paste(collapse = '|')
  match <- grepl(pattern, commentC_dish_poor$content)
  match_dish_poor_list[[i]] <- match
}

match_hood_good_list <- list()
for(i in tag_hood_name){
  match <- numeric()
  pattern <- tag_words$hood$pos$product[i]%>%unlist %>% as.character %>% paste(collapse = '|')
  match <- grepl(pattern, commentC_hood_good$content)
  match_hood_good_list[[i]] <- match
}

match_hood_poor_list <- list()
for(i in tag_hood_name){
  match <- numeric()
  pattern <- tag_words$hood$neg$product[i]%>%unlist %>% as.character %>% paste(collapse = '|')
  match <- grepl(pattern, commentC_hood_poor$content)
  match_hood_poor_list[[i]] <- match
}

match_ref_df_good <- do.call(cbind, match_ref_good_list) %>% as.data.frame %>%mutate(brand = commentC_ref_good$brand) %>% group_by(brand) %>%
  summarize(recognition = sum(recognition), style = sum(style), capacity = sum(capacity), noise = sum(noise), quality = sum(quality), energysaving = sum(energysaving), manupulation = sum(manupulation),
            frostless = sum(frostless), radiating = sum(radiating), cooling = sum(cooling), freshness = sum(freshness), price = sum(price)) %>% arrange(-recognition) %>% filter(!is.na(brand))

match_ref_df_poor <- do.call(cbind, match_ref_poor_list) %>% as.data.frame %>%mutate(brand = commentC_ref_poor$brand) %>% group_by(brand) %>%
  summarize(recognition = sum(recognition), style = sum(style), capacity = sum(capacity), noise = sum(noise), quality = sum(quality), energysaving = sum(energysaving), manupulation = sum(manupulation),
            frostless = sum(frostless), radiating = sum(radiating), cooling = sum(cooling), freshness = sum(freshness), price = sum(price)) %>% arrange(-recognition) %>% filter(!is.na(brand))

match_air_df_good <- do.call(cbind, match_air_good_list) %>% as.data.frame %>%mutate(brand = commentC_air_good$brand) %>% group_by(brand) %>%
  summarize(recognition = sum(recognition), style = sum(style), windpower = sum(windpower), noise = sum(noise), quality = sum(quality), energysaving = sum(energysaving), manupulation = sum(manupulation),
            heating = sum(heating), cooling = sum(cooling), price = sum(price)) %>% arrange(-recognition) %>% filter(!is.na(brand))

match_air_df_poor <- do.call(cbind, match_air_poor_list) %>% as.data.frame %>%mutate(brand = commentC_air_poor$brand) %>% group_by(brand) %>%
  summarize(recognition = sum(recognition), style = sum(style), windpower = sum(windpower), noise = sum(noise), quality = sum(quality), energysaving = sum(energysaving), manupulation = sum(manupulation),
            heating = sum(heating), cooling = sum(cooling), price = sum(price)) %>% arrange(-recognition) %>% filter(!is.na(brand))

match_wash_df_good <- do.call(cbind, match_wash_good_list) %>% as.data.frame %>%mutate(brand = commentC_wash_good$brand) %>% group_by(brand) %>%
  summarize(recognition = sum(recognition), style = sum(style), capacity = sum(capacity), noise = sum(noise), quality = sum(quality), energysaving = sum(energysaving), manupulation = sum(manupulation),
            washing = sum(washing), price = sum(price)) %>% arrange(-recognition) %>% filter(!is.na(brand))

match_wash_df_poor <- do.call(cbind, match_wash_poor_list) %>% as.data.frame %>%mutate(brand = commentC_wash_poor$brand) %>% group_by(brand) %>%
  summarize(recognition = sum(recognition), style = sum(style), capacity = sum(capacity), noise = sum(noise), quality = sum(quality), energysaving = sum(energysaving), manupulation = sum(manupulation),
            washing = sum(washing), price = sum(price)) %>% arrange(-recognition) %>% filter(!is.na(brand))

match_tv_df_good <- do.call(cbind, match_tv_good_list) %>% as.data.frame %>%mutate(brand = commentC_tv_good$brand) %>% group_by(brand) %>%
  summarize(recognition = sum(recognition), style = sum(style), screen = sum(screen), noise = sum(noise), quality = sum(quality), energysaving = sum(energysaving), manupulation = sum(manupulation),
            price = sum(price)) %>% arrange(-recognition) %>% filter(!is.na(brand))

match_tv_df_poor <- do.call(cbind, match_tv_poor_list) %>% as.data.frame %>%mutate(brand = commentC_tv_poor$brand) %>% group_by(brand) %>%
  summarize(recognition = sum(recognition), style = sum(style), screen = sum(screen), noise = sum(noise), quality = sum(quality), energysaving = sum(energysaving), manupulation = sum(manupulation),
            price = sum(price)) %>% arrange(-recognition) %>% filter(!is.na(brand))

match_hood_df_good <- do.call(cbind, match_hood_good_list) %>% as.data.frame %>%mutate(brand = commentC_hood_good$brand) %>% group_by(brand) %>%
  summarize(recognition = sum(recognition), style = sum(style), windpower = sum(windpower), noise = sum(noise), quality = sum(quality), energysaving = sum(energysaving), manupulation = sum(manupulation),
            fumecontrol = sum(fumecontrol), price = sum(price)) %>% arrange(-recognition) %>% filter(!is.na(brand))

match_hood_df_poor <- do.call(cbind, match_hood_poor_list) %>% as.data.frame %>%mutate(brand = commentC_hood_poor$brand) %>% group_by(brand) %>%
  summarize(recognition = sum(recognition), style = sum(style), windpower = sum(windpower), noise = sum(noise), quality = sum(quality), energysaving = sum(energysaving), manupulation = sum(manupulation),
            fumecontrol = sum(fumecontrol), price = sum(price)) %>% arrange(-recognition) %>% filter(!is.na(brand))

match_dish_df_poor <- do.call(cbind, match_dish_poor_list) %>% as.data.frame %>%mutate(brand = commentC_dish_poor$brand) %>% group_by(brand) %>%
  summarize(recognition = sum(recognition), style = sum(style), noise = sum(noise), quality = sum(quality), energysaving = sum(energysaving), manupulation = sum(manupulation),
            washing = sum(washing), price = sum(price)) %>% arrange(-recognition) %>% filter(!is.na(brand))

match_dish_df_good <- do.call(cbind, match_dish_good_list) %>% as.data.frame %>%mutate(brand = commentC_dish_good$brand) %>% group_by(brand) %>%
  summarize(recognition = sum(recognition), style = sum(style), noise = sum(noise), quality = sum(quality), energysaving = sum(energysaving), manupulation = sum(manupulation),
            washing = sum(washing), price = sum(price)) %>% arrange(-recognition) %>% filter(!is.na(brand))

match_gas_df_poor <- do.call(cbind, match_gas_poor_list) %>% as.data.frame %>%mutate(brand = commentC_gas_poor$brand) %>% group_by(brand) %>%
  summarize(recognition = sum(recognition), style = sum(style), noise = sum(noise), quality = sum(quality), energysaving = sum(energysaving), manupulation = sum(manupulation),
            safety = sum(safety), price = sum(price)) %>% arrange(-recognition) %>% filter(!is.na(brand))

match_gas_df_good <- do.call(cbind, match_gas_good_list) %>% as.data.frame %>%mutate(brand = commentC_gas_good$brand) %>% group_by(brand) %>%
  summarize(recognition = sum(recognition), style = sum(style), noise = sum(noise), quality = sum(quality), energysaving = sum(energysaving), manupulation = sum(manupulation),
            safety = sum(safety), price = sum(price)) %>% arrange(-recognition) %>% filter(!is.na(brand))

commentC_ref_good_n <- count(commentC_ref_good, brand) %>% filter(!is.na(brand))
commentC_ref_poor_n <- count(commentC_ref_poor, brand) %>% filter(!is.na(brand))
commentC_air_good_n <- count(commentC_air_good, brand) %>% filter(!is.na(brand))
commentC_air_poor_n <- count(commentC_air_poor, brand) %>% filter(!is.na(brand))
commentC_wash_good_n <- count(commentC_wash_good, brand) %>% filter(!is.na(brand))
commentC_wash_poor_n <- count(commentC_wash_poor, brand) %>% filter(!is.na(brand))
commentC_tv_good_n <- count(commentC_tv_good, brand) %>% filter(!is.na(brand))
commentC_tv_poor_n <- count(commentC_tv_poor, brand) %>% filter(!is.na(brand))
commentC_gas_good_n <- count(commentC_gas_good, brand) %>% filter(!is.na(brand))
commentC_gas_poor_n <- count(commentC_gas_poor, brand) %>% filter(!is.na(brand))
commentC_hood_good_n <- count(commentC_hood_good, brand) %>% filter(!is.na(brand))
commentC_hood_poor_n <- count(commentC_hood_poor, brand) %>% filter(!is.na(brand))
commentC_dish_good_n <- count(commentC_dish_good, brand) %>% filter(!is.na(brand))
commentC_dish_poor_n <- count(commentC_dish_poor, brand) %>% filter(!is.na(brand))


rm(commentC)
rm(match)
rm(list = c('commentC_ref_good', 'commentC_ref_poor', 'commentC_air_good', 'commentC_air_poor', 'commentC_wash_good', 'commentC_wash_poor', 'commentC_tv_good', 'commentC_tv_poor', 
            'commentC_hood_good', 'commentC_hood_poor', 'commentC_dish_good', 'commentC_dish_poor', 'commentC_gas_good', 'commentC_gas_poor'))
gc()
save.image(file = 'data.RData')

