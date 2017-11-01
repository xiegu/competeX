library(RColorBrewer)
library(magrittr)
library(jsonlite)

# Intro
steps_1 <- data.frame(step = c(1,2,3), element = c('#step1_1', '#step1_2', '#step1_3'), 
                      intro = c('显示该类别下各个品牌的在售商品的价格分布。箱形图代表该品牌按照价格从低到高排序后，排在前5%到95%的商品的价格区间。箱形图上下两侧的圆点代表价格极高/极低的商品。',
                                '显示该类别下在售商品数最多的10个品牌。',
                                '显示该类别下各个品牌占据的市场容量，主要通过用户评论和在售商品数组合计算得出。'),	
                      position = c('auto', 'auto', 'auto'))

steps_2 <- data.frame(step = c(1,2,3), element = c('#step2_1', '#step2_2', '#step2_3'),
                      intro= c('每个类别下各个品牌出现在热卖榜中的商品的信息。',
                               '显示各个类别的热卖商品中出现次数最多的关键字。',
                               '各个类别下热卖商品的排行榜，特别区分出品牌。'),
                      position = c('auto', 'auto', 'auto'))

steps_3 <- data.frame(step = c(1,2), element = c('#step3_1', '#step3_2'),
                      intro = c('热门搜索词的分布', '热门搜索词中各个类别的占比'),
                      position = c('auto', 'auto'))

steps_4 <- data.frame(step = c(1,2), element = c('#step4_1', '#step4_2'),
                      intro = c('显示各个类别下各个品牌的用户偏好度。好评率最高的品牌设为1，分别计算其他品牌的相对好评率。相对差评率按照相同的方法计算。',
                                '显示各个类别下各个品牌所有商品的偏好得分。商品偏好得分由好评数，差评数，总评论数等加权计算得出。将计算所得分数分别纵向横向排序。'),
                      position = c('auto', 'auto'))

steps_5 <- data.frame(step = c(1), element = c('#step5_1'),
                     intro = c('显示各个类别下各个品牌的用户感知度。用户的评论经过模型分词提取，再计算各个关键词出现在评论中的频率和比例，分别纵向横向展示品牌之间的差异。'),
                     position = c('auto'))
# DB info
db <- list(
            dbname = 'jd',
            username = 'root',
            password = '292773'
)

# Stop words
stop_words <- list(
  'ref' = c('冰箱', '升', 'BCD', '京东', '电冰箱'),
  'air' = c('柜机',  '空调', 'KFR', 'GW', '匹' , '挂机', '壁挂式', '京东'),
  'wash' = c('洗', '洗衣机', '公斤', '京东'),
  'tv' = c('电视机', '电视', '英寸', '京东'),
  'gas' =c('燃气灶', 'JZY', 'JZT', '煤气灶',  '京东'),
  'hood' = c('油烟机', '吸油烟机', '抽油烟机',  '安装', 'CXW', '京东'),
  'dish' = c('洗碗机' , '套', '京东')
)

# color pallete
color_brand <- c(c('#d62d20', '#ffa700', '#008744', '#0057e7'), brewer.pal(12, 'Paired'), brewer.pal(8, 'Dark2'), brewer.pal(8, 'Accent'), brewer.pal(12, 'Set3'), brewer.pal(11, 'Spectral')) %>% unique

# tag words

tag_words <- list(
  ref = list(
    pos = list(
      product = list(
        recognition= c('品牌'), #'Midea', 'Haier' , etc
        style = c('外观', '外形', '外型', '造型', '样子', '美丽', '漂亮', '好看', '受看', '中看', '入眼', '顺眼', '悦目', '美观', '精美'),
        capacity = c('容量', '储藏量', '空间', '大小', '尺寸', '体积'),
        noise = c('噪音', '噪声', '声音', '杂音', '动静', '安静'),
        quality = c('质量', '做工', '材质'),
        energysaving = c('节能', '节约', '节省', '节电', '用电', '省电', '耗电', '能效', '环保'),
        manupulation = c('操作', '智能', '温控', '控制'),
        frostless = c('无霜'),
        radiating = c('散热'),
        cooling = c('制冷'),
        freshness = c('保鲜'),
        price = c('便宜', '性价比', '价格', '贵')
      ),
      logistic = list('快递', '送货', '物流'),
      service = list('安装', '售后', '客服')
      ),
    neg = list(
      product = list(
        recognition= c('品牌'), #'Midea', 'Haier' , etc
        style = c('外观', '外形', '外型', '造型', '样子', '美丽', '漂亮', '好看', '受看', '中看', '入眼', '顺眼', '悦目', '美观', '精美'),
        capacity = c('容量', '储藏量', '空间', '大小', '尺寸', '体积'),
        noise = c('噪音', '噪声', '声音', '杂音', '动静', '安静'),
        quality = c('质量', '做工', '材质'),
        energysaving = c('节能', '节约', '节省', '节电', '用电', '省电', '耗电', '能效', '环保'),
        manupulation = c('操作', '智能', '温控', '控制'),
        frostless = c('无霜'),
        radiating = c('散热'),
        cooling = c('制冷'),
        freshness = c('保鲜'),
        price = c('便宜', '性价比', '价格', '贵')
      ),
      logistic = list('快递', '送货', '物流'),
      service = list('安装', '售后', '客服')
      )
    ),
  air = list(
    pos = list(
      product = list(recognition= c('品牌'), #'Midea', 'Haier' , etc
                     style = c('外观', '外形', '外型', '造型', '样子', '美丽', '漂亮', '好看', '受看', '中看', '入眼', '顺眼', '悦目', '美观', '精美'),
                     windpower = c('风力', '摆风', '扫风', '出风'),
                     noise = c('噪音', '噪声', '声音', '杂音', '动静', '安静'),
                     quality = c('质量', '做工', '材质'),
                     energysaving = c('节能', '节约', '节省', '节电', '用电', '省电', '耗电', '能效', '环保'),
                     manupulation = c('操作', '智能', '温控', '控制'),
                     heating = c('制热', '加热'),
                     cooling = c('制冷'),
                     price = c('便宜', '性价比', '价格', '贵')
                     ),
      logistic = list('快递', '送货', '物流'),
      service = list('安装', '售后', '客服')
    ),
    neg = list(
      product = list(recognition= c('品牌'), #'Midea', 'Haier' , etc
                     style = c('外观', '外形', '外型', '造型', '样子', '美丽', '漂亮', '好看', '受看', '中看', '入眼', '顺眼', '悦目', '美观', '精美'),
                     windpower = c('风力', '摆风', '扫风', '出风'),
                     noise = c('噪音', '噪声', '声音', '杂音', '动静', '安静'),
                     quality = c('质量', '做工', '材质'),
                     energysaving = c('节能', '节约', '节省', '节电', '用电', '省电', '耗电', '能效', '环保'),
                     manupulation = c('操作', '智能', '温控', '控制'),
                     heating = c('制热', '加热'),
                     cooling = c('制冷'),
                     price = c('便宜', '性价比', '价格', '贵')
                     ),
      logistic = list('快递', '送货', '物流'),
      service = list('安装', '售后', '客服')
    )
  ),
  wash = list(
    pos = list(
      product = list(recognition= c('品牌'), #'Midea', 'Haier' , etc
                     style = c('外观', '外形', '外型', '造型', '样子', '美丽', '漂亮', '好看', '受看', '中看', '入眼', '顺眼', '悦目', '美观', '精美'),
                     capacity = c('容量', '空间', '大小', '尺寸', '体积'),
                     noise = c('噪音', '噪声', '声音', '杂音', '动静', '安静'),
                     quality = c('质量', '做工', '材质'),
                     energysaving = c('节能', '节约', '节省', '节电', '节水', '用电', '用水', '省水', '省电', '耗水', '耗电', '能效', '环保'),
                     manupulation = c('操作', '智能', '控制'),
                     washing = c('干净'),
                     price = c('便宜', '性价比', '价格', '贵')
      ),
      logistic = list('快递', '送货', '物流'),
      service = list('安装', '售后', '客服')
    ),
    neg = list(product = list(recognition= c('品牌'), #'Midea', 'Haier' , etc
                              style = c('外观', '外形', '外型', '造型', '样子', '美丽', '漂亮', '好看', '受看', '中看', '入眼', '顺眼', '悦目', '美观', '精美'),
                              capacity = c('容量', '空间', '大小', '尺寸', '体积'),
                              noise = c('噪音', '噪声', '声音', '杂音', '动静', '安静'),
                              quality = c('质量', '做工', '材质'),
                              energysaving = c('节能', '节约', '节省', '节电', '节水', '用电', '用水', '省水', '省电', '耗水', '耗电', '能效', '环保'),
                              manupulation = c('操作', '智能', '控制'),
                              washing = c('不干净'),
                              price = c('便宜', '性价比', '价格', '贵')
    ),
    logistic = list('快递', '送货', '物流'),
    service = list('安装', '售后', '客服')
    )
  ),
  tv = list(
    pos = list(
      product = list(recognition= c('品牌'), #'Midea', 'Haier' , etc
                     style = c('外观', '外形', '外型', '造型', '样子', '美丽', '漂亮', '好看', '受看', '中看', '入眼', '顺眼', '悦目', '美观', '精美'),
                     screen = c('屏幕', '画面', '色差', '清晰'),
                     noise = c('噪音', '噪声', '声音', '杂音', '动静', '安静'),
                     quality = c('质量', '做工', '材质'),
                     energysaving = c('节能', '节约', '节省', '节电', '用电', '省电', '耗电', '能效', '环保'),
                     manupulation = c('操作', '智能', '控制'),
                     price = c('便宜', '性价比', '价格', '贵')
      ),
      logistic = list('快递', '送货', '物流'),
      service = list('安装', '售后', '客服')
    ),
    neg = list(
      product = list(recognition= c('品牌'), #'Midea', 'Haier' , etc
                     style = c('外观', '外形', '外型', '造型', '样子', '美丽', '漂亮', '好看', '受看', '中看', '入眼', '顺眼', '悦目', '美观', '精美'),
                     screen = c('屏幕', '画面', '色差', '清晰'),
                     noise = c('噪音', '噪声', '声音', '杂音', '动静', '安静'),
                     quality = c('质量', '做工', '材质'),
                     energysaving = c('节能', '节约', '节省', '节电', '用电', '省电', '耗电', '能效', '环保'),
                     manupulation = c('操作', '智能', '控制'),
                     price = c('便宜', '性价比', '价格', '贵')
      ),
      logistic = list('快递', '送货', '物流'),
      service = list('安装', '售后', '客服')
    )
  ),
  hood = list(
    pos = list(
      product = list(recognition= c('品牌'), #'Midea', 'Haier' , etc
                     style = c('外观', '外形', '外型', '造型', '样子', '美丽', '漂亮', '好看', '受看', '中看', '入眼', '顺眼', '悦目', '美观', '精美'),
                     windpower = c('风力', '出风'),
                     noise = c('噪音', '噪声', '声音', '杂音', '动静', '安静'),
                     quality = c('质量', '做工', '材质'),
                     energysaving = c('节能', '节约', '节省', '节电', '用电', '省电', '耗电', '能效', '环保'),
                     manupulation = c('操作', '智能', '控制', '清洗'),
                     fumecontrol = c('吸烟', '吸油'),
                     price = c('便宜', '性价比', '价格', '贵')
      ),
      logistic = list('快递', '送货', '物流'),
      service = list('安装', '售后', '客服')
    ),
    neg = list(
      product = list(recognition= c('品牌'), #'Midea', 'Haier' , etc
                     style = c('外观', '外形', '外型', '造型', '样子', '美丽', '漂亮', '好看', '受看', '中看', '入眼', '顺眼', '悦目', '美观', '精美'),
                     windpower = c('风力', '出风'),
                     noise = c('噪音', '噪声', '声音', '杂音', '动静', '安静'),
                     quality = c('质量', '做工', '材质'),
                     energysaving = c('节能', '节约', '节省', '节电', '用电', '省电', '耗电', '能效', '环保'),
                     manupulation = c('操作', '智能', '控制', '清洗'),
                     fumecontrol = c('吸烟', '吸油'),
                     price = c('便宜', '性价比', '价格', '贵')
      ),
      logistic = list('快递', '送货', '物流'),
      service = list('安装', '售后', '客服')
    )
  ),
  dish = list(
    pos = list(
      product = list(recognition= c('品牌'), #'Midea', 'Haier' , etc
                     style = c('外观', '外形', '外型', '造型', '样子', '美丽', '漂亮', '好看', '受看', '中看', '入眼', '顺眼', '悦目', '美观', '精美'),
                     noise = c('噪音', '噪声', '声音', '杂音', '动静', '安静'),
                     quality = c('质量', '做工', '材质'),
                     energysaving = c('节能', '节约', '节省', '节电', '节水', '用电', '用水', '省水', '省电', '耗水', '耗电', '能效', '环保'),
                     manupulation = c('操作', '智能', '控制', '清洗'),
                     washing = c('干净'),
                     price = c('便宜', '性价比', '价格', '贵')
      ),
      logistic = list('快递', '送货', '物流'),
      service = list('安装', '售后', '客服')
    ),
    neg = list(
      product = list(recognition= c('品牌'), #'Midea', 'Haier' , etc
                     style = c('外观', '外形', '外型', '造型', '样子', '美丽', '漂亮', '好看', '受看', '中看', '入眼', '顺眼', '悦目', '美观', '精美'),
                     noise = c('噪音', '噪声', '声音', '杂音', '动静', '安静'),
                     quality = c('质量', '做工', '材质'),
                     energysaving = c('节能', '节约', '节省', '节电', '节水', '用电', '用水', '省水', '省电', '耗水', '耗电', '能效', '环保'),
                     manupulation = c('操作', '智能', '控制', '清洗'),
                     washing = c('不干净'),
                     price = c('便宜', '性价比', '价格', '贵')
      ),
      logistic = list('快递', '送货', '物流'),
      service = list('安装', '售后', '客服')
    )
  ),
  gas = list(
    pos = list(
      product = list(recognition= c('品牌'), #'Midea', 'Haier' , etc
                     style = c('外观', '外形', '外型', '造型', '样子', '美丽', '漂亮', '好看', '受看', '中看', '入眼', '顺眼', '悦目', '美观', '精美'),
                     noise = c('噪音', '噪声', '声音', '杂音', '动静', '安静'),
                     quality = c('质量', '做工', '材质'),
                     energysaving = c('节能', '节约', '节省', '节电', '用电', '省电', '耗电', '能效', '环保'),
                     manupulation = c('操作', '智能', '控制'),
                     safety = c('安全', '稳定'),
                     price = c('便宜', '性价比', '价格', '贵')
      ),
      logistic = list('快递', '送货', '物流'),
      service = list('安装', '售后', '客服')
    ),
    neg = list(
      product = list(recognition= c('品牌'), #'Midea', 'Haier' , etc
                     style = c('外观', '外形', '外型', '造型', '样子', '美丽', '漂亮', '好看', '受看', '中看', '入眼', '顺眼', '悦目', '美观', '精美'),
                     noise = c('噪音', '噪声', '声音', '杂音', '动静', '安静'),
                     quality = c('质量', '做工', '材质'),
                     energysaving = c('节能', '节约', '节省', '节电', '用电', '省电', '耗电', '能效', '环保'),
                     manupulation = c('操作', '智能', '控制'),
                     safety = c('安全', '稳定'),
                     price = c('便宜', '性价比', '价格', '贵')
      ),
      logistic = list('快递', '送货', '物流'),
      service = list('安装', '售后', '客服')
    )
  )
)