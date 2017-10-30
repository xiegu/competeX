library(RColorBrewer)
library(magrittr)
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
  'tv' = c('电视机', '电视', '英寸', '京东')
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
        energysaving = c('节能', '节约', '节省', '节电', '节水', '用电', '用水', '省水', '省电', '耗水', '耗电', '能效', '环保'),
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
        energysaving = c('节能', '节约', '节省', '节电', '节水', '用电', '用水', '省水', '省电', '耗水', '耗电', '能效', '环保'),
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
                     manupulation = c('操作', '智能', '温控', '控制'),
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
                              manupulation = c('操作', '智能', '温控', '控制'),
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
                     energysaving = c('节能', '节约', '节省', '节电', '节水', '用电', '用水', '省水', '省电', '耗水', '耗电', '能效', '环保'),
                     manupulation = c('操作', '智能', '温控', '控制'),
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
                     energysaving = c('节能', '节约', '节省', '节电', '节水', '用电', '用水', '省水', '省电', '耗水', '耗电', '能效', '环保'),
                     manupulation = c('操作', '智能', '温控', '控制'),
                     price = c('便宜', '性价比', '价格', '贵')
      ),
      logistic = list('快递', '送货', '物流'),
      service = list('安装', '售后', '客服')
    )
  )
)