
require(fOptions)
require(xts)
require(dplyr)
require(ggplot2)
require(scales)
require(grid)
require(gridExtra)
require(foreach)
require(TTR)

# Final data preparations
load('rub.RData')
rub3m = rub[, c('rub', 'swap3m', 'iv3m')]
names(rub3m) = c('rub', 'swap', 'iv')


days = 90
otm = 0

#
# 3m options prices every day
# 

rub3m$call = rub3m %>% 
  as.data.frame %>% 
  mutate(call = GBSOption('c', rub, rub*(1+otm), days/365, 0, swap, iv)@price) %>%
  select(call) %>% unlist


#
# Maximum PL ration in option live
# data = data_window

MaxPlRatio = function(data, days = 90, otm = 0, at_exp = F){
  
 if(at_exp)
   data = c(data[1], data[length(index(data))]) #calc only the first and the last days
  
 data$t = last(index(data)) - index(data) #days til exp column
  
  strike = as.numeric(data[1, 'rub'] * (1+otm))
  
  call_t = data %>% 
    as.data.frame %>% 
    mutate(call_t = GBSOption('c', rub, strike, t/365, 0, swap, iv)@price) %>%
    select(call_t)
  
  call_t = as.numeric(unlist(call_t))
  
  pl_ratio = call_t / call_t[1] - 1
  plr_max = max(pl_ratio)
  
  return(plr_max)
}

#
# Max PL ratio for all options
#

AllMaxPlRations = function(data = rub3m, days = 90, otm = 0, at_exp = F){
  
  pls_maxs = NULL
  
  for(i in 1:(nrow(data)-1) ){ #(nrow(rub3m)-1)
    
    day_1 = index(data[i])
    day_t = day_1 + days
    
    if(day_t > last(index(rub3m))) break  #exit loop if last date out of range
    
    data_window = data[paste0(day_1,'::',day_t)]
    
    pls_max = MaxPlRatio(data_window, days = days, otm = otm, at_exp = at_exp)
    pls_maxs = c(pls_maxs, pls_max)
    
  }
  
  return(pls_maxs)
}


day_1 = index(rub3m[1])
day_t = last(index(rub3m)) - days
indexs = rub3m[paste0(day_1,'::',day_t)] %>% index



pls_maxs = AllMaxPlRations(at_exp=T)
pls_maxs1 = AllMaxPlRations(at_exp=F)
plot(pls_maxs)
plot(pls_maxs1)

pls_max_otms = sapply(c(-0.05, 0, 0.05), function(x)AllMaxPlRations(otm=x, at_exp=F)) 

pls_max_otms = as.data.frame(pls_max_otms) 
as.tbl(pls_max_otms)
pls_max_otms$dates = indexs


pls_max_otms_g = gather(pls_max_otms, key=otms, value = pl, -dates)
pls_max_otms_g %>% ggvis(~dates,~pl, fill=~otms) %>% layer_lines()

ggplot(data = pls_max_otms_g, aes(x=dates, y=pl, color=otms)) + geom_line()

plsm = pls_max_otms$V3

#
# Target PL Ratio probability
#
plr_interv = pretty(plsm, n = 30)
plr_cuts = cut(plsm, plr_interv, include.lowest=T)
plr_freq = table(plr_cuts)/length(plsm)
plr_cumfreq = 1-cumsum(plr_freq)

plot(plr_cumfreq)

plr_n = length(plsm)
plr_prob = length(plsm[plsm>5]) / plr_n
plr_prob







# What if we do not hedge?

  usd_diff = diff(rub3m$rub, days*5/7) %>% lag(., k=-days*5/7)
  usd_roc = usd_diff /  rub3m$rub %>% na.omit
  usd_roc = ROC(rub3m$rub, days*5/7) %>% na.omit
  
  rub3m$usd_roc = ROC(rub3m$rub, days*5/7)


# +--------------+
# | Call result
# +--------------+

  usd_up = lag.xts(rub3m$rub, k=-days*5/7) - rub3m$rub*(1+otm)  %>% na.omit #usd_diff
  usd_up[usd_up<0,] = 0
  
  call_result = (usd_up - rub3m$call) / rub3m$rub
  
  pl = (usd_up - rub3m$call)/rub3m$call
  
  #cbind(usd_up,rub3m$call)
  
  hist(pl[pl>0])
  
  length(pl[pl>0])/length(pl)
  length(pl[pl>10])/length(pl)

  # Средний прирост доллара за 90 дней:

  usd_roc_df =data.frame(
    Period = c('All', '2010 - 2013', '2014+'), 
    USD_risk = c(mean(-usd_roc, na.rm = T), 
                 mean(-usd_roc['2010/2013'], na.rm = T), 
                 mean(-usd_roc['2014::'], na.rm = T)),
    SD = c(sd(usd_roc, na.rm = T), 
           sd(usd_roc['2010/2013'], na.rm=T), 
           sd(usd_roc['2014::']))
    ) %>% mutate(., variance=SD/USD_risk)
    

  # Средний результа по коллу за 90 дней:
 
  call_res_df = data.frame(
    Period = c('All', '2010 - 2013', '2014+'), 
    Call_result = c(mean(call_result, na.rm = T), 
                    mean(call_result['2010/2013'], na.rm=T), 
                    mean(call_result['2014::'])), 
    SD = c(sd(call_result, na.rm = T), 
           sd(call_result['2010/2013'], na.rm=T), 
           sd(call_result['2014::']))
    ) %>% mutate(., variance=SD/Call_result)
  

aver_hedge_res = full_join(usd_roc_df, call_res_df, by = 'Period') %>% mutate(Hedged_risk = USD_risk + Call_result)

foreach( i=2:4) %do% {
  aver_hedge_res[, i] = paste(round(aver_hedge_res[, i]*100, digits = 2), '%', sep='')
}

names(aver_hedge_res) = c('Период', 'Валютный риск', 'Результат опциона', 'Хеджированный риск')
res_grob = tableGrob(aver_hedge_res)


#
# Chart call
#

final_res =  merge(merge(call_result, -usd_diff/rub3m$rub), call_result-usd_diff/rub3m$rub) %>% na.omit
names(final_res) = c('Call_Profit', 'USD_risk', 'Hedged')


chart2 = ggplot() +  theme_grey(base_size = 18) +
  geom_area(data=fortify(final_res[,c('Hedged', 'USD_risk')], melt=T), aes(x=Index, y=Value, fill=Series)) + 
  scale_fill_manual(
    values = c('red', 'black'),
    name = 'Валютный риск:',
    labels=c('Захеджированный', 'Без хеджрования')) +
  scale_y_continuous(labels = percent,
                     breaks=seq(-1,1,0.2)) +  
  theme(title = element_text(size=14),
        legend.position=c(0,0),
        legend.justification = c(0,0),
        legend.background = element_blank()) + 
  labs(title = 'Валютные риски импортёра' , x=NULL, y=NULL) +
  geom_segment(    
    aes(x=as.Date('2014-01-01'), y=-0.4, xend=as.Date('2014-06-01'), yend=-0.5), 
    arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  annotate('text', x=as.Date('2013-03-01'), y=-0.35, label='Рост доллара - риск для импортёра') +
  geom_segment(    
    aes(x=as.Date('2011-06-01'), y=0.3, xend=as.Date('2011-01-01'), yend=0.2), 
    arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_segment(    
    aes(x=as.Date('2012-01-01'), y=0.3, xend=as.Date('2012-01-01'), yend=0.2), 
    arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  annotate('text', x=as.Date('2011-06-01'), y=0.35, label='Импортёр выигрывает от укрепления рубля')

  
grid.arrange(chart2, res_grob, nrow=2, heights=c(1, 0.5))




# +--------------+
# | Put result
# +--------------+

rub3m$put  = (as.data.frame(rub3m) %>% mutate(put = GBSOption('p', rub, rub, 90/365, swap, 0, iv)@price))[, c('put')]

usd_down = usd_diff 
usd_down[usd_down>0, ]=0

put_result = (-usd_down - rub3m$put) / rub3m$rub




hedge_res = local({
  
  # Средний прирост доллара за 90 дней:
  usd_roc_df =data.frame(
    Period = c('All', '2010 - 2013', '2014+'), 
    USD_risk = c(mean(usd_roc, na.rm = T), 
                 mean(usd_roc['2010/2013'], na.rm = T), 
                 mean(usd_roc['2014/2015'], na.rm = T)) )
  
  # Средний результа по коллу за 90 дней:
  
  put_res_df = data.frame(
    Period = c('All', '2010 - 2013', '2014+'), 
    Put_result = c(mean(put_result, na.rm = T), 
                    mean(put_result['2010/2013']), 
                    mean(put_result['2014'])))
  
  aver_hedge_res = full_join(usd_roc_df, put_res_df, by = 'Period') %>% mutate(Hedged_risk = USD_risk + Put_result)
  
  foreach( i=2:4) %do% {
    aver_hedge_res[, i] = paste(round(aver_hedge_res[, i]  * 100, digits = 2), '%', sep='')
  }
  
  names(aver_hedge_res) = c('Период', 'Валютный риск', 'Результат опциона', 'Хеджированный риск')
  aver_hedge_res
  
})

res_grob_put = tableGrob(hedge_res, name='test')


#
# Chart put
#

final_res_put =  merge(merge(put_result, usd_diff/rub3m$rub), put_result+usd_diff/rub3m$rub) %>% na.omit
names(final_res_put) = c('Put_Profit', 'USD_risk', 'Hedged')

local({
  mean(final_res_put$Hedged, na.rm=T)/sd(final_res_put$Hedged, na.rm=T)
  mean(final_res_put$USD_risk, na.rm=T)/sd(final_res_put$USD_risk, na.rm = T)
  })

chart3 = ggplot() +  theme_grey(base_size = 18) +
  geom_area(data=fortify(final_res_put[,c('Hedged', 'USD_risk')], melt=T), aes(x=Index, y=Value, fill=Series)) + 
  scale_fill_manual(
    values = c('red', 'black'),
    name = 'Валютный риск:',
    labels=c('Захеджированный', 'Без хеджрования')) +
  scale_y_continuous(labels = percent,
                     breaks=seq(-1,2,0.2)) +  
  theme(title = element_text(size=14),
        legend.position=c(0,1),
        legend.justification = c(0,1),
        legend.background = element_blank()) + 
  labs(title = 'Валютные риски экспортёра' , x=NULL, y=NULL) +
  
  geom_segment(    
    aes(x=as.Date('2014-01-01'), y=-0.4, xend=as.Date('2014-06-01'), yend=-0.5), 
    arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  annotate('text', x=as.Date('2013-03-01'), y=-0.35, label='Рост доллара - риск для импортёра') +
  geom_segment(    
    aes(x=as.Date('2011-06-01'), y=0.3, xend=as.Date('2011-01-01'), yend=0.2), 
    arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  geom_segment(    
    aes(x=as.Date('2012-01-01'), y=0.3, xend=as.Date('2012-01-01'), yend=0.2), 
    arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) +
  annotate('text', x=as.Date('2011-06-01'), y=0.35, label='Импортёр выигрывает от укрепления рубля')


grid.arrange(chart3, res_grob_put, nrow=2, heights=c(1, 0.5))





