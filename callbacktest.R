
require(fOptions)
require(xts)
require(dplyr)
require(ggplot2)
require(scales)
require(grid)
require(gridExtra)

# Final data preparations
load('rubswap.RData')
rub3m = rubmrtk[, c('USD.RUB', 'swap3m_perc', 'iv3m')]
names(rub3m) = c('rub', 'swap', 'iv')
rub3m$iv = rub3m$iv/100

#
# 3m options prices every day
# 
rub3m$call = (as.data.frame(rub3m) %>% mutate(call = GBSOption('c', rub, rub, 90/365, swap, 0, iv)@price))[, c('call')]
rub3m$put  = (as.data.frame(rub3m) %>% mutate(put = GBSOption('p', rub, rub, 90/365, swap, 0, iv)@price))[, c('put')


# What if we do not hedge?


  usd_diff = lag(diff(rub3m$rub, 90*5/7), k=-90*5/7)
  usd_roc = usd_diff /  rub3m$rub %>% na.omit




# +--------------+
# | Call result
# +--------------+

  usd_up = usd_diff 
  usd_up[usd_up<0,] = 0
  
  call_result = (usd_up - rub3m$call) / rub3m$rub
   

  # Средний прирост доллара за 90 дней:

  usd_roc_df =data.frame(
    Period = c('All', '2010 - 2013', '2014+'), 
    USD_risk = c(mean(-usd_roc, na.rm = T), 
                      mean(-usd_roc['2010/2013'], na.rm = T), 
                      mean(-usd_roc['2014/2015'], na.rm = T)) )
  

  # Средний результа по коллу за 90 дней:
 
  call_res_df = data.frame(
    Period = c('All', '2010 - 2013', '2014+'), 
    Call_result = c(mean(call_result, na.rm = T), 
               mean(call_result['2010/2013']), 
               mean(call_result['2014'])))
  

aver_hedge_res = full_join(usd_roc_df, call_res_df, by = 'Period') %>% mutate(Hedged_risk = USD_risk + Call_result)

foreach( i=2:4) %do%{
  aver_hedge_res[, i] = paste(round(aver_hedge_res[, i]  * 100, digits = 2), '%', sep='')
}

names(aver_hedge_res) = c('Период', 'Валютный риск', 'Результат опциона', 'Хеджированный риск')
res_grob = tableGrob(aver_hedge_res, name='test')


usd_down = usd_diff 
usd_down[usd_down>0, ]=0

mean(-usd_diff[usd_diff<0, ]/rub3m$rub)
mean((-usd_diff[usd_diff<0, ] - rub3m$call)/rub3m$rub, na.rm = T)

 # Price change to call price
rub3m$difftocall = rub3m$diff / rub3m$call 

absDiffToCall = abs(rub3m$diff) / rub3m$call
summary(absDiffToCall)
autoplot(absDiffToCall)

final_res =  merge(merge(call_result, -usd_diff/rub3m$rub), call_result-usd_diff/rub3m$rub) %>% na.omit
names(final_res) = c('Call_Profit', 'USD_risk', 'Hedged')




chart1 = ggplot() +  theme_grey(base_size = 18) +
  geom_line(
    data=fortify(final_res[,c('Call_Profit')], melt=T), 
    aes(x=Index, y=Value, color=Series)) +
  scale_colour_manual(
    values = c("red"), 
    name='На графике:', 
    labels=c('Результат опциона call')) +
  theme(
    legend.position=c(0,1), 
    legend.justification=c(0,1), 
    legend.margin = unit(0, "cm"),
    legend.background = element_blank(),
    axis.text.x = element_blank()) +  #, plot.margin=unit(c(0,0,0,0), 'cm')
  labs(title='Хеджирование опционом call', x=NULL, y=NULL) + 
  scale_y_continuous(labels = percent) + 
  geom_segment(
    aes(x=as.Date('2014-01-01'), y=-0.4, xend=as.Date('2014-06-01'), yend=-0.5), 
    arrow = arrow(length = unit(0.2, 'cm'), type = 'closed')) #+
  #annotate('text', x=as.Date('2013-01-01'), y=-0.4, label='Рост доллара - риск для импортёра, \n поэтому график инвертирован')


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





