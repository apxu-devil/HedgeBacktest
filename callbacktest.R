
require(fOptions)
require(xts)
require(dplyr)
require(ggplot2)
require(scales)
require(grid)
require(gridExtra)
require(foreach)
require(TTR)
require(ggvis)
require(tidyr)

# Load data
load('rub2.RData')
rub3m = rub1[, c('spot', 'swap1M', 'iv1m')]
names(rub3m) = c('rub', 'swap', 'iv')

days = 30
otm = 0.05
target = 5

#
# 3m options prices every day
# 
XtsToDf = function(xtsData){
  
  xtsData = as.data.frame(xtsData)  # from xts to data frame
  xtsData$Date = as.Date(row.names(xtsData))
  
  return(xtsData)
}

#
# Cummulative PL: rolling strike on worsening price
# 

RollAgainstPl = function(data=data_window, days=90, otm=0, strat_mode='default'){
  
  data = XtsToDf(data)
  
  if(strat_mode=='roll')
    pl= data %>% mutate(chng = rub-lag(rub,default=0), 
                        t = as.numeric(last(Date)-Date),
                        cumchng = cumsum(chng)-rub[1], 
                        mincumchng = cummin(cumchng),
                        strike = (lag(rub, default=rub[1]) + cumchng) * (1+otm),
                        call1 = lag(GBSOption('c', rub, strike, t/365, 0, swap, iv)@price, default=0),
                        call2 = GBSOption('c', rub, lag(strike, default=Inf), t/365, 0, swap, iv)@price,
                        pl = call2-call1, 
                        cumpl = lag(cumsum(lead(pl))), 
                        cumplr = (cumpl+call1[2])/call1[2]-1 )
  
  if(strat_mode=='default')
    pl= data %>% mutate(chng = rub-lag(rub,default=0), 
                        t = as.numeric(last(Date)-Date),
                        cumchng = cumsum(chng)-rub[1], 
                        mincumchng = cummin(cumchng),
                        strike = rub[1] * (1+otm),
                        call1 = lag(GBSOption('c', rub, strike, t/365, 0, swap, iv)@price, default=0),
                        call2 = GBSOption('c', rub, lag(strike, default=Inf), t/365, 0, swap, iv)@price,
                        pl = call2-call1, 
                        cumpl = lag(cumsum(lead(pl))), 
                        cumplr = (cumpl+call1[2])/call1[2]-1 )

  pl_dd = data.frame(Date=data$Date[1], 
                     pl_max=max(pl$cumplr, na.rm=T), 
                     drawdown=min(pl$cumplr, na.rm=T))

  return(pl_dd)
}

# test: RollAgainstPl(data_window, days=90, otm=0.0, withroll=F)


#
# Maximum PL ratio through option live
#  

MaxPlRatio = function(data=data_window, days=90, otm=0, at_exp=F, target_date=F){
  
  data = XtsToDf(data)
  
  if(at_exp) data=data[c(1,nrow(data)),]   # Calc result only at expiration date

 # PL ratio every day through the option life
  pl_t = data %>%
    mutate(strike = rub[1]*(1+otm), 
           t = as.numeric(last(Date)-Date), 
           call_price = GBSOption('c', rub, strike, t/365, 0, swap, iv)@price,
           pl_ratio = call_price / call_price[1] - 1) 
  
    
  if(target_date==F)
    pl_t = pl_t %>% filter(pl_ratio==max(pl_ratio)) # return max PL ratio
  else 
    pl_t = pl_t %>% filter(pl_ratio>target) %>% filter(row_number()==1) # return PL ratio at the first reached day
  
  return(pl_t)
}

# test:
#  data_window = rub3m[paste0(index(rub3m)[11], "::",index(rub3m)[11]+days )]
#  MaxPlRatio(data_window)


#
# Max PL ratio for all options
#

AllMaxPlRations = function(data=rub3m, days=90, otm=0, at_exp=F,  target_date=F){
  
  lastday = last(index(data)) - days
  data = data[paste0(index(data[1]),'::',lastday)]

  pls_maxs = NULL
  
  for(i in 1:(nrow(data))){

    day_1 = index(data[i])
    day_t = day_1 + days
    data_window = data[paste0(day_1,'::',day_t)]
    
    #pls_max = MaxPlRatio(data_window, days=days, otm=otm, at_exp=at_exp, target_date=target_date)
    pls_max = RollAgainstPl(data_window, days = days, otm = otm, strat_mode = 'default')
    
    #if(nrow(pls_max)!=0) pls_max$dates=day_1 #c(empti, i)
    
    pls_maxs = rbind(pls_maxs, pls_max)
  }

  return(pls_maxs)
}


plstest = AllMaxPlRations(days=days, otm=otm, at_exp=F, target_date=F)

plot(plstest$pl_ratio ~ plstest$Date)
plot(plstest$pl_max ~ plstest$Date, type='l', ylim=c(0, 20))
plot(plstest$drawdown ~ plstest$Date)
plot(plstest$drawdown ~ plstest$pl_max)
#
# Plot result
#

as.tbl(plstest)

pls_max_otms_g = gather(pls_max_otms, key=otms, value = pl, -dates)
pls_max_otms_g %>% ggvis(~dates,~pl) %>% layer_lines()

ggplot(data = pls_max_otms_g, aes(x=dates, y=pl, color=otms)) + geom_line()



#
# Target PL Ratio probability
#
plsm = plstest$pl_max
plr_interv = pretty(plsm, n = 30)
plr_cuts = cut(plsm, plr_interv, include.lowest=T)
plr_freq = table(plr_cuts)/length(plsm)
plr_cumfreq = 1-cumsum(plr_freq)

plot(plr_interv[-1], plr_cumfreq)

plr_prob = sum(plsm>5) / length(plsm)
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





