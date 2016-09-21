


load('rub2.RData')

percs = rub1 %>% as.data.frame 
percs$Date = row.names(percs) %>% as.Date

percs = percs %>% as.tbl %>% 
  select(Date, contains('swap')) %>% 
  filter(Date>'2009-12-03') %>% 
  arrange(Date)

swap_values = percs %>% filter(Date=='2014-12-29') %>% select(contains('swap')) %>% unlist %>% as.vector()
swap_values

#
# Swap for the days til expiration
#
SwapCurve = function(swap_values){
  
  library(minpack.lm)
  
  swdays = c(7, 14, 30, 60, 90, 180, 270, 365)
  swdata = data.frame(perc = swap_values, swdays = swdays)
  sw_model = nlsLM(formula = paste('perc ~ a + b*swdays + cc*swdays^2 + d*swdays^3 + e*swdays^4 + f*swdays^5'), data=swdata, start = list(a=0.5, b=0.5, cc=0.5, d=0.5, e=0.5, f=0.5) )
  
  return(sw_model)
  
}

# test:

#
# Calc swap curves parametres
#
swap_curves = data.frame()

for(i in percs$Date){
  
  i = as.Date(i)
  swap_values = percs %>% filter(Date==i) %>% select(contains('swap')) %>% unlist %>% as.vector()
  
  if(swap_values!=0){
    swap_curve = SwapCurve(swap_values) %>% coef %>% t %>% as.data.frame
    swap_curve$Date = i
    swap_curves = rbind(swap_curves, swap_curve)
  }
  
}

swap_curves %>% as.tbl

###

swap_full = full_join(percs, swap_curves, by='Date') %>% filter(is.na(a))

swap_full %>% filter(is.na(a))




