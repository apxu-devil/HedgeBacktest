
load('rub2.RData')

ivs = rub1 %>% as.data.frame 
ivs$Date = row.names(ivs) %>% as.Date

ivs = ivs %>% as.tbl %>% 
  select(Date, iv1w, iv1m, iv2m, iv3m, iv6m, iv9m, iv1y) %>% 
  filter(Date>'2009-12-03') %>% 
  arrange(Date)

#
# Implied volatility for the days til expiration
#
IvCurve = function(iv_values){
  
  library(minpack.lm)
  
  ivdays = c(7, 30, 60, 90, 180, 270, 365)
  ivdata = data.frame(iv = iv_values, ivdays = ivdays)
  
  nmax = 1000
  set.seed(10000)
  start_df = data.frame(a=runif(nmax, 0.5, 1.5), b=runif(nmax, 0.5, 1.5), cc=runif(nmax, 0.5, 1.5),  d=runif(nmax, 0.5, 1.5))
  
  iv.mod=0
  n=0
  
  while(n <= nmax){
    
    start  = start_df[n,]
    #start = list(a=0.5, b=0.5, cc=0.5, d=0.5)
    iv.mod = try(nlsLM(formula='iv ~ a + b*ivdays + cc*ivdays^2 + d*ivdays^3', data=ivdata, start = start ), TRUE)
    if(class(iv.mod) == 'try-error') iv.mod = NULL else return((iv.mod))
    
    n=n+1
  }

  return(iv.mod)
  
}

# test:
# iv_values = ivs %>% filter(Date=='2012-07-10') %>% select(contains('iv')) %>% unlist %>% as.vector()
# iv_values
# ivm = IvCurve(iv_values)
# 
# plot(iv_values)
# lines(fitted(ivm))

#
# Calc swap curves parametres
#
iv_curves = data.frame()

for(i in ivs$Date){
  
  i = as.Date(i)
  iv_values = ivs %>% filter(Date==i) %>% select(contains('iv')) %>% unlist %>% as.vector()
  
  if(iv_values!=0){
    iv_curve = IvCurve(iv_values) %>% coef %>% t %>% as.data.frame
    iv_curve$Date = i
    iv_curves = rbind(iv_curves, iv_curve)
  }
  
}

iv_curves %>% as.tbl

###


 iv_curves$Date %in% ivs$Date 

iv_full = left_join(ivs, iv_curves, by='Date') %>% filter(!is.na(a)) 

#iv_full =xts(x = iv_full[, -1], order.by= (iv_full[[1]]))

#
# IV at date for days
#
# dataDate = '2009-12-04'
# ivDays = 20

IvAtDate = function(dataDate, ivDays, data = iv_full){
  
  dataDate = as.Date(dataDate)
  coefs = data %>% filter(Date==dataDate) %>% select(a, b, cc, d) %>% as.vector
  
  if(nrow(coefs)==0) return(NA)
  
  iv  = coefs$a + coefs$b*ivDays + coefs$cc*ivDays^2 + coefs$d*ivDays^3
  
  return(iv)
  
}

# test:
IvAtDate(dataDate='2009-12-07', ivDays=2)

