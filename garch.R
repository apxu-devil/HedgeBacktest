

#We select data for USDRUB priced and implied volatility history
xdata = rubmrtk[, c('USD.RUB', 'iv1m', 'iv3m', 'iv1y')]

# See the historical omplied volatilities
autoplot(xdata[,-1], facets=NULL)

# Here we can see volatilities correlation
cor(xdata)
correlationTest(x = xdata[,2], y=xdata[,3], method = 'spearman')

# And they are all correlated
pairs(as.data.frame(xdata))

# And we can be  sure they are
attach(as.data.frame(xdata))
lm.out = lm(iv1m~iv3m)
plot(iv1m~iv3m, xdata)  
abline(lm.out, col='red')


# Model iv3m from iv1m and iv1y
lmtest = lm(iv3m ~ iv1m + iv1y, data = as.data.frame(xdata), start = list(a=1, b=1))
lmtest$fitted.values

plot(lmtest$fitted.values, type='l')
lines(iv3m, col='red')


#garch
usd_roc = ROC(xdata[,1])
names(usd_roc) = 'USD.roc'
usd_roc[which(is.na(usd_roc)),] = 0
usd_roc_sqr = 
  rollsum(usd_roc^2, 65)

tail(usd_roc_sqr)

plot(usd_roc_sqr)

lag(usd_roc,1)


xdata = merge(xdata, usd_roc, check.names = F)

merge(lag(usd_roc,1), lag(iv1m/100,1))

madeldata = %>% merge(iv3m/100, .)
mean(iv1m)

names(xdata)[which(names(xdata)=='USD.roc.1')] = 'UsdRocSqr'

xdata = 
  merge(xdata, lag(usd_roc,1)^2)

modeldata = xdata[,c('iv3m', 'iv1m', 'UsdRocSqr')]
modeldata[,'iv1m'] = lag(modeldata[,'iv1m'] , 1)
modeldata = modeldata[-1,]

lmtest = lm(iv3m ~  I(UsdRocSqr^0.5) + iv1m, data = as.data.frame(modeldata))
attach(as.data.frame(modeldata))
plot(lmtest$fitted.values, type='l')
lines(iv3m, col='red')

plot(modeldata[,'iv3m', drop=T])
lines(lmtest$fitted.values, col='red')
