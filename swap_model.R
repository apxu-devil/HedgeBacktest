

percs = (rub1 %>% as.data.frame %>% as.tbl %>% select(starts_with('swap')))

sw = as.data.frame(percs)[1300, , drop=T] %>% unlist %>% as.vector()

swdata = data.frame(perc = sw, days = c(7, 14, 30, 60, 90, 180, 270, 365))


sw_model = nls(formula = paste('perc ~ a + b*days + cc*days^2 + d*days^3 + e*days^4 + f*days^5'), data=swdata, start = list(a=0.5, b=0.5, cc=0.5, d=0.5, e=0.5, f=0.5) )


sw_model %>% predict()

plot(perc ~ days, data=swdata)
lines(swdata$days, predict(sw_model))
