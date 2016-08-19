require(dplyr)

datas = NULL

for(f in list.files('./data')){

  idata = read.delim(paste0('./data/', f), as.is = T)
  idata[[1]] = as.Date(idata[[1]], format='%d.%m.%Y')
  
  vname = substr(f, start = 1, stop = nchar(f)-4)
  
  names(idata)[2] = vname
  
  if(is.null(datas)) 
    datas = idata
  else
    datas = full_join(datas, idata, by='Date')
  
}


dataswap = datas %>% as.tbl %>% arrange(Date) %>% select(Date, spot, starts_with('fwd')) %>%
  mutate(swap1m = (fwd1m/spot-1)/30*365) %>% select(Date, spot, fwd1m, swap1m)
