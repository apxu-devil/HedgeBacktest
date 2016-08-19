require(dplyr)

ivs = NULL

for(f in list.files('./data')){

  idata = read.delim(paste0('./data/', f), as.is = T)
  idata[[1]] = as.Date(idata[[1]], format='%d.%m.%Y')
  
  vname = substr(f, start = 1, stop = nchar(f)-4)
  
  names(idata)[2] = vname
  
  if(is.null(ivs)) 
    ivs = idata
  else
    ivs = full_join(ivs, idata, by='Date')
  
}


swaps = read.delim('swaps.txt', as.is = T)
swaps[[1]] = as.Date(swaps[[1]], format='%d.%m.%Y')
names(swaps)[-1] = paste0('swap', substr(names(swaps)[-1] , start = 2, stop = 3))

for(n in names(swaps)[-1]){
  
  swaps[[n]] = as.numeric(swaps[[n]])/100
  
}

for(n in names(ivs)[-c(1,9)]){
  
  ivs[[n]] = as.numeric(ivs[[n]])/100
  
}

as.tbl(swaps)
as.tbl(ivs) %>% select(starts_with('iv'))/100 %>% head

rub = left_join(swaps, ivs, by='Date') %>% as.tbl()

rub1 = xts(x=rub[-1], order.by = rub[[1]])
rub1 = na.locf(rub1)

save(rub1, file = 'rub2.RData')
write.zoo(x = rub1, './data/rub.txt', sep=';')

