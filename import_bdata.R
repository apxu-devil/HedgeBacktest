require(dplyr)

# Import IV data

ivs = NULL

for(f in list.files('./data/iv')){

  idata = read.delim(paste0('./data/iv/', f), as.is = T)
  idata[[1]] = as.Date(idata[[1]], format='%d.%m.%Y')
  
  vname = substr(f, start = 1, stop = nchar(f)-4)
  
  names(idata)[2] = vname
  
  if(is.null(ivs)) 
    ivs = idata
  else
    ivs = full_join(ivs, idata, by='Date')
}

for(n in names(ivs)[-c(1,9)])
  ivs[[n]] = as.numeric(ivs[[n]])/100

# Import swap data

swaps = read.delim('./data/swaps.txt', as.is = T)
swaps[[1]] = as.Date(swaps[[1]], format='%d.%m.%Y')
names(swaps)[-1] = paste0('swap', substr(names(swaps)[-1] , start = 2, stop = 3))

# Remove NA rows and duplicated dates
swaps = swaps %>% filter(!(is.na(swap1W) & is.na(swap2W))) %>% filter(!(row_number() %in% which(duplicated(Date))))

for(n in names(swaps)[-1])
  swaps[[n]] = as.numeric(swaps[[n]])/100
  

# Import spot

spot = read.delim('./data/spot.txt', as.is = T) %>% 
  mutate(Date=as.Date(Date, format='%d.%m.%Y')) %>% 
  as.tbl %>% 
  filter(Date>as.Date('2009-12-01')) 

# as.tbl(swaps)
# as.tbl(ivs) %>% select(starts_with('iv')) %>% head

# Join all

rub = spot %>% as.tbl %>% left_join(swaps) %>% left_join(ivs)

rub1 = (xts(x=rub[-1], order.by = rub[[1]], unique = T) %>% na.locf)[-1]


save(rub1, file = 'rub2.RData')
write.zoo(x = rub1, './data/rub.txt', sep=';')

