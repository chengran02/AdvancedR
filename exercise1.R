count <- list()
rs <- na.omit(ds) %>%
  mutate(start=sapply(ds$start,as.character)) %>%
  mutate(end=sapply(ds$end,as.character)) 
id <- list()
f <- list()
maxnum <- list()
for(j in 1:nlevels(rs$anest)){
  ars <- rs[which(rs$anest==levels(rs$anest)[j]),]
  f[[as.character(j)]] <- list()
  count[[as.character(j)]] <- vector(length = length(ars$end))
  for(i in 1:length(ars$end)){               #对每一结束时间遍历
    for(z in 1:length(ars$start)){            #对每一开始时间遍历
      if((ars$start[z]<ars$end[i])&(ars$start[i]<ars$end[z])){             #如果开始时间早于结束时间则记录
        count[[j]][i] = count[[j]][i]+1   
        f[[j]][[as.character(i)]] <- c(f[[j]][[as.character(i)]],ars$id[z])
      }
    }
    maxnum[[as.character(j)]] <- max(count[[j]])
    id[[as.character(j)]] <- f[[j]][[which(count[[j]]==maxnum[[j]])]]
  }
}
result <- list(maxnum,id)


#去掉不属于第j类资产的行
#



rs <- na.omit(ds) %>%
  mutate(start=sapply(ds$start,as.character)) %>%
  mutate(end=sapply(ds$end,as.character))  %>%
  group_by(anest) %>%
  lapply(findResult)
  
findResult <- function(x){
  a <- vector(length = nlevels(ds$anest))
  count <- vector(length = length(ds$end))
  for(i in 1:length(rs$end))
    for(z in 1:length(rs$start)){
      if(rs$start[z]<rs$end[i]){
        count[i] = count[i]+1
        d <- NULL
        d <- c(d,rs$id[z])
      }
    }
  return(count)
  return(c)
}