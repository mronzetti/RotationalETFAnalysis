############################################################################### 
# Select top N for each period
############################################################################### 
ntop <- function
(
  data,        # matrix with observations
  topn = 1,     # top n
  dirMaxMin = TRUE
) 
{
  out = data
  out[] = NA
  
  for( i in 1:nrow(data) ) {
    x = coredata(data[i,])
    o = sort.list(x, na.last = TRUE, decreasing = dirMaxMin)
    index = which(!is.na(x))
    x[] = NA
    
    if(len(index)>0) {
      n = min(topn, len(index))
      x[o[1:n]] = 1/n
    }
    out[i,] = x
  }
  out[is.na(out)] = 0    
  return( out )
}
############################################################################### 
# Select top N for each period, and keep them till they drop below keepn rank
############################################################################### 
ntop.keep <- function
(
  data,         # matrix with observations
  topn = 1,     # top n
  keepn = 1,     # keep n
  dirMaxMin = TRUE
) 
{
  out = data
  out[] = NA
  
  for( i in 1:nrow(data) ) {
    x = coredata(data[i,])
    o = sort.list(x, na.last = TRUE, decreasing = dirMaxMin)
    index = which(!is.na(x))
    x[] = NA
    
    if(len(index)>0) {
      n = min(topn, len(index))
      x[o[1:n]] = 1
      
      # keepn logic
      if( i>=2 ) {
        y = coredata(out[(i-1),])    # previous period selection
        n1 = min(keepn, len(index))
        y[-o[1:n1]] = NA    # remove all not in top keepn
        
        index1 = which(!is.na(y))
        if(len(index1)>0) {
          x[] = NA
          x[index1] = 1    # keep old selection
          
          for( j in 1:n ) {
            if( sum(x, na.rm = T) == topn ) break
            x[o[j]] = 1
          }
        }
      }
    }        
    out[i,] = x/sum(x, na.rm = T)    
  }
  out[is.na(out)] = 0    
  return( out )
}
# Load Systematic Investor Toolbox (SIT)
setInternet2(TRUE)
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')    
tickers = spl('SUSA,AGG,XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ')
#Alt tickers
#tickers = spl('SUSA,IWM,AGG')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '2015-01-01', env = data, auto.assign = T)
bt.prep(data, align='keep.all', dates='2015::2021')

#*****************************************************************
# Code Strategies
#****************************************************************** 
prices = data$prices  
n = len(tickers)  

# find month ends
month.ends = endpoints(prices, 'months')
month.ends = month.ends[month.ends > 0]        

# Equal Weight
data$weight[] = NA
data$weight[month.ends,] = ntop(prices, n)[month.ends,]    
capital = 100000
data$weight[] = (capital / prices) * data$weight
equal.weight = bt.run(data, type='share')


# Rank on 6 month return
position.score.100 = prices / mlag(prices, 100)
position.score.120 = prices / mlag(prices, 120)
position.score.140 = prices / mlag(prices, 140)
position.score.150 = prices / mlag(prices, 150)
position.score.160 = prices / mlag(prices, 160)
position.score.180 = prices / mlag(prices, 180)

# Seletop Top 3 funds,  and Keep then till they are in 1:6 rank, 100
data$weight[] = NA
data$weight[month.ends,] = ntop.keep(position.score.100[month.ends,], 3, 6)    
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)        
score100 = bt.run(data, type='share', trade.summary=T)
# Seletop Top 3 funds,  and Keep then till they are in 1:6 rank, 120
data$weight[] = NA
data$weight[month.ends,] = ntop.keep(position.score.120[month.ends,], 3, 6)    
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)        
score120 = bt.run(data, type='share', trade.summary=T)
# Seletop Top 3 funds,  and Keep then till they are in 1:6 rank, 140
data$weight[] = NA
data$weight[month.ends,] = ntop.keep(position.score.140[month.ends,], 3, 6)    
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)        
score140 = bt.run(data, type='share', trade.summary=T)
# Seletop Top 3 funds,  and Keep then till they are in 1:6 rank, 150
data$weight[] = NA
data$weight[month.ends,] = ntop.keep(position.score.150[month.ends,], 3, 6)    
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)        
score150 = bt.run(data, type='share', trade.summary=T)
# Seletop Top 3 funds,  and Keep then till they are in 1:6 rank, 160
data$weight[] = NA
data$weight[month.ends,] = ntop.keep(position.score.160[month.ends,], 3, 6)    
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)        
score160 = bt.run(data, type='share', trade.summary=T)
# Seletop Top 3 funds,  and Keep then till they are in 1:6 rank, 180
data$weight[] = NA
data$weight[month.ends,] = ntop.keep(position.score.180[month.ends,], 3, 6)    
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)        
score180 = bt.run(data, type='share', trade.summary=T)

#*****************************************************************
# Create Report
#****************************************************************** 
plotbt.custom.report(score100,score120,score140,score150,score160,score180, trade.summary=T)
