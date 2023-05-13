library(logopt)
library(matrixStats)
library(genalg)
library(ggplot2)
library(dplyr)
library(plotly)

data(nyse.cover.1962.1984)
x <- coredata(nyse.cover.1962.1984)

wrap_graphs <- function(){
  
  xik <- x[,c("iroqu","kinar")]
  nDays <- dim(xik)[1]
  Days <- 1:nDays
  alphas <- seq(0,1,by=0.05)
  crps <- alphas
  for (i in 1:length(crps)) {
    crps[i] <- crp(xik, c(alphas[i], 1-alphas[i]))[nDays]
  }
  
  
  
  pik <- apply(xik,2,cumprod)
  alphas <- seq(0,1,by=0.05)
  universal <- xik[,1] * 0
  for (i in 1:length(alphas)) {
    universal <- universal + crp(xik, c(alphas[i], 1-alphas[i]))
  }
  
  
  universal <- universal/length(alphas)
  
  
  par(mfrow = c(2,2))
  
  plot(alphas, crps, col="blue", type="l", ylab="", 
       main='20 Year Return vs. mix of "iroqu" and "kinar"',
       xlab='Fraction of "iroqu" in Portfolio')
  points(alphas, crps, pch=19, cex=0.5, col="red")
  abline(h=mean(crps), col="green")
  text(0.5,mean(crps)*1.05,labels="Return from Universal Portfolio")
  grid()
  bk <- xik[,1] * 0
  w <- xik[,1] * 0
  for (i in 1:length(crps)) {
    # we calculate bk by weighting the b by the realized wealth lagged one
    weight <- lag(crp(xik, c(alphas[i], 1-alphas[i])), 1)
    bk <- bk + alphas[i] * weight
    w <- w + weight
  }
  bk <- bk / w
  bk[1] <- 0.5
  plot(Days, bk, col="blue", type="l", ylim = range(0.25, range(bk)), 
       main = 'Mix of "iroqu" and "kinar" in Universal Portfolio', 
       ylab='Fraction of "iroqu"')
  grid()
  
  
  
  plot(Days, pik[,"iroqu"], col="blue", type="l", ylim=range(pik, universal), 
       main = 'Universal Portfolios with "iroqu" and "kinar"', ylab="")
  
  lines(Days, pik[,"kinar"], col="red")
  lines(Days, universal, col="green")
  legend("topleft",c('"iroqu"','"kinar"','"universal"'),
         col=c("blue","red","green"),lty=c(1,1,1))
  
  grid()
  
  plot(Days, pik[,"iroqu"], col="blue", type="l", 
       ylim=range(pik), main = '"iroqu" and "kinar"', ylab="")
  lines(Days, pik[,"kinar"], col="red")
  grid()
  legend("topright",c('"iroqu"','"kinar"'),
         col=c("blue","red"),lty=c(1,1))
  grid()
  
  
  
}
wrap_graphs()


my.crp <- function(df=x,
                   days=DAYS,
                   vector=NULL,
                   rebalance=T,
                   how_far_back=HOW_FAR_BACK, #how far back
                   reb_freq=HOW_OFTEN       #how often do you want to rebalance?
){
  
  if(is.null(vector)){
    w = rep(1, dim(df)[2]) / dim(df)[2]
  }else{
    w=vector}
  
  W <- matrix(w,byrow=T,ncol=dim(df)[2])
  colnames(W) <- colnames(df)
  
  S <- 1
  
  for(i in 1:days){
    S <- c(S, w%*%df[i,]*tail(S,1))
    
    if(rebalance){
      
      if(i%%reb_freq==0){
        if(how_far_back==1){
          if(i>1){
            w<- colProds(df[(i-how_far_back):i,])/sum(colProds(df[(i-how_far_back):i,]))
          }
        }else{
          
          w<- colProds(df[(i-how_far_back):i,])/sum(colProds(df[(i-how_far_back):i,]))   
        }
        
        
      }
      
      
      
      
    }
    
    W <- rbind(W,w)
  }
  
  row.names(W) <- NULL
  
  #DF <- df[1:days,]
  #CPR <- S[2:(days+1)]
  #DF <- cbind(df[1:days,],CPR)
  #results <- list(S, W, DF)
  #print(tail(S,1))
  
  return(S)
}


#Constant Rebalanced Portfolio ALL THE WAY BACK 
crp.ATWB <- function(df=x,
                     days=DAYS,
                     vector=NULL,
                     rebalance=T,
                     reb_freq=HOW_OFTEN   #how often do you want to rebalance?
){
  
  if(is.null(vector)){
    w = rep(1, dim(df)[2]) / dim(df)[2]
  }else{
    w=vector}
  
  W <- matrix(w,byrow=T,ncol=dim(df)[2])
  colnames(W) <- colnames(df)
  
  S <- 1
  
  for(i in 1:days){
    S <- c(S, w%*%df[i,]*tail(S,1))
    
    if(rebalance){
      
      if(i%%reb_freq==0){
        if(i>1){
          w<- colProds(df[1:i,])/sum(colProds(df[1:i,]))
        }
        
        
      }
      
      
    }
    
    W <- rbind(W,w)
  }
  
  row.names(W) <- NULL
  
  #DF <- df[1:days,]
  #CPR <- S[2:(days+1)]
  #DF <- cbind(df[1:days,],CPR)
  #results <- list(S, W, DF)
  #print(tail(S,1))
  
  return(S)
}




DAYS= dim(x)[1]
x_ <- c(2,seq(10, 500, 10))

h1 <- c() #how often /frequency
h2 <- c() #how far back
z <- c()

for(i in x_){
  for(j in x_){
    if(i>=j){
      h1 <- c(h1,i)
      h2 <- c(h2,j)
      z <- c(z, tail(my.crp(df,reb_freq=i,how_far_back=j),1))
      
    }
  }
  print(i)
}

data <- data.frame(x1=h1, x2=h2, z=z)

#https://plotly.com/python/builtin-colorscales/
plot_ly(data, x = ~x1, y = ~x2, z = ~z,
        marker = list(color = ~z, showscale = TRUE,colorscale='Plasma')) %>%
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'Frequency to rebalance'),
                      yaxis = list(title = 'Time Window'),
                      zaxis = list(title = 'Portfolio Value')),
         annotations = list(x=1,y=1,
                            text='Portfolio Value',
                            xref='paper',
                            yref='paper',
                            showarrow=FALSE))



set.seed(1)
DAYS= dim(x)[1]
df = x[,c('iroqu','kinar')]
df1 <- x[,sample(colnames(x),2)]
df2 <- x[,sample(colnames(x),2)]
df3 <- x[,sample(colnames(x),2)]


x_ <- c(2,seq(10, 80, 10))
y <- c()
y1 <- c()
y2 <- c()
y3 <- c()

for(i in x_){
  y <- c(y, tail(crp.ATWB(df,reb_freq=i),1))
  y1 <- c(y1, tail(crp.ATWB(df1,reb_freq=i),1))
  y2 <- c(y2, tail(crp.ATWB(df2,reb_freq=i),1))
  y3 <- c(y3, tail(crp.ATWB(df3,reb_freq=i),1))
}

data.ATWB <- data.frame(x=x_, y=y)
ggplot(data.ATWB, aes(x=x_)) +
  geom_line(aes(y = y,colour='Iroqu & Kinar')) +
  geom_line(aes(y = y1,colour='Arco & Ahp')) +
  geom_line(aes(y = y2,colour='Sears & Kinar')) +
  geom_line(aes(y = y3,colour='Gm & Ibm')) +
  scale_color_manual(name = "Sample Portfolios", values = c("Iroqu & Kinar" = "darkred",
                                                            "Arco & Ahp" = "steelblue",
                                                            'Sears & Kinar' ='blue',
                                                            'Gm & Ibm'='darkblue')) +
  
  ggtitle("Sample of Portfolios rebalancing on different Dates") +
  labs(y = "Portfolio's Value", x = "Rebalancing frequency")

