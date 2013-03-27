# Adam Duncan, December 2012

# Before moving on to examine the duration of crises, I want to take a minute
# to run through some selected data and display the VaR break results. I also promised
# to clean up the summary function in a faster format. So, here goes.

library(quantmod,warn.conflicts=FALSE)
library(PerformanceAnalytics,warn.conflicts=FALSE)
library(xts,warn.conflicts=FALSE)
library(knitr,warn.conflicts=FALSE)

# NOTE: Please make sure you have loaded the function definition for varFun from part 1: VaR_analysis_script.R
# Let's go out and get the data we want to examine. This time we'll put each of the data elements
# in a list. We can pass this list of variables to our new summary functions.

tickers.equity=c("SP500","DJIA") # SP500 and DJIA
tickers.fx=c("DEXUSEU","DEXMXUS","DEXUSAL","DEXJPUS","DEXKOUS") # EURUSD, USDMXN, AUDUSD, USDJPY, and USDKRW
tickers.commod=c("DCOILWTICO","GOLDAMGBD228NLBM","DCOILBRENTEU") # WTI and Gold
tickers.rates=c("DSWP2","DSWP10","DSWP30")

getData<-function(tickers){
  for (i in 1:length(tickers)){
    getSymbols(tickers[i],src="FRED",auto.assign=TRUE)
  }
}

getData(tickers.equity)
getData(tickers.fx)
getData(tickers.commod)
getData(tickers.rates)

# For this application, I like to add each data set into its own variable and then
# combine them into lists as I need them.

data.equity<-list(SP500,DJIA)
data.fx<-list(DEXUSEU,1/DEXMXUS,DEXUSAL,DEXJPUS,1/DEXKOUS)
data.commod<-list(DCOILWTICO,GOLDAMGBD228NLBM,DCOILBRENTEU)
data.rates<-list(DSWP2,DSWP10,DSWP30)

# Now let's clean up that summary function so we can just get results if we want them and maybe do some plotting.
# This function will return a data frame containing summary information about the VaR breaks for 
# the variables in the list we pass in.
varFun<-function(x,p,window,method,inv.var,rtype){
  
  
  ret<-periodReturn(x,period="daily",subset=NULL,type="log")
  
  if (rtype=="vars"){
    ret.var<-rollapply(ret,width=window,
                       FUN=function(ret) VaR(R=ret,
                                             p=p,
                                             method="historical",
                                             clean="none",
                                             portfolio_method="single",
                                             weights=NULL,
                                             #mu=ret.mu,
                                             #sigma=ret.sig,
                                             #m3=ret.skew,#skewness of series
                                             #m4=ret.kurt,#kurtosis of series
                                             invert=inv.var),
                       align="right")
    ret.var<-as.xts(ret.var)
    names(ret.var)<-c("VaR")
    return(ret.var)
  } else {
    if (rtype=="returns"){
      ret<-as.xts(ret)
      names(ret)<-c("returns")
      return(ret)
    } else {
      if (rtype=="ratios"){
        ret.var<-rollapply(ret,width=window,
                           FUN=function(ret) VaR(R=ret,
                                                 p=p,
                                                 method="historical",
                                                 clean="none",
                                                 portfolio_method="single",
                                                 weights=NULL,
                                                 #mu=ret.mu,
                                                 #sigma=ret.sig,
                                                 #m3=ret.skew,#skewness of series
                                                 #m4=ret.kurt,#kurtosis of series
                                                 invert=inv.var),
                           align="right")
        first.date<-index(first(ret.var))
        ret.sub<-window(ret,from=first.date)
        act.var.ratio<-as.xts(ret.sub/ret.var)
        names(act.var.ratio)<-c("ratios")
        return(act.var.ratio)
      } else {
        if (rtype=="vol"){
          ret.sig<-rollapply(ret,width=window,FUN=function(ret) apply(X=ret,MARGIN=2, FUN=sd))
          names(ret.sig)<-c("vols")
          return(ret.sig)
        } else {
          if (rtype=="skew"){
            ret.skew<-as.xts(rollapply(ret,width=window,FUN=function(ret) skewness(ret,method="moment")))
            names(ret.skew)<-c("skew")
            return(ret.skew)
          } else {
            if (rtype=="kurt"){
              ret.kurt<-as.xts(rollapply(ret,width=window,FUN=function(ret) kurtosis(ret,method="moment")))
              names(ret.kurt)<-c("kurtosis")
              return(ret.kurt)
            } else {
              if (rtype=="mean"){
                ret.mu<-as.xts(rollapply(ret,width=window,FUN=mean))
                names(ret.mu)<-c("means")
                return(as.xts(ret.mu))
              } else {
                return(Null)
              }
            }
          }
        }
      }
    }
  }
} #varFun function from part 1...

vsum<-function(datalist,p,window,method,inv.var,draw){
  # now, tickers is a list. We'll cycle through the list and generate our summary output.
  # we can dispense with having to get the data on the fly.
  # draw is a boolean that tell the function whether to plot the ratio series.
  
  n<-length(datalist)
  out<-data.frame()
  
  for (i in 1:n){
    ratios<-varFun(datalist[[i]],p,window,method,inv.var,"ratios")
    ratios<-subset(ratios,subset=(ratios<0))
    out[i,1]<-names(datalist[[i]])
    out[i,2]<-min(ratios)
    out[i,3]<-mean(ratios)
    out[i,4]<-median(ratios)
    if (draw==TRUE){
      plot.xts(ratios,las=1,main=names(datalist[[i]]),ylab="act.ret/VaR est.")
      abline(h=-1.0,col="red") # a line to demark the ratios where actual return exactly equaled the VaR estimate.
    }
  }
  names(out)<-c("name","max.ratio","mean.ratio","median.ratio")
  return(out)
}

par(mfrow=(c(1,1)))
vb.summary<-vsum(data.fx,.95,1260,method="historical",inv.var=FALSE,draw=TRUE)
vb.summary

# The AUDUSD data looks a little suspicious with a -80x VaR realization. That probably merits some investigation.
# Our new summary function gives us the ability to produce a table of VaR breaks, some summary statistics about
# the breaks, and plot the breaks if we want. Let's run the whole lot so we can see the table of the breaks and
# the associated plots.
# Warning: Running them all will take a little while. Actually, let's time the code.

ptm<-proc.time()
par(mfrow=(c(2,2))) # We'll do 4 plots per page.
data.master<-list(SP500,DJIA,DEXUSEU,1/DEXMXUS,DEXUSAL,DEXJPUS,1/DEXKOUS,DCOILWTICO,GOLDAMGBD228NLBM,DCOILBRENTEU,
                  DSWP2,DSWP10,DSWP30)
vb.summary<-vsum(data.master,.95,1260,method="historical",inv.var=FALSE,draw=TRUE)
vb.summary
proc.time()-ptm

mean(vb.summary$max.ratio,trim=.10)

# So, throwing out the 10% best and worst cases for the VaR breaks, we get a trimmed mean of -12.5x VaR.
# From a visual inspection of the data, it would appear that for non-equity asset classes, somewhere between
# 5-9x daily VaR seems to capture the worst case scenario for 1 day negative excessions. Equities and high beta
# FX pairs would fall somewhere in the 12-18x VaR range. That's all for now. In the next part, we'll examine
# the duration aspect of crises. How long does it take to get to the bottom of an average crisis?
# How large are the cumulative drawdowns on average and how long does it take to get back to flat on average?

  