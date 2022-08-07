
options(warn=-1)
####Part1
require(data.table)
library(foreign) 
library(moments)
library(magrittr) 
library(roll)

#Part1
##################BONDS################### 

CRSP_Bonds <- as.data.table(read.dta("data1.dta"))

PS2Part1=function(CRSP_Bonds)
{
  CRSP_Bonds=CRSP_Bonds[,c('kycrspid','mcaldt','tmretnua','tmtotout')]
  CRSP_Bonds$tmtotout <- as.integer(CRSP_Bonds$tmtotout)  
  sapply(CRSP_Bonds,class)
  
  for (col in c('tmretnua','tmtotout')) CRSP_Bonds[is.na(get(col)), (col) := 0]
  
  CRSP_Bonds$Year=as.numeric(format(as.Date(CRSP_Bonds$mcaldt, format="%Y-%m-%d"),"%Y"))
  CRSP_Bonds$Month=as.numeric((format(as.Date(CRSP_Bonds$mcaldt, format="%Y-%m-%d"),"%m")))
  CRSP_Bonds$Year=as.integer(CRSP_Bonds$Year)
  CRSP_Bonds$Month=as.integer(CRSP_Bonds$Month)
  
   CRSP_Bonds[, Bond_lag_MV := shift(tmtotout), by=(kycrspid)]
   # CRSP_Bonds = CRSP_Bonds[!is.na(CRSP_Bonds$tmtotout)&!is.na(CRSP_Bonds$Bond_lag_MV),]
   
  for (col in c("Bond_lag_MV")) CRSP_Bonds[is.na(get(col)), (col) := 0]
  CRSP_Bonds$Bond_lag_MV <- as.numeric(CRSP_Bonds$Bond_lag_MV)
  
  CRSP_Bonds[, Wt := Bond_lag_MV/sum(Bond_lag_MV), by=list(Year,Month)]
  for (col in c("Wt")) CRSP_Bonds[is.na(get(col)), (col) := 0]
  
  CRSP_Bonds_output = CRSP_Bonds[, list(Bond_lag_MV=sum(Bond_lag_MV),
                                        Bond_Ew_Ret=mean(tmretnua),
                                        Bond_Vw_Ret = sum(Wt*tmretnua)), 
                                 by=list(Year,Month)]
  
  CRSP_Bonds_output$Bond_Ew_Ret=format(CRSP_Bonds_output$Bond_Ew_Ret, scientific = FALSE) 
  CRSP_Bonds_output$Bond_Ew_Ret <- as.numeric(CRSP_Bonds_output$Bond_Ew_Ret)
  
  CRSP_Bonds_output$Bond_Vw_Ret=format(CRSP_Bonds_output$Bond_Vw_Ret, scientific = FALSE) 
  CRSP_Bonds_output$Bond_Vw_Ret <- as.numeric(CRSP_Bonds_output$Bond_Vw_Ret)
  
  sapply(CRSP_Bonds_output,class)
  Monthly_CRSP_Bonds<<-tail(CRSP_Bonds_output,-1)
  return(Monthly_CRSP_Bonds)
}

##################BONDS################### 

##################STOCKS################### 
CRSP_Stocks <- as.data.table(read.dta("~/Desktop/MFE/UCLA/Quarter3/QAM/PSs/PS1data.dta"))

PS1Part1=function(CRSP_Stocks2)
{
CRSP_Stocks2 <- CRSP_Stocks[CRSP_Stocks$shrcd == 10|CRSP_Stocks$shrcd == 11,]
CRSP_Stocks3 <- CRSP_Stocks2[CRSP_Stocks2$exchcd==1|CRSP_Stocks2$exchcd==2|
                               CRSP_Stocks2$exchcd==3|CRSP_Stocks2$exchcd==4,]
# CRSP_Stocks2$exchcd==31|CRSP_Stocks2$exchcd==32|
# CRSP_Stocks2$exchcd==33,]
for (col in c("shrout","ret","dlret","prc")) CRSP_Stocks3[is.na(get(col)), (col) := 0]

CRSP_Stocks3$Year=as.numeric(format(as.Date(CRSP_Stocks3$date, format="%Y-%m-%d"),"%Y"))
CRSP_Stocks3$Month=as.numeric((format(as.Date(CRSP_Stocks3$date, format="%Y-%m-%d"),"%m")))
CRSP_Stocks3$prc = abs(CRSP_Stocks3$prc)

CRSP_Stocks3$Tot_Ret = ((1+CRSP_Stocks3$ret)*(1+CRSP_Stocks3$dlret))-1
CRSP_Stocks3$Stock_curr_MV=CRSP_Stocks3$prc*CRSP_Stocks3$shrout

# sapply(CRSP_Stocks3, class) #checking data types of columns
CRSP_Stocks3[, Stock_lag_MV := shift(Stock_curr_MV/1000), by=permno]
for (col in c("Stock_lag_MV")) CRSP_Stocks3[is.na(get(col)), (col) := 0]
CRSP_Stocks3[, MeWt := Stock_lag_MV/sum(Stock_lag_MV), by=list(Year,Month)]
# CRSP_Stocks3[, VW := sum(MeWt*Tot_Ret), by=list(Year,Month)]

CRSP_Stocks_output = CRSP_Stocks3[, list(Stock_lag_MV=sum(Stock_lag_MV),Stock_Ew_Ret=mean(Tot_Ret),Stock_Vw_Ret = sum(Tot_Ret*MeWt)), by=list(Year,Month)]
CRSP_Stocks_output=CRSP_Stocks_output[order(CRSP_Stocks_output$Year),]
# CRSP_Stocks_output=CRSP_Stocks_output[-1]
Monthly_CRSP_Stocks<<-CRSP_Stocks_output[-1]
return(Monthly_CRSP_Stocks)
}
PS1Part1(CRSP_Stocks)
##################STOCKS################### 

Monthly_CRSP_Riskless <- as.data.table(read.dta("~/Desktop/MFE/UCLA/Quarter3/QAM/PSs/PS2_data2.dta"))

####Part2
PS2Part2=function(Monthly_CRSP_Bonds,Monthly_CRSP_Stocks,Monthly_CRSP_Riskless)
{

Monthly_CRSP_Riskless$Year=as.numeric(format(as.Date(Monthly_CRSP_Riskless$caldt, format="%Y-%m-%d"),"%Y"))
Monthly_CRSP_Riskless$Month=as.numeric((format(as.Date(Monthly_CRSP_Riskless$caldt, format="%Y-%m-%d"),"%m")))
Monthly_CRSP_Riskless$Year=as.integer(Monthly_CRSP_Riskless$Year)
Monthly_CRSP_Riskless$Month=as.integer(Monthly_CRSP_Riskless$Month)

Monthly_CRSP_Bonds= Monthly_CRSP_Bonds[key="Year,Month"]
Monthly_CRSP_Stocks= Monthly_CRSP_Stocks[key="Year,Month"]

Monthly_CRSP_Universe1=merge(Monthly_CRSP_Stocks, Monthly_CRSP_Bonds,all = TRUE)

Monthly_CRSP_Universe1=Monthly_CRSP_Universe1[,c("Year","Month","Stock_lag_MV",
                                               "Stock_Vw_Ret","Bond_lag_MV",
                                               "Bond_Vw_Ret" )]
Monthly_CRSP_Universe1$Stock_lag_MV=Monthly_CRSP_Universe1$Stock_lag_MV/1000
Monthly_CRSP_Universe1$Stock_Excess_Vw_Ret=Monthly_CRSP_Universe1$Stock_Vw_Ret-
  Monthly_CRSP_Riskless$t30ret
Monthly_CRSP_Universe1$Bond_Excess_Vw_Ret=Monthly_CRSP_Universe1$Bond_Vw_Ret-
  Monthly_CRSP_Riskless$t30ret 

Monthly_CRSP_Universe<<-Monthly_CRSP_Universe1[,c("Year","Month","Stock_lag_MV",
                                               "Stock_Excess_Vw_Ret","Bond_lag_MV",
                                               "Bond_Excess_Vw_Ret")]
return(Monthly_CRSP_Universe)
}
  
####Part3
PS2Part3=function(Monthly_CRSP_Universe)
{
Port_Rets1=data.table(Year= Monthly_CRSP_Universe$Year,
                     Month=Monthly_CRSP_Universe$Month,
                     Stock_Excess_Vw_Ret=Monthly_CRSP_Universe$Stock_Excess_Vw_Ret,
                     Bond_Excess_Vw_Ret=Monthly_CRSP_Universe$Bond_Excess_Vw_Ret,
                     Excess_60_40_Ret=(0.6*Monthly_CRSP_Universe$Stock_Excess_Vw_Ret)+
                       (.4*Monthly_CRSP_Universe$Bond_Excess_Vw_Ret))
Monthly_CRSP_Universe$Stock_lag_MV=Monthly_CRSP_Universe$Stock_lag_MV*1000


Port_Rets1[,c("s1_wt_stock","s1_wt_bond"):=list(Monthly_CRSP_Universe$Stock_lag_MV/(Monthly_CRSP_Universe$Stock_lag_MV+Monthly_CRSP_Universe$Bond_lag_MV),Monthly_CRSP_Universe$Bond_lag_MV/(Monthly_CRSP_Universe$Stock_lag_MV+Monthly_CRSP_Universe$Bond_lag_MV))]

Port_Rets1[,Excess_Vw_Ret:=s1_wt_stock*Stock_Excess_Vw_Ret+s1_wt_bond*Bond_Excess_Vw_Ret]

# Unlevered
#36 months rolling window for sigma inverse
unl = rep(0,36)
unl2 = rep(0,nrow(Port_Rets1)-36)
for (i in 3:36) {unl[i] = sd(Port_Rets1$Stock_Excess_Vw_Ret[1:i-1])}
for (i in 37:nrow(Port_Rets1)) {unl2[i-36] = sd(Port_Rets1$Stock_Excess_Vw_Ret[(i-36):(i-1)])}
temp = 1/c(unl,unl2)
Port_Rets1[,Stock_inverse_sigma_hat:=temp]

unl = rep(0,36)
unl2 = rep(0,nrow(Port_Rets1)-36)
for (i in 3:36) {unl[i] = sd(Port_Rets1$Bond_Excess_Vw_Ret[1:i-1])}
for (i in 37:nrow(Port_Rets1)) {unl2[i-36] = sd(Port_Rets1$Bond_Excess_Vw_Ret[(i-36):(i-1)])}
temp = 1/c(unl,unl2)
Port_Rets1[,Bond_inverse_sigma_hat:=temp]

Port_Rets1[,Unlevered_k:=1/(Stock_inverse_sigma_hat+Bond_inverse_sigma_hat)]
Port_Rets1[,Stock_wt:=Unlevered_k*Stock_inverse_sigma_hat]
Port_Rets1[,Bond_wt:=Unlevered_k*Bond_inverse_sigma_hat]

Port_Rets1[,Excess_Unlevered_RP_Ret:=(Stock_Excess_Vw_Ret*Stock_wt)+
             (Bond_Excess_Vw_Ret*Bond_wt)]

#Levered

Vw_Ret_var = var(Port_Rets1$Excess_Vw_Ret)
Levered_k=sd(Port_Rets1$Excess_Vw_Ret,na.rm = TRUE)/sd((Port_Rets1$Stock_inverse_sigma_hat*Port_Rets1$Stock_Excess_Vw_Ret)+(Port_Rets1$Bond_inverse_sigma_hat*Port_Rets1$Bond_Excess_Vw_Ret),na.rm = TRUE)

Port_Rets1[,Levered_k:=Levered_k]
Port_Rets1[,":="(wt_stock_lev=Levered_k*Stock_inverse_sigma_hat,wt_bond_lev=Levered_k*Bond_inverse_sigma_hat)]
Port_Rets1[,wt_rf:=1-(wt_stock_lev+wt_bond_lev)]
Port_Rets1[,Excess_Levered_RP_Ret:=wt_stock_lev*Stock_Excess_Vw_Ret
                      +wt_bond_lev*Bond_Excess_Vw_Ret
                      +wt_rf*0]


is.na(Port_Rets1)<-sapply(Port_Rets1, is.infinite)
Port_Rets1[is.na(Port_Rets1)]<-0
Port_Rets=Port_Rets1
Port_Rets1 <<- Port_Rets1[which(Year==1930&Month==1):which(Year==2010&Month==6)]

s <- Monthly_CRSP_Universe[c(37:1014),]

Port_Rets2= data.table(Year= s$Year,
                       Month=s$Month,
                       Stock_Excess_Vw_Ret=s$Stock_Excess_Vw_Ret,
                       Bond_Excess_Vw_Ret=s$Bond_Excess_Vw_Ret,
                       Excess_60_40_Ret=(0.6*s$Stock_Excess_Vw_Ret)+
                         (.4*s$Bond_Excess_Vw_Ret))

Port_Rets2[,c("s1_wt_stock","s1_wt_bond"):=list(s$Stock_lag_MV/(s$Stock_lag_MV+s$Bond_lag_MV),s$Bond_lag_MV/(s$Stock_lag_MV+s$Bond_lag_MV))]
Port_Rets2[,Excess_Vw_Ret:=s1_wt_stock*Stock_Excess_Vw_Ret+s1_wt_bond*Bond_Excess_Vw_Ret]

Port_Rets2<<-Port_Rets2
Port_Rets<<-Port_Rets[,c("Year","Month","Stock_Excess_Vw_Ret","Bond_Excess_Vw_Ret","Excess_Vw_Ret" ,"Excess_60_40_Ret","Stock_inverse_sigma_hat" ,"Bond_inverse_sigma_hat","Unlevered_k","Excess_Unlevered_RP_Ret", "Levered_k","Excess_Levered_RP_Ret")]  

return(Port_Rets)
}


####Part4
PS2Part4=function(Port_Rets)
{

PPart4 <- data.frame(matrix(ncol = 6, nrow = 6))
column_names = c('Annualized Mean', 't-stat of Annualized Mean', 'Annualized Standard Deviation', 'Annualized Sharpe Ratio', 'Skewness',  'Excess Kurtosis')
row_names=c('CRSP stocks', 'CRSP bonds', 'Value-weighted portfolio', '60/40 portfolio', 'unlevered RP', 'levered RP')
colnames(PPart4) <- column_names
rownames(PPart4)=row_names
Port_Rets1 <<- Port_Rets1[which(Year==1930&Month==1):which(Year==2010&Month==6)]

PPart4['CRSP stocks','Annualized Mean']=mean(Port_Rets2$Stock_Excess_Vw_Ret)*12*100
PPart4['CRSP bonds','Annualized Mean']=mean(Port_Rets1$Bond_Excess_Vw_Ret)*12*100
PPart4['Value-weighted portfolio','Annualized Mean']=mean(Port_Rets2$Excess_Vw_Ret)*12*100
PPart4['60/40 portfolio','Annualized Mean']=mean(Port_Rets2$Excess_60_40_Ret)*12*100
PPart4['unlevered RP','Annualized Mean']=mean(Port_Rets1$Excess_Unlevered_RP_Ret)*12*100
PPart4['levered RP','Annualized Mean']=mean(Port_Rets1$Excess_Levered_RP_Ret)*12*100

PPart4['CRSP stocks','Annualized Standard Deviation']=sd(Port_Rets2$Stock_Excess_Vw_Ret)*sqrt(12)*100
PPart4['CRSP bonds','Annualized Standard Deviation']=sd(Port_Rets1$Bond_Excess_Vw_Ret)*sqrt(12)*100
PPart4['Value-weighted portfolio','Annualized Standard Deviation']=sd(Port_Rets2$Excess_Vw_Ret)*sqrt(12)*100
PPart4['60/40 portfolio','Annualized Standard Deviation']=sd(Port_Rets2$Excess_60_40_Ret)*sqrt(12)*100
PPart4['unlevered RP','Annualized Standard Deviation']=sd(Port_Rets1$Excess_Unlevered_RP_Ret)*sqrt(12)*100
PPart4['levered RP','Annualized Standard Deviation']=sd(Port_Rets1$Excess_Levered_RP_Ret)*sqrt(12)*100

PPart4['CRSP stocks','t-stat of Annualized Mean']=t.test(Port_Rets2$Stock_Excess_Vw_Ret)
PPart4['CRSP bonds','t-stat of Annualized Mean']=t.test(Port_Rets1$Bond_Excess_Vw_Ret)
PPart4['Value-weighted portfolio','t-stat of Annualized Mean']=t.test(Port_Rets2$Excess_Vw_Ret)
PPart4['60/40 portfolio','t-stat of Annualized Mean']=t.test(Port_Rets2$Excess_60_40_Ret)
PPart4['unlevered RP','t-stat of Annualized Mean']=t.test(Port_Rets1$Excess_Unlevered_RP_Ret)
PPart4['levered RP','t-stat of Annualized Mean']=t.test(Port_Rets1$Excess_Levered_RP_Ret)

PPart4['CRSP stocks','Annualized Sharpe Ratio']=PPart4['CRSP stocks','Annualized Mean']/PPart4['CRSP stocks','Annualized Standard Deviation']
PPart4['CRSP bonds','Annualized Sharpe Ratio']=PPart4['CRSP bonds','Annualized Mean']/PPart4['CRSP bonds','Annualized Standard Deviation']
PPart4['Value-weighted portfolio','Annualized Sharpe Ratio']=PPart4['Value-weighted portfolio','Annualized Mean']/PPart4['Value-weighted portfolio','Annualized Standard Deviation']
PPart4['60/40 portfolio','Annualized Sharpe Ratio']=PPart4['60/40 portfolio','Annualized Mean']/PPart4['60/40 portfolio','Annualized Standard Deviation']
PPart4['unlevered RP','Annualized Sharpe Ratio']=PPart4['unlevered RP','Annualized Mean']/PPart4['unlevered RP','Annualized Standard Deviation']
PPart4['levered RP','Annualized Sharpe Ratio']=PPart4['levered RP','Annualized Mean']/PPart4['levered RP','Annualized Standard Deviation']

PPart4['CRSP stocks','Skewness']=skewness(Port_Rets2$Stock_Excess_Vw_Ret)
PPart4['CRSP bonds','Skewness']=skewness(Port_Rets1$Bond_Excess_Vw_Ret)
PPart4['Value-weighted portfolio','Skewness']=skewness(Port_Rets2$Excess_Vw_Ret)
PPart4['60/40 portfolio','Skewness']=skewness(Port_Rets2$Excess_60_40_Ret)
PPart4['unlevered RP','Skewness']=skewness(Port_Rets1$Excess_Unlevered_RP_Ret)
PPart4['levered RP','Skewness']=skewness(Port_Rets1$Excess_Levered_RP_Ret)

PPart4['CRSP stocks','Excess Kurtosis']=kurtosis(Port_Rets2$Stock_Excess_Vw_Ret)-3
PPart4['CRSP bonds','Excess Kurtosis']=kurtosis(Port_Rets1$Bond_Excess_Vw_Ret)-3
PPart4['Value-weighted portfolio','Excess Kurtosis']=kurtosis(Port_Rets2$Excess_Vw_Ret)-3
PPart4['60/40 portfolio','Excess Kurtosis']=kurtosis(Port_Rets2$Excess_60_40_Ret)-3
PPart4['unlevered RP','Excess Kurtosis']=kurtosis(Port_Rets1$Excess_Unlevered_RP_Ret)-3
PPart4['levered RP','Excess Kurtosis']=kurtosis(Port_Rets1$Excess_Levered_RP_Ret)-3

PPart4<<-PS2Part4
return(PS2Part4)
}

PS2Part1(CRSP_Bonds)
PS2Part2(Monthly_CRSP_Bonds,Monthly_CRSP_Stocks,Monthly_CRSP_Riskless)
PS2Part3(Monthly_CRSP_Universe)
PS2Part4(Port_Rets)
