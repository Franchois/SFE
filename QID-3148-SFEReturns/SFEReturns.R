#------------------------------------------------------------------------------#
#                                  SFEReturns                                  #
#------------------------------------------------------------------------------#
# This scipt calculates the first order auto correlation of returns, squared 
# returns and absolute returns and skewness, kurtosis and the Bera Jarque 
# statistic for German and British stocks, 2002 - 2022"


#---------------------------------------------#
# Setting the environment and collecting data #
#---------------------------------------------#
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("fBasics", "tseries", "quantmod")
lapply(libraries, function(x) if (!(x %in% installed.packages())) 
  {install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# stocks symbols and full names
GER <- c("^GDAXI", "ADS.DE", "ALV.DE", "BAS.DE", "BAYN.DE", "BEI.DE", 
         "BMW.DE", "CON.DE", "DTG.DE", "DBK.DE", "DPW.DE", "DTE.DE", "EOAN.DE",
         "FRE.DE", "HEN.DE", "LIND.DE", "MUV2.DE", "RWE.DE", "SAP.DE", "SIE.DE",
         "VOW.DE")
GER_full_names <- c("DAX", "Adidas", "Allianz", "BASF", "Bayer", "Beiersdorf", 
                    "BMW", "Continental", "Daimler", "Deutsche Bank", 
                    "Deutsche Post", "Deutsche Telekom", "E.ON", "Fresnel", 
                    "Henkel", "Linde", "Muchener Ruck", "RWE", "SAP", "SIEMENS",
                    "Volkswagen")

GB <- c("^FTSE", "AZN.L", "BARC.L", "BHP.L", "BP.L", "BATS.L", "BT-A.L", 
        "DGE.L", "GSK.L", "HSBA.L", "LLOY.L", "NG.L", "PRU.L", "RKT.L", "RIO.L",
        "SHEL.L", "STAN.L", "ULVR.L", "VOD.L")
GB_full_names <- c("FTSE 100", "Astrazeneca", "Barclays", "BHP Group Limited", 
                   "BP", "British American Tabacco", "BT Group", "Diageo", 
                   "GSK", "HSBC", "Lloyds Banking Group", "National Grid", 
                   "Prudential", "Reckitt Benckiser Group", "Rio Tinto Group", 
                   "Shell", "Standard Chatered", "Unilever", "Vodafone Group")

all_symbols <- c(GER, GB)

# getting data from yahoo 
getSymbols(Symbols = all_symbols, src = "yahoo", from = "2002-01-01", 
           to = "2021-12-31")

# preparing data
GB[1] <- "FTSE"
GER[1] <- "GDAXI"
all_symbols <- c(GER, GB)

# remove any missing value at the head or the tail of the time series
# and any missing value in between is set to the value of the previous day
for(symbol in all_symbols){
  if(any(is.na(get(symbol)))){
    assign(symbol, 
           value = na.omit(get(symbol), method = "ir", interp = "before"))
  }
}


#------------------------------------#
# Calculating the values of interest #
#------------------------------------#

# Computing the daily (log-)returns of each symbol
for(symbol in all_symbols){
  ret <- dailyReturn(get(symbol), type = "log")
  assign(x = symbol, value = cbind(get(symbol), ret))
}

# set the result data frame
result <- data.frame(rho_ret = double(), rho_ret2 = double(),
                     rho_abs_ret = double(), S = double(), K = double(),
                     JB = double(), JB_pval = double())

for(symbol in all_symbols){
  #get the time serie of daily return from the symbol
  ret <- get(x = symbol)$daily.returns
  
  #row of result
  r <- c()
  
  # 1st order correlation of returns 
  r <- c(cor(ret[-1], ret[-length(ret)]))
  
  # 1st order correlation of squared returns
  r <- c(r, cor(ret[-1]^2, ret[-length(ret)]^2))
  
  # 1st order correlation of absolute returns
  r <- c(r, cor(abs(ret[-1]), abs(ret[-length(ret)])))
  
  # Skewness and kurtosis
  r <- c(r, skewness(ret), kurtosis(ret))
  
  # Bera-Jarque test statistic and its p-value
  r <- c(r, jarque.bera.test(ret)[[1]], jarque.bera.test(ret)[[3]])
  
  result <- rbind(result, r, deparse.level = 1)
}

row.names(result) <- c(GER_full_names, GB_full_names)
colnames(result) <- c("rho(ret)", "rho(ret^2)", "rho(|ret|)", "S", "K", "JB",
                      "JB p-val")

#--------------------#
# Exporting the data #
#--------------------#
# for the german stocks
write.csv(result[GER_full_names,], file = "ger.csv")
# for the british stocks
write.csv(result[GB_full_names,], file = "gb.csv")
