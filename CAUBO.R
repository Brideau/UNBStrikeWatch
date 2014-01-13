library(ggplot2)

data.folder <- "Data/CAUBO/"
data.sets <- c("19992000", "20002001", "20012002", "20022003", "20032004", "20042005", "20052006", "20062007", "20072008", "20082009", "20092010", "20102011", "20112012")
years <- as.numeric(substr(data.sets, 1, 4))

realdollars <- function(years, amounts) {
  cpiinflation <- read.csv("Data/CPI.csv")
  converted <- c()
  counter <- 1
  for (year in years) {
    cpi <- subset(cpiinflation, Ref_Date==year)$Value
    cpi2002 <- 100
    realvalue <- amounts[counter]*cpi2002/cpi
    counter = counter + 1
    converted <- c(converted, realvalue)
  }
  return(converted)
}

uni.income = list()
uni.income.gov = list()
uni.salary = list()
unb.government <- c()
unb.tuition <- c()
for (set in data.sets) {
  uni.income.file <- paste(data.folder, set, "/t_D_Rep_31.csv", sep="")
  uni.income[[set]] <- read.csv(uni.income.file, header=TRUE, stringsAsFactors=FALSE)
  # Discard unneeded columns
  uni.income[[set]] <- subset(uni.income[[set]], select=c("INSTNAME", "Lin_TIT", "C01", "C01pct"))
  names(uni.income[[set]]) <- c("Institution", "Line", "Total", "TotalPercent")
  uni.income[[set]] <- uni.income[[set]][grepl("Provincial|Credit course tuition|Non-credit tuition|Other fees", uni.income[[set]]$Line),]
  
  uni.income.gov[[set]] <- uni.income[[set]][grepl("Provincial", uni.income[[set]]$Line),]$Total
  
  # Focus on UNB
  yearly.amount <- uni.income[[set]][grepl("New Brunswick", uni.income[[set]]$Institution),]
  
  # Grab the government funding amount
  yearly.amount.gov <- yearly.amount[grepl("Provincial", yearly.amount$Line),]$Total # Total
  unb.government <- c(unb.government, c(yearly.amount.gov))
  
  # Grab the tuition amount
  yearly.amount.tuit <- yearly.amount[grepl("Credit course", yearly.amount$Line),]$Total + yearly.amount[grepl("Non-credit", yearly.amount$Line),]$Total + yearly.amount[grepl("Other fees", yearly.amount$Line),]$Total
  unb.tuition <- c(unb.tuition, c(yearly.amount.tuit))
  
  uni.salary.set <- paste(data.folder, set, "/t_D_Rep_32.csv", sep="")
  uni.salary[[set]] <- read.csv(uni.salary.set, header=TRUE, stringsAsFactors=FALSE)
  uni.salary[[set]] <- uni.salary[[set]][c("INSTNAME", "Lin_TIT", "C01", "C01pct")]
  uni.salary[[set]] <- uni.salary[[set]][c("INSTNAME", "Lin_TIT", "C01", "C01pct")]
  names(uni.salary[[set]]) <- c("Institution", "Line", "Total", "TotalPercent")
  uni.salary[[set]] <- uni.salary[[set]][grepl("Academic ranks|Other instruction and research|Other salaries and wages|Benefits|land and land improvements", uni.salary[[set]]$Line),]
}
remove(uni.income.file)

unb.government.real <- realdollars(years, unb.government)
qplot(data.sets, unb.government.real, ylim=c(min(unb.government.real),max(unb.government.real)), xlab="Year", ylab="Provincial Funding (000's)", geom="point")

unb.tuition.real <- realdollars(years, unb.tuition)
qplot(data.sets, unb.tuition.real, ylim=c(min(unb.tuition.real),max(unb.tuition.real)), xlab="Year", ylab="Total Tuition (000's)", geom="point")
