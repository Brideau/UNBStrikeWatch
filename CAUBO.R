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
uni.salary = list()
for (set in data.sets) {
  uni.income.file <- paste(data.folder, set, "/t_D_Rep_31.csv", sep="")
  uni.income[[set]] <- read.csv(uni.income.file, header=TRUE, stringsAsFactors=FALSE)
  # Discard unneeded columns
  uni.income[[set]] <- subset(uni.income[[set]], select=c("INSTNAME", "Lin_TIT", "C01", "C01pct"))
  names(uni.income[[set]]) <- c("Institution", "Line", "Total", "TotalPercent")
  uni.income[[set]] <- subset(uni.income[[set]], grepl("Provincial|Credit course tuition|Non-credit tuition|Other fees", Line))
  uni.income[[set]]$Year = set
  
  uni.salary.file <- paste(data.folder, set, "/t_D_Rep_32.csv", sep="")
  uni.salary[[set]] <- read.csv(uni.salary.file, header=TRUE, stringsAsFactors=FALSE)
  uni.salary[[set]] <- subset(uni.salary[[set]], select=c("INSTNAME", "Lin_TIT", "C01", "C01pct"))
  names(uni.salary[[set]]) <- c("Institution", "Line", "Total", "TotalPercent")
  uni.salary[[set]] <- subset(uni.salary[[set]], grepl("Academic ranks|Other instruction and research|Other salaries and wages|Benefits|land and land improvements", Line))
  uni.salary[[set]]$Year = set
}
remove(set, uni.income.file, uni.salary.file)

uni.income.df <- do.call("rbind", uni.income)
uni.salary.df <- do.call("rbind", uni.salary)




unb.government.real <- realdollars(years, unb.government)
qplot(data.sets, unb.government.real, ylim=c(min(unb.government.real),max(unb.government.real)), xlab="Year", ylab="Provincial Funding (000's)", geom="point")

unb.tuition.real <- realdollars(years, unb.tuition)
qplot(data.sets, unb.tuition.real, ylim=c(min(unb.tuition.real),max(unb.tuition.real)), xlab="Year", ylab="Total Tuition (000's)", geom="point")
