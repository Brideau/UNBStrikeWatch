library(ggplot2)
library(reshape2)
library(plyr)

data.folder <- "Data/CAUBO/"
data.sets <- c("19992000", "20002001", "20012002", "20022003", "20032004", "20042005", "20052006", "20062007", "20072008", "20082009", "20092010", "20102011", "20112012")
years <- as.numeric(substr(data.sets, 1, 4))

# Converts a vector of dollars to 2002 dollars
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

# Used to calculate the growth rate given a vector
growthcalc <- function(amounts) {
  growth <- c()
  i <- 1
  for (amount in amounts) {
    if (i == 1) {
      growth <- c(growth, NA)
      i <- i + 1
    } else {
      difference <- (amounts[i] - amounts[i-1])/amounts[i-1]
      growth <- c(growth, difference)
      i <- i + 1
    }
  }
  return(growth)
} 

# All the data is in a crazy format separated in a bunch of files. This brings it all together.
# I discovered the melt function late, so things get messy later as I try to combine things. 
counter <- 1
uni.income <- list()
uni.salary <- list()
uni.ftes <- list()
for (set in data.sets) {
  uni.income.file <- paste(data.folder, set, "/t_D_Rep_31.csv", sep="")
  uni.income[[set]] <- read.csv(uni.income.file, header=TRUE, stringsAsFactors=FALSE)
  # Discard unneeded columns
  uni.income[[set]] <- subset(uni.income[[set]], select=c("INSTNAME", "Lin_TIT", "C01", "C01pct"))
  names(uni.income[[set]]) <- c("Institution", "Line", "Total", "TotalPercent")
  uni.income[[set]] <- subset(uni.income[[set]], grepl("Provincial|Credit course tuition|Non-credit tuition|Other fees", Line))
  # Adds year column
  uni.income[[set]]$Year = years[counter]
  # Removes the numbers at the beginning
  ###### Move this just below names() above
  uni.income[[set]]$Line = gsub(" |-", "", sapply(strsplit(uni.income[[set]]$Line, "  "), '[',2))
  
  uni.salary.file <- paste(data.folder, set, "/t_D_Rep_32.csv", sep="")
  uni.salary[[set]] <- read.csv(uni.salary.file, header=TRUE, stringsAsFactors=FALSE)
  uni.salary[[set]] <- subset(uni.salary[[set]], select=c("INSTNAME", "Lin_TIT", "C01", "C01pct"))
  names(uni.salary[[set]]) <- c("Institution", "Line", "Total", "TotalPercent")
  uni.salary[[set]] <- subset(uni.salary[[set]], grepl("Academic ranks|Other instruction and research|Other salaries and wages|Benefits|land and land improvements", Line))
  uni.salary[[set]]$Year = years[counter]
  uni.salary[[set]]$Line = gsub(" |-", "", sapply(strsplit(uni.salary[[set]]$Line, "  "), '[',2))
  
  uni.ftes.file <- paste(data.folder, set, "/t_D_Rep_0.csv", sep="")
  uni.ftes[[set]] <- read.csv(uni.ftes.file, header=TRUE, stringsAsFactors=FALSE)
  uni.ftes[[set]] <- subset(uni.ftes[[set]], select=c("INSTNAME", "LANG", "FTE", "PROV_LANG"))
  names(uni.ftes[[set]]) <- c("Institution", "Language", "FTEs", "Province")
  uni.ftes[[set]]$Year = years[counter]
  
  counter <- counter + 1
}
# Memory management
remove(set, uni.income.file, uni.salary.file, uni.ftes.file)

uni.income.df <- do.call("rbind", uni.income)
uni.salary.df <- do.call("rbind", uni.salary)

# Get FTE data
uni.ftes.df <- do.call("rbind", uni.ftes)
# English speaking schools
uni.ftes.df <- subset(uni.ftes.df, Language == "e")
uni.ftes.df$Language <- NULL
uni.ftes.df$Province <- NULL


########## I didn't end up using this section for anything, but it might be useful later.

# Look at all schools between 5000 and 15000
uni.similar.unb <- subset(uni.ftes.df, FTEs > 6000 & FTEs < 14000)

# Get the list of universities similar to UNB
uni.similar.unb.names <- uni.similar.unb
uni.similar.unb.names$Institution <- as.factor(uni.similar.unb.names$Institution)
uni.similar.unb.names <- levels(uni.similar.unb.names$Institution)

# Remove schools with partial data
for (school in uni.similar.unb.names) {
  FTE.vec <- subset(uni.ftes.df, Institution == school)
  if (length(FTE.vec$FTEs) < 13) {
    uni.similar.unb <- subset(uni.similar.unb, Institution != school)
    uni.similar.unb.names <- uni.similar.unb.names[!uni.similar.unb.names == school]
  }
}

#####################################################################

# Get provincial funding data
unb.prov.fund <- subset(uni.income.df, Institution == "University of New Brunswick" & Line == "Provincial")
# Add new data
new.row <- data.frame(Institution = "University of New Brunswick", Line = "Provincial", Total = 124000, TotalPercent = NA, Year = 2012)
unb.prov.fund <- rbind(unb.prov.fund, new.row)
# Add real dollars column
unb.prov.fund$Total.real <- realdollars(unb.prov.fund$Year, unb.prov.fund$Total)
unb.prov.fund$Growth.nom <- growthcalc(unb.prov.fund$Total)
unb.prov.fund$Growth.real <- growthcalc(unb.prov.fund$Total.real)

# Get tuition data
unb.tuition <- subset(uni.income.df, Institution == "University of New Brunswick")
unb.tuition <- subset(unb.tuition, Line == "Creditcoursetuition" | Line == "Noncredittuition" | Line == "Otherfees")
for (year in years) {
  tuition.year <- sum(subset(unb.tuition, Year == year)$Total)
  percent.year <- sum(subset(unb.tuition, Year == year)$TotalPercent)
  unb.tuition[unb.tuition$Year == year & unb.tuition$Line == "Creditcoursetuition",]$Total <- tuition.year
  unb.tuition[unb.tuition$Year == year & unb.tuition$Line == "Creditcoursetuition",]$TotalPercent <- percent.year
}
unb.tuition <- subset(unb.tuition, Line == "Creditcoursetuition")
unb.tuition$Line <- NULL
unb.tuition$Institution <- NULL

# Set chart theme
theme <- theme(plot.title=element_text(size=rel(2)), axis.title.x=element_text(size=rel(1.5)),  axis.title.y=element_text(size=rel(1.5)), axis.text.x=element_text(size=rel(1.5)), axis.text.y=element_text(size=rel(1.5)) )

# UNB Funding, nominal dollars
ggplot(unb.prov.fund, aes(x=Year, y=Total/1000)) + geom_line(color="#d9381e") + theme +
  ggtitle("UNB Provincial Operating Grant Over Time, Nominal Dollars") +
  geom_point() + scale_x_continuous(breaks=seq(1999, 2012, 1)) +
  scale_y_continuous(name="Provincial Government Operating Grant (Millions)", labels = dollar, limits=c(74,130)) +
  geom_text(aes(label=as.character(round(Total/1000, 1) )), size=4, vjust=-1.5, hjust=1)

# UNB Funding, real dollars
ggplot(unb.prov.fund, aes(x=Year, y=Total.real/1000)) + geom_line(color="#d9381e") + theme +
  ggtitle("UNB Provincial Operating Grant Over Time, Real Dollars") +
  geom_point() + scale_x_continuous(breaks=seq(1999, 2012, 1)) +
  scale_y_continuous(name="Provincial Government Operating Grant (Millions)", labels = dollar, limits=c(79, 106)) +
  geom_text(aes(label=as.character(round(Total.real/1000, 1) )), size=4, vjust=-1.5, hjust=1)

# UNB funding, growth nominal
ggplot(unb.prov.fund, aes(x=Year, y=Growth.nom)) + geom_bar(stat="identity", fill="#d9381e") + theme +
  ggtitle("UNB Provincial Operating Grant Growth, Nominal Dollars") +
  scale_x_continuous(breaks=seq(1999, 2012, 1)) +
  scale_y_continuous(name="Provincial Government Operating Grant Growth Rate", labels = percent, limits=c(0, 0.09)) +
  geom_text(aes(label=as.character(round(Growth.nom*100, 1) )), size=4, vjust=-1)

# UNB funding, growth real
ggplot(unb.prov.fund, aes(x = Year, y=Growth.real)) + geom_bar(stat="identity", fill="#d9381e") + theme +
  ggtitle("UNB Provincial Operating Grant Growth, Real Dollars") +
  scale_x_continuous(breaks=seq(1999, 2012, 1)) +
  scale_y_continuous(name="Provincial Government Operating Grant Growth Percentage", labels = percent)  +
  geom_text(aes(label=as.character(round(Growth.real*100, 1) )), size=4, vjust=-1)

# Combine data frames for tuition and gov funding
fund.percent <- data.frame(Year = unb.prov.fund$Year, GovPercent = unb.prov.fund$TotalPercent/100, TuitionPercent = c(unb.tuition$TotalPercent/100, NA))

# UNB provincial government funding, percentage of total
ggplot(fund.percent, aes(Year)) + theme +
  geom_line(aes(y=GovPercent), colour="#d9381e") + geom_point(aes(y=GovPercent)) +
  geom_text(aes(y=GovPercent, label=round(100*GovPercent, 1)), vjust=-1) +
  geom_line(aes(y=TuitionPercent), colour="black") + geom_point(aes(y=TuitionPercent)) +
  geom_text(aes(y=TuitionPercent, label=round(100*TuitionPercent, 1)), vjust=-1) +
  ggtitle("Percent of UNB Operating Budget, Provincial Funding and Tuition") +
  scale_x_continuous(breaks=seq(1999, 2011, 1), limits=c(1999, 2011)) +
  scale_y_continuous(name="Percent of Total Budget", labels = percent, limits=c(0.30, 0.63)) +
  annotate("text", x=2005, y=0.55, label="Government Funding", size=6) +
  annotate("text", x=2004, y=0.37, label="Tuition and Fees", size=6)







