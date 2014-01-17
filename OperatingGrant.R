library(ggplot2)
library(reshape2)
library(plyr)

data.folder <- "Data/CAUBO/"
years <- 1999:2011
data.sets <- c()
for (year in years) {
  data.sets <- c(data.sets, paste(year, year+1, sep=""))
}
comp.schools <- c("McMaster University", "University of Waterloo", "University of Windsor", "Queen's University", "University of Guelph", "Carleton University", "Simon Fraser University", "University of Regina", "Dalhousie University", "Memorial University of Newfoundland", "University of Manitoba", "Concordia University", "University of Victoria", "University of New Brunswick", "University of Saskatchewan")

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
  uni.income[[set]] <- subset(uni.income[[set]], select=c("INSTNAME", "Lin_TIT", "C01", "C01pct", "Grp_TIT"))
  names(uni.income[[set]]) <- c("Institution", "Line", "Total", "TotalPercent", "GroupTitle")
  # There are two lines named Individual. This gets rid of one so we can focus on the one we want.
  uni.income[[set]] <- subset(uni.income[[set]], !(grepl("Non-government grants and contracts", GroupTitle)) )
  uni.income[[set]]$GroupTitle <- NULL
  uni.income[[set]] <- subset(uni.income[[set]], grepl("Provincial|Credit course tuition|Non-credit tuition|Other fees|Individuals|Endowment|Other investment", Line))
  # Adds year column
  uni.income[[set]]$Year = years[counter]
  # Removes the numbers at the beginning
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
# remove(set, uni.income.file, uni.salary.file, uni.ftes.file)

# Get all the data into data frames
uni.income.df <- do.call("rbind", uni.income)
uni.salary.df <- do.call("rbind", uni.salary)
uni.ftes.df <- do.call("rbind", uni.ftes)

# Filter to focus on comparison schools
comparable.df <- uni.income.df
comparable.df <- subset(comparable.df, Institution %in% comp.schools)

# Totals how much the endowment, individual donations and other investment income contributes to the operating budget
for (year in years) {
  print(year)
  for (school in comp.schools) {
    IndEndOther.Total <- sum(subset(comparable.df, (Line == "Individuals" | Line == "Endowment" | Line == "Otherinvestment") & Year == year & Institution == school)$Total)
    IndEndOther.Percent <- sum(subset(comparable.df, (Line == "Individuals" | Line == "Endowment" | Line == "Otherinvestment") & Year == year & Institution == school)$TotalPercent)
    newRow <- data.frame(Institution=school, Line="IndEndOther", Total=IndEndOther.Total, TotalPercent=IndEndOther.Percent, Year=year)
    comparable.df <- rbind(comparable.df, newRow)
  }
}

# Add a column for storing FTEs. FTE data for 2009 is missing from CAUBO data.
comparable.df$FTEs <- NA
for (year in years) {
  for (school in comp.schools) {
    print(school)
    if (year == 2009) {
      comparable.df[comparable.df$Institution == school & comparable.df$Year == year,]$FTEs <- NA
    } else {
      num.ftes <- uni.ftes.df[uni.ftes.df$Institution == school & uni.ftes.df$Year == year,]$FTEs
      comparable.df[comparable.df$Institution == school & comparable.df$Year == year,]$FTEs <- num.ftes
    }
  }
}

comparable.df.investments <- subset(comparable.df, Line == "IndEndOther")
comparable.df.investments$InvPerStudent <- round((comparable.df.investments$Total*1000)/comparable.df.investments$FTEs, 1)

# English speaking schools
# uni.ftes.df <- subset(uni.ftes.df, Language == "e")
# uni.ftes.df$Language <- NULL
# uni.ftes.df$Province <- NULL



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


theme_line <- theme(plot.title=element_text(size=rel(1.7)), axis.title.x=element_text(size=rel(1.5)),  axis.title.y=element_text(size=rel(1.5)), axis.text.x=element_text(size=rel(1.5)), axis.text.y=element_text(size=rel(1.5)), legend.text=element_text(size=rel(1.1)) )



# Plot that shows the combined investment income/student over time.

# Adds a column for specifying whether it is UNB or not
comparable.df.investments$IsUNB <- NA
comparable.df.investments$IsUNB <- comparable.df.investments$Institution == "University of New Brunswick"

# 2009 data is missing
comparable.df.investments <- subset(comparable.df.investments, Year != 2009)

investment.per.student.plot <- ggplot(comparable.df.investments, aes(x=Year, y=InvPerStudent, colour=Institution, linetype=IsUNB)) + 
  ggtitle("Total Investment per Student Used for Operating Budget") +
  geom_line(size=1.1) + 
  geom_point(size=2.5) +
  scale_y_continuous(name="Total Investments/Student", labels=dollar) +
  scale_x_continuous(name="", breaks=seq(1999, 2010, 1)) +
  scale_color_discrete() +
  labs(linetype="Is it UNB?") +
  scale_linetype_discrete(labels=c("No", "Yes")) +
  theme_line + theme_bw()
investment.per.student.plot
ggsave(filename="Plots/Investments/InvestmentPerStudentTime.png", plot=investment.per.student.plot, width=12, height=8, dpi=100, units="in")

library(grid)
investment.student.2011 <- subset(comparable.df.investments, Year == 2011)
investment.student.2011.plot <- ggplot(investment.student.2011, aes(x=reorder(Institution,InvPerStudent), y=InvPerStudent, fill=TotalPercent)) + 
  geom_bar(stat="identity") +
  theme + theme_bw() +
  theme(axis.text.x = element_text(angle=65, hjust=1, vjust=1, colour="black", size=rel(1.5))) +
  scale_y_continuous(labels = dollar, name="Total Investments/Student") +
  scale_x_discrete(name="") +
  ggtitle("Total Investments per Student, Used for Operating Budget, 2011-2012") +
  labs(fill="% of Operating Budget") +
  geom_text(aes(label=prettyNum(InvPerStudent, big.mark=",", scientific=F)), size=4, vjust=-1) +
  annotate("text", x="University of New Brunswick", y=450, label="UNB", size=6) +
  annotate("segment", x=5, xend=5, y=400, yend=250, arrow=arrow(ends="last", angle=30, length=unit(0.3, "cm")))
investment.student.2011.plot
ggsave(filename="Plots/Investments/InvestmentPerStudent2011.png", plot=investment.student.2011.plot, width=12, height=11, dpi=100, units="in")











