library(ggplot2)
library(scales)

data.file <- "Data/PresidentSalaries3.csv"
uni.ftes.file <- "Data/CAUBO/20112012/t_D_Rep_0.csv"

president.salaries <- read.csv(data.file, header=TRUE, stringsAsFactors=FALSE)
uni.ftes <- read.csv(uni.ftes.file, header=TRUE, stringsAsFactors=FALSE)

realdollars <- function(yearsfrom, amounts, yearto=2002) {
  cpiinflation <- read.csv("Data/CPI.csv")
  converted <- c()
  counter <- 1
  for (year in yearsfrom) {
    cpi <- subset(cpiinflation, Ref_Date==year)$Value
    cpi2002 <- subset(cpiinflation, Ref_Date==yearto)$Value
    realvalue <- amounts[counter]*cpi2002/cpi
    counter = counter + 1
    converted <- c(converted, realvalue)
  }
  return(converted)
}

# Fills in FTEs - needed initially when cleaning the data
# president.salaries$FTEs <- NA
# for (university in as.character(president.salaries$University)) {
#   if (length(uni.ftes[uni.ftes$INSTNAME == university,]$FTE) > 0 ) {
#     president.salaries[president.salaries$University == university,]$FTEs <- uni.ftes[uni.ftes$INSTNAME == university,]$FTE
#   } else {
#     president.salaries[president.salaries$University == university,]$FTEs == NA
#   }
# }
# 
# # Write out what was updated so it can be augmented manually
# write.csv(president.salaries, "presidentsalariesupdated.csv", row.names=FALSE)
# 

data.folder <- "Data/UCASS/"
salary.file.name <- "FacultySalaries"
years <- 2000:2009
professor.types <- c("Full professor","Associate professor","Assistant professor","Ranks/level below assistant professor")
# Data missing for Saskatchewan
comp.schools <- c("McMaster University", "University of Waterloo", "University of Windsor", "Queen's University", "University of Guelph", "Carleton University", "Simon Fraser University", "University of Regina", "Dalhousie University", "Memorial University of Newfoundland", "University of Manitoba", "Concordia University", "University of Victoria", "University of New Brunswick")

salary.data <- list()
for (year in years) {
  year.char <- as.character(year)
  salary.file <- paste(data.folder, salary.file.name, as.character(year), as.character(year+1), ".csv",sep="")
  salary.data[[year.char]] <- read.csv(salary.file, header=TRUE, stringsAsFactors=FALSE)
  
  # Exclude medical and dental staff
  salary.data[[year.char]] <- subset(salary.data[[year.char]], Medical.and.Dental.Included == "Excluding Medical/Dental" | Medical.and.Dental.Included == "No staff appointed to a faculty of medicine or dentistry")
  salary.data[[year.char]]$Medical.and.Dental.Included <- NULL
  
  # Filter out fields mentioning administrative roles/non-administrative roles. The salaries associated with these,
  # while important, are outside the scope of the debate.
  salary.data[[year.char]] <- subset(salary.data[[year.char]], Professor.Type %in% professor.types)
  
  # Filter for comparison group
  salary.data[[year.char]] <- subset(salary.data[[year.char]], University %in% comp.schools)
  
  # Add column for year for when I combine the data frames
  salary.data[[year.char]]$Year <-  year
  
  #Convert salary data to numeric
  salary.data[[year.char]]$Salary <- as.integer(salary.data[[year.char]]$Salary)
  print(salary.file)
}
remove(data.file, salary.file, data.folder, year.char, year, salary.file.name)

# Let's bring it all together
salary.data.df <- do.call("rbind", salary.data)
# And clean it up
NAs <- salary.data.df == "x" | salary.data.df == 0
salary.data.df[NAs] <- NA
remove(NAs)

# Now lets covert the salaries to 2002 dollars
salary.data.df$Salary2002 <- realdollars(salary.data.df$Year, salary.data.df$Salary)

# And add a column to specify which one is UNB
salary.data.df$IsUNB <- NA
salary.data.df$IsUNB <- salary.data.df$University == "University of New Brunswick"

# Sets a common theme across graphs
theme <- theme(plot.title=element_text(size=rel(1.7)), axis.title.x=element_text(size=rel(1.5)),  axis.title.y=element_text(size=rel(1.5)), axis.text.x=element_text(size=rel(1.5)), axis.text.y=element_text(size=rel(1.5)), legend.text=element_text(size=rel(1.1)) )

# Plot full professor median salaries, nominal
fullprof.median.nominal <- subset(salary.data.df, Professor.Type == "Full professor" & Salary.Measure == "Median Salary")
fullprof.median.nominal.plot <- ggplot(fullprof.median.nominal, aes(x=Year, y=Salary, colour=University, linetype=(IsUNB))) + 
  ggtitle("Full Professors' Median Salary 2000-2001 to 2009-2010") +
  geom_line(size=1.1) + 
  geom_point(size=2.5) +
  scale_y_continuous(name="Salary (Nominal Dollars)", labels=dollar, limits=c(40000,160000)) +
  scale_x_continuous(name="Year", breaks=seq(2000, 2010, 1)) +
  scale_color_discrete() +
  labs(linetype="Is it UNB?") +
  scale_linetype_discrete(labels=c("No", "Yes")) +
  theme
fullprof.median.nominal.plot
ggsave(filename="Plots/Salaries/FullprofMedianNominal.png", plot=fullprof.median.nominal.plot, width=12, height=8, dpi=100, units="in")

# Plot full professor 90th percentile salaries
fullprof.90 <- subset(salary.data.df, Professor.Type == "Full professor" & Salary.Measure == "90th percentile")
fullprof.90.plot <- ggplot(fullprof.90, aes(x=Year, y=Salary2002, colour=University, linetype=(IsUNB))) + 
  ggtitle("Full Professors' 90th Percentile Salary 2000-2001 to 2009-2010") +
  geom_line(size=1.1) + 
  geom_point(size=2.5) +
  scale_y_continuous(name="Salary (2002 Dollars)", labels=dollar, limits=c(40000,160000)) +
  scale_x_continuous(name="Year", breaks=seq(2000, 2010, 1)) +
  scale_color_discrete() +
  labs(linetype="Is it UNB?") +
  scale_linetype_discrete(labels=c("No", "Yes")) +
  theme
  fullprof.90.plot
ggsave(filename="Plots/Salaries/Fullprof90.png", plot=fullprof.90.plot, width=12, height=8, dpi=100, units="in")

# Plot full professor median salaries
fullprof.median <- subset(salary.data.df, Professor.Type == "Full professor" & Salary.Measure == "Median Salary")
fullprof.median.plot <- ggplot(fullprof.median, aes(x=Year, y=Salary2002, colour=University, linetype=(IsUNB))) + 
  ggtitle("Full Professors' Median Salary 2000-2001 to 2009-2010") +
  geom_line(size=1.1) + 
  geom_point(size=2.5) +
  scale_y_continuous(name="Salary (2002 Dollars)", labels=dollar, limits=c(40000,160000)) +
  scale_x_continuous(name="Year", breaks=seq(2000, 2010, 1)) +
  scale_color_discrete() +
  labs(linetype="Is it UNB?") +
  scale_linetype_discrete(labels=c("No", "Yes")) +
  theme
  fullprof.median.plot
ggsave(filename="Plots/Salaries/FullprofMedian.png", plot=fullprof.median.plot, width=12, height=8, dpi=100, units="in")

# Plot associate professor 90th percentile salaries
associate.90 <- subset(salary.data.df, Professor.Type == "Associate professor" & Salary.Measure == "90th percentile")
associate.90.plot <- ggplot(associate.90, aes(x=Year, y=Salary2002, colour=University, linetype=(IsUNB))) + 
  ggtitle("Assoc. Professors' 90th Percentile Salary 2000-2001 to 2009-2010") +
  geom_line(size=1.1) + 
  geom_point(size=2.5) +
  scale_y_continuous(name="Salary (2002 Dollars)", labels=dollar, limits=c(40000,160000)) +
  scale_x_continuous(name="Year", breaks=seq(2000, 2010, 1)) +
  scale_color_discrete() +
  labs(linetype="Is it UNB?") +
  scale_linetype_discrete(labels=c("No", "Yes")) +
  theme
  associate.90.plot
ggsave(filename="Plots/Salaries/Associate90.png", plot=associate.90.plot, width=12, height=8, dpi=100, units="in")

# Plot associate professor median salaries
associate.median <- subset(salary.data.df, Professor.Type == "Associate professor" & Salary.Measure == "Median Salary")
associate.median.plot <- ggplot(associate.median, aes(x=Year, y=Salary2002, colour=University, linetype=(IsUNB))) + 
  ggtitle("Assoc. Professors' Median Salary 2000-2001 to 2009-2010") +
  geom_line(size=1.1) + 
  geom_point(size=2.5) +
  scale_y_continuous(name="Salary (2002 Dollars)", labels=dollar, limits=c(40000,160000)) +
  scale_x_continuous(name="Year", breaks=seq(2000, 2010, 1)) +
  scale_color_discrete() +
  labs(linetype="Is it UNB?") +
  scale_linetype_discrete(labels=c("No", "Yes")) +
  theme
  associate.median.plot
ggsave(filename="Plots/Salaries/AssociateMedian.png", plot=associate.median.plot, width=12, height=8, dpi=100, units="in")

# Plot assistant professor median salaries
assistant.median <- subset(salary.data.df, Professor.Type == "Assistant professor" & Salary.Measure == "Median Salary")
assistant.median.plot <- ggplot(assistant.median, aes(x=Year, y=Salary2002, colour=University, linetype=(IsUNB))) + 
  ggtitle("Assistant Professors' Median Salary 2000-2001 to 2009-2010") +
  geom_line(size=1.1) + 
  geom_point(size=2.5) +
  scale_y_continuous(name="Salary (2002 Dollars)", labels=dollar, limits=c(40000,160000)) +
  scale_x_continuous(name="Year", breaks=seq(2000, 2010, 1)) +
  scale_color_discrete() +
  labs(linetype="Is it UNB?") +
  scale_linetype_discrete(labels=c("No", "Yes")) +
  theme
  assistant.median.plot
ggsave(filename="Plots/Salaries/AssistantMedian.png", plot=assistant.median.plot, width=12, height=8, dpi=100, units="in")

# Plot assistant professor 10th percentile salaries
assistant.10 <- subset(salary.data.df, Professor.Type == "Assistant professor" & Salary.Measure == "10th percentile")
assistant.10.plot <- ggplot(assistant.10, aes(x=Year, y=Salary2002, colour=University, linetype=(IsUNB))) + 
  ggtitle("Assist. Professors' 10th Percentile Salary 2000-2001 to 2009-2010") +
  geom_line(size=1.1) + 
  geom_point(size=2.5) +
  scale_y_continuous(name="Salary (2002 Dollars)", labels=dollar, limits=c(40000,160000)) +
  scale_x_continuous(name="Year", breaks=seq(2000, 2010, 1)) +
  scale_color_discrete() +
  labs(linetype="Is it UNB?") +
  scale_linetype_discrete(labels=c("No", "Yes")) +
  theme
  assistant.10.plot
ggsave(filename="Plots/Salaries/Assistant10.png", plot=assistant.10.plot, width=12, height=8, dpi=100, units="in")


president.salaries$Salary2011 <- realdollars(president.salaries$Year, president.salaries$Salary, 2011)


# Plots the university president salaries
library(grid)
plot <- ggplot(president.salaries, aes(x=reorder(University,Salary2011), y=Salary2011, fill=FTEs)) + theme +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=70, hjust=1, vjust=1, colour="black", size=rel(1.5))) +
  scale_y_continuous(labels = dollar, name="Salary, 2011 Dollars") +
  scale_x_discrete(name="University") +
  ggtitle("Canadian University President's Salaries") +
  labs(fill="Total Students") +
  geom_text(aes(label=paste("$",prettyNum(Salary2011, big.mark=",", scientific=F), sep="")), size=4, angle=90, colour="white", hjust=1, vjust=0.5) +
  annotate("text", x="University of New Brunswick", y=420000, label="UNB", size=6) +
  annotate("segment", x=33, xend=33, y=410000, yend=383000, arrow=arrow(ends="last", angle=30, length=unit(0.3, "cm")))
plot


# Because the above numbers are biased for a whole bunch of reasons, I'm inventing a "President Index"
# that compares prof salaries to the salary of the president.

# Adds a column with each university's president's salary
salary.data.df$PresidentSalary2011 <- NA
for (school in comp.schools) {
  school.char <- as.character(school)
  school.cell <- salary.data.df$University == school
  president.salary <- president.salaries[president.salaries$University == school,]$Salary2011
  salary.data.df$PresidentSalary2011[school.cell] <- president.salary
}

# I need a vector of dates to pass into the realdollars function
salary.data.df$PresidentSalaryYear <- 2011
# Creates a column for the salaries in 2002 dollars
salary.data.df$PresidentSalary2002 <- realdollars(salary.data.df$PresidentSalaryYear, salary.data.df$PresidentSalary2011, 2002)


# Creates a column for the ratio of the president's salary to professors
salary.data.df$ratio <- salary.data.df$Salary2002/salary.data.df$PresidentSalary2002

# Full prof ratio
fullprof.median.ratio <- subset(salary.data.df, Professor.Type == "Full professor" & Salary.Measure == "Median Salary")
fullprof.median.ratio.plot <- ggplot(fullprof.median.ratio, aes(x=Year, y=ratio, colour=University, linetype=(IsUNB))) + 
  ggtitle("Full Professors' Salary As A Percentage of University President's Salary") +
  geom_line(size=1.1) + 
  geom_point(size=2.5) +
  scale_y_continuous(name="Prof. Salary as a Percentage of President's Salary (Both 2002 Dollars)", labels=percent, limits=c(0, 0.5)) +
  scale_x_continuous(name="Year", breaks=seq(2000, 2010, 1)) +
  scale_color_discrete() +
  labs(linetype="Is it UNB?") +
  scale_linetype_discrete(labels=c("No", "Yes")) +
  theme
fullprof.median.ratio.plot
ggsave(filename="Plots/Salaries/FullprofMedianRatio.png", plot=fullprof.median.ratio.plot, width=14, height=10, dpi=100, units="in")

# Associate prof ratio
associate.median.ratio <- subset(salary.data.df, Professor.Type == "Associate professor" & Salary.Measure == "Median Salary")
associate.median.ratio.plot <- ggplot(associate.median.ratio, aes(x=Year, y=ratio, colour=University, linetype=(IsUNB))) + 
  ggtitle("Associate Professors' Salary As A Percentage of University President's Salary") +
  geom_line(size=1.1) + 
  geom_point(size=2.5) +
  scale_y_continuous(name="Prof. Salary as a Percentage of President's Salary (Both 2002 Dollars)", labels=percent, limits=c(0, 0.5)) +
  scale_x_continuous(name="Year", breaks=seq(2000, 2010, 1)) +
  scale_color_discrete() +
  labs(linetype="Is it UNB?") +
  scale_linetype_discrete(labels=c("No", "Yes")) +
  theme
associate.median.ratio.plot
ggsave(filename="Plots/Salaries/AssociateMedianRatio.png", plot=associate.median.ratio.plot, width=14, height=10, dpi=100, units="in")

# Assistant prof ratio
assistant.median.ratio <- subset(salary.data.df, Professor.Type == "Assistant professor" & Salary.Measure == "Median Salary")
assistant.median.ratio.plot <- ggplot(assistant.median.ratio, aes(x=Year, y=ratio, colour=University, linetype=(IsUNB))) + 
  ggtitle("Assistant Professors' Salary As A Percentage of University President's Salary") +
  geom_line(size=1.1) + 
  geom_point(size=2.5) +
  scale_y_continuous(name="Prof. Salary as a Percentage of President's Salary (Both 2002 Dollars)", labels=percent, limits=c(0, 0.5)) +
  scale_x_continuous(name="Year", breaks=seq(2000, 2010, 1)) +
  scale_color_discrete() +
  labs(linetype="Is it UNB?") +
  scale_linetype_discrete(labels=c("No", "Yes")) +
  theme
assistant.median.ratio.plot
ggsave(filename="Plots/Salaries/AssistantMedianRatio.png", plot=assistant.median.ratio.plot, width=14, height=10, dpi=100, units="in")


