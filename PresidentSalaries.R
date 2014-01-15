library(ggplot2)
library(scales)

data.file <- "Data/PresidentSalaries2.csv"
uni.ftes.file <- "Data/CAUBO/20112012/t_D_Rep_0.csv"

president.salaries <- read.csv(data.file, header=TRUE)
uni.ftes <- read.csv(uni.ftes.file, header=TRUE)

# Fills in FTEs
president.salaries$FTEs <- NA
for (university in as.character(president.salaries$University)) {
  if (length(uni.ftes[uni.ftes$INSTNAME == university,]$FTE) > 0 ) {
    president.salaries[president.salaries$University == university,]$FTEs <- uni.ftes[uni.ftes$INSTNAME == university,]$FTE
  } else {
    president.salaries[president.salaries$University == university,]$FTEs == NA
  }
}

# Write out what was updated so it can be augmented manually
write.csv(president.salaries, "presidentsalariesupdated.csv", row.names=FALSE)

theme <- theme(plot.title=element_text(size=rel(2)), axis.title.x=element_text(size=rel(1.5)),  axis.title.y=element_text(size=rel(1.5)), axis.text.x=element_text(size=rel(1.5)), axis.text.y=element_text(size=rel(1.5)) )

library(grid)
plot <- ggplot(president.salaries, aes(x=reorder(University,Salary), y=Salary, fill=FTEs)) + theme +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=70, hjust=1, vjust=1, colour="black", size=rel(1.5))) +
  scale_y_continuous(labels = dollar) +
  scale_x_discrete(name="University") +
  ggtitle("Canadian University President's Salaries") +
  labs(fill="Total Students") +
  geom_text(aes(label=paste("$",prettyNum(Salary, big.mark=",", scientific=F), sep="")), size=4, angle=90, colour="white", hjust=1, vjust=0.5) +
  annotate("text", x="University of New Brunswick", y=400000, label="UNB", size=6) +
  annotate("segment", x=30, xend=30, y=390000, yend=360000, arrow=arrow(ends="last", angle=30, length=unit(0.3, "cm")))
plot


