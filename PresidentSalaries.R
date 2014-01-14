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


plot <- ggplot(president.salaries, aes(x=reorder(University,Salary), y=Salary, fill=FTEs)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=70, hjust=1, vjust=1, colour="black", size=rel(1.5))) +
  scale_y_continuous(labels = dollar)
  scale_color_gradient(low="blue", high="red", limits=c( min(FTEs), max(FTEs) )) +
  ggtitle("Canadian University President's Salaries")
plot


