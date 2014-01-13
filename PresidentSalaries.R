library(ggplot2)
library(scales)

data.file <- "Data/PresidentSalaries.csv"

president.salaries <- read.csv(data.file, header=FALSE)
names(president.salaries) <- c("University", "President", "Amount")

plot <- ggplot(president.salaries, aes(x=reorder(University,Amount), y=Amount)) +
        geom_bar(stat="identity", colour="black", fill="#669933") +
        theme(axis.text.x = element_text(angle=70, hjust=1, vjust=1, colour="black", size=rel(1.5))) +
        scale_y_continuous(labels = dollar)
plot

# Color the salaries by the colour of the bars