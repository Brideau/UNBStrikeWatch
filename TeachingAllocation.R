library(ggplot2)

data.file <- "Data/FullTimeTeaching.csv"

teaching.stats <- read.csv(data.file)
ranks <- levels(teaching.stats$RANK)
geos <- levels(teaching.stats$GEO)

rankdf <- function(geo) {
  list <- list()
  geo.stats <- subset(teaching.stats, GEO == geo)
  list[["Year"]] <- as.numeric(substr(geo.stats$Ref_Date, 1, 4))
  for (rank in ranks) {
    list[[rank]] <- as.numeric(as.character(subset(geo.stats, RANK == rank)$Value))
  }
  df <- do.call(cbind.data.frame, list)
  return(df)
}

for (geo in geos) {
  assign(paste(gsub(" ", "", geo), "df", sep="."), rankdf(geo))
}

plot(Canada.df[[1]], Canada.df[[3]], col="Blue") # Associate
points(Canada.df[[1]], Canada.df[[2]], col="Green") # Assistant 
points(Canada.df[[1]], Canada.df[[4]], col="Red") # Full prof

plot(NewBrunswick.df[[1]], NewBrunswick.df[[4]], col="Red") # Full prof
points(NewBrunswick.df[[1]], NewBrunswick.df[[3]], col="Blue") # Associate
points(NewBrunswick.df[[1]], NewBrunswick.df[[2]], col="Green") # Assistant 
