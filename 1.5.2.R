# number of suicides every year by means adopted

suicideMeans <- function(Dataset)
{
  # generating the list of unique causes
  
  means_adopted <- vector() # Null Vector
  Dataset$Type_code <- as.character(Dataset$Type_code)
  Dataset$Type <- as.character(Dataset$Type)
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Means_adopted') {
      means_adopted<-c(means_adopted,Dataset$Type[i])
    }
  }
  means_adopted <- unique(means_adopted)
  means_adopted <- sort(means_adopted)
  suicides <- vector("integer",length(means_adopted))
  means_adopted_wise <- data.frame(means_adopted,suicides)
  
  # performs the calculation
  
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Means_adopted') {
      for(j in 1:length(means_adopted)) {
        if(Dataset$Type[i] == means_adopted_wise$means_adopted[j]) {
          means_adopted_wise$suicides[j] = means_adopted_wise$suicides[j] + Dataset$Total[i]
        }
      }
    }
  }
  write.table(means_adopted_wise,"output/1.5.2.csv", row.names = FALSE, sep = ",")
  
  # bar graph
  
  library(ggplot2)
  png("plots/Suicides_means.png")
  options(scipen=999)
  g <- ggplot(data.frame(means_adopted_wise$suicides), 
              aes(x=means_adopted_wise$means_adopted, y=means_adopted_wise$suicides,means_adopted_wise$suicidesfill=means_adopted_wise$means_adopted)) + geom_bar(stat = "identity") + 
    theme(legend.position="none", axis.text.x=element_text(angle=90)) +
    xlab("Means Adopted") + ylab("Suicide Count") +
    ggtitle("Means Adopted for Suicides in India from 2001 to 2012")
  print(g)
  dev.off()
}
