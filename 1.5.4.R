# number of suicides every year by social status

suicideStatus <- function(Dataset)
{
  # generating the list of unique causes
  
  status <- vector() # Null Vector
  Dataset$Type_code <- as.character(Dataset$Type_code)
  Dataset$Type <- as.character(Dataset$Type)
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Social_Status') {
      status<-c(status,Dataset$Type[i])
    }
  }
  status <- unique(status)
  status <- sort(status)
  suicides <- vector("integer",length(status))
  status_wise <- data.frame(status,suicides)
  
  # performs the calculation
  
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Social_Status') {
      for(j in 1:length(status)) {
        if(Dataset$Type[i] == status_wise$status[j]) {
          status_wise$suicides[j] = status_wise$suicides[j] + Dataset$Total[i]
        }
      }
    }
  }
  write.table(status_wise,"output/1.5.4.csv", row.names = FALSE, sep = ",")
  
  # bar graphs
  library(ggplot2)
  png("plots/Suicides_social.png")
  options(scipen=999)
  g <- ggplot(data.frame(status_wise$suicides), 
              aes(x=status_wise$status, y=status_wise$suicides, 
                  fill=status_wise$status)) + geom_bar(stat = "identity") + 
    theme(legend.position="bottom", axis.text.x=element_text(angle=90)) +
    xlab("Social Status") + ylab("Suicide Count") +
    ggtitle("Social Status of people who committed Suicides in India from 2001-2012")
  print(g)
  dev.off()
}