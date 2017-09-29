# number of suicides every year by causes

suicideCause <- function(Dataset)
{
  # generating the list of unique causes
  
  cause <- vector() # Null Vector
  Dataset$Type_code <- as.character(Dataset$Type_code)
  Dataset$Type <- as.character(Dataset$Type)
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Causes') {
      cause<-c(cause,Dataset$Type[i])
    }
  }
  cause <- unique(cause)
  cause <- sort(cause)
  suicides <- vector("integer",length(cause))
  cause_wise <- data.frame(cause,suicides)
  
  # performs the calculation

  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Causes') {
      for(j in 1:length(cause)) {
        if(Dataset$Type[i] == cause_wise$cause[j]) {
          cause_wise$suicides[j] = cause_wise$suicides[j] + Dataset$Total[i]
        }
      }
    }
  }
  write.table(cause_wise,"output/1.5.1.csv", row.names = FALSE, sep = ",")
}
