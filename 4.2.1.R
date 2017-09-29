# gender wise causes of suicide

genderCause <- function(Dataset) {
  
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
  
  gender <- c('Male','Female')
  
  mat <- matrix(0, nrow = 2, ncol = length(cause))
  dimnames(mat) <- list(gender,cause)
  
  #making a legend for readability
  #legend <- data.frame(cause)
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0 && Dataset$Type_code[i] == 'Causes') {
      for(j in 1:2) {
        if(Dataset$Gender[i] == gender[j]) {
          for(k in 1:length(cause)) {
            if(Dataset$Type[i] == cause[k]) {
              mat[j,k] = mat[j,k] + Dataset$Total[i]
            }
          }
        }
      }
    }
  }
  write.table(mat,"output/4.2.1.csv", row.names = TRUE, col.name = TRUE, sep = ",")
  #print(legend)
}