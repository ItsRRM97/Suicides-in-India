# State wise Means Adopted for suicide

stateMeans <- function(Dataset) {
  
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
  
  state <- c(unique(as.character((Dataset$State))))
  
  mat <- matrix(0, nrow = length(state), ncol = length(means_adopted))
  dimnames(mat) <- list(state,1:length(means_adopted))
  
  #making a legend for improving readability
  legend <- data.frame(list(1:length(means_adopted)),means_adopted)
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0 && Dataset$Type_code[i] == 'Means_adopted') {
      for(j in 1:length(state)) {
        if(Dataset$State[i] == state[j]) {
          for(k in 1:length(means_adopted)) {
            if(Dataset$Type[i] == means_adopted[k]) {
              mat[j,k] = mat[j,k] + Dataset$Total[i]
            }
          }
        }
      }
    }
  }
  print(mat)
  print(legend)
}