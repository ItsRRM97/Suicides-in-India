# State wise Causes of suicide

stateCause <- function(Dataset) {
  
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
  
  state <- c(unique(as.character((Dataset$State))))
  
  mat <- matrix(0, nrow = length(state), ncol = length(cause))
  dimnames(mat) <- list(state,1:length(cause))
  
  #making a legend for readability
  legend <- data.frame(list(1:length(cause)),cause)
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0 && Dataset$Type_code[i] == 'Causes') {
      for(j in 1:length(state)) {
        if(Dataset$State[i] == state[j]) {
          for(k in 1:length(cause)) {
            if(Dataset$Type[i] == cause[k]) {
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