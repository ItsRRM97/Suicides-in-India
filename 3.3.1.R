# year wise causes of suicide

yearCause <- function(Dataset) {
  
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
  
  year <- c(unique(as.character((Dataset$Year))))
  
  mat <- matrix(0, nrow = length(year), ncol = length(cause))
  dimnames(mat) <- list(year,1:length(cause))
  
  #making a legend for readability
  legend <- data.frame(list(1:length(cause)),cause)
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0 && Dataset$Type_code[i] == 'Causes') {
      for(j in 1:length(year)) {
        if(Dataset$Year[i] == year[j]) {
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