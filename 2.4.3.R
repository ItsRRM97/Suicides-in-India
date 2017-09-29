# State wise profiles of suicide

stateProfile <- function(Dataset) {
  
  profile <- vector() # Null Vector
  Dataset$Type_code <- as.character(Dataset$Type_code)
  Dataset$Type <- as.character(Dataset$Type)
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Professional_Profile') {
      profile<-c(profile,Dataset$Type[i])
    }
  }
  profile <- unique(profile)
  profile <- sort(profile)
  
  state <- c(unique(as.character((Dataset$State))))
  
  mat <- matrix(0, nrow = length(state), ncol = length(profile))
  dimnames(mat) <- list(state,profile)
  
  #making a legend for readability
  #legend <- data.frame(list(1:length(profile)),profile)
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0 && Dataset$Type_code[i] == 'Professional_Profile') {
      for(j in 1:length(state)) {
        if(Dataset$State[i] == state[j]) {
          for(k in 1:length(profile)) {
            if(Dataset$Type[i] == profile[k]) {
              mat[j,k] = mat[j,k] + Dataset$Total[i]
            }
          }
        }
      }
    }
  }
  write.table(mat,"output/2.4.3.csv", row.names = TRUE, col.name = TRUE, sep = ",")
  #print(legend)
}