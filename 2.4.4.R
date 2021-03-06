# State wise social_status of suicide

stateSocial <- function(Dataset) {
  
  social_stat <- vector() # Null Vector
  Dataset$Type_code <- as.character(Dataset$Type_code)
  Dataset$Type <- as.character(Dataset$Type)
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Social_Status') {
      social_stat<-c(social_stat,Dataset$Type[i])
    }
  }
  social_stat <- unique(social_stat)
  social_stat <- sort(social_stat)
  
  state <- c(unique(as.character((Dataset$State))))
  
  mat <- matrix(0, nrow = length(state), ncol = length(social_stat))
  dimnames(mat) <- list(state,social_stat)
  
  #making a legend for readability
  #legend <- data.frame(list(1:length(social_stat)),social_stat)
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0 && Dataset$Type_code[i] == 'Social_Status') {
      for(j in 1:length(state)) {
        if(Dataset$State[i] == state[j]) {
          for(k in 1:length(social_stat)) {
            if(Dataset$Type[i] == social_stat[k]) {
              mat[j,k] = mat[j,k] + Dataset$Total[i]
            }
          }
        }
      }
    }
  }
  write.table(mat,"output/2.4.4.csv", row.names = TRUE, col.name = TRUE, sep = ",")
  #print(legend)
}