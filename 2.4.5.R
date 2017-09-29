# State wise edu_status of suicide

stateEducation <- function(Dataset) {
  
  edu_stat <- vector() # Null Vector
  Dataset$Type_code <- as.character(Dataset$Type_code)
  Dataset$Type <- as.character(Dataset$Type)
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Education_Status') {
      edu_stat<-c(edu_stat,Dataset$Type[i])
    }
  }
  edu_stat <- unique(edu_stat)
  edu_stat <- sort(edu_stat)
  
  state <- c(unique(as.character((Dataset$State))))
  
  mat <- matrix(0, nrow = length(state), ncol = length(edu_stat))
  dimnames(mat) <- list(state,edu_stat)
  
  #making a legend for readability
  #legend <- data.frame(list(1:length(edu_stat)),edu_stat)
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0 && Dataset$Type_code[i] == 'Education_Status') {
      for(j in 1:length(state)) {
        if(Dataset$State[i] == state[j]) {
          for(k in 1:length(edu_stat)) {
            if(Dataset$Type[i] == edu_stat[k]) {
              mat[j,k] = mat[j,k] + Dataset$Total[i]
            }
          }
        }
      }
    }
  }
  write.table(mat,"output/2.4.5.csv", row.names = TRUE, col.name = TRUE, sep = ",")
  #print(legend)
}