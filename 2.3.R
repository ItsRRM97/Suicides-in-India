# State wise suicides in each age

stateAgeSuicide <- function(Dataset) {
  
  age <- c(unique(as.character(sort((Dataset$Age_group)))))
  state <- c(unique(as.character((Dataset$State))))
  
  mat <- matrix(0, nrow = length(state), ncol = length(age))
  dimnames(mat) <- list(state,age)
  
  # problem solving
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0) {
      for(j in 1:length(state)) {
        if(Dataset$State[i] == state[j]) {
          for(k in 1:length(age)) {
            if(Dataset$Age_group[i] == age[k]) {
              mat[j,k] = mat[j,k] + Dataset$Total[i]
            }
          }
        }
      }
    }
  }
  print(mat)
}