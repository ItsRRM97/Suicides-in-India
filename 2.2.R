# State wise suicides in each gender

stateGenderSuicide <- function(Dataset) {
  
  gender <- c(unique(as.character(Dataset$Gender)))
  state <- c(unique(as.character((Dataset$State))))
  
  mat <- matrix(0, nrow = length(state), ncol = length(gender))
  dimnames(mat) <- list(state,gender)
  
  # problem solving
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0) {
      for(j in 1:length(state)) {
        if(Dataset$State[i] == state[j]) {
          if(Dataset$Gender[i] == 'Male') {
          mat[j,1] = mat[j,1] + Dataset$Total[i]
          }
          else {
            mat[j,2] = mat[j,2] + Dataset$Total[i]
          }
        }
      }
    }
  }
  print(mat)
}