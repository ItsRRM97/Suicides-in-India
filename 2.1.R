# State wise suicides in each year

stateSuicide <- function(Dataset) {
  
  year <- c(unique(Dataset$Year))
  state <- c(unique(as.character((Dataset$State))))
  
  mat <- matrix(0, nrow = length(state), ncol = length(year))
  dimnames(mat) <- list(state,year)
  
  # problem solving
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0) {
      for(j in 1:length(state)) {
        if(Dataset$State[i] == state[j]) {
          for(k in 1:length(year)) {
            if(Dataset$Year[i] == year[k]) {
              mat[j,k] = mat[j,k] + Dataset$Total[i]
            }
          }
        }
      }
    }
  }
  write.table(mat,"output/2.1.csv", row.names = TRUE, col.name = TRUE, sep = ",")
}