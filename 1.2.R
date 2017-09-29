# State wise total no of suicides

SuicideState <- function(Dataset)
{
  state <- unique(as.character(Dataset$State))
  no_of_suicides <- vector(mode="integer", length=length(state))
  state <- sort(state)
  
  mat <- matrix(0, nrow = length(state), ncol = 1)
  dimnames(mat) <- list(state)
  
  # problem solving
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0) {
      mat[Dataset$State[i]] = mat[Dataset$State[i]] + Dataset$Total[i] 
    }
  }
  write.table(mat,"output/1.2.csv", row.names = TRUE, sep = ",")
}