# gender wise total no of suicides

SuicideGender <- function(Dataset)
{
  gender <- c('Female','Male')
  
  mat <- matrix(0, nrow = length(gender), ncol = 1)
  dimnames(mat) <- list(gender)
  
  # problem solving
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0) {
      mat[Dataset$Gender[i]] = mat[Dataset$Gender[i]] + Dataset$Total[i] 
    }
  }
  print(mat)
}