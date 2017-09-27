# year wise suicides in each age group

yearAgeSuicide <- function(Dataset) {
  
  age <- c(unique(as.character(sort((Dataset$Age_group)))))
  year <- c(unique(as.character((Dataset$Year))))
  
  mat <- matrix(0, nrow = length(year), ncol = length(age))
  dimnames(mat) <- list(year,age)
  
  # problem solving
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0) {
      for(j in 1:length(year)) {
        if(Dataset$Year[i] == year[j]) {
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