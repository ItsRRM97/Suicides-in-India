# Year wise suicides in each gender
yearGender <- function(Dataset) {
  
  gender <- c(unique(as.character(sort((Dataset$Gender)))))
  year <- c(unique(as.character((Dataset$Year))))
  
  mat <- matrix(0, nrow = length(year), ncol = 2)
  dimnames(mat) <- list(year,gender)
  
  # problem solving
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0) {
      for(j in 1:length(year)) {
        if(Dataset$Year[i] == year[j]) {
          if(Dataset$Gender[i] == 'Male') {
            mat[j,2] = mat[j,2] + Dataset$Total[i]
          }
          else {
            mat[j,1] = mat[j,1] + Dataset$Total[i]
          }
        }
      }
    }
  }
  write.table(mat,"output/3.1.csv", row.names = TRUE, col.name = TRUE, sep = ",")
}