# gender wise suicides in each age group

genderAgeSuicide <- function(Dataset) {
  
  age <- c(unique(as.character(sort((Dataset$Age_group)))))
  gender <- c(unique(as.character((Dataset$Gender))))
  
  mat <- matrix(0, nrow = length(gender), ncol = length(age))
  dimnames(mat) <- list(gender,age)
  
  # problem solving
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0) {
      for(j in 1:length(gender)) {
        if(Dataset$Gender[i] == gender[j]) {
          for(k in 1:length(age)) {
            if(Dataset$Age_group[i] == age[k]) {
              mat[j,k] = mat[j,k] + Dataset$Total[i]
            }
          }
        }
      }
    }
  }
  write.table(mat,"output/4.1.csv", row.names = TRUE, col.name = TRUE, sep = ",")
}