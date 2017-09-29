# gender wise meanss of suicide

genderMeans <- function(Dataset) {
  
  means <- vector() # Null Vector
  Dataset$Type_code <- as.character(Dataset$Type_code)
  Dataset$Type <- as.character(Dataset$Type)
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Means_adopted') {
      means<-c(means,Dataset$Type[i])
    }
  }
  means <- unique(means)
  means <- sort(means)
  
  gender <- c('Male','Female')
  
  mat <- matrix(0, nrow = 2, ncol = length(means))
  dimnames(mat) <- list(gender,means)
  
  #making a legend for readability
  #legend <- data.frame(list(1:length(means)),means)
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0 && Dataset$Type_code[i] == 'Means_adopted') {
      for(j in 1:2) {
        if(Dataset$Gender[i] == gender[j]) {
          for(k in 1:length(means)) {
            if(Dataset$Type[i] == means[k]) {
              mat[j,k] = mat[j,k] + Dataset$Total[i]
            }
          }
        }
      }
    }
  }
  write.table(mat,"output/4.2.2.csv", row.names = TRUE, col.name = TRUE, sep = ",")
  #print(legend)
}