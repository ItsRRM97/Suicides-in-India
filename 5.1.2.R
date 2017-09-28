# age wise means of suicide

ageMeans <- function(Dataset) {
  
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
  
  age <- c(sort(unique(as.character((Dataset$Age_group)))))
  
  mat <- matrix(0, nrow = length(age), ncol = length(means))
  dimnames(mat) <- list(age,1:length(means))
  
  #making a legend for readability
  legend <- data.frame(list(1:length(means)),means)
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0 && Dataset$Type_code[i] == 'Means_adopted') {
      for(j in 1:length(age)) {
        if(Dataset$Age_group[i] == age[j]) 
          for(k in 1:length(means)) {
            if(Dataset$Type[i] == means[k]) {
              mat[j,k] = mat[j,k] + Dataset$Total[i]
            }
          }
        }
      }
    }
  print(mat)
  print(legend)
}