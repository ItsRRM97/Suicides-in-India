# age wise social of suicide

ageSocial <- function(Dataset) {
  
  social <- vector() # Null Vector
  Dataset$Type_code <- as.character(Dataset$Type_code)
  Dataset$Type <- as.character(Dataset$Type)
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Social_Status') {
      social<-c(social,Dataset$Type[i])
    }
  }
  social <- unique(social)
  social <- sort(social)
  
  age <- c(sort(unique(as.character((Dataset$Age_group)))))
  
  mat <- matrix(0, nrow = length(age), ncol = length(social))
  dimnames(mat) <- list(age,social)
  
  #making a legend for readability
  #legend <- data.frame(list(1:length(social)),social)
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0 && Dataset$Type_code[i] == 'Social_Status') {
      for(j in 1:length(age)) {
        if(Dataset$Age_group[i] == age[j]) {
          for(k in 1:length(social)) {
            if(Dataset$Type[i] == social[k]) {
              mat[j,k] = mat[j,k] + Dataset$Total[i]
            }
          }
        }
      }
    }
  }
  write.table(mat,"output/5.1.4.csv", row.names = TRUE, col.name = TRUE, sep = ",")
  #print(legend)
}