# gender wise social status of suicide

genderSocial <- function(Dataset) {
  
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
  
  gender <- c('Male','Female')
  
  mat <- matrix(0, nrow = 2, ncol = length(social))
  dimnames(mat) <- list(gender,social)
  
  #making a legend for readability
  #legend <- data.frame(list(1:length(social)),social)
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0 && Dataset$Type_code[i] == 'Social_Status') {
      for(j in 1:2) {
        if(Dataset$Gender[i] == gender[j]) {
          for(k in 1:length(social)) {
            if(Dataset$Type[i] == social[k]) {
              mat[j,k] = mat[j,k] + Dataset$Total[i]
            }
          }
        }
      }
    }
  }
  print(mat)
  #print(legend)
}