# gender wise educational status of suicide

genderEducation <- function(Dataset) {
  
  edu_stat <- vector() # Null Vector
  Dataset$Type_code <- as.character(Dataset$Type_code)
  Dataset$Type <- as.character(Dataset$Type)
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Education_Status') {
      edu_stat<-c(edu_stat,Dataset$Type[i])
    }
  }
  edu_stat <- unique(edu_stat)
  edu_stat <- sort(edu_stat)
  
  gender <- c('Male','Female')
  
  mat <- matrix(0, nrow = 2, ncol = length(edu_stat))
  dimnames(mat) <- list(gender,1:length(edu_stat))
  
  #making a legend for readability
  legend <- data.frame(list(1:length(edu_stat)),edu_stat)
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0 && Dataset$Type_code[i] == 'Education_Status') {
      for(j in 1:2) {
        if(Dataset$Gender[i] == gender[j]) {
          for(k in 1:length(edu_stat)) {
            if(Dataset$Type[i] == edu_stat[k]) {
              mat[j,k] = mat[j,k] + Dataset$Total[i]
            }
          }
        }
      }
    }
  }
  print(mat)
  print(legend)
}