# age wise profile of suicide

ageProfile <- function(Dataset) {
  
  profile <- vector() # Null Vector
  Dataset$Type_code <- as.character(Dataset$Type_code)
  Dataset$Type <- as.character(Dataset$Type)
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Professional_Profile') {
      profile<-c(profile,Dataset$Type[i])
    }
  }
  profile <- unique(profile)
  profile <- sort(profile)
  
  age <- c(sort(unique(as.character((Dataset$Age_group)))))
  
  mat <- matrix(0, nrow = length(age), ncol = length(profile))
  dimnames(mat) <- list(age,1:length(profile))
  
  #making a legend for readability
  legend <- data.frame(list(1:length(profile)),profile)
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0 && Dataset$Type_code[i] == 'Professional_Profile') {
      for(j in 1:length(age)) {
        if(Dataset$Age_group[i] == age[j]) {
          for(k in 1:length(profile)) {
            if(Dataset$Type[i] == profile[k]) {
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