# verifying the dataset

verification <- function(Dataset) {
  type_code <- unique(Dataset$Type_code)
  total_causes <- 0
  total_edu <- 0
  total_soc <- 0
  total_prof <- 0
  total_mean <- 0
  for(i in  1:237519) {
    if(Dataset$Total[i] != 0) {
          if(Dataset$Type_code[i] == 'Causes')  { 
            total_causes <- total_causes + Dataset$Total[i]
          }
      else if (Dataset$Type_code[i] == 'Means_adopted') {
        total_mean <- total_mean + Dataset$Total[i]
      }
      else if (Dataset$Type_code[i] == 'Education_Status') {
        total_edu <- total_edu + Dataset$Total[i]
      }
      else if (Dataset$Type_code[i] == 'Social_Status') {
        total_soc <- total_soc + Dataset$Total[i]
      }
      else {
        total_prof <- total_prof + Dataset$Total[i]
      }
    } 
  }
  print(total_prof)
  print(total_mean)
  print(total_soc)
  print(total_edu)
  print(total_causes)
}