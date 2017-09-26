# number of suicides every year by educational edu

suicideEducation <- function(Dataset)
{
  # generating the list of unique causes
  
  edu <- vector() # Null Vector
  Dataset$Type_code <- as.character(Dataset$Type_code)
  Dataset$Type <- as.character(Dataset$Type)
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Education_Status') {
      edu<-c(edu,Dataset$Type[i])
    }
  }
  edu <- unique(edu)
  edu <- sort(edu)
  suicides <- vector("integer",length(edu))
  edu_wise <- data.frame(edu,suicides)
  
  # performs the calculation
  
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Education_Status') {
      for(j in 1:length(edu)) {
        if(Dataset$Type[i] == edu_wise$edu[j]) {
          edu_wise$suicides[j] = edu_wise$suicides[j] + Dataset$Total[i]
        }
      }
    }
  }
  print(edu_wise)
}