# number of suicides every year by means adopted

suicideProfiles <- function(Dataset)
{
  # generating the list of unique causes
  
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
  suicides <- vector("integer",length(profile))
  profile_wise <- data.frame(profile,suicides)
  
  # performs the calculation
  
  for(i in 1:236583) {
    if(Dataset$Type_code[i] == 'Professional_Profile') {
      for(j in 1:length(profile)) {
        if(Dataset$Type[i] == profile_wise$profile[j]) {
          profile_wise$suicides[j] = profile_wise$suicides[j] + Dataset$Total[i]
        }
      }
    }
  }
  print(profile_wise)
}
