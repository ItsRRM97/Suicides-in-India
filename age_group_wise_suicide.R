# Age_group wise total no of suicides

maxSuicideAgeGroup <- function(Dataset)
{
  age_group_levels <- unique(Dataset$Age_group)
  age_group_names <- c("Unknown","Children","Teenagers","Middle-Aged Adult","Older Adults","Senior Citizens")
  no_of_suicides <- c(0,0,0,0,0,0)
  age_group_levels <- sort(age_group_levels)
  
  age_group <- data.frame(age_group_levels,age_group_names,no_of_suicides)
  
  for(i in 1:236583) {
    for(j in 1:6) {
      if(Dataset$Age_group[i] == age_group[j,"age_group_levels"]) {
        age_group[j,"no_of_suicides"] = age_group[j,"no_of_suicides"]+Dataset$Total[i]
      }
    }
  }
  print(age_group)
}