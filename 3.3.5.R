# year wise edu_stat of suicides

yearEducation <- function(Dataset) {
  
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
  
  year <- c(unique(as.character((Dataset$Year))))
  
  mat <- matrix(0, nrow = length(year), ncol = length(edu_stat))
  dimnames(mat) <- list(year,edu_stat)
  
  #making a legend for readability
  #legend <- data.frame(list(1:length(edu_stat)),edu_stat)
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0 && Dataset$Type_code[i] == 'Education_Status') {
      for(j in 1:length(year)) {
        if(Dataset$Year[i] == year[j]) {
          for(k in 1:length(edu_stat)) {
            if(Dataset$Type[i] == edu_stat[k]) {
              mat[j,k] = mat[j,k] + Dataset$Total[i]
            }
          }
        }
      }
    }
  }
  write.table(mat,"output/3.3.5.csv", row.names = TRUE, col.name = TRUE, sep = ",")
  #print(legend)
}