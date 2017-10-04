# gender wise total no of suicides

SuicideGender <- function(Dataset)
{
  library(plotrix)
  gender <- c('Female','Male')
  
  mat <- matrix(0, nrow = length(gender), ncol = 1)
  dimnames(mat) <- list(gender)
  
  # problem solving
  
  for(i in 1:236583) {
    if(Dataset$Total[i] != 0) {
      mat[Dataset$Gender[i]] = mat[Dataset$Gender[i]] + Dataset$Total[i] 
    }
  }
  write.table(mat,"output/1.3.csv", row.names = TRUE, sep = ",")
  
  #pie graph
  v <- c(mat[1],mat[2])
  piepercent<- round(100*v/sum(v), 1)
  
  png(file = "plots/male_female_suicides.jpg")
  
  pie3D(v,labels = piepercent,explode = 0.1, main = "Suicide by Gender(in percentages)")
  legend("topright", gender, cex = 0.8, fill = rainbow(length(v)))
  dev.off()
  
}