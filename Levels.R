# List of all Unique Levels in the Dataset

levelfunc <- function(Dataset) {
  
  state_list<-c(unique(Dataset$State))
  state_length<-length(state_list)
  print("Levels for State")
  for(i in 1:state_length) {
    print(state_list[i])
  }
  
  year_list<-c(unique(Dataset$Year))
  year_length<-length(year_list)
  print("Levels for Year")
  for(i in 1:year_length) {
    print(year_list[i])
  }
  
  type_code_list<-c(unique(Dataset$Type_code))
  type_code_length<-length(type_code_list)
  print("Levels for Type_code")
  for(i in 1:type_code_length) {
    print(type_code_list[i])
  }
  
  type_list<-c(unique(Dataset$Type))
  type_length<-length(year_list)
  print("Levels for Type")
  for(i in 1:type_length) {
    print(type_list[i])
  }
  
  gender_list<-c(unique(Dataset$Gender))
  gender_length<-length(gender_list)
  print("Levels for Gender")
  for(i in 1:gender_length) {
    print(gender_list[i])
  }
  
  age_list<-c(unique(Dataset$Age_group))
  age_length<-length(age_list)
  print("Levels for Age")
  for(i in 1:age_length) {
    print(age_list[i])
  }
  
}