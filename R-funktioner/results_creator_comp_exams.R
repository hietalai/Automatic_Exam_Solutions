##############################################
# Auto-grading function for computer exams
#
# Author: Isak Hietala
# Date: 2015-04-15
##############################################

auto_grading <- function(){
  pass <- as.numeric(readline(prompt = "What is the 'pass' level? "))
  pass_distinction <- as.numeric(readline(prompt = "What is the 'pass with distinction' level? "))
  
  if (pass_distinction < pass){
    cat("VG has lower criteria than G. You did something wrong!")
    pass_distinction <- as.numeric(readline(prompt = "What is the 'pass with distinction' level? "))
  }
  
  nr_students <- as.numeric(readline(prompt = "How many students took the exam? "))
  
  dataset <- data.frame(matrix(NA, nrow = nr_students, ncol = 3))
  colnames(dataset) <- c("student", "points", "grade")
  
  for(i in 1:nr_students){
    
    student <- readline(prompt = paste("Name of student", i, ": "))
    points <- as.numeric(readline(prompt = "How many points: "))
    if (points < pass){
      grade <- "U"
    } else if (points < pass_distinction){
      grade <- "G"
    } else {
      grade <- "VG"
    }
    
    dataset[i,] <- cbind(student, points, grade)
    
  }
    
  dataset <- dataset[order(dataset$student),]
  print(dataset)
  
  check <- readline(prompt = "Do the grades look correct? (y/n)")
  
  while(tolower(substr(check, 1, 1)) == "n"){
   
    lines <- readline(prompt = "Which rows are not correct? ")
    lines <- as.numeric(unlist(strsplit(lines, " ")))
    
    for(i in lines){
      print(dataset[i,])
      
      student <- readline(prompt = paste("Name of student", i, ": "))
      points <- as.numeric(readline(prompt = "How many points: "))
      if(is.na(points)){
        cat("Missing value detected. Resubmit the points.")
        points <- as.numeric(readline(prompt = "How many points: "))
      }
      if (points < pass){
        grade <- "U"
      } else if (points < pass_distinction){
        grade <- "G"
      } else {
        grade <- "VG"
      }
      
      dataset[i,] <- cbind(student, points, grade)
    }
    
    print(dataset)
    
    check <- readline(prompt = "Do the grades look correct? (y/n) ")
  }  
  
  # Asking and specifying the output directory of the file.
  dir <- readline(prompt = "Specify the output directory: (\\ allowed) ")
  setwd(dir)
  
  write.csv2(x = dataset, file = "results.csv", row.names = FALSE)

}

if(interactive()) auto_grading()

























