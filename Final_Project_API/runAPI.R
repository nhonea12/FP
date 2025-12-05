library(plumber)

r <- plumb("Final_Project_API/projectAPI.R")

r$run(port=8000)
