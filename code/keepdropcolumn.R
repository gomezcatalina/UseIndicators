Select_PatternMatching <- function(x,y) names(y[grep(x, names(y))])

#R Function : Keep / Drop Column Function

#The following program automates keeping or dropping columns from a data frame.
KeepDrop = function(data=df,cols="var",newdata=df2,drop=1) {
  # Double Quote Output Dataset Name
  t = deparse(substitute(newdata))
  
  # Drop Columns
  if(drop == 1){
    newdata = data [ , !(names(data) %in% scan(textConnection(cols), what="", sep=" "))]}
  
  # Keep Columns
  else {
    newdata = data [ , names(data) %in% scan(textConnection(cols), what="", sep=" ")]}
  assign(t, newdata, .GlobalEnv)
}

# To keep variables 'a' and 'x', use the code below. The drop = 0 implies keeping variables that are specified in the parameter "cols". The parameter "data" refers to input data frame. "cols" refer to the variables you want to keep / remove. "newdata" refers to the output data frame.
# KeepDrop(data=mydata,cols="a x", newdata=dt, drop=0)
# 
# To drop variables, use the code below. The drop = 1 implies removing variables which are defined in the second parameter of the function.
# KeepDrop(data=mydata,cols="a x", newdata=dt, drop=1)