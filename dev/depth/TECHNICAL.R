######## paste this in the Console first ##########
library(devtools); options(shiny.launch.browser = .rs.invokeShinyWindowExternal); 

######## optional - if there are any EPATADA changes, you need to run this
document("C:/Data_and_Tools/tada/working/app/EPATADA")

######## after any edit, run the app using this  ###########
rshiny::runApp(".", launch.browser = TRUE)