
source("oms_server.R")
source("week_server.R")
source("pregnation_server.R")
source("premature_server.R")
source("function_app/grafica_function.R")
source("function_app/tabla_function.R")
source("function_app/read_function.R")

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# # data.frame with credentials info
credentials <- data.frame(
  user = c("1", "fanny", "victor", "benoit"),
  password = c("1", "azerty", "12345", "azerty"),
  # comment = c("alsace", "auvergne", "bretagne"), %>%
  stringsAsFactors = FALSE
)


# con <- dbConnect( MySQL(),
#                  user = 'root',
#                  password = '1234',
#                  host = 'localhost',
#                  dbname = 'datashiny')
# 
# credentials <- data.frame(
#   user = dbReadTable(con, "Usuario")[,"username"],
#   password = dbReadTable(con, "Usuario")[,"pass"],
#   stringsAsFactors = FALSE)


shinyServer(function(input, output,session) {
 
  result_auth <- secure_server(
    check_credentials = check_credentials(credentials))

  
 
   omsServer("TABLA_OMS")
   weekServer("TABLA_WEEK")
   pregnationServer("TABLA_PREGNATION")
   prematureServer("TABLA_PREMATURE")
  

  
## Create Variable With Reactive ===========
   
# # Create all Plot ================
#  
#     output$plot_1 = renderPlot( {
#       if (x()==1) {
#        g= ggplot( 
#           length_age_0_19[ length_age_0_19$Sex==sex() &
#                              length_age_0_19$Distribution == distribution(),], 
#           aes(x= Months, y= Length, col = Desviacion)) +
#           geom_line() + 
#           geom_point(mapping = aes(x=date_born(), y = height()))
#        return(g)
#         
#       }
#       else if (x()==2 ) {
#       g=ggplot( 
#         length_weeks[ length_weeks$Sex==sex_week() &
#                         length_weeks$Distribution == distribution_week(),], 
#         aes(x= Weeks, y= Length, col = Desviacion)) +
#         geom_line() + 
#         geom_point(mapping = aes(x=date_born_week(), y = height_week() ))
#       return(g)
#       }
#     })
#     
#     
#     
#     output$plot_2 = renderPlot( {
#         if( bmi() > 0){
#           ggplot( BMI_age_0_19[ BMI_age_0_19$Sex==sex() &
#                                   BMI_age_0_19$Distribution == distribution(),], 
#                   aes(x= Months, y= BMI, col = Desviacion)) +
#             geom_line() + 
#             geom_point(mapping = aes(x=date_born(), y = bmi()))
#         }
#              })
# 

#     
# # Show plot to ui ===================
#     output$plot_1_ui = renderUI({
# 
#         plotOutput(outputId = "plot_1")
# 
#     })
#     
#     output$plot_2_ui = renderUI({
#       if ( height() > 0 & weight()>0 ) {
#         plotOutput(outputId = "plot_2")
#       }
#     })
#     

})
