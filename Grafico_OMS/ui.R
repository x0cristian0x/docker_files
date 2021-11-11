#reactlog::reactlog_enable()
source("packages.R")
source("input_UI.R")


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

options(encoding = 'UTF-8')
Style_titulo <- "color:#e9e4f0; padding:0px; border:0px"

ui <-
  secure_app(
    head_auth = tags$script(inactivity),
    language = "es",
    
    fluidPage(
     #Jorunal
      theme = shinythemes::shinytheme("slate"),
      
      h1(strong("DETERMINE SU ESTADO NUTRICIONAL"),
         align = "center", style = Style_titulo),
      div(img(src="imagen_2.jpg", 
              width = "400px", height = "150px"),align = "center"),
      p(),
      withTags(
        nav(div(), ul(
          ul( "Esta página determina el estado nutricional de la persona, 
              mediante gráficos e interpretación los resultados, se debe 
              llenar los datos utilizando las siguientes unidades de medición:"), 
          p(),
          li("Utilizar ",strong("Solo")," los meses de nacimiento o indicar la fecha de 
             nacimiento en el calendario"),
          li("Peso en kilogramos, puede incluir decimales, ejemplo: 5.5"),
          li("Estatura en centímetros (Niños/as de 0 a 23 meses medir su estatura acostado/a), puede incluir decimales, ejemplo: 75.5"),
          li("Perímetro cefácilo en centímetros, puede incluir decimales, ejemplo: 42.5"),
          li("Perímetro medio braquial en centímetros, puede incluir decimales, 
             ejemplo: 17.5"),
          ))
      ),
      withTags(
        nav(div(), 
            
            ul(h3("Fuentes de información"),
               li("Tabla de la Organización Mundial de la Salud (OMS) para 
               recien nacidos de 0 a 13 semanas"),
               li("Tabla de la Organización Mundial de la Salud (OMS) para niños/as 
                  entre los 0 a 18 años"),
               li("Tabla de Fenton para niños/as prematuros"),
               li("Tabla del Ministerio de salud de Chile para las embarazadas 
                  con 10 semanas de gestación"),
               li("Tabla de Atalah para las embarazadas")
               )
            )),
      
      
      
      
      
      
    #                tags$h1("Otro Título "), # no sirve align con tags
    #                tags$h2( em("Subtítulo")),
    #                tags$code("aqui entrara algo"),
    #                div("Parrafo con estilo", 
    #                    style ="background-color:#ad9e6d"),
    #                p("Parrafo normal",
    #                  span("Parrafo con color", style = "color:#ad6d9b"),
    #                  "ahora sigue siendo normal"),
    #                tags$a(href="https://www.google.com/", "Pagina de google"),
    #                p(),
    # 
    #                
    #                tags$style( "p{color:orange}"), # Este style domina toda la page
    #                  p("parrafo con tags y style"),
    #                
    #                
    #                tags$script(
    #                  "document.write('usando javascript');
    #  // change text color
    #  function changeColor(color){
    #    document.getElementById('hello').style.color = color;
    #  }
    # "
    #                ),
    #                p(id = "hello", onclick="changeColor('green')", "Hello World"),
    #                
    #                
    #                
                   
                   # titlePanel("PROYECTO"),
                   # helpText("ayuda de texto"),
                   selectInput(inputId = "age_group", label = "Seleccione el grupo etario", 
                               choices = 
                                 list("",
                                      "1. Tabla OMS de 0 a 18 años" = "oms",
                                      "2. Recién Nacidos hasta las 13 semanas" = "RN",
                                      "3. Embarazada" = "pregnation",
                                      "4. Prematuros de 22 a 50 semanas"="premature"),  
                   ),
                   
                   conditionalPanel( 
                     condition = "input.age_group == 'oms'",
                     
                     sliders_0_19("TABLA_OMS"),  
                   ),
                   
                   conditionalPanel(
                     condition = "input.age_group == 'RN'",
                     sliders_0_13("TABLA_WEEK"),
                     
                   ),
                   conditionalPanel(
                     condition = "input.age_group == 'pregnation'",
                     sliders_pregnation("TABLA_PREGNATION"),
                     
                   ),
                   conditionalPanel(
                     condition = "input.age_group == 'premature'",
                     sliders_premature("TABLA_PREMATURE"),

                   ),
                   
    h3("Cualquier duda o sugerencia aquí"),
    h3("Para mayor información"),
                   h1("Esto sera el final"),
                 )
                 
                 
  )# No BORRAR


#            plotOutput("plot_1"),
# #           tableOutput("tabla_1"),
# #           uiOutput(outputId = "plot_2_ui"),