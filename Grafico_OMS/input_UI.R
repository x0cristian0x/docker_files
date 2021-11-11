
# Function UI input for Table OMS of 0 to 18 years ========
sliders_0_19 <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
    sidebarPanel(
      
    numericInput( ns( "months"), "Ingrese los Meses de Nacimiento", 
                  value=0, min = 0 ),
    dateInput( ns("calendar"), label = "Fecha de Nacimiento",
               value = NULL, language = "es"),
    numericInput( ns( "height"), "Estatura en cm", value=0, min = 0 ),
    numericInput( ns( "weight"), "Peso en kg", value = 0, min = 0),
    numericInput( ns( "head"), "Circunferencia de la Cabeza en cm", 
                  value=0, min = 0 ),
    numericInput( ns( "arm"), "Perímetro Medio Braquial en cm", 
                  value=0, min = 0 ),
    selectInput( ns( "sex"), label = "Sexo del paciente",
                  choices = c("Hombre", "Mujer")),
    selectInput( ns("distribution"),label = "Elija la tabla",
                choices = c( "Z-score","Percentil")),
    actionButton( ns( "click_0_19"), label = "Click Aquí!",
                  class = "btn btn-warning"),
    
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Gráficos",  # Icono
                
      h2("Gráfico de la Estatura/Longitud para la Edad"),
      plotOutput( ns( "plot_1")),
      uiOutput(ns( "prueba_1")),
      DTOutput( ns("table_oms_1")), 
      h4(textOutput( ns("read_oms_1"))),

      h2("Gráfico del Peso para la edad"),
      plotOutput(NS(id, "plot_2")),
      DTOutput(NS(id,"table_oms_2")),
      h4(textOutput(NS(id,"read_oms_2"))),

      h2("Gráfico del IMC para la edad"),
      plotOutput(NS(id, "plot_3")),
      DTOutput(NS(id,"table_oms_3")),
      h4(textOutput(NS(id,"read_oms_3"))),
      
      h2("Gráfico de la Circunferencia del Brazo para la Edad"),
      plotOutput( ns( "plot_4")),
      DTOutput( ns("table_oms_4")),
      h4(textOutput( ns("read_oms_4"))),
      
      h2("Gráfico de la Circunferencia de la Cabeza para la Edad"),
      plotOutput( ns( "plot_5")),
      DTOutput( ns("table_oms_5")),
      h4(textOutput( ns("read_oms_5"))),
      
      h2("Gráfico del Peso para la Estatura/Longitud"),
      plotOutput( ns( "plot_6")),
     ),
     tabPanel(
       "Estado Nutricional",
       h2(strong("Clasificación de Waterlow-Rutishauser para menores de 10 años")),
       h4(textOutput(ns("waterlow"))),
              
              ),
     tabPanel("Dieta"))
     
     )
  ))
}


# Function UI input for RN of 0 to 13 weeks ========
sliders_0_13 <- function(id) {
  ns <- NS(id)
  tagList(
    
    sidebarLayout(
      sidebarPanel(
        
        numericInput( ns( "week"), "Ingrese las Semanas de Nacimiento",
                      value=0, min = 0, max = 13 ),

        numericInput( ns( "height_week"), "Estatura en cm", value=0, 
                      min = 0 ),
        numericInput( ns( "weight_week"), "Peso en kg", value = 0,
                      min=0),
        numericInput( ns( "head_week"), "Circunferencia de la Cabeza en cm",
                      value=0, min=0 ),
        selectInput( ns( "sex_week"), label = "Sexo del paciente",
                     choices = c("Hombre", "Mujer")),
        selectInput( ns( "distribution_week"),label = "Elija la tabla",
                     choices = c( "Z-score","Percentil")),
        actionButton( ns( "click_0_13"), label = "Click Aquí!"),
        
      ),
      mainPanel(
        h2("Gráfico de la Longitud para la Edad"),
        plotOutput( ns( "plot_1")),
        #uiOutput(NS(id, "plot_1_ui")),
        DTOutput( ns("table_week_1")), 
        h4(textOutput( ns("read_week_1"))),
        
        h2("Gráfico del IMC para la edad"),
        plotOutput( ns( "plot_2")),
        DTOutput( ns("table_week_2")),
        h4(textOutput( ns("read_week_2"))),
        
        h2("Gráfico del Peso para la Edad"),
        plotOutput( ns( "plot_3")),
        DTOutput( ns("table_week_3")),
        h4(textOutput( ns("read_week_3"))),
        
        h2("Gráfico de la Circunferencia de la Cabeza para la Edad"),
        plotOutput( ns( "plot_4")),
        DTOutput( ns("table_week_4")),
        h4(textOutput( ns("read_week_4"))),
      )
    ))
}

# Function UI input for PREGNATION of 10 to 42 weeks ========
sliders_pregnation <- function(id) {
  ns <- NS(id)
  tagList(
    
    sidebarLayout(
      sidebarPanel(
        
        numericInput( ns("week_pregnation"), "Semanas de Embarazo",
                      value=10 ,min = 10, max=42 ),

        numericInput( ns( "height_pregnation"), "Estatura en cm", value=0 ),
        numericInput( ns( "weight_pregnation"), "Peso en kg", value = 0),
        
        selectInput( ns("autor"),label = "Segun que Tabla estandar",
                     choices = c( "Chile 2019","Atalah")),
        actionButton( ns( "click_pregnation"), label = "Click Aquí!"),
        
      ),
      mainPanel(
        h2("Gráfico del Indice de Masa Corporal por semanas de gestación"),
        plotOutput( ns( "plot_1_gestacion")),
      
        DTOutput( ns("table_week_1")), 
        h4(textOutput( ns("read_pregnation"))),
        
        
      )
    ))
}

# Function UI input for PREMATURE of 22 to 50 weeks ========
sliders_premature <- function(id) {
  ns <- NS(id)
  tagList(
     sidebarLayout(
      sidebarPanel(
        
        numericInput( ns( "week_premature"), "Ingrese las Semanas de Nacimiento",
                      value=22, min = 22, max = 50 ),
        numericInput( ns( "height_premature"), "Longitud en cm (Ejm:34.54)", value=0, 
                      min = 0 ),
        numericInput( ns( "weight_premature"), "Peso en kg (Ejm:2.45)", value = 0,
                      min = 0),
        numericInput( ns( "head_premature"), "Circunferencia de la Cabeza en cm (Ejm:35.44)",
                      value = 0, min = 0 ),
        selectInput( ns( "sex_premature"), label = "Seleccione el sexo",
                     choices = c("Hombre", "Mujer")),
        actionButton( ns( "click_premature"), label = "Click para resultado"),
        
      ),
      mainPanel(
        h2("Gráfico de la Longitud para la Edad"),
        plotOutput( ns( "plot_1_premature")),
        DTOutput( ns("table_premature_1")), 
        h4(textOutput( ns("read_premature_1"))),
        
        h2("Gráfico del Peso para la edad"),
        plotOutput( ns( "plot_2_premature")),
        DTOutput( ns("table_premature_2")),
        h4(textOutput( ns("read_premature_2"))),
        
        h2("Gráfico de la Circunferencia de la cabeza para la Edad"),
        plotOutput( ns( "plot_3_premature")),
        DTOutput( ns("table_premature_3")),
        h4(textOutput( ns("read_premature_3") ))
      )
    ))
}

# ID_week  <- c( "week", "weight_week", "height_week", "head_week") 
# label_week <- c( "Semanas de Nacimiento", "Peso en kg (Ejm: 7.5)",
#               "Longitud/Estatura en cm", "Perimetro Cefalico en cm")
# mySliderInput_week <- function( ID_week , label_week ){
#   numericInput( ID_week, label_week, value=0)
#   }
# input_0_13_week <- pmap( list( ID_week, label_week), mySliderInput_week )
# 
# ## Add more variables for weeks of neonate
# sliders_0_13 = list(
#   dateInput(inputId = "calendar_week", label = "Fecha de Nacimiento", 
#             value = NULL, language = "es"),
#   input_0_13_week,
#   selectInput(inputId = "sex_week", label = "Sexo del paciente", 
#               choices = c("Hombre", "Mujer")),
#   selectInput(inputId = "distribution_week",label = "Eliga la tabla", 
#               choices = c( "Z-score","Percentil")),
#   actionButton( inputId = "click_0_13_week", label = "Click Aquí!"))
