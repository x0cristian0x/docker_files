
library(shiny)
# Grafica de MPG ====================================================
shinyUI(fluidPage(

    titlePanel(" Plot mpg and predict"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("MPG", "Millas por galon", min=10, max = 35, value = 20),
            checkboxInput("Mostrar_1", "Mostrar modelo 1", value = T),
            checkboxInput("Mostrar_2", "Mostrar modelo 2", value = T),
            submitButton("Enviar")
        ),

        mainPanel(
            h4("modelo 1"),
            plotOutput("plot_1"),
            h4("prediccion 1"),
            textOutput("pred_1"),
            h4("prediccion 2"),
            textOutput("pred_2")
        )
    )
))


