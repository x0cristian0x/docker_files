library(shiny)
# Grafica de MPG ====================================================
shinyServer(function(input, output) {
    mtcars$mpgsp <- ifelse(mtcars$mpg - 20 > 0, mtcars$mpg - 20, 0)
    modelo_1 = lm(hp~ mpg, mtcars)
    modelo_2 = lm(hp~ mpg+ mpgsp, mtcars)

    model_pred_1 = reactive({
        mpg_x = input$MPG
        predict(modelo_1,  newdata= data.frame(mpg = mpg_x) )}
                                                  )
    model_pred_2 = reactive({
        mpg_x = input$MPG
        predict(modelo_2,  data.frame(mpg = mpg_x,
                                      mpgsp = ifelse(mpg_x - 20 > 0,
                                                     mpg_x - 20, 0)) )}
     )

     output$plot_1 = renderPlot( {
        mpg_x = input$MPG

        plot(mtcars$mpg, mtcars$hp, main="mpg y disp")
         if (input$Mostrar_1) {
             abline(modelo_1, col ="red", lw = 2)
         }
         if (input$Mostrar_2) {
             modelo_2_lines = predict(modelo_2, newdata = data.frame(
                 mpg = 10:35, mpgsp = ifelse(10:35 - 20 > 0, 10:35 - 20, 0)))
             lines(10:35, modelo_2_lines, col = "blue", lwd = 2)
         }
        legend("topright", c("Mod 1", "Mod 2 "), pch = 16, col = c("red", "blue"))
        points(mpg_x,  model_pred_1(), col="gray", cex=2, pch=19)
        points(mpg_x, model_pred_2(), col="yellow", cex=2, pch=19)

         })

     output$pred_1 = renderText( {
         model_pred_1()
     })
     output$pred_2 = renderText( {
         model_pred_2()
     })
    })

