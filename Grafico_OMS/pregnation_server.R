load("Data_app/pregnation_weeks.Rda")

pregnationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Seccion Input ==========    
    week_pregnation = eventReactive( input$click_pregnation,{ input$week_pregnation})
    height_pregnation = eventReactive( input$click_pregnation, { round( 
      input$height_pregnation, 1 )  } )
    weight_pregnation = eventReactive( input$click_pregnation, { round( 
      input$weight_pregnation, 1 )  } )
    bmi_pregnation = eventReactive( input$click_pregnation,
                              { bmi_pregnation = round( 
                                weight_pregnation()/(height_pregnation()/100)^2, 1) })
    
    autor = eventReactive( input$click_pregnation, {
      if (input$autor =="Chile 2019") {
        autor = "Chile_2019"
      } else {autor = "Atalah"}   })
    
    
    # Seccion Grafico ==========

    output$plot_1_gestacion = renderPlot( {
      ggplot( pregnation[ pregnation$Autor == autor(), ], 
              aes(x= Week, y= IMC, col=`Estado Nutricional` )) + 
        geom_line(size=1) +
        geom_point( aes( x = week_pregnation(), 
                         y = bmi_pregnation()), col="black", size=2)+
        annotate( "text", label = "Bajo Peso", x = 26, y = 19.5, size = 5, 
                  colour = "red") +
        annotate( "text", label = "Peso Normal", x = 20, y = 24, size = 5, 
                  colour = "green")+
        annotate( "text", label = "Sobrepeso", x = 18, y = 28, size = 5, 
                  colour = "Orange")+
        annotate( "text", label = "Obesidad", x = 16, y = 32, size = 5, 
                  colour = "red") + 
        labs(x="Semanas De Gestacion", 
             y_label = "Indice de Masa Corporal (kg/m2)" )
    })
    
    
    
# Seccion Interpretation ===========
   
    observeEvent(input$click_pregnation,
    output$read_pregnation <- renderText({  
      
      
      tablePregnation<- pregnation %>% 
        filter( pregnation$Autor == autor() ) %>% 
        spread("Estado Nutricional", IMC)
      
      
      if ( bmi_pregnation() >= tablePregnation[ 
        tablePregnation$Week==week_pregnation(), 3]  ) {
        text_pregnation = paste("La gestante tiene OBESIDAD")
        
      } else if ( bmi_pregnation() < tablePregnation[ 
        tablePregnation$Week == week_pregnation(), 3] & 
        bmi_pregnation() >= tablePregnation[ 
                    tablePregnation$Week == week_pregnation(), 4]  ) {
        text_pregnation =paste("La gestante tiene SOBREPESO")
        
      } else if ( bmi_pregnation() < tablePregnation[ 
        tablePregnation$Week == week_pregnation() , 4] &
        bmi_pregnation() > tablePregnation[ 
                    tablePregnation$Week == week_pregnation(), 5]  ) {
        text_pregnation =paste("La gestante tiene peso NORMAL" )
        
      } else  {
        text_pregnation =paste("La gestante tiene BAJO PESO" )
      }
})
    )
      
      
      
  })
}

