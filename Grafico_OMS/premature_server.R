
load("Data_app/fenton_2003.Rda")

prematureServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Seccion Input ==========    
    height_premature = eventReactive( input$click_premature, { round( input$height_premature, 2 )  } )
    weight_premature = eventReactive( input$click_premature, { round( input$weight_premature, 2 )  } )
    head_premature = eventReactive( input$click_premature, { round( input$head_premature, 2 )  } )
    bmi_premature = eventReactive( input$click_premature,
                              { bmi_premature = round( weight_premature()/(height_premature()/100)^2, 2 ) })
    
    sex_premature = eventReactive( input$click_premature,{
      if (input$sex_premature=="Hombre") {
        sex_premature = "Boy"
      } else {sex_premature = "Girl"}    })
    
    
    date_premature = eventReactive( input$click_premature, 
                                         {input$week_premature} )
      
    # Seccion Grafico ==========
    graf_fun = function(datos, x, y, sex, variable, x_input, y_input, y_label){
      datos %>% 
        filter( Sex == sex & 
                  Variable == variable ) %>% 
        select(x,y, Desviacion, Variable, Valor ) %>% 
        ggplot( aes( x=.data[[x]], y = .data[[y]], col=Desviacion)) + 
        geom_line() +
          geom_point(x=x_input, y=y_input, col="black", size=2) +
          labs(x="Semanas", y=y_label )
    }
    
    output$plot_1_premature = renderPlot( {
      if( height_premature() > 0){
        graf_fun( datos = premature, x = "Week", y = "Valor", 
                  variable = "length",  x_input = date_premature(),
                  y_input = height_premature(), sex = sex_premature(), 
                  y_label= "Estatura (cm)")
      } 
    })
    
    output$plot_2_premature = renderPlot( {
      if( weight_premature() > 0){
        graf_fun( datos = premature, x = "Week", y = "Valor",
                  variable = "weight", x_input = date_premature(), 
                  y_input = weight_premature(), sex = sex_premature(),
                  y_label= "Peso(Kg)")
      }
    })
    
    output$plot_3_premature = renderPlot( {
      if( head_premature() > 0){
        graf_fun( datos = premature, x = "Week", y = "Valor",
                  variable = "head_circ", x_input = date_premature(), 
                  y_input = head_premature(), sex = sex_premature(),
                  y_label= "Peso(Kg)")
      }
    })
    
    
    # Seccion Tabla ==========
    tablaPremature <- eventReactive(input$click_premature, {
      tablaPremature <- tidyr::spread( premature, key = "Desviacion",
                              value = "Valor")
      tablaPremature <- tablaPremature[ tablaPremature$Week == date_premature() &
                                          tablaPremature$Sex == sex_premature(),  
                                        c(1:3,10,9,8,7,6,5,4)]
    } )  
    
    
    table_1 <- eventReactive(input$click_premature, {
      tablaPremature() %>%  filter(Variable=="length") %>% 
        select(!"Variable")  
    })
    
    table_2 <- eventReactive(input$click_premature, {
      tablaPremature() %>%  filter(Variable=="weight") %>% 
        select(!"Variable") })
    
    table_3 <- eventReactive(input$click_premature, {
      tablaPremature() %>% filter(Variable=="head_circ") %>% 
        select(!"Variable")  })
    
  ## Show table
    output$table_premature_1 = renderDT({
      datatable(
        table_1(),
        rownames = FALSE,
        options = list(dom = 't') )
    }  )
   
    output$table_premature_2 = renderDT({
      datatable(
        table_2(),
        rownames = FALSE,
        options = list(dom = 't') )
    }  )
    
    output$table_premature_3 = renderDT({
      datatable(
        table_3(),
        rownames = FALSE,
        options = list(dom = 't') )
      }  )
    
    # Seccion Read =================
    # readFuncPremture = function( variable, tabla){
    #   
    #   if ( variable <= tabla[[3]]) {
    #       text_oms_1 = "El valor se encuentra dentro de -3 SD" 
    #       return(text_oms_1)}
    #     
    #     else if ( variable > tabla[[3]] & variable <= tabla[[4]] )  {
    #       text_oms_2 ="El valor se encuentra dentro de -2 SD" 
    #       return(text_oms_2)}
    #     
    #     else if ( variable > tabla[[4]] & variable < tabla[[5]])  {
    #       text_oms_3 = "El valor se encuentra dentro de -1 SD" 
    #       return(text_oms_3)}
    #     
    #     else if ( variable > tabla[[5]] & variable < tabla[[7]] )  {
    #       text_oms_4 = "El valor se encuentra dentro de la Mediana" 
    #       return(text_oms_4)}
    #     
    #     else if ( variable >= tabla[[7]] & variable < tabla[[8]] )  {
    #       text_oms_5 = "El valor se encuentra dentro de +1 SD" 
    #       return(text_oms_5)}
    #     
    #     else if ( variable >= tabla[[8]] & variable < tabla[[9]] )  {
    #       text_oms_6 = "El valor se encuentra dentro de +2 SD" 
    #       return(text_oms_6)}
    #     
    #     else  {
    #       text_oms_7 = "El valor se encuentra dentro de +3 SD" 
    #       return(text_oms_7)}
    #     }
        

    output$read_premature_1 <- renderText({

      read_function( tabla = table_1(), 
                        variable = height_premature() )
    })
    
    output$read_premature_2 <- renderText({

      read_function( tabla = table_2(), 
                        variable = weight_premature() )
    })

    output$read_premature_3 <- renderText({

      read_function( tabla = table_3(),
                        variable = head_premature() )
    })


    
  })
}

