load("Data_app/BMI_weeks.Rda")
load("Data_app/head_weeks.Rda")
load("Data_app/length_weeks.Rda")
load("Data_app/weight_weeks.Rda")

weekServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Seccion Input ==========    
    height_week = eventReactive( input$click_0_13, { round( input$height_week, 1 )  } )
    weight_week = eventReactive( input$click_0_13, { round( input$weight_week, 1 )  } )
    head_week = eventReactive( input$click_0_13, { round( input$head_week, 1 )  } )
    bmi_week = eventReactive( input$click_0_13,
                         { bmi_week = round( weight_week()/(height_week()/100)^2, 1 ) })
    
    sex_week = eventReactive( input$click_0_13,{
      if (input$sex_week=="Hombre") {
        sex_week = "Boy"
      } else {sex_week = "Girl"}    })
    
    distribution_week = eventReactive( input$click_0_13, {
      if (input$distribution_week =="Z-score") {
        distribution_week = "z"
      } else {distribution_week = "p"}   })
    
    date_born_week = eventReactive( input$click_0_13, {input$week})

    
    # Seccion Grafico ==========
    
    output$plot_1 = renderPlot( {
      if( height_week() > 0){
        graf_fun( datos = length_weeks, x = "Weeks", y = "Length",
                  x_input = date_born_week(), y_input = height_week(),
                  sex = sex_week(),
                  distribution = distribution_week(),
                  x_label="Semanas", y_label= "Estatura (cm)")
      } 
    })
    
    output$plot_2 = renderPlot( {
      if( bmi_week() > 0){
        graf_fun( datos = BMI_weeks, x = "Weeks", y = "BMI",
                  x_input = date_born_week(), y_input = bmi_week(), 
                  sex = sex_week(),
                  distribution = distribution_week(),
                  x_label="Semanas",
                  y_label= "Indice de Masa Corporal (Kg/m2)")
      }
    })
    
    output$plot_3 = renderPlot( {
      if( weight_week() > 0){
        graf_fun( datos = weight_weeks, x = "Weeks", y = "Weight",
                  x_input = date_born_week(), y_input = weight_week(), 
                  sex = sex_week(),
                  distribution = distribution_week(),
                  x_label="Semanas", y_label= "Peso (kg)")
      }
    })
    
    output$plot_4 = renderPlot({
      if ( head_week() > 0){
        graf_fun( datos = head_weeks, x = "Weeks", y = "Head",
                  x_input = date_born_week(), y_input = head_week(), 
                  sex = sex_week(),
                  distribution = distribution_week(),
                  x_label="Semanas",
                  y_label= "Circunferencia De La Cabeza (cm)")
      }
    })
    
    # Seccion Tabla ==========

    table_1 <- eventReactive(input$click_0_13, {
      table_function(datos = length_weeks, "Length",
                     distribution = distribution_week(), 
                     sex = sex_week(), birth = date_born_week()  
                     )  })
    
    table_2 <- eventReactive(input$click_0_13, {
      table_function(datos = BMI_weeks, "BMI",
                     distribution = distribution_week(), 
                     sex = sex_week(), birth = date_born_week()
                     )  })
    
    table_3 <- eventReactive(input$click_0_13, {
      table_function(datos = weight_weeks, "Weight",
                     distribution = distribution_week(), 
                     sex = sex_week(), birth = date_born_week()
                     )  })
    
    table_4 <- eventReactive(input$click_0_13, {
      table_function(datos = head_weeks, "Head",
                     distribution = distribution_week(), 
                     sex = sex_week(), birth = date_born_week()
                     )  })
    
    
    output$table_week_1 = renderDT({ 
      datatable(
        table_1(),
        rownames = FALSE,
        options = list(dom = 't') )
    }  )
    output$table_week_2 = renderDT({ 
      datatable(
        table_2(),
        rownames = FALSE,
        options = list(dom = 't') )
    })
    output$table_week_3 = renderDT({ 
      datatable(
        table_3(),
        rownames = FALSE,
        options = list(dom = 't') )
      })
    output$table_week_4 = renderDT({ datatable(
      table_4(),
      rownames = FALSE,
      options = list(dom = 't') ) 
      })
    
    
    # Seccion Interpretation ===========

    
    output$read_week_1 <- renderText({  
      
      read_function(distribution= distribution_week(),
                    variable= height_week(), tabla= table_1() )
    })
    
    output$read_week_2 <- renderText({  
      
      read_function(distribution = distribution_week(), 
                    variable = bmi_week(), tabla = table_2() )
    })
    
    output$read_week_3 <- renderText({  
      
      read_function(distribution = distribution_week(), 
                    variable = weight_week(), tabla = table_3() )
    })
    
    output$read_week_4 <- renderText({  
      
      read_function(distribution = distribution_week(),
                    variable = head_week(), tabla = table_4() )
    })
    
  })
  }

