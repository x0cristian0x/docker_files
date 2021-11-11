load("Data_app/arm_3m_5y.Rda")
load("Data_app/BMI_age_0_19.Rda")
load("Data_app/head_age_0_5.Rda")
load("Data_app/length_age_0_19.Rda")
load("Data_app/weight_length_0_5.Rda")
load("Data_app/weight_age_0_10.Rda")

omsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
# Seccion Input ==========    
    height = eventReactive( input$click_0_19, { round( input$height, 1 )  } )
    weight = eventReactive( input$click_0_19, { round( input$weight, 1 )  } )
    arm = eventReactive( input$click_0_19, { round( input$arm, 1 )  } )
    head = eventReactive( input$click_0_19, { round( input$head, 1 )  } )
    bmi = eventReactive( input$click_0_19,
                         { bmi = round( weight()/(height()/100)^2, 1 ) })
    
    sex = eventReactive( input$click_0_19,{
              if (input$sex=="Hombre") {
                  sex = "Boy"
                  } else {sex = "Girl"}    })

    distribution = eventReactive( input$click_0_19, {
              if (input$distribution =="Z-score") {
                        distribution = "z"
              } else {distribution = "p"}   })

    date_born = eventReactive( input$click_0_19, {
            if ( input$months >0  ) {
                date_born = input$months
              } else {
                entrada = as_date(input$calendar )
                hoy = as_date(Sys.time())
                x <- hoy - entrada
                date_born = round( time_length( x , unit = "month"))
              }  }           )

# Seccion Grafico ==========
    
    output$plot_1 = renderPlot( {
      if( height() > 0){
        graf_fun( datos = length_age_0_19, x = "Months", y = "Length",
                 x_input = date_born(), y_input = height(), sex = sex(),
                 distribution = distribution(), x_label="Meses",
                 y_label= "Estatura (cm)" )
        }  })
    
    output$plot_2 = renderPlot( {
      if( weight() > 0){
        graf_fun( datos = weight_age_0_10, x = "Months", y = "Weight",
                  x_input = date_born(), y_input = weight(), sex = sex(),
                  distribution = distribution(), x_label="Meses",
                  y_label= " Peso (kg)" )
      }})
    
    output$plot_3 = renderPlot( {
      if( height() > 0 & weight()>0) {
        graf_fun( datos = BMI_age_0_19, x = "Months", y = "BMI",
                  x_input = date_born(), y_input = bmi(), sex = sex(),
                  distribution = distribution(), x_label="Meses",
                  y_label= "Indice de Masa Corporal (kg/m2)" )
            }  })

        output$plot_4 = renderPlot( {
          if( arm() >=3 ){
            graf_fun( datos = arm_3m_5y, x = "Months", y = "Arm",
                      x_input = date_born(), y_input = arm(), sex = sex(),
                      distribution = distribution(), x_label="Meses",
                      y_label= "Perimetro Medio Braquial (cm)" )
          }   })

        output$plot_5 = renderPlot({
          if ( head() > 0){
            graf_fun( datos = head_age_0_5, x = "Months", y = "Head",
                      x_input = date_born(), y_input = head(), sex = sex(),
                      distribution = distribution(), x_label="Meses",
                      y_label= "Circunferencia De La Cabeza (cm)")
          }  })

    output$plot_6 = renderPlot({
      if( height()> 0 & weight() > 0) {
        if (date_born()<=24) {
          ggplot( weight_length_0_5[ weight_length_0_5$Sex==sex() &
                                     weight_length_0_5$Distribution == distribution() &
                                     weight_length_0_5$Table==1, ],
                  aes(x= cm, y= Weight, col = Desviacion)) +
            geom_line() +
            geom_point(mapping = aes(x= height(), y = weight()),
                       col="black", size=2) +
            labs( x="Estatura (cm)", y="Peso (kg)")

              } else{
                ggplot( weight_length_0_5[ weight_length_0_5$Sex==sex() &
                                             weight_length_0_5$Distribution == distribution() &
                                             weight_length_0_5$Table==2, ],
                        aes(x= cm, y= Weight, col = Desviacion)) +
                  geom_line() +
                  geom_point(mapping = aes(x=height(), y = weight()),
                             col="black", size=2)+
                  labs( x="Estatura (cm)", y="Peso (kg)")
            }
                }   })

# Seccion Tabla ==========

        table_1 <- eventReactive(input$click_0_19, {
          table_function(datos = length_age_0_19, "Length",
                         distribution=distribution(), sex=sex(), 
                         birth=date_born()  )  
          })
          
        table_2 <- eventReactive(input$click_0_19, {
          table_function(datos = weight_age_0_10, "Weight",
                         distribution=distribution(), sex=sex(), 
                         birth = date_born()  )  })
        
        table_3 <- eventReactive(input$click_0_19, {
          table_function(datos = BMI_age_0_19, "BMI",
                         distribution=distribution(), sex=sex(), 
                         birth=date_born()  )  })
        
        table_4 <- eventReactive(input$click_0_19, {
          table_function(datos = arm_3m_5y, "Arm",
                         distribution=distribution(), sex=sex(), 
                         birth=date_born()  )  })
        
        table_5 <- eventReactive(input$click_0_19, {
          table_function(datos = head_age_0_5, "Head",
                         distribution=distribution(), sex=sex(), 
                         birth=date_born()  )  })
        

        output$table_oms_1 = renderDT({ 
          datatable(
            table_1(),
            rownames = FALSE,
            options = list(dom = 't',
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#900C3F',
                             'color': '#fff'});","}") ))
                }  )
        output$table_oms_2 = renderDT({ 
          datatable(
            table_2(),
            rownames = FALSE,
            options = list(dom = 't',
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#900C3F',
                             'color': '#fff'});","}") ))
        })
        output$table_oms_3 = renderDT({ 
          datatable(
            table_3(),
            rownames = FALSE,
            options = list(dom = 't',
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#900C3F',
                             'color': '#fff'});","}") ))
          })
        output$table_oms_4 = renderDT({ 
          datatable(
            table_4(),
            rownames = FALSE,
            options = list(dom = 't',
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#900C3F',
                             'color': '#fff'});","}") ))})
        output$table_oms_5 = renderDT({ datatable(
          table_5(),
          rownames = FALSE,
          options = list(dom = 't',
                         initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#900C3F',
                             'color': '#fff'});","}") )) })
        
            
# Seccion Interpretation ===========
        
         output$read_oms_1 <- renderText({  
           
          read_function(distribution= distribution(), variable= height(),
                         tabla= table_1() )
        })
        
        output$read_oms_2 <- renderText({  
          
          read_function(distribution = distribution(), variable = weight(),
                        tabla = table_2() )
        })
        
        output$read_oms_3 <- renderText({  
          
          read_function(distribution = distribution(), variable = bmi(),
                        tabla = table_3() )
        })
        
        output$read_oms_4 <- renderText({  
          
          read_function(distribution = distribution(), variable = arm(),
                        tabla = table_4() )
        })
        
        output$read_oms_5 <- renderText({  
          
          read_function(distribution = distribution(), variable = head(),
                        tabla = table_5() )
        })
        
        # APARTADO Nutricion =====================   
        ## Clasificación de Waterlow-Rutishauser
        
        
        output$waterlow <- renderText({
          if (date_born()<= 120) {
            cronico= round( (height() / table_1()[["Median"]]) *100, 1 )
            agudo  = round( (weight() / table_2()[["Median"]]) *100, 1 )
            
            if (cronico >90 & agudo < 80) {
              waterlow = "Desnutrido agudo (emaciado)"
            } else if(cronico <90 & agudo <80){
              waterlow = "Desnutrido crónico agudizado (emaciado y acortado)"
            } else if (cronico <90& agudo >80){
              waterlow = "Desnutrido crónico (acortado)"
            } else {waterlow = "Normal"}  
          } else{ waterlow= "La clasificación es para menores de 10 años"}
          })        
        
    
  })}




  