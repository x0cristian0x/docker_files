graf_fun = function(datos, x, y, x_input, y_input, sex, distribution,
                    x_label, y_label){
  
  datos %>% 
    filter( Sex == sex & 
              Distribution == distribution ) %>% 
    select(x, y, Desviacion) %>% 
    ggplot( aes( x=.data[[x]], y = .data[[y]], col=Desviacion)) + 
    geom_line()+
    geom_point(x=x_input, y=y_input, col="black", size=2) +
    labs(x=x_label , y = y_label)
}