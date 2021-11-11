table_function = function(datos, variable, distribution, birth, sex){
  
  if( distribution == "z" ) {
    tabla <- tidyr::spread( datos, key = "Desviacion",
                            value = variable)
    tabla <- tabla[ tabla[ ,1] == birth &
                      tabla[,"Sex"]== sex, c(1,2,14,11,10,9,8,7,5)]
    tabla <- tabla[complete.cases(tabla),]
  } else{
    tabla <- tidyr::spread( datos, key = "Desviacion",
                            value = variable )
    tabla <- tabla[ tabla[ ,1] == birth &
                      tabla[, "Sex"] == sex, c(1,2,13,12,9,6,4)]
    tabla <- tabla[complete.cases(tabla),]
  }  }