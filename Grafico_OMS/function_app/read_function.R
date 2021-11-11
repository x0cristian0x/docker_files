read_function = function(distribution="z", variable, tabla ){

  if (distribution == "z" ) {
    if ( variable <= tabla[[3]]) {
      texto_read = "El valor se encuentra dentro de -3 SD"
    } else if ( variable > tabla[[3]] & variable <= tabla[[4]] )  {
      texto_read ="El valor se encuentra dentro de -2 SD" 
    } else if ( variable > tabla[[4]] & variable < tabla[[5]])  {
      texto_read = "El valor se encuentra dentro de -1 SD" 
    } else if ( variable > tabla[[5]] & variable < tabla[[7]] )  {
      texto_read = "El valor se encuentra dentro de la Mediana" 
    } else if ( variable >= tabla[[7]] & variable < tabla[[8]] )  {
      texto_read = "El valor se encuentra dentro de +1 SD" 
    } else if ( variable >= tabla[[8]] & variable < tabla[[9]] )  {
      texto_read = "El valor se encuentra dentro de +2 SD" 
    } else{
      texto_read = "El valor se encuentra dentro de +3 SD" 
    }
  }else{
    if ( variable <= tabla[[3]]) {
      texto_read = "El valor se encuentra dentro de 3 percentil" 
    } else if ( variable > tabla[[3]] & variable <= tabla[[4]] )  {
      texto_read = "El valor se encuentra dentro de 15 percentil" 
    } else if ( variable > tabla[[4]] & variable < tabla[[6]])  {
      texto_read = "El valor se encuentra dentro de la Mediana" 
    } else if ( variable >= tabla[[6]] & variable < tabla[[7]] )  {
      texto_read = "El valor se encuentra dentro de 85 percentil"
    } else {
      texto_read = "El valor se encuentra dentro de 97 percentil" 
    }
  }
  
  
} 