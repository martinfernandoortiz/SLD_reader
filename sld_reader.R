library(tidyverse)
library(stringr)
library(readr)



####### Rules Count
#### La iteración principal del código se hace según la cantidad de reglas (rules) que tenga el sld

x1 <- c()
escala <- c()
y1 <- c()
df <- data.frame(x1,escala,y1)
rm(x1,escala,y1)
#fx_cantidadReglas <- function(x){
#  rule_count <-  x %>% str_locate_all("<se:Rule>")
  #a <-  rule_count %>% as_vector() 
  #ini <- a[1]
  #fin <- a[2]
#  rule_extract <- str_sub(edificio_salud,a[1],a[2])
#  return(rule_extract)
#}

fx_extraerComentarios <- function(x){
  if (str_detect(x,"<!--")== TRUE){
    print("Hay comentarios")
    comentarioIni <- as_vector(str_locate(x,"<!--"))[1]
    comentarioFin <- as_vector(str_locate(x,"-->"))[2]
    rule_extract1 <- str_sub(x,comentarioIni,comentarioFin )
    rule_extract <- str_remove(x,rule_extract1)
    
    #rule_extract <- str_sub(edificio_salud,a[1],a[2])
    return(rule_extract)
  }else{ print("No hay Comentarios")
    rule_extract <- x
    return(rule_extract)}
}

fx_escalaMinima <-  function(x){
  str_detect(x,"<se:MinScaleDenominator>") #Agregar if true
  
  MinScaleDenominator_ini <-as_vector(x %>%
                                        str_locate("<se:MinScaleDenominator>"))
  
  MinScaleDenominator_end <- as_vector(x %>%
                                         str_locate("</se:MinScaleDenominator>"))
  
  MinScaleDenominator <- as.numeric(str_sub(x,
                                            MinScaleDenominator_ini[2]+1,
                                            MinScaleDenominator_end[1]-1))
  
  rm(MinScaleDenominator_ini,MinScaleDenominator_end)
  return(MinScaleDenominator)
}
fx_escalaMaxima <-  function(x){  
  str_detect(rule_extract,"<se:MaxScaleDenominator>") #Agregar if true
  
  MaxScaleDenominator_ini <-as_vector(x %>%
                                        str_locate("<se:MaxScaleDenominator>"))
  
  MaxScaleDenominator_end <- as_vector(x %>%
                                         str_locate("</se:MaxScaleDenominator>"))
  
  MaxScaleDenominator <- as.numeric(str_sub(x,
                                            MaxScaleDenominator_ini[2]+1,
                                            MaxScaleDenominator_end[1]-1))
  
  rm(MaxScaleDenominator_ini,MaxScaleDenominator_end)
  return(MaxScaleDenominator)
  
}
fx_escalaMinimaRepresentacion <- function(y){
  for (x in 1:19) {
    #print(x)
    # if condition with break
    if(y > escalas[x]) {
      minimo <- x- 1
      break
    }
    
  }
  return(minimo)
}
fx_escalaMaximaRepresentacion <- function(y){
  for (x in 1:19) {
    print(x)
    # if condition with break
    if(escalaMaxima > escalas[x]) {
      maximo <- x
      break
      
    }
  }
  return(maximo)
}
fx_simboloExtract <- function(x){
  
  if (str_detect(x,"<se:PointSymbolizer>")== TRUE){
    
    
    simboloIni <- as_vector(str_locate(x,"<se:PointSymbolizer>"))[1]
    simboloFin <- as_vector(str_locate(x,"</se:PointSymbolizer>"))[2]
    simbolo_extrac <- str_sub(x,simboloIni,simboloFin )
    tipo <- "point"
    salida <- c(simbolo_extrac,tipo)
    return(salida)
    a <-  tipo_simbolo %>% as_v
  }
}





#Lectura de archivos

zoomLevel <- read_table("zoomLevel.txt")
edificio_salud <-  read_file("D:/martin/ign-argentina/github/estilos-sld/infraestructura-social/edificio_salud.sld")
escalas <-  as_vector (zoomLevel$Escala)





#Cantidad de Reglas
rule_count <- length (as_vector(edificio_salud %>% str_locate_all("<se:Rule>"))) 


rule_count <-  edificio_salud %>% str_locate_all("<se:Rule>")
a <-  rule_count %>% as_vector() 
iteraciones <- length(a)/2

for (x in 1:iteraciones) {
 print(2)

  ini <- a[1]
  fin <- a[2]
  rule_extract <- str_sub(edificio_salud,a[1],a[2])
  a <- a[-c(1,8)]


#Extracto de SLD
#rule_extract <- fx_cantidadReglas(edificio_salud)

#Eliminar comentarios
rule_extract <-fx_extraerComentarios(rule_extract)


 #Nombre Regla 
 name_ini <-as_vector(rule_extract %>% str_locate("<se:Name>"))
 name_end <- as_vector(rule_extract %>% str_locate("</se:Name>"))
 name_rule <- str_sub(rule_extract,name_ini[2]+1,name_end[1]-1)
 
 rm(name_ini,name_end)
 
 #Escalas
 #MinScaleDenominator es el zoom maximo
 #MaxScaleDenominator es el zoom mínimo
 #Si no está el mínimo el zoom va hasta el máximo
 #Si no está el máximo el zoom va del mínimo al máximo
 #Si no están van en todos los niveles
 
 
 if( str_detect(rule_extract,"<se:MinScaleDenominator>") == TRUE &
     str_detect(rule_extract,"<se:MaxScaleDenominator>")== TRUE) {
  escalaMinima <-  fx_escalaMinima(rule_extract)
  escalaMaxima <-  fx_escalaMaxima(rule_extract)
  escalaMinimaRepresentacion <-  fx_escalaMinimaRepresentacion(escalaMinima)
  escalaMaximaRepresentacion <-  fx_escalaMaximaRepresentacion (escalaMaxima)
  
  
  #Simbologia Estilo
  
  rule_extract <-  fx_simboloExtract(rule_extract)
  rule_extract1 <-rule_extract[1] 
  

  #Creacion Tabla
  
  escala <- c(escalaMaximaRepresentacion:escalaMinimaRepresentacion)
  repeticiones <-  escalaMinimaRepresentacion-escalaMaximaRepresentacion+1
  
  
  x1<-c(rep(c(name_rule),each=repeticiones))
  y1 <-c(rep(c(rule_extract1),each=repeticiones))
  
  df1 <- data.frame(x1,escala,y1) 
  
 } else if ( str_detect(rule_extract,"<se:MaxScaleDenominator>") == TRUE &
             str_detect(rule_extract,"<se:MinScaleDenominator>")== FALSE){
   escalaMinima <-  1066
   escalaMaxima <-  fx_escalaMaxima(rule_extract)
   escalaMinimaRepresentacion <-  19
   escalaMaximaRepresentacion <-  fx_escalaMaximaRepresentacion (escalaMaxima)
   
   rule_extract <-  fx_simboloExtract(rule_extract)
   rule_extract1 <-rule_extract[1] 

   #Creacion Tabla
   
   escala <- c(escalaMaximaRepresentacion:escalaMinimaRepresentacion)
   repeticiones <-  escalaMinimaRepresentacion-escalaMaximaRepresentacion+1
   
   
   x1<-c(rep(c(name_rule),each=repeticiones))
   y1 <-c(rep(c(rule_extract1),each=repeticiones))
   
   df1 <- data.frame(x1,escala,y1) 
   
 }
 
 
df <- rbind(df,df1)

 
}
 
 
