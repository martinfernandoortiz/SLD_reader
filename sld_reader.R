library(tidyverse)
library(stringr)


library(readr)
edificio_salud <- read_csv("D:/martin/ign-argentina/github/estilos-sld/infraestructura-social/edificio_salud.sld", 
                           col_names = FALSE, trim_ws = FALSE)


 edificio_salud <-  read_file("D:/martin/ign-argentina/github/estilos-sld/infraestructura-social/edificio_salud.sld")

 
 #Eliminar comentarios
 #detectar "<!--" y "-->"
 

 
 #Cantidad de Reglas
 rule_count <-  edificio_salud %>% str_locate_all("<se:Rule>")
 a <-  rule_count %>% as_vector() 
 ini <- a[1]
 fin <- a[2]
 rule_extract <- str_sub(edificio_salud,a[1],a[2])

 #Nombre Regla 
 name_ini <-as_vector(rule_extract %>% str_locate("<se:Name>"))
 name_end <- as_vector(rule_extract %>% str_locate("</se:Name>"))
 name_rule <- str_sub(rule_extract,name_ini[2]+1,name_end[1]-1)
 
 rm(name_ini,name_end)
 
 #Escalas
 #MinScaleDenominator es el zoom mínimo
 #MaxScaleDenominator es el zoom máximo
 
 
 #Si no está el mínimo el zoom va hasta el máximo
 #Si no está el máximo el zoom va del mínimo al máximo
 #Si no están van en todos los niveles
 
 
 
 
 #Min Escala
 str_detect(rule_extract,"<se:MinScaleDenominator>") #Agregar if true
 
 MinScaleDenominator_ini <-as_vector(rule_extract %>%
                                       str_locate("<se:MinScaleDenominator>"))
 
 MinScaleDenominator_end <- as_vector(rule_extract %>%
                                        str_locate("</se:MinScaleDenominator>"))
 
MinScaleDenominator <- as.numeric(str_sub(rule_extract,
                               MinScaleDenominator_ini[2]+1,
                               MinScaleDenominator_end[1]-1))
 
rm(MinScaleDenominator_ini,MinScaleDenominator_end)
 

#Max Escala
str_detect(rule_extract,"<se:MaxScaleDenominator>") #Agregar if true

MaxScaleDenominator_ini <-as_vector(rule_extract %>%
                                      str_locate("<se:MaxScaleDenominator>"))

MaxScaleDenominator_end <- as_vector(rule_extract %>%
                                       str_locate("</se:MaxScaleDenominator>"))

MaxScaleDenominator <- as.numeric(str_sub(rule_extract,
                                          MinScaleDenominator_ini[2]+1,
                                          MinScaleDenominator_end[1]-1))

rm(MaxScaleDenominator_ini,MaxScaleDenominator_end)

#####################################################################################################

 
 
 str_detect(rule_extract,"<se:MaxScaleDenominator>")
 
 
 count(rule_count)
  
 edificio_salud %>% str_count("<se:Name>")
 edificio_salud %>% str_count("<se:Description>")
 edificio_salud %>% str_count("<se:Title>")
 edificio_salud %>% str_count("<se:Title>")
 
 edificio_salud %>% str_count("<se:MinScaleDenominator>")
 edificio_salud %>% str_count("<se:MaxScaleDenominator>")
 edificio_salud %>% str_count("<se:PointSymbolizer>")
 
 lengths(rule_count)
 length(rule_count)
 length(rule_count[[3]])
 
 class(rule_count)

a <-  rule_count %>% as_vector() 
a 

leng
