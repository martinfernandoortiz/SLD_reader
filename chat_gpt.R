# Load necessary libraries
library(xml2)
library(stringr)
library(readr)
library(tidyverse)
#setwd("~/Documents/GitHub/SLD_reader")
setwd("D:/martin/ign-argentina/SLD_reader/SLD_reader")



#Esto sale del geoserver
orden_Capas <- read_delim("orden_Capas.txt", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)


orden_Capas <- orden_Capas %>% mutate(Estilo = paste0(Estilo,".sld"))



############################## FX PARA ZOOM SCALE #############################################

#df <- data.frame(
#  Zoom = 1:19,
#  Escala = c(279541132, 139770566, 69885283, 34942642, 17471321, 8735660, 4367830, 2183915, 1091958, 545979, 272989, 136495, 68247, 34124, 17062, 8531, 4265, 2133, 1066)
#)
zoomLevel <- read_table("zoomLevel.txt")
# Define a function to get the zoom level
get_zoom_level <- function(scale_value, zoomLevel) {
  idx <- which.min(abs(zoomLevel$Escala - scale_value)) # find the index of the closest value
  return(zoomLevel$Zoom[idx]) # return the corresponding Zoom value
}
############################## 


############################### Crear df vacio ############################## 
columns = c("capa","orden_geoserver","file_name", "rule_name","rule_desc",
            "filter","min_scale_denom","max_scale_denom","max_zoom","min_zoom",  
            "symbolizer","PointSymbolizer","LineSymbolizer","PolygonSymbolizer",
            "TextSymbolizer")
df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df) = columns

############################## 




############################### Directorio con los sld
sld_dir <- getwd()

############################## # Lista de sld
#sld_files <- list.files(paste0(sld_dir,"/argenmap/"), pattern = "\\.sld$", full.names = TRUE) #chequear dnd están
#sld_files <- list.files(paste0(sld_dir,"/"), pattern = "\\.sld$", full.names = TRUE)
#sld_count <- length(sld_files)



sld_folder <- as.character("D:/martin/ign-argentina/github/estilos-sld/argenmap/")





#Primer Loop que lee los archivos de la carpeta

###############for (xx in 1:13) {        VERSION VIEJA DE PRUEBA. EL LOOP DE ABAJO ES EL QUE VA


for (xx in seq_len(nrow(orden_Capas))) {  
print(paste("SLD NUMERO",xx, se=" ")) #ESTO ES UN INDICATIVO
print(as.character(orden_Capas[xx,3]))

sld_file <-  as.character(orden_Capas[xx,3])
sld_xml <- read_xml(paste0(sld_folder,sld_file))

orden_geoserver <- as.character(orden_Capas[xx,1])
capa <- as.character(orden_Capas[xx,2])
file_name <- as.character(orden_Capas[xx,3])


###############################################sld_xml <- read_xml(sld_files[xx]) VERSION VIEJA

if( as.character(sld_xml)%>% str_detect("xmlns:se") == TRUE) { #ESTO PARA VER EL TIPO DE ESCRITURA SLD
   
    
    
rules <- "" 

#1 saber el nombre de la capa

#Saber si tiene rules

count_rule <- length(sld_xml %>% xml_find_all( '//se:Rule') %>% xml_length()) 



result <- list()
for (i in 1:count_rule) {
  
  print(i)
  rules <- xml_find_first(sld_xml, "//se:Rule")
  #file_name  <-  basename(sld_files[xx])
  
  rule_name <- xml_text(xml_find_first(rules, ".//se:Name"))
  rule_desc <- xml_text(xml_find_first(rules, ".//se:Description/se:Title"))
  
  # Extract the filter of the rule
  #La estructura de los filtros tiene multiples sentencias que pueden estar conectadas por AND u OR y cada sentencia características
  #particulares. Entonces hay que extraer tanto los predicados como las sentencias
  
  filter <- xml_find_first(rules, ".//ogc:Filter") #De acá extraes todos los renglones del filtro. ES XML
  filter1 <- xml_text(filter)
  filterPredicado <- filter %>% xml_children() %>% xml_children() #ES XML
  
  
  tipoFiltro <- xml_name( xml_children( filter ) )#Ejemplo or, and, etc. ES TEXTO
  predicados <- xml_name( filterPredicado)# clase de predicado. ver el lenght
  valores <-xml_name( xml_children(filterPredicado))# clase de predicado. ver el lenght
  
  #Es decir la consulta se construye con 
  # valores[1] predicado[1] valores[2]
  #valores[3] predicado[2] valores[4]
  #valores[5] predicado[3] valores[6]
  #y asi sucesivamente. Lo importante es ver el length(xml_length(filterPredicado)) para asi ver como se traduce el filtro
  
  
  ####PASAMOS a SCALE
  min_scale_denom <-as.numeric(xml_text(xml_find_first(rules, ".//se:MinScaleDenominator")))
  min_scale_denom <- if_else(!is.na(min_scale_denom),min_scale_denom,1000)
  
  max_scale_denom <- as.numeric(xml_text(xml_find_first(rules, ".//se:MaxScaleDenominator")))
  max_scale_denom <- if_else(!is.na(max_scale_denom),max_scale_denom,300000000)
  
  max_zoom<- get_zoom_level(min_scale_denom,zoomLevel)
  min_zoom <-get_zoom_level(max_scale_denom,zoomLevel)
  
  ###PASAMOS A SIMBOLOGIA
  
  PolygonSymbolizer <- xml_find_first(rules, ".//se:PolygonSymbolizer")
  PolygonSymbolizerText <- as.character(PolygonSymbolizer)
  
  PointSymbolizer <- xml_find_first(rules, ".//se:PointSymbolizer")
  PointSymbolizerText <- as.character(PointSymbolizer)
  
  LineSymbolizer <- xml_find_first(rules, ".//se:LineSymbolizer")
  LineSymbolizerText <- as.character(LineSymbolizer)
  
  TextSymbolizer <- xml_find_first(rules, ".//se:TextSymbolizer")
  TextSymbolizerText <- as.character(TextSymbolizer)
  

  
  area <- xml_remove(rules)
  
  
  
  ##############UNIMOS TODO PARA PASAR A DF
  result[[i]] <- list(
    capa = capa,
    orden_geoserver= orden_geoserver,
    file_name = file_name,
    rule_name = rule_name,
    rule_desc = rule_desc,
    filter = filter1,
    min_scale_denom = min_scale_denom,
    max_scale_denom = max_scale_denom,
    max_zoom=max_zoom,
    min_zoom=min_zoom,
    symbolizer="",
    PointSymbolizer= PointSymbolizerText,
    LineSymbolizer=LineSymbolizerText,
    PolygonSymbolizer=PolygonSymbolizerText,
    TextSymbolizer=TextSymbolizerText
    #fill_color = fill_color,
    #fill_opacity = fill_opacity,
    #stroke_color = stroke_color,
    #stroke_width = stroke_width
  )
}
result_df <- do.call(rbind.data.frame, result)
df <- rbind(df,result_df)
}else (print("CODIFICACION MOCHA"))

}




########################################## AGREGAR REGISTROS POR NIVEL DE ZOOM ###################


#Secuencia para agregar registros por n'umero de escala
df <- df %>% mutate(id =row_number())

# Create the original table
original_table <-df


# Create an empty dataframe to store the new rows
new_rows <- data.frame(id = numeric(), number = numeric())

# Loop through each row in the original table
for (i in seq_len(nrow(original_table))) {
  
  print(i)
  
  # Calculate the difference between min and max
  diff <- original_table$max_zoom[i] - original_table$min_zoom[i]
  
  # Generate a sequence of numbers from min to max
  numbers <- seq(from = original_table$min_zoom[i], to = original_table$max_zoom[i])
  
  # Create a new data frame with field1 and number columns
  new_row <- data.frame(
    id = rep(original_table$id[i], diff + 1),
    number = numbers
  )
  
  # Add the new row(s) to the empty dataframe
  new_rows <- rbind(new_rows, new_row)
}




df_completo <- new_rows %>% left_join(df, by = "id")
df_3 <- df_completo %>%filter(number ==3)
