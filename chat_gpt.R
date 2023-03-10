# Load necessary libraries
library(xml2)
library(stringr)
#setwd("~/Documents/GitHub/SLD_reader")

#Crear df vacio
columns = c("file_name", "rule_name","rule_desc",
            "filter","min_scale_denom","max_scale_denom",
            "symbolizer","PointSymbolizer","LineSymbolizer","PolygonSymbolizer",
            "TextSymbolizer")
df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df) = columns



getwd()


# Set path to directory containing sld files
sld_dir <- getwd()

# Get list of all sld files in directory
sld_files <- list.files(paste0(sld_dir,"/argenmap/"), pattern = "\\.sld$", full.names = TRUE)
sld_count <- length(sld_files)



#Primer Loop que lee los archivos de la carpeta
for (xx in 1:13) {
  
print(paste("SLD NUMERO",xx, se=" ")) #ESTO ES UN INDICATIVO

sld_xml <- read_xml(sld_files[xx]) #LEO UNO A UNO ARCHIVO DE LA CARPETA
if( as.character(sld_xml)%>% str_detect("xmlns:se") == TRUE) { #ESTO PARA VER EL TIPO DE ESCRITURA SLD
   
    
    
rules <- "" 

#1 saber el nombre de la capa

#Saber si tiene rules

count_rule <- length(sld_xml %>% xml_find_all( '//se:Rule') %>% xml_length()) 



result <- list()
for (i in 1:count_rule) {
  
  print(i)
  rules <- xml_find_first(sld_xml, "//se:Rule")
  file_name  <-  basename(sld_files[xx])
  
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
  min_scale_denom <- xml_text(xml_find_first(rules, ".//se:MinScaleDenominator"))
  max_scale_denom <- xml_text(xml_find_first(rules, ".//se:MaxScaleDenominator"))
  
  
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
    file_name = file_name,
    rule_name = rule_name,
    rule_desc = rule_desc,
    filter = filter1,
    min_scale_denom = min_scale_denom,
    max_scale_denom = max_scale_denom,
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













