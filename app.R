# carga de librerias
library(shiny)
library(shinydashboard)
library( shinyWidgets )
library(dplyr)
library(sf)
library(terra)
library(raster)
library(rgdal)
library(DT)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(ggplot2)
library(graphics)
library(tidyverse)
library(RColorBrewer)
#library(spData)
#library(spDataLarge)


#Lectura datos zonas
zonas <-
  st_read("https://raw.githubusercontent.com/mauguemu/prueba_tablero/master/Datos/capas/zonas_delimitadas.geojson",
          quiet = TRUE
  )

# Transformación del CRS del objeto zonas
zonas <-
  zonas %>%
  st_transform(4326)

#Lectura datos cuadrantes
cuadrantes <-
  st_read("https://raw.githubusercontent.com/mauguemu/prueba_tablero/master/Datos/capas/cuadrantes.geojson",
          quiet = TRUE
  )

# Transformación del CRS de cuadrantes 
cuadrantes <-
  cuadrantes %>%
  st_transform(4326)

#Lectura datos recursos patimoniales  
recursos_patrimoniales <-
  st_read("https://raw.githubusercontent.com/mauguemu/prueba_tablero/master/Datos/capas/datos_mapa_material.geojson",
          quiet = TRUE
  )

# Transformación del CRS de recursos patrimoniales

recursos_patrimoniales <-
  recursos_patrimoniales %>%
  st_transform(4326)

# Lista ordenada de estado + "Todos"
lista_estado <- unique(recursos_patrimoniales$estado)
lista_estado <- sort(lista_estado)
lista_estado <- c("Todas", lista_estado)

# Lista ordenada de subcategorias + "Todas"
lista_subcategorias <- unique(recursos_patrimoniales$subcategoria)
lista_subcategorias <- sort(lista_subcategorias)
lista_subcategorias <- c("Todas", lista_subcategorias)

#lectura patrimonio_inmaterial
patrimonio_inmaterial <-
  st_read(
    "/vsicurl/https://raw.githubusercontent.com/mauguemu/prueba_tablero/master/Datos/tablas/recursos_patrimonio_inmat.csv",
    quiet = TRUE
  )

# 1° operación espacial

# Cruce espacial de recursos_patrimoniales con cuadrantes para extraer el campo del cuadrante

# se presenta un error en la geometría y se resulve con el siguiente comando
sf::sf_use_s2(FALSE)

cuadrantes_recursos <- 
  recursos_patrimoniales %>%
  st_join(cuadrantes["id_cuadrante"])

# 2° operación espacial 

#selección de recursos del casco histórico

# Selección del casco histórico
casco_hist <- zonas[zonas$id_zona == "Z1-Li",]

# Selección de los recursos del casco histórico
recursos_casco <- cuadrantes_recursos[casco_hist, , op = st_within]

# Componentes de la aplicación Shiny
# Definición del objeto ui

ui <- dashboardPage(skin = "purple",
                    
                    #tabsetPanel(
                    #  tabPanel(
                    
                    dashboardHeader(title ="Patrimonio material"),
                    
                    dashboardSidebar(sidebarMenu(
                      menuItem(
                        text = "Filtros",
                        selectInput(
                          inputId = "estado",
                          label = "Estado",
                          choices = lista_estado,
                          selected = "Todas"
                        ),
                        selectInput(
                          inputId = "subcategoria",
                          label = "Subcategoría",
                          choices = lista_subcategorias,
                          selected = "Todas"
                        ),
                        numericRangeInput(
                          inputId = "valor_ponderado",
                          label = "Evaluación multicriterio",
                          value = c(3, 6.5),
                          width = NULL,
                          separator = " a ",
                          min = 3,
                          max = 6.5,
                          step = NA
                        ),
                        menuSubItem(text = "Mapa patrimonio material", tabName = "mapa_material"),
                        menuSubItem(text = "Tabla patrimonio material", tabName = "tabla_material"),
                        menuSubItem(text = "Gráfico patrimonio material", tabName = "grafico_material"),
                        menuSubItem(text = "Página principal",href = "https://mauguemu.github.io/Proyecto_1_Tablero_mgm/"),
                        #menuSubItem(text = "Recursos del casco histórico", tabName = "casco_historico"),
                        startExpanded = TRUE
                      )
                    )),
                    dashboardBody(tabItems(
                      tabItem(
                        tabName = "mapa_material",
                        box(
                          title = "Mapa recursos del patrimonio material", solidHeader = TRUE,status = "success",
                          leafletOutput(outputId = "mapa",width="100%", height = 800),
                          width = 12
                          
                        )
                      ),
                      
                      # tabItem(
                      #   tabName = "casco_historico",
                      #   box(
                      #     title = "Mapa registros del casco histórico", solidHeader = TRUE, status = "success",
                      #     leafletOutput(outputId = "mapa_1",width="100%", height = 600),
                      #     width = 6
                      #   ),
                      #   box(
                      #     title = "Registros del casco histórico",solidHeader = TRUE, status = "danger",
                      #     DTOutput(outputId = "tabla_1"),
                      #     width = 6
                      #   )
                      # ),
                      
                      tabItem(
                        tabName = "tabla_material",
                        fluidRow(
                          box(
                            title = "Recursos del patrimonio material",  solidHeader = TRUE,status = "info",
                            DTOutput(outputId = "tabla",width="100%", height = 800),
                            width = 12
                          )
                        )),
                      tabItem(
                        tabName = "grafico_material", 
                        fluidRow(
                          box(
                            title = "Valoración de los recursos del patrimonio material", solidHeader = TRUE,status = "primary",
                            plotlyOutput(outputId = "grafico_evaluacion",width="100%", height = 800),
                            width = 12
                          ))))
                    ))


server <- function(input, output, session) {
  
  filtrarRegistros <- reactive({
    # Remoción de geometrías y selección de columnas
    patrimonio_filtrado <-
      recursos_patrimoniales %>%
      dplyr::select(codigo,denominacion,subcategoria,estado,economico,disponibilidad,identidad_territorial,condicion,valor_ponderado,registro_fotografico,ficha,id_recurso)
    
    # Filtrado por rango
    patrimonio_filtrado <-
      patrimonio_filtrado %>%
      filter(
        valor_ponderado >= input$valor_ponderado[1] &
          valor_ponderado <= input$valor_ponderado[2]
      )
    # Filtrado de registros por estado
    if (input$estado != "Todas") {
      patrimonio_filtrado <-
        patrimonio_filtrado %>%
        filter(estado == input$estado)
    }
    # Filtrado de registros por subcategoría
    if (input$subcategoria != "Todas") {
      patrimonio_filtrado <-
        patrimonio_filtrado %>%
        filter(subcategoria == input$subcategoria)
    }
    
    return(patrimonio_filtrado)
  })  
  
  output$tabla <- renderDT({
    registros <- filtrarRegistros()
    
    registros %>%
      st_drop_geometry() %>%
      dplyr::select(codigo,denominacion, subcategoria, estado,valor_ponderado)%>%
      datatable(registros, options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.11.3/i18n/es_es.json'), pageLength = 20))
  }) 
  
  output$tabla_1 <- renderDT({
    #registros <- filtrarRegistros()
    
    recursos_casco %>%
      st_drop_geometry() %>%
      dplyr::select(codigo,denominacion, subcategoria, estado,id_cuadrante)%>%
      datatable(recursos_casco, options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.11.3/i18n/es_es.json'), pageLength = 13))
  })
  
  output$mapa <- renderLeaflet({
    registros <-
      filtrarRegistros()
    
    colores <- c('red', 'orange', 'yellow')
    
    c_zona <- levels(as.factor(zonas$id_zona))
    
    paleta <- colorFactor(palette = colores, domain = c_zona)
    
    # Mapa leaflet básico con capas de zonas y recursos patrimoniales 
    leaflet() %>%
      addTiles() %>%
      setView(-83.0232, 9.9952, 15) %>%
      
      addProviderTiles(
        providers$CartoDB.Positron, group = "Mapa base Carto_DB") %>%
      addProviderTiles(
        providers$Esri.WorldImagery, group = "Maba base Esri") %>%
      
      addPolygons(
        data = zonas,
        color = ~paleta(id_zona),
        smoothFactor = 0.3,
        fillOpacity = 0.3,
        popup =  ~nombre,
        label= ~id_zona,
        stroke = TRUE,
        weight = 2.0,
        group = "Zonas delimitadas"
      )  %>%
      
      addPolygons(
        data = cuadrantes,
        color = "black",
        smoothFactor = 0.3,
        stroke = TRUE,
        weight = 1.0,
        group = "Cuadrantes"
      ) %>%
      
      addCircleMarkers(
        data = registros,
        stroke = F,
        radius = 4,
        popup = paste0("<strong>Recurso: </strong>",
                       registros$denominacion,
                       "<br>",
                       "<strong>Subcategoría: </strong>",
                       registros$subcategoria,
                       "<br>",
                       "<strong>Estado de conservación: </strong>",
                       registros$estado,
                       "<br>",
                       "<img src='",registros$registro_fotografico,"","'width='200'/>",
                       "<br>",
                       "<a href='",registros$ficha,"", "'>Ficha</a>"),
        label = ~codigo,
        fillColor = 'orange',
        fillOpacity = 1,
        group = "Recursos patrimoniales"
      )%>%
      addSearchOSM()%>%
      addResetMapButton()%>%
      addMouseCoordinates()%>%
      addLayersControl(
        baseGroups = c("Mapa base Carto_DB","Mapa base Esri"),
        overlayGroups = c("Zonas delimitadas","Cuadrantes", "Recursos patrimoniales"),
        options = layersControlOptions(collapsed = T)
      )
  })
  
  output$mapa_1 <- renderLeaflet({
    # registros <-
    #   filtrarRegistros()
    
    colores <- c('red', 'orange', 'yellow')
    
    c_zona <- levels(as.factor(zonas$id_zona))
    
    paleta <- colorFactor(palette = colores, domain = c_zona)
    
    # Mapa leaflet básico con capas de zonas y recursos patrimoniales 
    leaflet() %>%
      addTiles() %>%
      setView(-83.0232, 9.9952, 15) %>%
      
      addProviderTiles(
        providers$CartoDB.Positron, group = "Mapa base Carto_DB") %>%
      addProviderTiles(
        providers$Esri.WorldImagery, group = "Maba base Esri") %>%
      
      addPolygons(
        data = zonas,
        color = ~paleta(id_zona),
        smoothFactor = 0.3,
        fillOpacity = 0.3,
        popup =  ~nombre,
        label= ~id_zona,
        stroke = TRUE,
        weight = 2.0,
        group = "Zonas delimitadas"
      )  %>%
      
      addPolygons(
        data = cuadrantes,
        color = "black",
        smoothFactor = 0.3,
        stroke = TRUE,
        weight = 1.0,
        group = "Cuadrantes"
      ) %>%
      
      addCircleMarkers(
        data = recursos_casco,
        stroke = F,
        radius = 4,
        popup = paste0("<strong>Recurso: </strong>",
                       recursos_casco$denominacion,
                       "<br>",
                       "<strong>Subcategoría: </strong>",
                       recursos_casco$subcategoria,
                       "<br>",
                       "<strong>Estado de conservación: </strong>",
                       recursos_casco$estado,
                       "<br>",
                       "<img src='",recursos_casco$foto,"","'width='200'/>",
                       "<br>",
                       "<a href='",recursos_casco$ficha,"", "'>Ficha</a>"),
        label = ~codigo,
        fillColor = 'black',
        fillOpacity = 1,
        group = "Recursos casco histórico"
      )%>%
      addSearchOSM()%>%
      addResetMapButton()%>%
      addMouseCoordinates()%>%
      addLayersControl(
        baseGroups = c("Mapa base Carto_DB","Mapa base Esri"),
        overlayGroups = c("Zonas delimitadas","Cuadrantes", "Recursos casco histórico"),
        options = layersControlOptions(collapsed = T)
      )
  })
  
  output$grafico_evaluacion <- renderPlotly({
    registros <- filtrarRegistros()
    
    registros %>%
      st_drop_geometry() %>%
      plotly::select(denominacion,economico,disponibilidad,identidad_territorial,condicion)%>%
      pivot_longer(c("economico","disponibilidad","identidad_territorial","condicion"), names_to = "criterio",values_to = "valoracion")%>%
      ggplot(aes(x = valoracion, y = denominacion, fill = criterio)) +
      ggtitle("Valoración de los recursos patrimoniales") +
      ylab("Recurso") +
      xlab("Valoración multicriterio") +
      scale_fill_manual(values=brewer.pal(n = 5, name = "Blues"))+
      geom_col()%>%
      config(locale = "es")
    
  })
  
  
}


shinyApp(ui, server)
