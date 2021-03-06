
library(shiny)
library(ggplot2)
library(dplyr)
library(haven)
library(shinythemes)
library(readr)
datos <- read_csv("datos.csv")
  dat_7 <- datos %>%
  select(form, ano, g11, g12, g13, g14, g15) %>%
  mutate(ano = as.factor(recode(ano, `1` = 2010, `2` = 2011)))

ui <- fluidPage(
  
  theme = shinytheme("darkly"),
  
  titlePanel(h1("Mapa de calor", span(h5("Proporción de los desempeños de las cooperativas en un área  en relación a otra")), align = "center")),
  
  
  plotOutput("heatmap"),
  
  hr(),
  
  fluidRow(
  column(4,
   selectInput(inputId = "anio",
               label = "Año",
               c("2010" = "2010","2011" = "2011")
   )
  ),

  column(4,
   selectInput(inputId = "var1",
               label = "Variable en el eje x",
               c("Tecnología" = "g11",
                 "Capacidad de producción" = "g12",
                 "Marketing/Comercialización" = "g13",
                 "Capacitación de personal" = "g14",
                 "Maquinarias y equipos" = "g15"
              )
   )
  
  ),
  
  column(4,
   selectInput(inputId = "var2",
                label = "Variable en el eje y",
                c("Tecnología" = "g11",
                  "Capacidad de producción" = "g12",
                  "Marketing/Comercialización" = "g13",
                  "Capacitación de personal" = "g14",
                  "Maquinarias y equipos" = "g15"
                )
   )                   
  )
),

tags$a(href = "http://www.iecon.ccee.edu.uy/bases-de-datos/contenido/300/es/", h5("Fuente: Encuesta a cooperativas de producción (IECON, 2010-2011)"))
 
   

)

server <- function(input, output) {
  
  output$heatmap <- renderPlot({
    
    etiqs <- c("Tecnología" = "g11",
                  "Capacidad de producción" = "g12",
                  "Marketing/Comercialización" = "g13",
                  "Capacitación de personal" = "g14",
                  "Maquinarias y equipos" = "g15")
    
    dat_plot <- reactive({
      dat_7 %>%
      filter(ano == input$anio) %>%
      group_by(!!sym(input$var1), !!sym(input$var2)) %>%
      summarise(n = n()) %>%
      mutate(prop = round((n/sum(n)), 4))
    })
  
  ggplot(dat_plot(), aes(x = as.factor(!!sym(input$var1)), y = as.factor(!!sym(input$var2)))) +
      geom_tile(aes(fill = prop)) +
      geom_text(aes(label = prop), color = "white") +
      scale_x_discrete(labels = c("Mucho peor", "Peor", "Igual", "Mejor", "Mucho mejor")) +
      scale_y_discrete(labels = c("Mucho peor", "Peor", "Igual", "Mejor", "Mucho mejor")) +
      labs(x = names(etiqs)[etiqs == input$var1], y = names(etiqs)[etiqs == input$var2], fill = "Proporción") +
      theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
  })

}

shinyApp(ui, server)
