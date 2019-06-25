library(shiny)
library(tidyverse)
library(haven)

datos <- read_dta("2011_2013panel.dta")
dat_7 <- datos %>%
  select(form, año, g11, g12, g13, g14, g15) %>%
  mutate(año = as.factor(recode(año, `1` = 2010, `2` = 2011)))

ui <- fluidPage(
  titlePanel(h3("Mapa de calor")),
  selectInput(inputId = "anio",
              label = "Año",
              c("2010" = "2010","2011" = "2011")
  ),

  
  selectInput(inputId = "var1",
              label = "Variable en el eje x",
              c("Tecnología" = "g11",
                "Capacidad de producción" = "g12",
                "Marketing/Comercialización" = "g13",
                "Capacitación de personal" = "g14",
                "Maquinarias y equipos" = "g15"
              )
  ),
  
  selectInput(inputId = "var2",
               label = "Variable en el eje y",
               c("Tecnología" = "g11",
                 "Capacidad de producción" = "g12",
                 "Marketing/Comercialización" = "g13",
                 "Capacitación de personal" = "g14",
                 "Maquinarias y equipos" = "g15"
               )
                   
  ),
 
   plotOutput("heatmap")

)


server <- function(input, output) {
  
  output$heatmap <- renderPlot({
    
    dat_plot <- reactive({
      dat_7 %>%
      filter(año == input$anio) %>%
      group_by(input$var1, input$var2) %>%
      summarise(n = n()) %>%
      mutate(prop = n())
    })
  
  ggplot(dat_plot(), aes(x = as.factor(input$var1), y = as.factor(input$var2))) +
      geom_tile(aes(fill = prop)) +
      geom_text(aes(label = prop), color = "white") +
      scale_x_discrete(labels = c("Mucho peor", "Peor", "Igual", "Mejor", "Mucho mejor")) +
      scale_y_discrete(labels = c("Mucho peor", "Peor", "Igual", "Mejor", "Mucho mejor")) +
      labs(x = "", y = "", fill = "Proporción")
  })
}

shinyApp(ui, server)
