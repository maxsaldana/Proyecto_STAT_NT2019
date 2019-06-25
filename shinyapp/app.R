library(shiny)
library(tidyverse)
library(shinyWidgets)

datos <- read_dta("2011_2013panel.dta")
dat_7 <- datos %>%
  select(form, año, g11, g12, g13, g14, g15)

ui <- fluidPage(
  selectInput(inputId = "var1",
              ),
  plotOutput("heatmap")
  
)


server <- function(input, output) {
  output$heatmap <- renderPlot({
    dat_7 %>%
      filter(año == 1) %>%
      group_by(g15, g12 ) %>%
      summarise(n = n()) %>%
      mutate(prop = round(n/sum(n), 4)) %>%
      ggplot(aes(x = as.factor(g15), y = as.factor(g12))) +
      geom_tile(aes(fill = prop)) +
      geom_text(aes(label = prop), color = "white") +
      scale_x_discrete(labels = c("Mucho peor", "Peor", "Igual", "Mejor", "Mucho mejor")) +
      scale_y_discrete(labels = c("Mucho peor", "Peor", "Igual", "Mejor", "Mucho mejor")) +
      labs(x = "Maquinaria y equipos", y = "Capacidad de producción", fill = "Proporción")
  })
}

shinyApp(ui, server)
