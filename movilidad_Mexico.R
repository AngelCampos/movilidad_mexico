library(shiny)
library(ggplot2)
library(magrittr)
library(data.table)
library(viridis)
# Load data
mxDATA <- readRDS("mexData.rds") %>% data.table()
mov_lims <- c(min(mxDATA$Movilidad, na.rm= T) + 5, max(mxDATA$Movilidad, na.rm= T) + 5)
estados <- mxDATA$region %>% unique
tTransp <- mxDATA$transportation_type %>% unique
ciudades <- mxDATA$region[mxDATA$geo_type != "Estado"]

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Movilidad en México por estados: Contingencia COVID-19"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        mainPanel(
            plotOutput("distPlot", height = "700px")
        ),
        sidebarPanel(
            fluidRow(h4("Autor: ", a(href="https://angelcampos.github.io/", "AngelCampos"))),
            fluidRow("Datos abiertos: ", a(href="https://www.apple.com/covid19/mobility", "Apple Mobility Trends Reports")),
            hr(),
            fluidRow(p("Esta aplicación grafica el porcentaje de movilidad diario por estado y para algunas de las ciudades de México.",
                       "El punto de referencia (0%) es la movilidad calculada para el día lunes, 13 de enero del 2020."),
                     p("Para graficar distintos valores, usa el menú inferior para seleccionar los",
                       strong("estados o ciudades,"), "cambiar el", strong("rango de días,"),
                       "el tipo de", strong("transporte,"), "y presiona el boton", strong("¡GRAFICAR!"))),
            hr(),
            sliderInput("days",
                        "Rango de días:",
                        min = min(mxDATA$day),
                        max = max(mxDATA$day),
                        value = c(as.Date("2020-02-22"),max(mxDATA$day))),
            checkboxGroupInput(inputId = "transport",
                               label = "Tipos de transporte",
                               choices = tTransp,
                               selected = c("Automovil", "Peatones")),
            fluidRow(column(width = 6, checkboxGroupInput(inputId = "states",
                                                          label = "Selecciona los estados a visualizar",
                                                          choices = estados,
                                                          selected = ciudades)),
                     column(width = 6, actionButton(inputId = "plot",
                                                    label = "¡GRAFICAR!")))
        )
    ),
    fluidRow("Los datos usados en esta aplicación web son abiertos, pero Apple mantiene todos los derechos reservados. Esta aplicación sólo tiene fines informativos. Más información en:", a(href="https://www.apple.com/covid19/mobility", "https://www.apple.com/covid19/mobility"))
)

# Define server logic required to draw a histogram
server <- function(input, output){
    output$distPlot <- renderPlot({
        input$plot
        isolate(
            tmpDATA <- mxDATA[region %in% input$states &
                                  transportation_type %in% input$transport &
                                  day >= input$days[1] & day <= input$days[2],])
        ggplot(tmpDATA, aes(y = Movilidad, x = day, group = region)) +
            geom_line(size = 1.5, aes(colour = region)) + theme_minimal(base_size = 16) +
            scale_x_date(breaks = unique(tmpDATA$day)[seq(1, length(unique(tmpDATA$day)), by = 5)]) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            scale_y_continuous(breaks = seq(140, -80, -20), limits = mov_lims) +
            ylab("Movilidad %") + xlab("Día") +
            scale_color_viridis(discrete = TRUE, name = "Estado") +
            theme(legend.position = "bottom") + facet_grid(transportation_type ~ .) +
            geom_hline(yintercept = 0)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
