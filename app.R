library(shiny)
library(ggplot2)
library(magrittr)
library(data.table)
library(viridis)

# Load data
mxDATA <- readRDS("mexData.rds") %>% data.table()
mov_lims <- c(min(mxDATA$Movilidad, na.rm= T) + 5, max(mxDATA$Movilidad, na.rm= T) + 5)
estados <- mxDATA$region %>% unique
tTransp <- c("Automovil", "Peatones")
ciudades <- mxDATA$region[mxDATA$geo_type == "Ciudad"] %>% unique %>% as.character()

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("COVID-19: Movilidad en México por estados y ciudades"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        mainPanel(
            plotOutput("distPlot", height = "700px")
        ),
        sidebarPanel(
            fluidRow(h4("Autor: ", a(href="https://angelcampos.github.io/", "AngelCampos"))),
        fluidRow(h4("Datos abiertos: ", a(href="https://www.apple.com/covid19/mobility", "Apple Mobility Trends Reports.")), "Última actualización de datos: 15 de Junio, 2020"),
            fluidRow(align = "center", HTML("<style>.bmc-button img{height: 34px !important;width: 35px !important;margin-bottom: 1px !important;box-shadow: none !important;border: none !important;vertical-align: middle !important;}.bmc-button{padding: 7px 15px 7px 10px !important;line-height: 35px !important;height:51px !important;text-decoration: none !important;display:inline-flex !important;color:#ffffff !important;background-color:#5F7FFF !important;border-radius: 5px !important;border: 1px solid transparent !important;padding: 7px 15px 7px 10px !important;font-size: 28px !important;letter-spacing:0.6px !important;box-shadow: 0px 1px 2px rgba(190, 190, 190, 0.5) !important;-webkit-box-shadow: 0px 1px 2px 2px rgba(190, 190, 190, 0.5) !important;margin: 0 auto !important;font-family:'Cookie', cursive !important;-webkit-box-sizing: border-box !important;box-sizing: border-box !important;}.bmc-button:hover, .bmc-button:active, .bmc-button:focus {-webkit-box-shadow: 0px 1px 2px 2px rgba(190, 190, 190, 0.5) !important;text-decoration: none !important;box-shadow: 0px 1px 2px 2px rgba(190, 190, 190, 0.5) !important;opacity: 0.85 !important;color:#ffffff !important;}</style><link href='https://fonts.googleapis.com/css?family=Cookie' rel='stylesheet'><a class='bmc-button' target='_blank' href='https://www.buymeacoffee.com/AngelCampos'><img src='https://cdn.buymeacoffee.com/buttons/bmc-new-btn-logo.svg' alt='Dona!'><span style='margin-left:5px;font-size:28px !important;'>Dona!</span></a>")),
            hr(),
            fluidRow(p("Esta aplicación grafica el porcentaje de movilidad diario por estado y para algunas de las ciudades de México.",
                       "El punto de referencia (0%) es la movilidad calculada para el día lunes, 13 de enero del 2020."),
                     p("Para graficar distintos valores, usa el menú inferior para seleccionar los",
                       strong("estados o ciudades,"), "cambiar el", strong("rango de días,"),
                       "y el tipo de", strong("transporte."))),
                     hr(),
            sliderInput("days",
                        "Rango de días:",
                        min = min(mxDATA$day),
                        max = max(mxDATA$day),
                        value = c(as.Date("2020-03-01"), max(mxDATA$day))),
            checkboxGroupInput(inputId = "transport",
                               label = "Tipos de transporte",
                               choices = tTransp,
                               selected = c("Automovil", "Peatones")),
            checkboxGroupInput(inputId = "states",
                                                          label = "Selecciona los estados a visualizar",
                                                          choices = estados,
                                                          selected = ciudades)
        )
    ),
    fluidRow("Los datos usados en esta aplicación web son abiertos, pero Apple mantiene todos sus derechos reservados. Esta aplicación sólo tiene fines informativos. Para más información de los datos:", a(href="https://www.apple.com/covid19/mobility", "https://www.apple.com/covid19/mobility"), "Código abierto en:", a(href="https://github.com/AngelCampos/movilidad_mexico_covid19", "https://github.com/AngelCampos/movilidad_mexico_covid19"))
)

# Define server logic required to draw a histogram
server <- function(input, output){
    output$distPlot <- renderPlot({
        tmpDATA <- mxDATA[region %in% input$states &
                              transportation_type %in% input$transport &
                              day >= input$days[1] & day <= input$days[2],]
        gg <- ggplot(tmpDATA, aes(y = Movilidad, x = day, group = region)) +
            geom_line(size = 1.25, aes(colour = region)) + theme_minimal(base_size = 18) +
            scale_y_continuous(breaks = seq(140, -80, -20), limits = mov_lims) +
            scale_x_date(breaks = unique(tmpDATA$day)[seq(1, length(unique(tmpDATA$day)),
                                                          by = ceiling(as.numeric(input$days[2] - input$days[1])/31))]) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            ylab("Movilidad %") + xlab("Día") +
            scale_color_viridis(discrete = TRUE, name = "Estado") +
            theme(legend.position = "bottom", panel.spacing = unit(1.75, "lines")) + facet_grid(transportation_type ~ .) +
            geom_hline(yintercept = 0)
        gg
    })
}


# Run the application
shinyApp(ui = ui, server = server)
