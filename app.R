
library(shiny)
library(ggplot2)




body_drawing <- png::readPNG("data/whole_body.png")
head_drawing <- png::readPNG("data/head_drawing.png")
tail_drawing <- png::readPNG("data/tail_drawing.png")

neuron_coords <- qs::qread("data/neuron_coords.qs")

neur_coords_head <- readxl::read_excel("data/Table S2 - Positional variability and color of all neurons in males and hermaphrodites v2.xlsx",
                                       sheet = "Hermaphrodite Head Positions") |>
  dplyr::select(neuron_id = Neuron,
                x = `A-P Position (μm)`,
                y = `D-V Position (μm)`,
                z = `L-R Position (μm)`)
neur_coords_tail <- readxl::read_excel("data/Table S2 - Positional variability and color of all neurons in males and hermaphrodites v2.xlsx",
                                       sheet = "Hermaphrodite Tail Positions") |>
  dplyr::select(neuron_id = Neuron,
                x = `A-P Position (μm)`,
                y = `D-V Position (μm)`,
                z = `L-R Position (μm)`) |>
  dplyr::mutate(x = max(x) - x)


neurons_table <- readr::read_csv("data/neuron_properties.csv",
                                 col_types = "cccc")
readr::stop_for_problems(neurons_table)

source("utils.R")



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Worm Neurons Drawer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          textInput(inputId = "neuron_to_draw",
                    label = "Neurons:",
                    value = "ALL"),
          width = 2
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidPage(
            fluidRow(
              plotOutput("w_body")
            ),
            fluidRow(
              column(width = 6, plotOutput("w_head")),
              column(width = 6, plotOutput("w_tail")),
            )
          )
        )
    )
)


server <- function(input, output) {

  r_neuron_to_draw <- reactive({
    input$neuron_to_draw |>
      stringr::str_to_upper() |>
      split_text_to_vector() |>
      validate_neurons(neurons_table)
  })
  
    output$w_head <- renderPlot({
      message("Plot: ", r_neuron_to_draw())
      neur_coords_head |>
        dplyr::mutate(selected = neuron_id %in% r_neuron_to_draw()) |>
        ggplot() +
        theme_void() +
        annotation_custom(grid::rasterGrob(head_drawing),
                          xmin = -50, xmax = 90,
                          ymin = -15, ymax = 50) +
        coord_equal(clip = "off") +
        theme(plot.margin = unit(c(0,1,0,12), "lines")) +
        scale_color_manual(values = c("grey", "red4")) +
        scale_size_manual(values = c(1,2)) +
        geom_point(aes(x , y, color = selected, size = selected),
                   show.legend = FALSE) +
        ggrepel::geom_label_repel(aes(x , y, label = neuron_id),
                                  data = neur_coords_head |>
                                    dplyr::filter(neuron_id %in% r_neuron_to_draw()))
    })
    
    output$w_tail <- renderPlot({
      neur_coords_tail |>
        dplyr::mutate(selected = neuron_id %in% r_neuron_to_draw()) |>
        ggplot() +
        theme_void() +
        annotation_custom(grid::rasterGrob(tail_drawing),
                          xmin = 0, xmax = 200,
                          ymin = 2, ymax = 50) +
        coord_equal(clip = "off") +
        theme(plot.margin = unit(c(0,6,0,1), "lines")) +
        scale_color_manual(values = c("grey", "red4")) +
        scale_size_manual(values = c(1,2)) +
        geom_point(aes(x , y, color = selected, size = selected),
                   show.legend = FALSE) +
        ggrepel::geom_label_repel(aes(x , y, label = neuron_id),
                                  data = neur_coords_tail |>
                                    dplyr::filter(neuron_id %in% r_neuron_to_draw()))
    })
    
    
    
    
    
    output$w_body <- renderPlot({
      neuron_coords |>
        dplyr::mutate(selected = neuron_id %in% r_neuron_to_draw()) |>
        ggplot() +
        theme_void() +
        annotation_custom(grid::rasterGrob(body_drawing),
                          xmin = -1000, xmax = 1800,
                          ymin = -22, ymax = 35) +
        coord_equal(clip = "off", expand = TRUE) +
        scale_color_manual(values = c("grey", "red4")) +
        scale_size_manual(values = c(1,2)) +
        geom_point(aes(x , y, color = selected, size = selected),
                   show.legend = FALSE) +
        ggrepel::geom_label_repel(aes(x , y, label = neuron_id),
                                  data = neuron_coords |>
                                    dplyr::filter(neuron_id %in% r_neuron_to_draw()))
    })
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
