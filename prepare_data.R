# download.file("https://raw.githubusercontent.com/bluevex/elegans-atlas/main/LowResAtlas.csv",
#               "data/LowResAtlas.csv")

# download.file("https://raw.githubusercontent.com/openworm/NeuroPAL/main/data/CanonicalPositions/LowResAtlasWithHighResHeadsAndTails.csv",
#               "data/neuropal_LowResAtlasWithHighResHeadsAndTails.csv")

# download.file("https://raw.githubusercontent.com/openworm/NeuroPAL/main/NeuroML2/NeuroPAL_Canonical.net.nml",
#               "data/openworm.nml")



# Convert neuroML file

content <- xml2::read_xml("data/openworm.nml") |>
  xml2::as_list()

all_neurs <- content[[1]][[2]]

neuron_ids <- sapply(all_neurs,
       \(neur) attr(neur, "id", exact = TRUE))

neuron_pos <- lapply(all_neurs,
                     \(neur) neur$instance[[1]] |>
                       attributes() |>
                       as.data.frame()) |>
  dplyr::bind_rows()

neuron_coords <- tibble::tibble(neuron_id = neuron_ids,
               neuron_pos) |>
  dplyr::mutate(x = as.numeric(x),
                y = as.numeric(y),
                z = as.numeric(z))


qs::qsave(neuron_coords, "data/neuron_coords.qs")







neuron_coords <- qs::qread("data/neuron_coords.qs")

body_drawing <- png::readPNG("data/whole_body.png")
head_drawing <- png::readPNG("data/head_drawing.png")
tail_drawing <- png::readPNG("data/tail_drawing.png")

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


