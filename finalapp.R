# Clear environment and unload packages
rm(list = ls())
# The following line might show a note if no packages were loaded, which is normal.
pacman::p_unload(pacman::p_loaded())

# Load necessary packages
pacman::p_load(mclust, tidyverse, ggplot2, patchwork, cluster, here, purrr, fmsb, 
               remotes, factoextra, clustMixType, glue, FeatureImpCluster, flexclust,
               ggradar, clustMD, DataExplorer, caret, parallel, snow, MASS, GGally, 
               dplyr, knitr, ClustOfVar, progress, distances, gt, shiny, tidyr, plotly,
               httr, jsonlite, spotifyr)

library(dplyr)
# =========================
# LOAD DATA & MODELS
# =========================
#pca_df_samp <- pca_df %>% sample_n(5000)
#keep_songs <- pca_df %>% filter(track_id %in% full_data$track_id)

full_data <- read.csv("SpotifyFeatures.csv") %>%
  distinct(track_id, .keep_all = TRUE)

#load("preview_df.RData")

#full_data$preview_url <- as.character(preview_df$preview_url)

# get rid of comedy
full_data <- full_data %>% filter(genre!="Comedy")

# Remove duplicate tracks
# duplicated_track_id <- full_data[duplicated(full_data$track_id),"track_id"]
# full_data <- full_data %>% distinct(track_id, .keep_all = TRUE)

# Scale tempo and loudness
scale <- preProcess(full_data %>% dplyr::select(tempo), method = c("range"))
scaled_cols <- predict(scale, full_data %>% dplyr::select(tempo))
full_data <- full_data %>% mutate(tempo = scaled_cols$tempo)

# Choose vars used for clustering
vars <- c("acousticness", "danceability", "valence", "energy", "tempo")

cont_data <- full_data %>% dplyr::select(all_of(vars))
data <- full_data %>% dplyr::select(c("track_id", "artist_name", "track_name", all_of(vars)))

gmm <- readRDS("models/spotify_adevt_gmm.rds")
km  <- readRDS("models/spotify_adevt_km.rds")

# Add cluster labels
gmm_data <- data
gmm_data$class <- as.factor(gmm$classification)

km_data <- data
km_data$class <- as.factor(km$cluster)

get_itunes_preview <- function(song, artist, limit = 1) {
  
  term <- paste(song, artist)
  
  url <- paste0(
    "https://itunes.apple.com/search?",
    "term=", URLencode(term),
    "&entity=song",
    "&limit=", limit
  )
  
  response <- httr::GET(url)
  json <- jsonlite::fromJSON(rawToChar(response$content))
  
  if (length(json$results) == 0) return(NA_character_)
  
  result = json$results
  return(result$previewUrl)
}


# =========================
# PCA EMBEDDING (NUMERIC FEATURES)
# =========================

pca_model <- prcomp(cont_data, scale. = TRUE)

pca_df <- data.frame(
  PC1 = pca_model$x[,1],
  PC2 = pca_model$x[,2]
)

pca_df <- bind_cols(pca_df, data %>% dplyr::select(track_id, artist_name, track_name))
pca_df$km_class  <- as.factor(km$cluster)
pca_df$gmm_class <- as.factor(gmm$classification)

# =========================
# FUNCTIONS
# =========================

find_closest_songs <- function(dataset, artist, track, n = 5) {
  if(!any(dataset$artist_name == artist & dataset$track_name == track)) {
    stop("Song not found in dataset")
  }
  
  target_song <- dataset %>%
    filter(artist_name == artist, track_name == track)
  
  target_cluster <- target_song$class
  
  cluster_songs <- dataset %>%
    filter(class == target_cluster) %>%
    filter(!(artist_name == artist & track_name == track))
  
  numeric_cols <- sapply(cluster_songs, is.numeric)
  numeric_cols["class"] <- FALSE
  
  song_features   <- as.matrix(cluster_songs[, numeric_cols])
  target_features <- as.numeric(target_song[, numeric_cols])
  
  X_norm2 <- rowSums(song_features^2)
  q_norm2 <- sum(target_features^2)
  cross   <- song_features %*% target_features
  
  d2 <- X_norm2 + q_norm2 - 2 * cross
  
  cluster_songs$dist <- sqrt(d2)
  
  closest_songs <- cluster_songs %>%
    arrange(dist) %>%
    head(n = n)
  
  closest_songs
}

choose_random_song <- function(data) {
  rand_song <- data %>% sample_n(1)
  artist <- rand_song %>% dplyr::pull(artist_name)
  track  <- rand_song %>% dplyr::pull(track_name)
  c(artist, track)
}

# =========================
# UI
# =========================

ui <- fluidPage(
  titlePanel("Spotify Clustering & Song Similarity Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "model_type", "Clustering Model",
        choices = c("K-Means" = "km", "GMM" = "gmm"),
        selected = "km"
      ),
      
      selectizeInput("artist", "Artist", choices = NULL),
      selectizeInput("track", "Track", choices = NULL),
      
      sliderInput("n_closest", "Number of Similar Songs",
                  min = 1, max = 20, value = 5),
      
      actionButton("random_song", "Pick Random Song")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Closest Songs",
                 uiOutput("closest_table"),
                 plotlyOutput("pca_plot"))
      )
    )
  )
)

# =========================
# SERVER
# =========================

server <- function(input, output, session) {
  
  preview_cache <- reactiveVal(list())
  
  get_preview_cached <- function(artist, track) {
    key <- paste(artist, track)
    cache <- preview_cache()
    
    if (!is.null(cache[[key]])) return(cache[[key]])
    
    url <- get_itunes_preview(artist, track)
    
    cache[[key]] <- url
    preview_cache(cache)
    
    url
  }
  
  
  current_data <- reactive({
    if (input$model_type == "km") km_data else gmm_data
  })
  
  observe({
    updateSelectizeInput(
      session, "artist",
      choices = sort(unique(current_data()$artist_name)),
      server = TRUE
    )
  })
  
  observeEvent(input$artist, {
    tracks <- current_data() %>%
      filter(artist_name == input$artist) %>%
      pull(track_name) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "track", choices = tracks)
  })
  
  observeEvent(input$random_song, {
    rs <- choose_random_song(current_data())
    
    updateSelectInput(session, "artist", selected = rs[1])
    
    tracks <- current_data() %>%
      filter(artist_name == rs[1]) %>%
      pull(track_name) %>%
      unique()
    
    updateSelectInput(session, "track",
                      choices = tracks,
                      selected = rs[2])
  })
  
  closest_reactive <- reactive({
    req(input$artist, input$track)
    df <- find_closest_songs(
      current_data(),
      input$artist,
      input$track,
      n = input$n_closest
    )
    df$preview_url <- purrr::map2_chr(
      df$artist_name,
      df$track_name,
      get_preview_cached
    )
    df
  })
  
  output$closest_table <- renderUI({
    df <- closest_reactive()
    
    if(nrow(df) == 0) return(tags$h4("No similar songs found."))
    
    tags$table(
      class = "table table-striped",
      tags$thead(
        tags$tr(
          lapply(c("Artist", "Track", "Cluster", vars, "Distance", "Preview"), tags$th)
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          song <- df[i, ]
          
          tags$tr(
            tags$td(song$artist_name),
            tags$td(song$track_name),
            tags$td(song$class),
            lapply(vars, function(v) tags$td(round(song[[v]], 3))),
            tags$td(round(song$dist, 3)),
            tags$td(
              if (!is.na(song$preview_url)) {
                HTML(paste0(
                  '<audio controls preload="none" style="width:150px;">',
                  '<source src="', song$preview_url, '" type="audio/mp4">',
                  'Your browser does not support the audio element.',
                  '</audio>'
                ))
              } else {
                "No preview"
              }
            )
          )
        })
      )
    )
  })
  
  output$audio_players <- renderUI({
    df <- closest_reactive()
    
    df <- df %>%
      filter(!is.na(preview_url) & nzchar(preview_url)) %>%
      slice(1:5)  # show top 5 with valid preview URLs
    
    if(nrow(df) == 0) return(tags$h4("No audio previews available for these songs."))
    
    tagList(
      lapply(seq_len(nrow(df)), function(i) {
        song <- df[i, ]
        
        tags$div(
          style = "margin-bottom: 15px;",
          tags$strong(paste0(i, ". ", song$artist_name, " – ", song$track_name)),
          tags$br(),
          HTML(paste0(
            '<audio controls preload="none" style="width:300px;">',
            '<source src="', song$preview_url, '" type="audio/mp4">',
            'Your browser does not support the audio element.',
            '</audio>'
          ))
        )
      })
    )
  })
  
  # =========================
  # PCA PLOT
  # =========================
  
  output$pca_plot <- renderPlotly({
    df <- closest_reactive()
    
    cls <- if (input$model_type == "km") "km_class" else "gmm_class"
    
    # ----------------------------
    # Selected song
    # ----------------------------
    selected_song <- pca_df %>%
      filter(artist_name == input$artist,
             track_name == input$track) %>%
      mutate(hover = paste0(
        artist_name, " – ", track_name, "<br>",
        "Cluster: ", .data[[cls]]
      ))
    
    # ----------------------------
    # Closest songs
    # ----------------------------
    closest_pts <- pca_df %>%
      filter(track_id %in% df$track_id) %>%
      mutate(hover = paste0(
        artist_name, " – ", track_name, "<br>",
        "Cluster: ", .data[[cls]]
      ))
    
    # ----------------------------
    # Background points (keep cluster for color)
    # ----------------------------
    bg <- pca_df %>%
      sample_n(5000) %>%
      mutate(hover = paste0(
        artist_name, " – ", track_name, "<br>",
        "Cluster: ", .data[[cls]]
      ))
    
    # ----------------------------
    # Plotly interactive PCA
    # ----------------------------
    plt <- plot_ly() %>%
      
      # Background cluster-colored points
      add_trace(
        data = bg,
        x = ~PC1,
        y = ~PC2,
        type = "scatter",
        mode = "markers",
        color = ~.data[[cls]],
        colors = "Set2",
        marker = list(size = 6, opacity = 0.45),
        text = ~hover,
        hoverinfo = "text",
        name = ~.data[[cls]],
        showlegend = TRUE
      ) %>%
      
      # Closest songs (red)
      add_trace(
        data = closest_pts,
        x = ~PC1,
        y = ~PC2,
        type = "scatter",
        mode = "markers",
        marker = list(size = 10, color = "red"),
        text = ~hover,
        hoverinfo = "text",
        name = "Closest Songs"
      ) %>%
      
      # Selected song (yellow with black outline)
      add_trace(
        data = selected_song,
        x = ~PC1,
        y = ~PC2,
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 16,
          color = "yellow",
          line = list(color = "black", width = 2)
        ),
        text = ~hover,
        hoverinfo = "text",
        name = "Selected Song"
      ) %>%
      
      layout(
        title = "PCA Embedding (Hover Labels)",
        xaxis = list(title = "PC1"),
        yaxis = list(title = "PC2")
      )
    
    plt
  })
  
  
}

# =========================
# RUN APP
# =========================

shinyApp(ui = ui, server = server)
