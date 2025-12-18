# Clear environment and unload packages
#rm(list = ls())
#pacman::p_unload(pacman::p_loaded())

# Load necessary packages
library(tidyr)
library(shinycssloaders)
library(plotly)
library(shiny)
library(dplyr)
library(purrr)
library(jsonlite)

# =========================
# LOAD DATA & MODELS
# =========================
# Read data
full_data = read.csv("SpotifyFeatures.csv")

# Remove duplicate tracks
duplicated_track_id = full_data[duplicated(full_data$track_id),"track_id"]
full_data = full_data %>% distinct(track_id, .keep_all = TRUE)

# Scale tempo and loudness
scale = function(x, na.rm = TRUE) {
  min_val <- min(x, na.rm = na.rm)
  max_val <- max(x, na.rm = na.rm)
  return((x - min_val) / (max_val - min_val))
}

# Apply the function to the data
scaled_tempo = scale(full_data$tempo)
full_data = full_data %>% mutate(tempo = scaled_tempo)

# Get rid of comedy
full_data = full_data %>% filter(genre!="Comedy")

# Choose vars for model
vars = c("acousticness", "danceability", "energy", 
         "valence", "tempo", "speechiness")

# Subset data to use later
cont_data = full_data %>% dplyr::select(all_of(vars))
data = full_data %>% dplyr::select(c("track_id", "artist_name", "track_name", all_of(vars)))

# Load models
gmm = readRDS("models/FINALGMM.rds")
km  = readRDS("models/FINALKM.rds")

# Add cluster labels to GMM data
gmm_data = data
gmm_data$class = as.factor(gmm$classification)

# Add cluster labels to KM data
km_data = data
km_data$class = as.factor(km$cluster)

# =========================
# PCA EMBEDDING
# =========================

pca_model = prcomp(cont_data, scale. = TRUE)

pca_df = data.frame(
  PC1 = pca_model$x[,1],
  PC2 = pca_model$x[,2]
)

pca_df = bind_cols(pca_df, data %>% dplyr::select(track_id, artist_name, track_name))
pca_df$km_class  = as.factor(km$cluster)
pca_df$gmm_class = as.factor(gmm$classification)

# =========================
# FUNCTIONS
# =========================

# Function to find closest song
find_closest_songs = function(dataset, artist, track, n = 5) {
  if(!any(dataset$artist_name == artist & dataset$track_name == track)) {
    stop("Song not found in dataset")
  }
  
  target_song = dataset %>%
    filter(artist_name == artist, track_name == track)
  
  # if (nrow(target_song) == 0) {
  #   # Song not found, return empty df
  #   return(data.frame())
  # }
  
  target_cluster = target_song$class
  
  cluster_songs = dataset %>%
    filter(class == target_cluster) %>%
    filter(!(artist_name == artist & track_name == track))
  
  numeric_cols = sapply(cluster_songs, is.numeric)
  numeric_cols["class"] = FALSE
  
  song_features   = as.matrix(cluster_songs[, numeric_cols])
  target_features = as.numeric(target_song[, numeric_cols])
  
  target_mat = t(replicate(nrow(song_features), target_features))
  diff = target_mat-song_features
  d2 = rowSums(diff^2)
  cluster_songs$dist = sqrt(d2)
  
  closest_songs = cluster_songs %>%
    arrange(dist) %>%
    head(n = n)
  
  closest_songs
}

# Function to choose random song
choose_random_song = function(data) {
  rand_song = data %>% sample_n(1)
  artist = rand_song %>% dplyr::pull(artist_name)
  track  = rand_song %>% dplyr::pull(track_name)
  return(c(artist, track))
}

# Function to get preview
get_itunes_preview = function(song, artist, limit = 1) {
  
  term = paste(song, artist)
  
  url = paste0(
    "https://itunes.apple.com/search?",
    "term=", URLencode(term),
    "&entity=song",
    "&limit=", limit
  )
  
  response = httr::GET(url)
  json = jsonlite::fromJSON(rawToChar(response$content))
  
  if (length(json$results) == 0) return(NA_character_)
  
  result = json$results
  return(result$previewUrl)
}


# =========================
# UI
# =========================

ui = fluidPage(
  titlePanel("Spotify Clustering & Song Similarity Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons(
        "model_type", "Clustering Model",
        choices = c( "GMM" = "gmm", "K-Means" = "km"),
        selected = "gmm"
      ),
      
      selectizeInput("artist", "Artist", choices = NULL),
      selectizeInput("track", "Track", choices = NULL),
      sliderInput("n_closest", "Number of Similar Songs",
                  min = 1, max = 20, value = 5),
      
      actionButton("random_song", "Pick Random Song")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Closest Songs",
                 fluidRow(
                   column(
                     width=12,
                     withSpinner(uiOutput("closest_table"))
                   )
                 ),
                 fluidRow(
                   column(
                     width = 7,
                     withSpinner(plotlyOutput("pca_plot"))
                   ),
                   column(
                     width = 5,
                     withSpinner(plotlyOutput("cluster_boxplots"))
                   )
                 ))
      )
    )
  )
)

# =========================
# SERVER
# =========================

server = function(input, output, session) {
  
  preview_cache = reactiveVal(list())
  
  get_preview_cached = function(artist, track) {
    key = paste(artist, track)
    cache = preview_cache()
    
    if (!is.null(cache[[key]])) return(cache[[key]])
    
    url = get_itunes_preview(artist, track)
    
    cache[[key]] = url
    preview_cache(cache)
    
    url
  }
  
  current_data = reactive({
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
    tracks = current_data() %>%
      filter(artist_name == input$artist) %>%
      pull(track_name) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "track", choices = tracks)
  })
  
  observeEvent(input$random_song, {
    dataset = current_data()
    rs = choose_random_song(dataset)
    artist_sel = rs[1]
    track_sel = rs[2]
    
    # update artist choices
    updateSelectizeInput(session, "artist",
                         choices = sort(unique(dataset$artist_name)),
                         selected = NULL,
                         server = TRUE)
    
    # schedule the selection to happen after choices are loaded
    session$onFlushed(function() {
      updateSelectizeInput(session, "artist",
                           choices = sort(unique(dataset$artist_name)),
                           selected = artist_sel,
                           server = TRUE)
      
      # Update tracks immediately after artist selection
      tracks = dataset %>%
        filter(artist_name == artist_sel) %>%
        pull(track_name) %>% unique() %>% sort()
      
      updateSelectInput(session, "track",
                        choices = tracks,
                        selected = track_sel)
    }, once = TRUE)
  })
  
  
  closest_reactive = reactive({
    req(input$artist, input$track)
    
    df = tryCatch(
      find_closest_songs(current_data(), input$artist, input$track, n = input$n_closest),
      error = function(e) data.frame()
    )
    
    # Prevent error while loading
    if (nrow(df) == 0) return(df)
    
    df = find_closest_songs(
      current_data(),
      input$artist,
      input$track,
      n = input$n_closest
    )
    
    # Show preview url
    df$preview_url = purrr::map2_chr(
      df$artist_name,
      df$track_name,
      get_preview_cached
    )
    df
  })
  
  output$closest_table = renderUI({
    df = closest_reactive()
    
    # If no similar songs...
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
          song = df[i, ]
          
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
  
  output$audio_players = renderUI({
    df = closest_reactive()
    
    # If no previews...
    if(nrow(df) == 0) return(tags$h4("No audio previews available for these songs."))
    
    tagList(
      lapply(seq_len(nrow(df)), function(i) {
        song = df[i, ]
        
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
  
  output$pca_plot = renderPlotly({
    df = closest_reactive()
    
    cls = if (input$model_type == "km") "km_class" else "gmm_class"
    
    # ----------------------------
    # Selected song
    # ----------------------------
    selected_song = pca_df %>%
      filter(artist_name == input$artist,
             track_name == input$track) %>%
      mutate(hover = paste0(
        artist_name, " – ", track_name, "<br>",
        "Cluster: ", .data[[cls]]
      ))
    
    # ----------------------------
    # Closest songs
    # ----------------------------
    closest_pts = pca_df %>%
      filter(track_id %in% df$track_id) %>%
      mutate(hover = paste0(
        artist_name, " – ", track_name, "<br>",
        "Cluster: ", .data[[cls]]
      ))
    
    # ----------------------------
    # Background points
    # ----------------------------
    bg = pca_df %>%
      sample_n(5000) %>%
      mutate(hover = paste0(
        artist_name, " – ", track_name, "<br>",
        "Cluster: ", .data[[cls]]
      ))
    
    # ----------------------------
    # Plotly PCA
    # ----------------------------
    plt = plot_ly() %>%
      
      # Background points
      add_trace(
        data = bg,
        x = ~PC1,
        y = ~PC2,
        type = "scatter",
        mode = "markers",
        color = ~.data[[cls]],
        colors = c(
          "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072",
          "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5",
          "#D9D9D9", "#BC80BD"
        ),
        marker = list(size = 6, opacity = 0.6),
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
        marker = list(size = 8, color = "red"),
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
          size = 10,
          color = "yellow",
          line = list(color = "black", width = 1)
        ),
        text = ~hover,
        hoverinfo = "text",
        name = "Selected Song"
      ) %>%
      
      layout(
        title = "PCA Embedding",
        xaxis = list(title = "PC1"),
        yaxis = list(title = "PC2")
      )
    
    plt
  })
  
  output$cluster_boxplots = renderPlotly({
    req(input$artist, input$track)
    
    dataset = current_data()
    
    # Selected song
    selected_song = dataset %>%
      filter(artist_name == input$artist,
             track_name == input$track)
    req(nrow(selected_song) > 0)
    
    cluster_id = selected_song$class[1]
    
    # All songs in cluster
    cluster_songs = dataset %>% filter(class == cluster_id)
    
    closest = closest_reactive()
    
    # Long format for boxplots
    cluster_long = cluster_songs %>%
      pivot_longer(all_of(vars),
                   names_to = "feature",
                   values_to = "value")
    
    # ---- BOX PLOT ----
    plt = plot_ly() %>%
      add_trace(
        data = cluster_long,
        type = "box",
        x = ~feature,
        y = ~value,
        boxpoints = "outliers",
        jitter = 0,
        pointpos = 0,
        marker = list(color = "blue"),
        name = "Distribution"
      )
    
    # ---- CLOSEST SONGS ----
    if (nrow(closest) > 0) {
      closest_long = closest %>%
        pivot_longer(all_of(vars),
                     names_to = "feature",
                     values_to = "value") %>%
        mutate(hover_text = paste0(
          "<b>", track_name, "</b><br>",
          artist_name, "<br>"
        ))
      
      plt = plt %>% add_trace(
        data = closest_long,
        type = "scatter",
        mode = "markers",
        x = ~feature,
        y = ~value,
        text = ~hover_text,
        hoverinfo = "text",
        marker = list(color = "red", size = 8),
        name = "Closest Songs"
      )
      
      # ---- SELECTED SONG ----
      selected_long = selected_song %>%
        pivot_longer(all_of(vars),
                     names_to = "feature",
                     values_to = "value") %>%
        mutate(hover_text = paste0(
          "<b>", track_name, "</b><br>",
          artist_name, "<br>"
        ))
      
      plt = plt %>% add_trace(
        data = selected_long,
        type = "scatter",
        mode = "markers",
        x = ~feature,
        y = ~value,
        text = ~hover_text,
        hoverinfo = "text",
        marker = list(
          color = "yellow",
          size = 10,
          line = list(color = "black", width = 2),
          opacity = 0.8
        ),
        name = "Selected Song"
      )
    }
    
    plt %>% layout(
      title = paste("Cluster", cluster_id, "Feature Distributions"),
      xaxis = list(title = ""),
      yaxis = list(title = "Scaled Value")
    )
  })
  
}

# =========================
# RUN APP
# =========================

shinyApp(ui = ui, server = server)
