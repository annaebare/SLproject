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
vars <- c("acousticness", "danceability", "energy", "instrumentalness",
          "valence", "speechiness")

cont_data <- full_data %>% dplyr::select(all_of(vars))
data <- full_data %>% dplyr::select(c("track_id", "artist_name", "track_name", all_of(vars)))

gmm <- readRDS("models/spotify_adevt_gmm.rds")
km  <- readRDS("models/spotify_adevt_km.rds")

# Add cluster labels
gmm_data <- data
gmm_data$class <- as.factor(gmm$classification)

km_data <- data
km_data$class <- as.factor(km$cluster)


# =========================
# GET PREVIEW URLS
# =========================
# Sys.setenv(
#   SPOTIFY_CLIENT_ID     = "ccc922e2b87d4316ac0883cee2e7add6",
#   SPOTIFY_CLIENT_SECRET = "3c8edab6128a493bbedabcde311b1683"
# )
# 
# access_token <- get_spotify_access_token()
# 
# library(httr)
# library(jsonlite)
# library(purrr)
# 
# get_preview_urls_safe <- function(track_id, access_token) {
#   
#     url <- paste0(
#       "https://api.spotify.com/v1/tracks?ids=",
#       paste(track_id, collapse = ",")
#     )
# 
#     res <- GET(
#       url,
#       add_headers(Authorization = paste("Bearer", access_token))
#     )
# 
#     if (status_code(res) != 200) {
#       warning("Spotify API request failed with status ", status_code(res))
#       return(rep(NA_character_, length(id_batch)))
#     }
# 
#     parsed <- content(res, as = "parsed", simplifyVector = TRUE)
#     return(parsed)
  # 
  # track_ids <- track_ids[!is.na(track_ids)]
  # track_ids <- track_ids[track_ids != ""]
  
  #batches <- split(track_ids, ceiling(seq_along(track_ids) / 50))
  
  # preview_urls <- map(batches, function(id_batch) {
  #   
  #   url <- paste0(
  #     "https://api.spotify.com/v1/tracks?ids=",
  #     paste(id_batch, collapse = ",")
  #   )
  #   
  #   res <- GET(
  #     url,
  #     add_headers(Authorization = paste("Bearer", access_token))
  #   )
  #   
  #   if (status_code(res) != 200) {
  #     warning("Spotify API request failed with status ", status_code(res))
  #     return(rep(NA_character_, length(id_batch)))
  #   }
  #   
  #   parsed <- content(res, as = "parsed", simplifyVector = FALSE)
  #   return(parsed)
  #   
  #   if (is.null(parsed$tracks)) {
  #     return(rep(NA_character_, length(id_batch)))
  #   }
  #   
  #   map_chr(parsed$tracks, function(x) {
  #     
  #     # ✅ THIS IS THE CRITICAL SAFETY FIX
  #     if (is.null(x)) return(NA_character_)
  #     if (!is.list(x)) return(NA_character_)
  #     if (is.null(x$preview_url)) return(NA_character_)
  #     
  #     x$preview_url
  #   })
  #   
  # }) |> unlist()
  # 
  # preview_urls
# }

# 
# test <- get_preview_urls_safe(
#   "11dFghVXANMlKmJXsNCbNl",
#   access_token
# )
# 
# library(httr)
# 
# res <- GET(
#   "https://api.spotify.com/v1/tracks/11dFghVXANMlKmJXsNCbNl",
#   add_headers(Authorization = paste("Bearer", access_token))
# )
# 
# status_code(res)
# 
# 
# status_code(res)
# content(res, as = "text")
# parsed <- content(res, as = "parsed", simplifyVector = TRUE)
# parsed$preview_url
# 
# library(httr)
# library(jsonlite)
# 
# library(httr)
# library(jsonlite)
# 
get_deezer_preview <- function(artist, track) {
  # Construct the query safely
  query <- URLencode(paste0('artist:"', artist, '" track:"', track, '"'))

  url <- paste0("https://api.deezer.com/search?q=", query)

  res <- GET(url)
  parsed <- content(res, as = "parsed", simplifyVector = TRUE)

  if (length(parsed$data$preview) != 1) {
    return(NA_character_)
  }

  return(parsed$data$preview)
}

#get_deezer_preview("Nicki Minaj", "Barbie Dreams")
# 
# # Example
# 
# preview_df <- data %>%
#   dplyr::select(artist_name, track_name) %>%
#   rowwise() %>%
#   mutate(preview_url = get_deezer_preview(artist_name, track_name)) %>%
#   ungroup()
# 
# # Assuming 'my_dataframe' is the data frame to save
# save(preview_df, file = "preview_urls.RData")


# =========================
# PCA EMBEDDING (NUMERIC FEATURES)
# =========================

numeric_cols <- sapply(cont_data, is.numeric)
pca_model <- prcomp(cont_data[, numeric_cols], scale. = TRUE)

pca_df <- data.frame(
  PC1 = pca_model$x[,1],
  PC2 = pca_model$x[,2]
)

pca_df <- bind_cols(pca_df, data %>% dplyr::select(artist_name, track_name))
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
                 uiOutput("closest_table")),
        
        tabPanel("Distance Plot",
                 plotlyOutput("distance_plot")),
        
        tabPanel("PCA Embedding",
                 plotlyOutput("pca_plot")),
        
        tabPanel("Audio Feature Comparison",
                 plotlyOutput("feature_plot")),
        
        tabPanel("Audio Previews",
                 uiOutput("audio_players"))
      )
    )
  )
)

# =========================
# SERVER
# =========================

server <- function(input, output, session) {
  
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
    df$preview_url <- purrr::map2_chr(df$artist_name, df$track_name, get_deezer_preview)
    
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
                  '<source src="', song$preview_url, '" type="audio/mpeg">',
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
            '<source src="', song$preview_url, '" type="audio/mpeg">',
            'Your browser does not support the audio element.',
            '</audio>'
          ))
        )
      })
    )
  })
  
  
  
  output$distance_plot <- renderPlotly({
    df <- closest_reactive()
    
    p <- ggplot(df, aes(x = reorder(track_name, dist), y = dist)) +
      geom_col() +
      coord_flip() +
      labs(x = "Track", y = "Distance",
           title = "Distances to Closest Songs")
    
    ggplotly(p)
  })
  
  # =========================
  # PCA PLOT
  # =========================
  
  output$pca_plot <- renderPlotly({
    cls <- if (input$model_type == "km") "km_class" else "gmm_class"
    
    p <- ggplot(pca_df, aes(x = PC1, y = PC2, color = .data[[cls]])) +
      geom_point(alpha = 0.5) +
      labs(title = "PCA Embedding")
    
    # Highlight selected song
    if (!is.null(input$artist) && !is.null(input$track)) {
      sel <- pca_df %>%
        filter(artist_name == input$artist,
               track_name == input$track)
      
      p <- p + geom_point(data = sel, size = 4, color = "black")
    }
    
    ggplotly(p)
  })
  
  # =========================
  # AUDIO FEATURE COMPARISON
  # =========================
  
  output$feature_plot <- renderPlotly({
    df <- closest_reactive()
    
    target <- current_data() %>%
      filter(artist_name == input$artist,
             track_name == input$track)
    
    numeric_cols <- sapply(df, is.numeric)
    numeric_cols["dist"]  <- FALSE
    numeric_cols["class"] <- FALSE
    
    feature_names <- names(df)[numeric_cols]
    
    target_long <- target %>%
      select(all_of(feature_names)) %>%
      pivot_longer(everything(), names_to = "feature", values_to = "value") %>%
      mutate(type = "Target")
    
    compare_long <- df %>%
      slice(1) %>%
      select(all_of(feature_names)) %>%
      pivot_longer(everything(), names_to = "feature", values_to = "value") %>%
      mutate(type = "Closest Neighbor")
    
    plot_df <- bind_rows(target_long, compare_long)
    
    p <- ggplot(plot_df, aes(x = feature, y = value, fill = type)) +
      geom_col(position = "dodge") +
      coord_flip() +
      labs(title = "Audio Feature Comparison")
    
    ggplotly(p)
  })
}

# =========================
# RUN APP
# =========================

shinyApp(ui = ui, server = server)
