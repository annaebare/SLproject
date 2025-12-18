## Description
This R Shiny app was for a class project. We used KM and GMM to cluster Spotify song data sourced from (https://www.kaggle.com/datasets/zaheenhamidani/ultimate-spotify-tracks-db), and created an R Shiny app that returns songs similar to a user selected song. In this case, "similar" is defined in terms of the Euclidean distance, so that the app returns the top $n$ songs with the smallest Euclidean distance to the selected song (based on variables $\texttt{acousticness}$, $\texttt{danceability}$, $\texttt{valence}$, $\texttt{energy}$, $\texttt{tempo}$, and $\texttt{speechiness}$). The app also includes song previews and interactive visualizations.

## Link to app
The app can be accessed at https://bare.shinyapps.io/slproject/
