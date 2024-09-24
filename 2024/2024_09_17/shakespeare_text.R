# text analysis of 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidytext)
library(ggplot2)

tuesdata <- tidytuesdayR::tt_load('2024-09-17')

hamlet <- tuesdata$hamlet
macbeth <- tuesdata$macbeth
romeo_juliet <- tuesdata$romeo_juliet

# build function to extract sentiment by scene & by word in each scene
sentiment_by_scene <- function(df) {
  df <- df |>
    filter(character != "[stage direction]") |>
    group_by(act, scene) |>
    mutate(act_scene_id = cur_group_id()) |>
    ungroup()
  
  total_act_list <- max(df$act_scene_id)
  
  # initialize lists to save corpus dfs & average sentiments
  summary_per_scene <- list()
  corpus_per_scene <- list()

  for (act_count in 1:max(df$act_scene_id)){

    act_df <- df |>
      dplyr::filter(act_scene_id == act_count)
    
    # get all tokens in this act in this scene
    words <- act_df |> 
      unnest_tokens("word", dialogue)
    
    num_words <- nrow(words)
    
    # count positive and negative sentiments & add continuous measure
    word_sentiments <- words |>
      anti_join(stop_words) |>
      inner_join(get_sentiments("bing")) |>
      left_join(get_sentiments("afinn"))
    
    average_val <- mean(word_sentiments$value, na.rm = T)
    
    # construct df with scene-level info
    scene_data <- data.frame (
        act = act_df$act[1],
        scene = act_df$scene[1],
        act_scene_count = act_count,
        total_num_words = num_words,
        average_sentiment = average_val
      )
    
    summary_per_scene[[act_count]] <- scene_data
    corpus_per_scene[[act_count]] <- word_sentiments

  }
  # combine lists into dfs to return
  sentiment_df <- do.call(rbind, summary_per_scene)
  corpus_df <- do.call(rbind, corpus_per_scene)
  
  return_object <- list(sentiment = sentiment_df,
                        corpus = corpus_df)
  return(return_object)
}

# apply function to list of plays
play_list <- list(hamlet = hamlet, 
                  macbeth = macbeth, 
                  romeo_juliet = romeo_juliet)

summary_list <- lapply(play_list, sentiment_by_scene)
summary_df <- bind_rows(lapply(summary_list, `[[`, "sentiment"), .id = "play_title")
corpus_df <- bind_rows(lapply(summary_list, `[[`, "corpus"), .id = "play_title")

# build timeline with cells corresponding to acts/scenes
# scale by total word count per scene
timeline_builder <- function(df){
  total_words <- sum(df$total_num_words)
  
  df_timeline <- df |>
    mutate(
      proportion = total_num_words / total_words,
      scaled_width = proportion * 100,
      start = c(0, cumsum(scaled_width)[-n()]),
      end = cumsum(scaled_width)
    )
  
  return(df_timeline)
}

timeline_stacked_df <- summary_df %>%
  group_by(play_title) %>%
  group_modify(~ timeline_builder(.x)) %>%
  ungroup()

# combine corpus with timeline info
corpus_df <- left_join(corpus_df, timeline_stacked_df,
                       by = c('play_title', 'act', 'scene'))

# assign X and Y values as a grid within each scaled width rectangle
# something isn't right about this but tbh I can't figure out what.
# leaving this here but didn't end up using
assign_grid_coordinates <- function(df) {
  df |>
    group_by(play_title,start, end) |>
    mutate(
      n_cols = round(scaled_width),
      word_count = n(),  
      n_rows = ceiling(word_count / n_cols),  
      row = (row_number() - 1) %/% n_cols + 1,  
      col = (row_number() - 1) %% n_cols + 1, 
      
      # evenly space x values within start and end limits
      x_position = start + (col - 1) * ((end - start) / (n_cols - 1)),
            y_position = row+30) |>
    ungroup()
}

assign_random_coordinates <- function(df) {
  df |>
    group_by(play_title, start, end) |>  # Group by play_title
    mutate(
      # Randomly assign x values between start and end
      x_position = runif(n(), min = start, max = end),
      # Randomly assign y values between 0 and 40
      y_position = runif(n(), min = 20, max = 40)
    ) |>
    ungroup()
}


df_with_grid_coords <- assign_random_coordinates(corpus_df)

ggplot(df_with_grid_coords) +
  geom_rect(aes(xmin = start, xmax = end, ymin = 0, ymax = 20,
                fill = average_sentiment)) +
  geom_text(aes(x = x_position, y = y_position, label = word, color = sentiment),
            size = 1, hjust = .25) +
  facet_wrap(~play_title, nrow = 3) +
  labs(x = "Play Progression", y = "", fill = "Scene") +
  #theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid = element_blank())


ggsave('temp.png', width = 20, height = 4)

