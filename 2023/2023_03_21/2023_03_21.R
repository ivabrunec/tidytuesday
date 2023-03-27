# programming languages

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2023-03-21')
languages <- tuesdata$languages

# languages post-1940 & get index for plotting
# keep only 
languages <- languages |>
  filter(appeared > 1940 & language_rank <= 1000) |>
  group_by(appeared) |>
  mutate(id = rev(row_number())) |>
  ungroup()

# get list of top 50
lang_top_50 <- languages |>
  top_n(10, number_of_users)

# make list of top 50 languages
lang_top_list <- lang_top_50$title

languages$top_vals <- ifelse(languages$title %in% lang_top_list,1,0)
languages$top_vals <- as.factor(languages$top_vals)


library(ggtext)
a <- ggplot(data = languages, aes(xmin = appeared-1, xmax = appeared, 
                             ymin = id-1, ymax = id)) +
  geom_rect(aes(fill = top_vals, text = title), color='white',linewidth=.05) +
  theme_minimal() +
  labs(
    title = "<span style='color:grey40;'> When did the
    <span style='color:#fc9878;'>50 most popular</span>
    programming languages first appear?
    </span>") +
  theme(plot.title = element_markdown(lineheight = 1.1,size = 10),
        plot.background = element_rect(fill='#f8fffd',color=NA),
        panel.grid.major = element_line(linewidth = .2),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color='grey20'),
        axis.title = element_blank())

library(plotly)
ggplotly(a, tooltip = 'title')

  annotate("text", x = 1948, y = 52, 
           label = "Fortran", color = 'grey40', size = 2.5) 
  
 
ggsave('temp.png')
  annotate("text", x = 1965.5, y = 52, label = "50 most common", color = '#fc9878') +
  annotate("text", x = 1997, y = 52, label = "programming languages first appear?", color = 'grey40')


k## wordcloud code ####
library(wordcloud)

# get color scale of 50 colors
col_pal <- as.character(MetBrewer::met.brewer("Hokusai1", n=50))
# combine into temporary df
temp_df <- data.frame(lang_top_list, col_pal)
names(temp_df) <- c('title','colors2')

# merge this temporary df
languages <- left_join(languages, temp_df, by=c('title'))

tiff("test.tiff", units="in", width=5, height=3, res=300)
wordcloud(words = languages$title, freq = languages$number_of_users, 
          scale = c(3,.5),colors = languages$colors2,
          random.order = FALSE, ordered.colors = TRUE, max.words=50)
dev.off()
