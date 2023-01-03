# Jan 3 2023
# bring your own data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(rvest)
library(tidytext)
library(showtext)

font_add_google(name = 'Staatliches', family = 'Staatliches')
font_add_google(name = 'Inter', family = 'Inter')
showtext_auto()


# scrape transcripts
website_list <- c('https://transcripts.foreverdreaming.org/viewtopic.php?t=28118&sid=6f892a6fd7a184bbce0452dc81e33452&view=print',
                  'https://transcripts.foreverdreaming.org/viewtopic.php?t=28133&view=print',
                  'https://transcripts.foreverdreaming.org/viewtopic.php?t=28204&view=print',
                  'https://transcripts.foreverdreaming.org/viewtopic.php?t=28256&view=print',
                  'https://transcripts.foreverdreaming.org/viewtopic.php?t=28299&view=print',
                  'https://transcripts.foreverdreaming.org/viewtopic.php?t=28364&view=print',
                  'https://transcripts.foreverdreaming.org/viewtopic.php?t=34716&view=print',
                  'https://transcripts.foreverdreaming.org/viewtopic.php?t=34717&view=print',
                  'https://transcripts.foreverdreaming.org/viewtopic.php?t=34718&view=print',
                  'https://transcripts.foreverdreaming.org/viewtopic.php?t=34719&view=print',
                  'https://transcripts.foreverdreaming.org/viewtopic.php?t=34720&view=print',
                  'https://transcripts.foreverdreaming.org/viewtopic.php?t=34721&view=print')

# episode title list
title_list <- c('1.1','1.2','1.3','1.4','1.5','1.6',
                '2.1','2.2','2.3','2.4','2.5','2.6')

# first, build a corpus of all sentiments
full_corpus_list <- list()
val_list <- list()

for (website in 1:length(website_list)){
  website_full <- read_html(website_list[website])
  text_clean <- website_full %>%
    html_text()
  
  text_df <- as.data.frame(text_clean)
  
  # get all tokens (= words)
  words <- text_df %>% 
    unnest_tokens("word", text_clean)
  
  # count positive and negative sentiments
  bing_word_counts <- words %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  bing_word_counts$episode <- title_list[[website]]
  
  # get value by word
  afinn_sentiments <- words %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments('afinn'))
  afinn_sentiments$episode <- title_list[[website]]
  
  #average_val <- mean(afinn_sentiments$value)
  
  val_list[[website]] <- afinn_sentiments
  full_corpus_list[[website]] <- bing_word_counts
  
}

full_corpus <- do.call(rbind, full_corpus_list)
val_corpus <- do.call(rbind, val_list)

# remove the word 'popular' - it's part of the website description
full_corpus <- filter(full_corpus, word != 'popular')
val_corpus <- filter(val_corpus, word != 'popular')

val_corpus <- val_corpus |>
  group_by(episode) |>
  mutate(index = row_number())

col_pal = colorspace::diverging_hcl(100, palette = "Cork")
col_pal = MetBrewer::met.brewer('Veronese',100)

ggplot() +
  geom_tile(data=val_corpus,aes(x=episode,y=index,fill=value), width =.6)+
  scale_fill_gradientn(colors=col_pal) +
  theme_minimal() +
  theme(plot.background = element_rect(fill='grey8',color=NA),
        legend.position='bottom',
        legend.text = element_text(color='grey90',family='Inter',size=20),
        legend.key.height = unit(dev.size()[2] / 40, 'inches'),
        legend.title = element_blank(),
        axis.text = element_text(color='grey90',family='Inter',size=20),
        axis.title = element_text(color='grey90',family='Inter',size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color='white', family='Staatliches',
                                  hjust=.5, size = 90),
        plot.subtitle = element_text(color='white', family='Staatliches',
                                  hjust=.5, size = 30),
        plot.caption=element_text(color='white',family='Inter',
                                  hjust=.5,size=20)) +
  ylab('valenced words in episode') +
  labs(title = 'FLEABAG',
       subtitle = 'emotions in each episode',
       caption = 'word emotional valence')
ggsave('valence_by_episode.png',height=3,width = 4,dpi=300)

val_sum <- val_corpus |>
  group_by(episode) |>
  distinct() |>
  summarise(mean_sentiment = mean(value))

# read in ratings
ratings <- read.csv('fleabag_ratings.csv')

val_ratings <- merge(val_sum, ratings, by=c('episode'))

ggplot(data=val_ratings) +
  geom_hline(yintercept=0, color='grey40', size=.1, linetype='dashed') +
  geom_segment(aes(x=episode,xend=episode, y=0, yend=mean_sentiment), color='grey40') +
  geom_point(aes(x=episode, y=mean_sentiment, color=mean_sentiment),size=2) +
  scale_color_gradientn(colors=col_pal) +
  theme_minimal() +
  theme(plot.background = element_rect(fill='grey8',color=NA),
        legend.position='none',
        legend.text = element_blank(),
        legend.key.height = unit(dev.size()[2] / 30, 'inches'),
        legend.title = element_blank(),
        axis.text = element_text(color='grey90',family='Inter',size=20),
        axis.title = element_text(color='grey90',family='Inter',size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(color='white',family='Inter',
                                    hjust=.5, size = 16)) +
  ylab('mean valence per episode') 
ggsave('mean_valence.png',height=1.5,width = 4,dpi=300)

# combine into full 'poster'
library(magick)

per_ep <- image_read('valence_by_episode.png')
mean_val <- image_read("mean_valence.png")
fox <- image_read("fleabag.jpg")
fox1 <- image_crop(fox, "1920x100+0+170")
fox2 <- image_crop(fox, "1920x100+0+450")
fox3 <- image_crop(fox, "1920x200+0+860")
fox1 <- image_scale(fox1,'1200')
fox2 <- image_scale(fox2,'1200')
fox3 <- image_scale(fox3,'1200')

img <- c(fox1, mean_val, fox2, per_ep, fox3)

img2<-image_append(img, stack = TRUE)
image_write(img2,'fleabag_final_stack.png')

