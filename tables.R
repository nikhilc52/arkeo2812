library(tidyverse)

greek_translation <- tibble(
  greek = head(new_greek$word, 5),
  english = c('the','and','but','him/herself','am/be')
)


cherokee_translation <- tibble(
  cherokee = head(cherokee$Cherokee, 5),
  english = c('and','it','this/these','were','never/not')
)

chars <- c('a','b','c','d','e','_')
words <- data.frame(
  word = c('')
) 

words$word <- list(paste(sample(chars, 1000000, replace=TRUE), collapse=''))

words <- words |> 
  separate_longer_delim(word, delim="_")

freq_rand_words <- words |> 
  group_by(word) |> 
  summarize(
    count = n()
  ) |> 
  filter(nchar(word) >= 1) |> 
  mutate(freq_rank = rank(-count,ties.method="min"))


freq_rand_words |> 
  ggplot()+
  geom_point(aes(x=freq_rank, y=count),color="black",shape=20,size=4)+
  
  scale_color_identity()+
  labs(
    title="Zipf's law in Random Words",
    subtitle = "Log/Log | Fitted: y ~ 1/x",
    caption="Nikhil Chinchalkar | Arkeo 2812"
  )+
  ylab("Word Frequency")+
  xlab("Word Rank")+
  
  scale_x_continuous(transform = "log10",labels = scales::label_comma(),n.breaks=10)+
  scale_y_continuous(transform = "log10",labels = scales::label_comma(),n.breaks=10)+
  
  theme_set(theme_gray(base_size = 20, base_family = "EB Garamond" ))+
  theme(plot.title = element_text(size = 30, hjust =0, face = "bold",color="black"), 
        plot.subtitle = element_text(size = 22, hjust =0, color="black"))+
  theme(legend.title = element_text( size=9), legend.text=element_text(size=9))+
  theme(panel.background = element_rect(fill="white", color="white"))+
  theme(panel.grid.major = element_line(color="#e3e3e3"))+
  theme(legend.background = element_rect(fill="white", color="white"))+
  theme(axis.ticks.x = element_line(colour="#e3e3e3"))+
  theme(axis.ticks.y = element_line(colour="#e3e3e3"))+
  theme(legend.box.background = element_rect(fill="white", color="white"))+
  
  geom_abline(slope=-1, intercept=log(4730, 10), color='blue')

