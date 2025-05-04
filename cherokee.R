library(tidyverse)
library(ggthemes)
library(ggrepel)
library(dplyr)
library(showtext)

data <- readxl::read_xlsx('cherokee/cherokee.xlsx')

font_add_google("EB Garamond")
showtext_auto()

cleaned <- data |> 
  select(Cherokee) |> 
  separate_longer_delim(Cherokee, delim=" ")

cherokee <- cleaned |> 
  group_by(Cherokee) |> 
  summarise(
    count=n()
  ) |> 
  mutate(freq_rank = rank(-count,ties.method="min"))

cherokee |> 
  ggplot()+
  geom_point(aes(x=freq_rank, y=count),color="black",shape=20,size=4)+

  scale_color_identity()+
  labs(
    title="Zipf's law in Cherokee",
    subtitle = ""
  )+
  ylab("Frequency")+
  xlab("Rank")+
  
  scale_x_continuous(transform = "log10",labels = scales::label_comma(),n.breaks=6)+
  scale_y_continuous(transform = "log10",labels = scales::label_comma(),n.breaks=6)+
  
  theme_set(theme_gray(base_size = 20, base_family = "EB Garamond" ))+
  theme(plot.title = element_text(size = 30, hjust =0, face = "bold",color="black"), 
        plot.subtitle = element_text(size = 22, hjust =0, color="black"))+
  guides(color = guide_legend(title.position = "top", 
                              title.hjust = 0.5,
                              label.position = "bottom"))+
  theme(legend.title = element_text( size=9), legend.text=element_text(size=9))+
  theme(panel.background = element_rect(fill="white", color="white"))+
  theme(panel.grid.major = element_line(color="#e3e3e3"))+
  theme(legend.background = element_rect(fill="white", color="white"))+
  theme(axis.ticks.x = element_line(colour="#e3e3e3"))+
  theme(axis.ticks.y = element_line(colour="#e3e3e3"))+
  theme(legend.box.background = element_rect(fill="white", color="white"))
