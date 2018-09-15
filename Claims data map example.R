#BEFORE RUNNING SCRIPT:
#installed ImageMagick from https://www.imagemagick.org/script/download.php#windows
#Used this guide for installing Rtools http://stat.sfu.ca/statgen/resources/r-tools-for-building-packages-on-windows.html

#Packages:
install.packages("devtools")
library(devtools)
install_github("thomasp85/gganimate")

install.packages("tidyverse")
library(tidyverse)

claims <- download.file("https://data.cdc.gov/api/views/iw6q-r3ja/rows.csv", destfile="claims.csv")
claims <- read.csv("claims.csv")

#Clean up labels
agedata$Indicator <- str_extract(agedata$Indicator, "aged [0-9][0-9] [^|]+(?= with)") %>% unique
agedata$Break_Out <- str_replace(agedata$Break_Out, "Overall", "   ") %>% unique

#Make plot
ggplot(data=agedata, 
       aes(x=Indicator, y=Data_Value, fill=Indicator)) +
  scale_fill_manual(values=c("cadetblue1", "cadetblue3", "cadetblue4")) +
  geom_violin() +
  ylim(0,200) +
  # Here comes the gganimate code
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') +
  transition_states(
    paste(Break_Out_Category, Break_Out, sep=": "),
    transition_length = 2,
    state_length = 6
  ) +
  #scale_x_discrete(position="top") +
  labs(title = 'Heart failure hospitalizations per 1,000 people at state level', 
       subtitle = "{closest_state}",
       x = '', y = 'Rate',
       caption='Adults with heart failure as the principal \ndiagnosis among FFS Medicare beneficiaries\nSource: https://data.cdc.gov/api/views/iw6q-r3ja/rows.csv') +
  theme_classic() +
  theme(legend.position = "none",
        plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))





