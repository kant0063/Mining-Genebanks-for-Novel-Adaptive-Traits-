# Load libraries
library(ggplot2)
library(tidyverse)
library(emo)
library(viridis)
library(hrbrthemes)
library(ggpubr)
library(emojifont)
library(showtext)
library(emoGG)

# Create the dataset with crop info and emojis
data <- data.frame(
  Crop = c("Sugarbeet", "Soybean", "Cassava", "Rice", "Sugarcane", 
           "Tomato", "Potato", "Wheat", "Maize", "Oil Palm"),
  genebank =  c("GB", "GB", "GB", "GB", "GB", 
                "GB", "GB", "GB", "GB", "GB"),
  CollectionSize = c(2694, 73898, 13974, 259892, 1665, 
                     57164, 20298, 431075, 139466, 638),
  YearsToExplore = c(725.5, 546084.1, 19525.9, 6754359.2, 277.1, 
                     326766.6, 41198.9, 18582522.5, 1945062.6, 40.6),
  Emoji = c("🥬", "🌱", "🌿", "🌾", "🍬", 
            "🍅", "🥔", "🌾", "🌽", "🌴")  # Add crop emojis
)

# Combine emoji with crop name for labeling
data$CropLabel <- paste(data$Emoji, data$Crop)

# Convert to long format for stacked bar chart
data_long <- data %>%
  pivot_longer(cols = c(CollectionSize, YearsToExplore),
               names_to = "Metric", values_to = "Value")

# Load emojis and their fonts (if necessary)
showtext_auto() # Automatically set the font for the plot

# Plot the stacked bar chart
ggplot(data_long, aes(x = reorder(CropLabel, Value), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(
    title = "Crop Collection Size and Exploration Time",
    x = "Crop",
    y = "Value",
    fill = "Metric"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("CollectionSize" = "#1f77b4", "YearsToExplore" = "#ff7f0e")) +
  theme_minimal(base_size = 14)

data<-read.csv("Figure_for_science_V2.csv")
head(data)

A<-ggplot(data, aes(x = reorder(genebank, Collection.Size), y = Collection.Size, fill = Crop, label = Crop)) +
  geom_bar(position="stack",stat = "identity") +
 # geom_text(label = paste0(data$Crop," ","(",data$CollectionSize, ")"),
  #          size = 4, position = position_stack(vjust = 0.5))+
 labs(
    title = "Global Crop Collection Size",
    x = "Crop",
    y = "Number of Accessions"
  ) +
   scale_fill_viridis(discrete = T) +
   theme_ipsum() +
  scale_y_continuous(labels = scales::comma) +
 theme_minimal(base_size = 20) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(strip.text = element_text(size = 20, face = "bold"))

B<-ggplot(data, aes(x = reorder(genebank, Number.of.potential.pairwise.crosses), 
                    y = Number.of.potential.pairwise.crosses, fill = Crop, label = Crop)) +
  geom_bar(position="stack",stat = "identity") +
  # geom_text(label = paste0(data$Crop," ","(",data$CollectionSize, ")"),
  #          size = 4, position = position_stack(vjust = 0.5))+
  labs(
    title = "Potential Crosses",
    x = "Crop",
    y = "Number of Populations"
  ) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 20) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(strip.text = element_text(size = 20, face = "bold"))



C<-ggplot(data, aes(x = reorder(genebank, Time_Years), y = Time_Years, fill = Crop)) +
  geom_bar(position="stack",stat = "identity") +
 # geom_text(label = paste0(data$Crop," ","(",data$YearsToExplore, ")"),
#    size = 4, position = position_stack(vjust = 0.5))+
  labs(
    title = "Time to Evaluate Populations",
    x = "Crop",
    y = "Number of Years") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 20) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(strip.text = element_text(size = 20, face = "bold"))


ggarrange(A,B,C, labels="AUTO", nrow =1, ncol =3, common.legend = T)

