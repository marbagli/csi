#This script will calculate the Cognitive Salience Index starting from an Excel file organised as follows. 
#In the first column there is a code for informants: e.g., P1, P2, P3, etc.
#In the second column there is the Ranking of the item in the list. 
#In the third column there is the actual item. 

#Let's get the libraries going. 

library(readxl)
library(dplyr)
library(tidyverse)

#Now let's load the file
data <- read.csv("CSI_Italian_Data.csv")
#Remove any extra spaces (just in case)
colnames(data) <- trimws(colnames(data))
#Check column names and structure
colnames(data)
head(data)
print(colnames(data))

#1. Let's compute the total number of participants
N <- length(unique(data$Informant))
print(N)
#2. Let's count the number of distinct items. 
num_distinct_items_english <- data %>%
  summarise(unique_items = n_distinct(Item))
print(num_distinct_items_english)
#3. Let's calculate the mean of items per informant
mean_items_per_participant <- data %>%
  group_by(Informant) %>%  # Group by participant
  summarise(num_items = n()) %>%  # Count items per participant
  summarise(mean_items = mean(num_items))  # Compute the mean
print(mean_items_per_participant)

#Now let's compute the Frequency (F) and Mean Position (mP)
CSI_data <- data %>%    
  group_by(Item) %>%
  summarise(
#Frequency of the item
    F= n(),
#Mean Position (average Rank)
    MP=mean(Rank))%>%
#Remove items with Frequency lower than 3
filter(F >= 3)
#View the results
print(CSI_data)


#Now we can finally compute the CSI using the formula CSI = F/(N x MP)
CSI_data <- CSI_data %>%
  mutate(CSI = F/ (N*MP))
#Sort items by CSI in descending order
CSI_data <- CSI_data %>%
  arrange(desc(CSI))
#View the results
print (CSI_data) 
# Save the table as a CSV file
write.csv(CSI_data, "CSI_results_feb25.csv", row.names = FALSE)


#Let's create a Table
install.packages("kableExtra")
library(kableExtra)
#Generate the table
CSI_data %>%
  mutate(MP = round(MP, 0),
         CSI= round(CSI, 4))%>%
  kable(digits=3)%>%
  kable_styling(full_width = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

#Let's visualize the CSI with a graph
library(ggplot2)
# Create a bar plot of CSI values
ggplot(CSI_data, aes(x = reorder(Item, CSI), y = CSI)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  coord_flip() +  # Flip to make labels readable
  labs(title = "Cognitive Salience Index (CSI) by Item",
       x = "Item",
       y = "CSI") +
  theme_minimal() +
  theme(text = element_text(size = 14))

# Let's try a different visualization.
#Let's select only the first 10 items. 

CSI_top12 <- CSI_data %>%
  arrange(desc(CSI)) %>%
  head(12)
CSI_top12

#let's assign colors
color <- c(
  "rosso"= "red",
  "verde"= "green",
  "arancione"= "orange",
  "blu"= "blue",
  "viola"= "purple",
  "giallo" = "yellow",
  "nero"= "black",
  "rosa"= "pink",
  "bianco"= "white",
  "azzurro" = "#89CFF0",
  "marrone" = "brown",
  "grigio" = "grey"
)

# Convert 'Item' to factor with correct levels
CSI_top12$Item <- factor(CSI_top12$Item, levels = names(color))

# Bubble Chart: CSI size represents salience
ggplot(CSI_top12, aes(x = MP, y = F, size = CSI, label = Item, fill = Item)) +
  geom_point(alpha = 0.8, stroke=0.2, color= "black", shape=21 ) +
  geom_text(vjust = 2.8, size = 3, color= "black") +  # Add item labels above points
  scale_size_continuous(range = c(3, 10)) +  # Adjust bubble sizes
  scale_fill_manual(values = color, na.value = "gray") +
  #scale_x_continuous(breaks= seq(0, max(CSI_top12$MP), by=2))+
  #scale_y_continuous(breaks= seq(0, max(CSI_top12$F), by=2))+
  scale_x_continuous(breaks= seq(2, 16, by = 2), limits = c(2,16))+
  scale_y_continuous(breaks= seq(8,18, by=2), limits=c(8,18))+
  labs(
       x = "Mean Position (MP)",
       y = "Frequency (F)",
       size = "CSI",
       fill = "Item Color") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        panel.grid= element_blank(),
        legend.position= "none")
  
#Now let's repeat the same code for the English CSI among Italian students
#Now let's load the file
data2 <- read.csv("CSI English data.csv")
#Remove any extra spaces (just in case)
colnames(data2) <- trimws(colnames(data2))
#Check column names and structure
colnames(data2)
head(data2)
print(colnames(data2))

#Let's first count the number of distinct items. 
num_distinct_items_english <- data2 %>%
  summarise(unique_items = n_distinct(Item))
print(num_distinct_items_english)

mean_items_per_participant_eng <- data2 %>%
  group_by(Informant) %>%  # Group by participant
  summarise(num_items = n()) %>%  # Count items per participant
  summarise(mean_items = mean(num_items))  # Compute the mean
print(mean_items_per_participant_eng)

#Now let's compute the Frequency (F) and Mean Position (mP)
CSI_data_eng <- data2 %>%    
  group_by(Item) %>%
  summarise(
    #Frequency of the item
    F= n(),
    #Mean Position (average Rank)
    MP=mean(Rank))%>%
  #Remova items with Frequency lower than 3
  filter(F >= 3)
#View the results
print(CSI_data_eng)

#Now we need to compute the total number of participants
N <- length(unique(data$Informant))
CSI_data_eng <- CSI_data_eng %>%
  mutate(CSI = F/ (N*MP))
#Sort items by CSI in descending order
CSI_data_eng <- CSI_data_eng %>%
  arrange(desc(CSI))

#View the results
print (CSI_data_eng) 
# Save the table as a CSV file
write.csv(CSI_data_eng, "CSI_results_eng_feb25.csv", row.names = FALSE)

#Generate the table
CSI_data_eng %>%
  mutate(MP = round(MP, 0),
         CSI= round(CSI, 4))%>%
  kable(digits=3)%>%
  kable_styling(full_width = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Create a bar plot of CSI values
ggplot(CSI_data_eng, aes(x = reorder(Item, CSI), y = CSI)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  coord_flip() +  # Flip to make labels readable
  labs(title = "Cognitive Salience Index (CSI) by Item",
       x = "Item",
       y = "CSI") +
  theme_minimal() +
  theme(text = element_text(size = 14))

#let's assign colors
color <- c(
  "red"= "red",
  "green"= "green",
  "orange"= "orange",
  "blue"= "blue",
  "purple"= "purple",
  "yellow" = "yellow",
  "black"= "black",
  "pink"= "pink",
  "white"= "white",
  "brown" = "brown",
  "grey" = "grey",
  "magenta" = "magenta"
)

# Convert 'Item' to factor with correct levels
CSI_top11 <- CSI_data_eng %>%
  arrange(desc(CSI)) %>%
  head(11)
CSI_top11

CSI_top11$Item <- factor(CSI_top11$Item, levels = names(color))

# Bubble Chart: CSI size represents salience
ggplot(CSI_top11, aes(x = MP, y = F, size = CSI, label = Item, fill = Item)) +
  geom_point(alpha = 0.8, stroke=0.2, color= "black", shape=21 ) +
  geom_text(vjust = 2.8, size = 3, color= "black") +  # Add item labels above points
  scale_size_continuous(
    name= "CSI value", 
    range = c(3, 10),
    breaks = round(seq(min(CSI_top11$CSI), max(CSI_top11$CSI), length.out = 4), 4)) +
                          # Adjust bubble sizes
  scale_fill_manual(values = color, na.value = "gray", guide = "none") +
  scale_x_continuous(breaks= seq(0, max(CSI_top11$MP), by=2))+
  scale_y_continuous(breaks= seq(0, max(CSI_top11$F), by=2))+
  #scale_x_continuous(breaks= seq(2, 16, by = 2), limits = c(2,16))+
  #scale_y_continuous(breaks= seq(8,18, by=2), limits=c(8,18))+
  labs(
    x = "Mean Position (MP)",
    y = "Frequency (F)",
    size = "CSI value",
    fill = "Item Color") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        panel.grid= element_blank(),
        legend.text = element_text(size=10))+
  guides(
    size = guide_legend(
      override.aes = list(label = CSI_top11$Item),  # ✅ Adds item names to legend
      title = "CSI Value\n(Item Names)",  # ✅ Clear title with new line
      title.position = "top",
      title.hjust = 0.5))

#Now let's load the file of Sandford's original paper
dataS <- read.csv("Sandford_CSI_details.csv")
#Remove any extra spaces (just in case)
colnames(dataS) <- trimws(colnames(dataS))
#Check column names and structure
colnames(dataS)
head(dataS)
print(colnames(dataS))

#Since I only have access to the calculated data, I'll set the Number of participants (N) manually. 
N <- 29
dataS$CSI <- dataS$F / (N * dataS$MP)

# View the results
head(dataS)

ggplot(dataS, aes(x = MP, y = F, size = CSI, label = Item, fill = Item)) +
  geom_point(alpha = 0.8, stroke=0.2, color= "black", shape=21 ) +
  geom_text(vjust = 2.8, size = 3, color= "black") +  # Add item labels above points
  scale_size_continuous(range = c(3, 10)) +  # Adjust bubble sizes
  scale_fill_manual(values = color, na.value = "gray") +
  scale_x_continuous(breaks= seq(0, max(dataS$MP), by=2))+
  scale_y_continuous(breaks= seq(0, max(dataS$F), by=2))+
  labs(
    x = "Mean Position (MP)",
    y = "Frequency (F)",
    size = "CSI",
    fill = "Item Color") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        panel.grid= element_blank(),
        legend.position= "none")
