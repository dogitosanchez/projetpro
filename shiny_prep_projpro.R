library(readr)
library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra)

aglp1 <- read_xlsx("aGLP1 - Cas Erreur médicamenteuses_Surdosages.xlsx", 
                     sheet = "Complet")
# Requête 1

# 1. Source des cas : Cas CRPV/CEIP ;
# 2. Type des cas : 
#   - Erreur médicamenteuse ;
# - Erreur médicamenteuse sans effet indésirable ;e
# - Surdosage ;
# - Surdosage accidentel ;
# 3. Date : Sans restriction ;
# 4. Médicament (Substance) : 
#   - EXENATIDE ;
# - LIRAGLUTIDE ;
# - DULAGLUTIDE ;
# - SEMAGLUTIDE ;
# 5. Imputabilité OMS : Suspect ou Interaction ;
# 6. Gravité : Sans restriction ;
# 7. Population : Sans restriction ;
# 8. Effet(s) : Sans restriction ;


# Au total, 32 cas ont été identifiés.

# Filter the data for the specified medications
#exénatide, liraglutide non present --> cherche dans le colonne de 'Narratif' en suite ajouté avec le reste. 
#exénatide pas trouvé même sur le fichier Excel d'aglp1. 
medications <- c("XULTOPHY", "TRULICITY", "OZEMPIC")
data <- aglp1[stringr::str_detect(aglp1$Médicaments, paste(medications, collapse = "|", sep = "\\b")), ]

# Filter the data for the specified type of cases
typ_cases <- c("Erreur médicamenteuse", "Erreur médicamenteuse sans effet indésirable", "Surdosage", "Surdosage accidentel")
data <- subset(data, data$`Typ Cas` %in% typ_cases)
# Imputabilité OMS : Suspect ou Interaction 
# Print the filtered data to the console
#print(data)

# Export the filtered data as a CSV file
#write.csv(data, file = "requete1.csv", row.names = FALSE)

########## Filtering ############

###### Sex ######
df <- data%>%select(4,8)
df_group <- df %>% group_by(`Typ Cas`)
formula <- 'M ~ F ~ `Typ Cas`'
df$count <- 1
df_count <- aggregate(df$count,by=list(df$`Typ Cas`, df$Sex),sum)

colnames(df_count) <- c("Cas","Sex","n")


##### Graph Sex Filter #####

sexplot <- ggplot(df_count, aes(x = Cas, y = n, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Incidents par catégorie et sexe",
       x = "Catégorie",
       y = "Nombre d'incidents",
       fill = "Sexe") +
  theme_minimal() + 
  scale_fill_manual(values = c("lightblue", "pink"))

print(sexplot)

###### Age moyen + Ecart Type ######

df_age <- data%>% select(9)

# Remove rows with NA values
df_age <- na.omit(df_age)

# Extract the numeric part of the Age variable and calculate the average age
average_age <- mean(as.numeric(sub(" A$", "", df_age$Age)))

# Extract the numeric part of the Age variable and calculate the standard deviation of age
standard_deviation <- sd(as.numeric(sub(" A$", "", df_age$Age)))

# Print the results
print(paste("The average age is:", average_age))
print(paste("The standard deviation of age is:", standard_deviation))

# L'âge moyen suggère que la personne typique dans cet ensemble de données a environ 63,17 ans. 
# L'écart type représente la dispersion ou la répartition des âges dans l'ensemble de données. 
# Un écart type plus élevé signifie que les âges sont plus dispersés, indiquant une plage d'âge plus large dans l'ensemble de données. 
# Dans ce cas, l'âge moyen est quelque peu plus élevé par rapport à l'âge moyen dans une population générale, et la plage d'âge dans cet ensemble de données est plus large que la moyenne. 

df_age <- as.data.frame(lapply(df_age, function(x) as.numeric(sub("A", "", x)) ))
df_age$Patient <- c(1:18)
df_age$Sex <- c("M","M", "F", "M", "F","M", "M", "F", "F","F", "F", "M", "M", "F", "F", "M", "M", "F")

# Create the scatter plot
ageplot <- ggplot(df_age, aes(x = Patient, y = Age, color = Sex)) +
  geom_point() +
  scale_x_continuous(breaks = seq(1, 18, 1))+
  labs(title = "Age Scatter Plot with Mean",
       x = "Patient",
       y = "Age",
       color = "Sex") +
  geom_abline(intercept = average_age, slope = 0, color = "red") +
  scale_color_manual(values = c("M" = "blue", "F" = "orange"))

print(ageplot)

# Create two age distribution graphs
age_distribution_male <- ggplot(df_age[df_age$Sex == "M", ], aes(x = Age)) +
  geom_density(color = "blue", fill = "lightblue") +
  labs(title = "Age Distribution for Male Patients",
       x = "Age",
       y = "Density")

age_distribution_female <- ggplot(df_age[df_age$Sex == "F", ], aes(x = Age)) +
  geom_density(color = "red", fill = "pink") +
  labs(title = "Age Distribution for Female Patients",
       x = "Age",
       y = "Density")

# Create a grid to place the graphs side by side
distageplot <- grid.arrange(age_distribution_male, age_distribution_female, ncol = 2)
print(distageplot)

#### Type d'erreurs ####

df_erreur <- data %>% select(1,3,4,6)

# Calculate percentages
cas_counts <- df_erreur %>% 
  count(`Typ Cas`) %>% 
  mutate(percentage = n / sum(n) * 100)

dec_counts <- df_erreur %>% 
  count(`Typ Décl`) %>% 
  mutate(percentage = n / sum(n) * 100)

not_counts <- df_erreur %>% 
  count(`Typ Notif`) %>% 
  mutate(percentage = n / sum(n) * 100)

# Create a pie chart
pie_chart <- ggplot(cas_counts, aes(x = "", y = percentage, fill = `Typ Cas`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Types of Cases Distribution",
       fill = "Cases",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "right")

print(pie_chart)


pie2_chart <- ggplot(dec_counts, aes(x = "", y = percentage, fill = `Typ Décl`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Types of Declaration Distribution",
       fill = "Declaration Methods",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "right")

print(pie2_chart)

pie3_chart <- ggplot(not_counts, aes(x = "", y = percentage, fill = `Typ Notif`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Types of Notifications Distribution",
       fill = "Notifications",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "right")

print(pie3_chart)


#### Gravité par age, BMI et sex ####

data_grave <- data %>% select(1,8,9,13,14)
data_grave <- na.omit(data_grave)
# removal of 2 patients due to missing values 
data_grave$Age <- as.numeric(sub("A", "", data_grave$Age))

# comparaison with BMI is interesting but not possible due to missing values (only 5 patient data)

data_grave$Grave <- factor(data_grave$Grave, levels = c("N", "O"))

percentage_data <- data_grave %>%
  group_by(Age, Sex, Grave) %>%
  summarise(Count = n()) %>%
  group_by(Age, Sex) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create a bar chart with percentages
plot <- ggplot(percentage_data, aes(x = as.factor(Age), y = Percentage, fill = Grave)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(Sex ~ .) +
  labs(title = "Percentage of Gravity by Age, Colored by Sex",
       x = "Age",
       y = "Percentage") +
  scale_fill_manual(values = c("N" = "skyblue", "O" = "pink"),
                    labels = c("N" = "No", "O" = "Yes")) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            position = position_stack(vjust = 0.5), size = 3) +
  theme_minimal()

# Display the plot
print(plot)

plot2 <- ggplot(data_grave, aes(x = "", fill = Grave)) +
  geom_bar(position = "fill", width = 1) +
  coord_polar(theta = "y") +
  facet_grid(Sex ~ .) +
  labs(title = "Percentage of Gravity, Colored by Sex",
       fill = "Outcome",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("N" = "skyblue", "O" = "pink"),
                    labels = c("N" = "No", "O" = "Yes")) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

print(plot2)

# Antécedents et des prédiction, intéressant à analyser car bcp des datas manquant. 
# Après les filtrage demandé il reste 20 patients sur 32 et il reste quand même beaucoup des data manquants qui complique les analyses et la fiabilité des résultats. 



