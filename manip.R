# Manipulation de données ####
#install.packages("tidyverse")
# 1.
test <- read.table("data/gapminder.csv", sep = ";", header = TRUE)

# 2.
df1 <- read.table("data/gapminder.txt", header = TRUE, stringsAsFactors = FALSE)
identical(test,df1) # Teste si deux objets sont identiques

# 3.
class(df1)

# 4.
colnames(df1) # Afficher le nom des colonnes
colnames(df1)[1] <- c("pays")
colnames(df1)

# 5.
head(df1[5])
head(df1["pop"])
head(df1$pop)

# 6.
head(df1[["pays"]])
head(df1["pays"])

# 7.
unique(df1["continent"])

# 8.
saveRDS(df1, "data/save.rds")

# Manipulation: filtrer, selectionner et créer une variable ####

# 1.
df07 <- df1[df1["year"] == 2007,]

# 2.
#df07 <- df07[-3] # Supprimer une colonne par son numéro de ligne
df07["year"] <- NULL # Supprimer une colonne par son nom
subset(df1,select = -year)

# 3.
head(df07$gdp_tot <- df07$pop*df07$gdpPercap)

# 4.
df07[which.min(df07$gdp_tot), c("pays", "gdp_tot")] #Trouver pays mini GDP
df07[df07$gdp_tot == max(df07$gdp_tot), c("pays", "gdp_tot")] #Trouver pays maxi GDP

# 5.
sum(df07$gdp_tot)
mean(df07$gdp_tot)
sd(df07$gdp_tot)

# 6.
summary(df07$gdp_tot)

# Opérations sur les données ####


# 1.
tapply(df1$gdpPercap,df1$year,mean)

# 2.
aggregate(df1$gdpPercap,list(df1$year),mean)

# Tidyverse ####

# install.packages("tidyverse") filter, select, mutate
library(dplyr)

# 1.
df08 <- filter(df1, year == 2007)

df08 <- select(df08,-year)

df08 <- mutate(df08,gdp_tot = gdpPercap * pop) 

# 2. pipe : ctrl + maj + m
df1 %>% 
  filter(year == 2007) %>% 
  select(-year) %>% 
  mutate(gdp_tot = gdpPercap * pop) -> df09

# 3.
df09 %>% summarise(somme = sum(gdp_tot),
                   moyenne = mean(gdp_tot),
                   ecart_type = sd(gdp_tot)
                   )
# 4.
df1 %>% group_by(year) %>% summarise(moyenne_GDP = mean(gdpPercap))

# 5.
df1 %>% group_by(continent,year) %>% 
  summarise(moyenne_GDP = mean(gdpPercap))->results_gdp

# Tidyr ####

library(tidyr)

# 1.
results_gdp %>% pivot_wider(names_from = year, values_from = moyenne_GDP) -> my_df

# 2.
my_df %>% pivot_longer(-continent, names_to = "year", values_to = "gdpPercap")

# ggplot2 ####

library(ggplot2)

# 1.
df1 %>% filter(year == 2007) %>% 
  ggplot(aes(x = lifeExp, y = gdpPercap)) + 
  geom_point(aes(color = continent)) +
  labs(x = "Espérance de vie (années") + 
  labs(y = "PIB/hab") + 
  labs(title = "Espérance de vie et PIB/hab en 2007")

# 2.
# df1 %>% filter(year == 2007) %>% 
#   ggplot(aes(x = lifeExp, y = gdpPercap, color = continent)) + 
  #geom_line()
  #geom_area()
  #geom_jitter()

# 4.

# Second graphique
df1 %>% group_by(continent,year) %>% summarise(moyenne_esp_vie = mean(lifeExp)) %>% 
  ggplot(aes(x = year, y = moyenne_esp_vie)) + 
  geom_line(aes(color = continent)) +
  labs(x = "Années", y = "Espérance de vie (années)", title = "Espérance de vie et PIB/hab en 2007")

# Troisième graphique
df1 %>% group_by(continent,year) %>% 
  summarise(moyenne_esp_vie = mean(lifeExp)) %>% 
  ggplot(aes(x = year, y = moyenne_esp_vie)) + 
  geom_bar(stat = "identity") +
  facet_wrap(vars(continent), ncol = 3)
