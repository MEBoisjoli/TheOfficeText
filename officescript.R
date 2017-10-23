#Chargement des packages
library(tidytext) #install.packages("tidytext") 
library(tidyverse) #install.packages("tidyverse") 
#Chargement des données
office <- read_csv("theoffice.csv")
#Thème personnalisé pour ggplot2
theme_mebt <- theme(text = element_text(size = 10, colour = "#000000"),
                    panel.background = element_rect(fill = "#efefef"),
                    plot.background = element_rect(fill = "#efefef"),
                    plot.margin = margin(10,10,10,10)
)
#Supprimer "stop words"
office <- office %>%
  anti_join(stop_words)

speakfreq <- office %>%
  count(speaker, sort = TRUE) %>% #Nombre de mots par personnage
  mutate(screentime = n/218830*100) %>% #Création d'une nouvelle variable de % 
  slice(1:15) #Garder que les 15 plus importants personnages

speakfreqbar <- ggplot(speakfreq, aes(reorder(speaker, screentime), screentime))+
  geom_bar(stat= "identity", fill = "#282b87")+
  coord_flip()+
  labs(title = "Temps à l'écran des 15 personnages plus récurrents",
       x = NULL,
       y = "Temps à l'écran (%)") +
  guides(fill = FALSE) +
  theme_mebt
print(speakfreqbar)

michael <- c("Michael", "michael") #Vecteur Michael
michaeladress <- office %>%
  filter(word %in% michael) %>% #Filtre mot inclus dans vecteur Michael
  filter(speaker != "Michael") %>% #Filtre excluant Michael comme personnage
  count(speaker, sort = TRUE) %>% #Nombre de "Michael" par personnage
  slice(1:10) #Garder que le top 10 

michaelbar <- ggplot(michaeladress, aes(reorder(speaker, n), n))+
  geom_bar(stat="identity", fill = "#282b87")+
  coord_flip()+
  labs(title = "Personnages s'adressant le plus à Michael",
       x = NULL,
       y= "Nombre d'adresses à Michael")+
  guides(fill = FALSE)+
  theme_mebt
print(michaelbar)

afinn <- get_sentiments("afinn") #Fonction permettant de se procurer la base de donnée AFINN
afinnwords <- office %>% 
  inner_join(afinn) #Joindre à notre base de données initiale
episodesents <- afinnwords %>% 
  group_by(overallno) %>% #Regrouper les mots de chaques épisodes
  summarise(finalscore = sum(score)) #Somme du pointage de chaques mots par épisode

sentline <- ggplot(episodesents, aes(overallno, finalscore))+
  geom_bar(stat= "identity", fill = "#282b87")+
  geom_smooth(color = "#dd1313")+
  labs(title = "Pointage AFINN par épisode",
       x = "Épisode",
       y = "Pointage AFINN")+
  guides(size=FALSE, color =FALSE)+
  theme_mebt
print(sentline)

charsents <- afinnwords %>%
  filter(speaker %in% c("Michael", "Dwight", "Jim", "Andy", "Pam", "Kevin", "Angela", "Erin", "Oscar", "Ryan", "Darryl", "Kelly","Toby", "Phyllis", "Jan")) %>%
  #Filtre des personnages les plus importants
  group_by(speaker) %>% #Grouper par personnage
  summarise(finalscore = sum(score)) #Somme du pointage de chaque mot

charsentsbar <- ggplot(charsents, aes(reorder(speaker, finalscore), finalscore)) +
  geom_point(size = 2, color = "#282b87") +
  geom_segment(aes(x=speaker, y = 0, xend = speaker, yend= finalscore))+
  coord_flip()+
  labs(title = "Pointage AFINN par personnage",
       x = NULL,
       y = "Pointage AFINN") +
  guides(fill = FALSE) +
  theme_mebt
print(charsentsbar)

avgsents <- afinnwords %>%
  filter(speaker %in% c("Michael", "Dwight", "Jim", "Andy", "Pam", "Kevin", "Angela", "Erin", "Oscar", "Ryan", "Darryl", "Kelly","Toby", "Phyllis", "Jan")) %>%
  #Filtre des personnages les plus importants
  count(speaker, sort = TRUE) %>% #Nombre total de mots par personnage
  full_join(charsents) %>% #Joindre table du pointage final par personnage
  mutate(avgsent = finalscore/n) #Nouvelle variable de pointage pondéré

avgsentline <- ggplot(avgsents, aes(reorder(speaker, avgsent), avgsent))+
  geom_point(size = 2, color = "#282b87") +
  geom_segment(aes(x= speaker, y= 0, xend= speaker, yend= avgsent))+
  coord_flip()+
  labs(title = "Pointage AFINN pondéré des personnages principaux",
       x= NULL,
       y= "Pointage AFINN pondéré")+
  theme_mebt
print(avgsentline)

