install.packages(c("readxl", "tidyr", "dplyr" , "tidyverse","WDI"))
library(WDI)
library(tidyverse)
library(dplyr)
library(tidyr)
###############################
library(readxl)
data1 <- read_excel("//profils.uppa.univ-pau.fr/folderredir/bkande/Desktop/Nouveau dossier/data1.xlsx", 
                    col_types = c("numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric"))
View(data1)

library(corrplot)
M <- cor(data1[c("hours" ,  "kidslt6" ,"kidsge6" ,"age"   ,  "educ")])
corrplot(M, method="number", col=1:4
)

mod=lm(wage~hours+educ+exper+huswage+huseduc, data = data1)
install.packages("GGally")

library(GGally)
ggcoef_model(mod,intercept = T)

#########################
ggplot(data=data1,aes(x=educ,y=wage))+
  geom_point(color="red")+geom_smooth(method = "lm")+labs(title = "bla",y="education",x="salaire",caption = "boubacar")

#######################
ggplot(data = data1) +
  aes(x = wage) +
  geom_line(stat = "density", color="blue") +
  expand_limits(x = c(0, 40)) +theme_bw()

#####################
library(readr)
panel_wage <- read_csv("//profils.uppa.univ-pau.fr/folderredir/bkande/Downloads/panel_wage.csv")
View(panel_wage)
panel_wage <- panel_wage%%
#################
library(readxl)
agriculsn <- read_excel("//profils.uppa.univ-pau.fr/folderredir/bkande/Desktop/Nouveau dossier/agriculsn.xlsx")
View(agriculsn)


agriculsn1 <- filter(agriculsn,Region!="SENEGAL")
agr <- agriculsn1[c("Region","2021")]
agr$production <- agr$`2021`

agr <- agr[c("Region","production")]

total <- agriculsn[1,7]

agr$percent <- (agr$production/1677804)*100

agr1 <- agr%>%filter(str_starts(Region, "REGION"))

install.packages("ggpubr")
library(ggpubr)

ggbarplot(agr1,
          x = "Region", y = "percent",
           # Set bar border colors to white
          palette = "jco", # jco journal color palett. see ?ggpar
          sort.val = "asc", # Sort the value in ascending order
        
          x.text.angle = 90, # Rotate vertically x axis texts
          ylab = "Production en tonne (%)",
          xlab = FALSE
        
)


agr1$group <- factor(ifelse(agr1$percent < 10, "Faible", "Elevé"),
                      levels = c("Faible", "Elevé"))

ggbarplot(agr1,
          x = "Region", y = "percent",
          fill = "group", # change fill color by mpg_level
          color = "white", # Set bar border colors to white
          palette = "jco", # jco journal color palett. see ?ggpar
          sort.val = "asc", # Sort the value in ascending order
          sort.by.groups = FALSE, # Don't sort inside each group
          x.text.angle = 90, # Rotate vertically x axis texts
          ylab = "Production en tonne (%)",
          xlab = FALSE,
          legend.title = "Groupe",
)


  # a refaire avec les rendements aussi

ggdotchart(agr1,
           x = "Region", y = "percent",
           color = "group", # Color by groups
           palette = c("#E7B800", "#FC4E07"), # Custom color palette
           sorting = "descending", # Sort value in descending order
           add = "segments", # Add segments from y = 0 to dots
           add.params = list(color = "lightgray", size = 2), # Change segment color and size
           group = "group", # Order by groups
           dot.size = 9, # Large dot size
           label = round(agr1$percent, 1), # Add mpg values as dot labels
           font.label = list(
             color = "white", size = 9,
             vjust = 0.5
           ), # Adjust label parameters
           ggtheme = theme_pubr() # ggplot2 theme
) +
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray")


#######################

load(url("https://larmarange.github.io/analyse-R/data/connaissances.RData"))
glimpse(quest)

summary(quest$conn_a)

library(questionr)
freq.na(quest)

conn <- quest %>%
  select(starts_with("conn_")) %>%
  pivot_longer(
    cols = starts_with("conn_"),
    names_to = "question",
    values_to = "reponse"
  )
glimpse(conn)

ggplot(conn) +
  aes(x = question, fill = reponse) +
  geom_bar(position = "fill")


####################
conn$reponse <- conn$reponse %>%
  fct_explicit_na("NSP") %>%
  fct_relevel("non", "NSP", "oui") %>%
  fct_recode("ne sait pas / manquant" = "NSP")

ggplot(conn) +
  aes(x = question, fill = reponse) +
  geom_bar(position = "fill") +
  theme(legend.position = "bottom")

##############
conn$etiquette <- conn$question %>%
  fct_recode(
    "R est disponible seulement pour Windows" = "conn_a",
    "R possède un puissant moteur graphique" = "conn_b",
    "Il est possible de réaliser des modèles mixtes avec R" = "conn_c",
    "Le package 'foreign' est le seul permettant d'importer des fichiers de données SPSS" = "conn_d",
    "Il n'est pas possible de produire un rapport PDF avec R" = "conn_e",
    "R peut gérer des données d'enquêtes avec un plan d'échantillonnage complexe" = "conn_f",
    "R est utilisée par des scientifiques de toutes disciplines, y compris des sciences sociales" = "conn_g"
  )
ggplot(conn) +
  aes(x = etiquette, fill = reponse) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme(legend.position = "bottom")

################
ggplot(conn) +
  aes(x = etiquette, fill = reponse) +
  geom_bar(position = "fill", width = .66) +
  scale_x_discrete(labels = scales::label_wrap(50)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  theme(legend.position = "bottom")

####################"
library(GGally)
ggplot(conn) +
  aes(x = etiquette, fill = reponse, by = etiquette) +
  geom_bar(position = "fill", width = .66) +
  geom_text(
    stat = "prop", position = position_fill(.5),
    colour = "white", fontface = "bold", size = 3
  ) +
  scale_x_discrete(labels = scales::label_wrap(50)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  theme(legend.position = "bottom")


######################"
library(ggcharts)
data("popch")
pyramid_chart(data = popch, x = age, y = pop, group = sex)
########################


### voir base construite


library(ggplot2)
library(ggrepel)
library(ggrepel)

dat <- subset(mtcars, wt > 2.75 & wt < 3.45)
dat$car <- rownames(dat)
p <- ggplot(dat) +
  aes(wt, mpg, label = car) +
  geom_point(color = "red")
p + geom_text_repel() +
  labs(title = "geom_text_repel()")


