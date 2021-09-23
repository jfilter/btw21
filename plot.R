library(tidyverse)

df <- read_csv('data.csv')

parties <- c('Linke', 'SPD', 'Grüne', 'CDU', 'FDP', 'AfD')

# correct ordering of words
df$party <- factor(df$party, levels = parties)
 
# correct ordering of parties 
df$word <- factor(df$word, levels = c("Volk", "Respekt", "Abschaffung", "Wettbewerb", "Sprache", "Innovation", "Identität", "Prozent", "Chance", "Beschäftigte", "Freiheit", "Zusammenarbeit", "Eltern", "Beispiel", "Zugang", "Welt", "Maßnahme", "Ausbau", "Politik", "Rahmen", "Demokratie", "Euro", "Kommune", "Bereich", "Arbeit", "Möglichkeit", "Zukunft", "Schule", "Familie", "Schutz", "Bund", "Sicherheit", "Leben", "Frau", "Entwicklung", "Wirtschaft", "Recht", "Bildung", "EU", "Europa", "Staat", "Gesellschaft", "Ziel", "Unternehmen", "Jahr", "Kind", "Land", "Mensch", "Deutschland"))

# rankings start at 1
df$value <- df$value + 1  
 
# previously invalid were set to -1, but now 0 
df <- df %>%
  mutate(value_label = ifelse(value == 0, '-', paste(value, '.', sep=''))) %>%
  mutate(value = ifelse(value > 100, NA, value)) %>%
  mutate(value = replace(value, value == 0, NA)) %>%
  mutate(color_label = ifelse(value > 30 | is.na(value), 'black', 'white'))


ggplot(data=df, aes(x = as.numeric(party), y=word, fill=value)) +
  geom_tile(color='white', size=1) +
  scale_fill_gradient(name = "value", low = "darkblue", high = "white", na.value = 'white', trans='log10', guide=FALSE) +
  geom_text(size=3,fontface=2, aes(label = value_label, color=color_label)) +
  scale_color_manual(values = c("grey30", "white"), guide=FALSE) +
  xlab(label = "") + ylab(label = "") + 
  theme_classic(base_size = 12, base_family = "Roboto") +
  scale_x_continuous(sec.axis = dup_axis(), breaks = 1:length(parties), labels = parties) +
  labs(title = "Häufigste Substantive in den\nWahlprogrammen zur BTW21", subtitle = "Je dunkler das Kästchen, desto häufiger wurde das Wort genutzt. Die Nummer\nim Kästchen zeigt die Platzierung pro Partei an. So ist \"Deutschland\" das\nhäufigste Wort im Programm der CDU und das zweithäufigste für SPD, FDP\nund AfD. Bei den Grünen ist es auf Platz 3 und bei den Linken auf Platz 10.", caption = 'Nur die 20 häufigsten Substantive wurden pro Partei berücksichtigt. Analyse: @fil_ter, bereinigter Text: @fussballinguist') +
  theme(axis.line=element_blank(), plot.subtitle=element_text(size=9), plot.caption = element_text(size=7), plot.title = element_text(margin=margin(20,0,20,0)))

ggsave('wahlprogramme_1.jpg', width = 6, height = 10)

ggsave('wahlprogramme_1.svg', width = 6, height = 10)


