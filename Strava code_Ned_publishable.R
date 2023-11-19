# Pakketten
library(lubridate)   # Voor het werken met datums en tijden
library(tidyverse)   # Een bundel van populaire pakketten voor gegevensmanipulatie en visualisatie
library(ggplot2)     # Voor het maken van grafieken en visualisaties
library(writexl)     # Voor het schrijven van gegevens naar Excel-bestanden

# Pad (XXX for privacy)
setwd("C:/XXX/Strava Case")

# Inlezen van data

## Exporteer CSV data naar een R-dataset (XXX for privacy)
## Interessante bestanden =  Activities: datum, titel van de rit, afstand (kan je een mooie timeline mee maken)
Strava_act <- read.csv("~/XXX/export_10_11_2023/activities.csv")

#checken of het inlezen gelukt is
head(Strava_act$Activity.Date)

# Selecteer enkele interessante kolommen:
## Bekijk welke kolommen interessant zijn (volgens naam):
names(Strava_act)

#zeker ook eens visueel checken door dataset "Strava_act" open te klikken
# Zo zien we dat  'Moving.Time' interessanter is dan 'Elapsed Time'.

#zo nemen we beslissingen welke variabelen (en dus kolommen) we zullen selecteren
Strava_act_select <- Strava_act %>% select("Activity.Date", "Activity.Name", "Activity.Type", "Moving.Time", "Distance", "Elevation.High")

# Werken aan de datumstructuur zodat we hiermee verder.
##zicht krijgen op welke soort data we per kolom hebben (categorisch of numeriek?)
class(Strava_act_select$Activity.Date)

##momenteel bevat "Activity.Date" zowel informatie over datum, jaar als over tijdstip in één variabele.
## dit is niet zo werkbaar omdat dit apart belangrijke categorische variabelen zijn voor mij analyse. 
##straks zal je ook merken dat ik sommige van deze categorische variabelen niet nodig heb. 

Strava_act_select[c('Datum', 'Jaar', 'Tijdstip')] <- str_split_fixed(Strava_act_select$Activity.Date, ',', 3)

## Laat de kolommen die je niet meer nodig hebt vallen
Strava_act_select2 <- Strava_act_select %>% select(-"Tijdstip")

## Het gaat ook niet over een specifieke dag in de maand, het is dus voldoende om de maand te kennen.
Strava_act_select2[c('Maand', 'Dag')] <- str_split_fixed(Strava_act_select2$Datum, ' ', 2)

## Laat de kolommen die je niet meer nodig hebt vallen
Strava_act_select3 <- Strava_act_select2 %>% select(-"Dag", -"Datum")

# Bedoeling van visualisatie = overzicht van 12 maanden: 11/2022 tot 11/2023
class(Strava_act_select3$Jaar)  # Klasse van deze variabele bepaalt volgende lijn code
unique(Strava_act_select3$Jaar) # Belangrijk om dit te doen, om te zien hoe deze waarden weggeschreven zijn - anders zou je in de volgende lijn "2023" zetten ipv " 2023" en dus foutmelding krijgen.

#er zijn meerdere jaren aan data, naast 2022 en 2023. Deze jaren laten we dus vallen. 
Strava_act_select3.1 <- Strava_act_select3[((Strava_act_select3$Jaar == " 2022") | (Strava_act_select3$Jaar == " 2023")), ]

#kijken van welke maanden we data hebben.
unique(Strava_act_select3.1$Maand)  # Zorg ervoor dat we deze onmiddellijk juist targetten

# Omdat maanden wel een logische volgorde hebben, zullen we die proberen te selecteren op basis van logische volgorde:
## Definieer de volgorde van de maanden
maand_volgorde <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Zorg ervoor dat 'Maand' als een factor wordt behandeld met de gespecificeerde volgorde (vastgelegd in vorige lijn)
Strava_act_select3.1$Maand <- factor(Strava_act_select3.1$Maand, levels = maand_volgorde)

# Selecteer gegevens van november 2022 tot nu: we hebben dus voor de data van 2022 maar in bepaalde maanden interesse.
Strava_act_select3.2 <- Strava_act_select3.1[
  ((Strava_act_select3.1$Jaar == " 2022" & (Strava_act_select3.1$Maand == "Nov" | Strava_act_select3.1$Maand == "Dec")) |
     (Strava_act_select3.1$Jaar == " 2023")), ]

#we zijn voor de visualisatie enkel geïnteresseerd in alles over fietsen, zwemmen en lopen. 
#onlang begon ik ook "workouts" te registreren. Daarin ben ik niet geïnteresseerd. 
#dus eerst zicht krijgen op aanwezige "levels" binnen de categorische variabele, en daarna de onnodige lijnen data weglaten.
unique(Strava_act_select3.2$Activity.Type)
Strava_act_select3.3 <- Strava_act_select3.2[Strava_act_select3.2$Activity.Type != "Workout", ]

# Voorlopig 94 observaties in deze 12 maanden, die lopen, fietsen of zwemmen zijn.

# De activiteiten naam kolom nog droppen, want dat is overbodige info voor wat ik hier wil doen.
# Net als de initiële kolom van Activity.Date
Strava_act_select4 <- Strava_act_select3.3 %>% select(-"Activity.Date", -"Activity.Name")

# Kolommen herstructureren: er zijn kolommen die we gebruiken als categorische (nl. om bepaalde waarden - de andere variabelen dan - te groeperen)
## Nieuwe volgorde van kolommen: de groeperende variabelen eerst, daarna de numerieke variabelen die bij die "categorisaties" horen.
new_order <- c("Jaar", "Maand", "Activity.Type",
               "Moving.Time", "Distance", "Elevation.High")

## Wijzig de volgorde van kolommen
Strava_act_select4 <- Strava_act_select4[, new_order]

# Daarop volgen dan de kolommen die zéker numeriek moeten worden gemaakt, want met deze waarden willen we aan de slag.
## even checken of ze wel allemaal numeriek zijn. 
class(Strava_act_select4$Moving.Time)  # Numeric: goed
class(Strava_act_select4$Distance)     # Character: owow!
class(Strava_act_select4$Elevation.High) # Numeric: goed

# één variabele moeten we omzetten van character naar numeriek. 
Strava_act_select4$Distance <- as.numeric(Strava_act_select4$Distance)

# Als we dit doen komen er problemen met de manueel ingebrachte zwemsessies.
# dit was niet duidelijk geworden als ik data niet goed had gecheckt (en dan was ik in de problemen gekomen...)
# Dit lossen we op met onderstaande code:
Strava_act_select4$Distance <- ifelse(Strava_act_select4$Activity.Type == "Swim"
                                      & Strava_act_select4$Maand == "Jan"
                                      & Strava_act_select4$Moving.Time == 2160
                                      & is.na(Strava_act_select4$Distance), 1000, Strava_act_select4$Distance)


# ... (Vervolg van de code)

# Moving.Time in minuten zetten
Strava_act_select4$Moving.Time.Min <- Strava_act_select4$Moving.Time/60

# De initiële Moving.Time droppen
## Laat de kolommen die je niet meer nodig hebt vallen
Strava_act_select4 <- Strava_act_select4 %>% select(-"Moving.Time")

#verder checken van de data:
## Checken of er nog ergens NA's zijn
unique(Strava_act_select4$Distance)
is.na(Strava_act_select4$Distance)

## Dubbelchecken of de class nu écht klopt
class(Strava_act_select4$Distance)

## Checken op uitschieters
unique(Strava_act_select4$Activity.Type)

summary(Strava_act_select4[Strava_act_select4$Activity.Type == "Ride", ])
summary(Strava_act_select4[Strava_act_select4$Activity.Type == "Run", ])  # Hier een vreemde Max
summary(Strava_act_select4[Strava_act_select4$Activity.Type == "Swim", ])

# Mogelijke uitschieter aanpakken: (to 18.85, 1.885000e+01)
Strava_act_select4$Moving.Time.Min <- ifelse(Strava_act_select4$Activity.Type == "Run"
                                             & Strava_act_select4$Maand == "Jul"
                                             & Strava_act_select4$Distance == 2.90, 1.885000e+01, Strava_act_select4$Moving.Time.Min)

# Nu nog voor alle categorieën data voorzien. Ook voor momenten waarop er geen sport was (dit hebben we uiteindelijk manueel gedaan via Excel)
Strava_act_sport_month <- Strava_act_select4 %>% 
  group_by(Jaar, Maand, Activity.Type) %>% 
  summarize(Tot.afstand = sum(Distance), Tot.minuten = sum(Moving.Time.Min)) 

# Deze gecleande dataset alvast even opslaan als een Rdataset.
# kan later nog gebruikt worden voor andere analyses/visuals.
save(Strava_act_select4, file = "dataset_sportactiviteiten_nov2023.Rda")

# Outputten in Excel voor verdere verwerking.
write_xlsx(Strava_act_sport_month, path = "Strava_act_sport_month.xlsx")
