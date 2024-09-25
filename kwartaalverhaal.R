#
# Script om tweet over SR klaar te zetten
# MELP, oktober 2023
#

library(cbsodataR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

# Hoofdtekst ----
hoofdtekst <- strwrap("Volgens het @statistiekcbs is het BNI in het %s 
                       uitgekomen op %s miljard, gecorrigeerd voor seizoen en 
                       werkdagen. Dit is %s miljoen %s dan het BBP. Vorig
                       kwartaal was het BNI %s miljoen %s dan het BBP. 
                       #economie", 
                      width = 280)

# Statline ----
statline_meta <- cbs_get_meta("85886NED")
statline_data <- cbs_get_data("85886NED",
                              SeizoenEnWerkdagcorrectie = "A042500",
                              select = c("Perioden",
                                         "SeizoenEnWerkdagcorrectie",
                                         "BrutoBinnenlandsProduct_1",
                                         "SaldoPrimaireInkomensBruto_3"))

data_bewerkt <- statline_data %>% 
    cbs_add_date_column() %>%
    cbs_add_label_columns() %>%
    filter(Perioden_freq == "Q",
           Perioden_Date >= as.Date("2014-01-01"))

# Input voor tekst maken ----
staart_perioden <- tail(unique(data_bewerkt$Perioden_label), 2)
staart_bbp <- tail(data_bewerkt$BrutoBinnenlandsProduct_1, 2)
staart_bni <- tail(data_bewerkt$SaldoPrimaireInkomensBruto_3, 2)
verschil   <- staart_bni - staart_bbp

huidig_kwartaal <- paste(
    substr(staart_perioden[2], 6, 16), "van",
    substr(staart_perioden[2], 1, 4))

bni_bedrag <- format(round(staart_bni[2]/1000, 1), 
                     big.mark = ".", decimal.mark = ",")

huidig_verschil <- format(abs(verschil[2]), big.mark = ".", decimal.mark = ",")
vorig_verschil  <- format(abs(verschil[1]), big.mark = ".", decimal.mark = ",")

huidig_richting  <- ifelse(verschil[2] >= 0, "hoger", "lager")
vorig_richting   <- ifelse(verschil[1] >= 0, "hoger", "lager")

# Plot maken ----
plot_data <- data_bewerkt |>
    pivot_longer(c("BrutoBinnenlandsProduct_1",
                   "SaldoPrimaireInkomensBruto_3"))

plot_titel <- paste0("Het bruto nationaal inkomen was in het ", 
                     huidig_kwartaal, " ", bni_bedrag, 
                     " miljard, en zodoende \n", 
                     huidig_verschil, " miljoen ",
                     huidig_richting, " dan het bruto binnenlands product, ",
                     "seizoen- en werkdaggecorrigeerd.")

p <- ggplot(plot_data,
            aes(Perioden_Date, value, color = name)) +
    ggtitle(plot_titel) +
    theme_minimal() +
    
    # Assen instellen
    xlab("") +
    ylab("miljard") + 
    scale_y_continuous(labels = scales::label_number(scale = 1e-3)) +
    scale_x_date(date_breaks = "1 year",
                 labels = function(x) format(as.Date(x), "%Y")) +
    
    # Legenda
    scale_color_colorblind(
        name = "",
        labels = c("Bruto binnenlands product",
                   paste0("Bruto nationaal inkomen"))) +
    theme(legend.position = "bottom",
          plot.background = element_rect(fill = "white")) +
    
    # Caption
    labs(caption = "@martinvanelp, Bron: CBS Statline") +
    
    # Data plotten
    geom_line(linewidth = 1.2)

ggsave(filename = tempfile(fileext = ".png"), 
       plot = p,
       device = "png", 
       width = 21.0, height = 14.8, units = "cm")

shell(paste("explorer", tempdir()), intern = TRUE)

# Tekst samenvoegen ----
tweet_tekst <- sprintf(
    # tekst
    hoofdtekst,      
    # rest wordt in tekst ingeplakt
    huidig_kwartaal, 
    bni_bedrag,
    huidig_verschil,
    huidig_richting,
    vorig_verschil,
    vorig_richting)

writeClipboard(tweet_tekst)
