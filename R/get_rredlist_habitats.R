## Paket rredlist installieren
# install.packages("rredlist", dependencies = T)

## daten abrufen für z.B. Tangara palmarum.
load("RedListAPI.RData")
rredlist::rl_habitats(name = "Erythrogenys erythrogenys", 
                      key = RedLIstAPI)


