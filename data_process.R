library(data.table)
library(magrittr)
library(stringr)
library(tidyr)

# Loading data
data <- fread(grep(pattern = "^applemobilitytrends*", list.files(), value = T, ),
              sep = ",", header = T)

# A Español y correcciones de nombres
mxDATA <- data[country == "Mexico" | region == "Mexico",]
mxDATA$geo_type <- gsub(pattern = "country/region", replacement = "Nacional", x = mxDATA$geo_type)
mxDATA$geo_type <- gsub(pattern = "city", replacement = "Ciudad", x = mxDATA$geo_type)
mxDATA$geo_type <- gsub(pattern = "sub-region", replacement = "Estado", x = mxDATA$geo_type)

mxDATA$region[str_detect(mxDATA$region, pattern = "^Mexico$")] <- "Nacional"
mxDATA$region[str_detect(mxDATA$region, pattern = "^Ju(.)+rez$")] <- "Juárez"
mxDATA$region[str_detect(mxDATA$region, pattern = "^Mexico City$")] <- "Cd. de México"
mxDATA$region[str_detect(mxDATA$region, pattern = "^Le(.)+n$")] <- "León"
mxDATA$region[str_detect(mxDATA$region, pattern = "^Nuevo Le(.)+n$")] <- "Nuevo León"
mxDATA$region[str_detect(mxDATA$region, pattern = "^Puebla$")] <- "Cd. de Puebla"
mxDATA$region[str_detect(mxDATA$region, pattern = "^Puebla(.)+$")] <- "Estado de Puebla"
mxDATA$region[str_detect(mxDATA$region, pattern = "^State of Mex(.)+$")] <- "Estado de México"
mxDATA$region[str_detect(mxDATA$region, pattern = "^Michoa(.)+$")] <- "Michoacán"
mxDATA$region[str_detect(mxDATA$region, pattern = "^Quer(.)+taro$")] <- "Querétaro"
mxDATA$region[str_detect(mxDATA$region, pattern = "^San Luis Poto(.)+$")] <- "San Luis Potosí"
mxDATA$region[str_detect(mxDATA$region, pattern = "^Yucat(.)+$")] <- "Yucatán"

mxDATA$transportation_type[str_detect(mxDATA$transportation_type, pattern = "^driving$")] <- "Automovil"
mxDATA$transportation_type[str_detect(mxDATA$transportation_type, pattern = "^walking$")] <- "Peatones"
mxDATA$transportation_type[str_detect(mxDATA$transportation_type, pattern = "^transit$")] <- "Público"
mxDATA <- mxDATA[,c(-4:-6)]

# Long data format
longDATA <- pivot_longer(mxDATA, names_to = "day", -c("region", "transportation_type", "geo_type"), values_to = "Movilidad") %>% data.table()
longDATA$day <- longDATA$day %>% as.Date()
longDATA$Movilidad <- longDATA$Movilidad-100
longDATA$region <- longDATA$region %>% as.factor %>% relevel(ref = "Nacional")

saveRDS(longDATA, "mexData.rds")
