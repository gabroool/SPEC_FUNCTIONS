### EXAMPLE

library(pavo)
library(tidyr)
library(dplyr)

spp = data.frame(sp = c("Acanthus_montanus_flor",
                        "Adenium_obesum_flor",
                        "Aechmea_nudicaulis_bractea",
                        "spp_folhas")) ### exemplo de planilha de especies



spp_folhas = getspec_full(sp = "spp_folhas", ext = "txt")

spp_spec = lapply(X = spp$sp, FUN = getspec_full)
names(spp_spec) = spp$sp


spp_contrasts_cs = bind_rows(lapply(spp_spec, FUN = contrast_cs, background = spp_folhas))
  row.names(spp_contrasts_cs) = spp$sp

spp_contrasts_nl = bind_rows(lapply(spp_spec, FUN = contrast_nl, background = spp_folhas))
  row.names(spp_contrasts_nl) = spp$sp
