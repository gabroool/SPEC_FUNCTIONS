# SPEC_FUNCTIONS

## SPEC FUNCTIONS, Gabo Coïmbra 2019-11-06

library(pavo)
library(tidyr)
library(dplyr)

############## GETSPEC_PARTE(SP, PARTE = 'FLOR')
## get spec + proc spec + average spec
## entra: nome da especie, nome da parte, ext do arquivo | sai: spec/curva media da especie

getspec_parte = function(sp, parte = 'flor', ext = 'txt') {
  spec = getspec(where = paste(sp, parte, sep = "_"),
                  ext = ext, lim = c(300, 700), decimal = ",")
  spec = procspec(spec, fixneg = 'zero', opt = 'smooth', span = 0.1)
  spec = procspec(spec, fixneg = 'zero', opt = c('min', 'max'))
  spec = aggspec(spec)
  return(spec)}


############ QCATCH(SPEC, VIS_MODEL = 'APIS', BACKGROUND = 'GREEN')
## (entra: spec, sai quantum catches)

qcatch = function(spec, vis_model = 'apis', background = spp_folhas, illum = 'D65') {

# spp_folhas = getspec_parte(sp = "spp", # importar curva media de folhagem das spp
#                           parte = "folhas",
#                           ext = "txt")

  if(vis_model == "apis") {

    sp_vismodel = vismodel(spec,
                           visual = vis_model,
                            qcatch = 'Ei', relative = FALSE,
                            vonkries = TRUE, achro = 'l',
                            bkg = background, illum = illum)
    return(sp_vismodel)

  } else if(vis_model == 'bombus') {

    bombus = sensmodel(c(328, 428, 536)) # 'Bombus terrestris' (Peitsch et al. 1992)
    names(bombus) = c('wl', 's', 'm', 'l')
    sp_vismodel = vismodel(spec, visual = bombus,
                            qcatch = 'Ei', relative = FALSE,
                            vonkries = TRUE, achro = 'l',
                            bkg = background, illum = illum)
    return(sp_vismodel)

  } else if(vis_model == "sephanoides") {

    sephanoides = sensmodel(c(370, 440, 508, 560)) # 'Sephanoides sephaniodes' (Herrera et al. 2008)
    names(sephanoides) = c("wl", "u", "s", "m", "l")
    sp_vismodel = vismodel(spec, visual = sephanoides,
                            achro = 'l',
                            bkg = background, relative = FALSE,
                            illum = illum,
                           qcatch = 'Qi')
    return(sp_vismodel)

  } else if(vis_model == "avg.v") {
    sp_vismodel = vismodel(spec, visual = vis_model,
                            achro = 'l',
                            bkg = background, relative = FALSE,
                            illum = illum,
                           qcatch = 'Qi')
    return(sp_vismodel)

  } else if(vis_model == "avg.uv") {
    sp_vismodel = vismodel(spec, visual = vis_model,
                            achro = 'l',
                            bkg = background, relative = FALSE,
                            illum = 'D65',
                           qcatch = 'Qi')
    return(sp_vismodel)
  }
}


### SPP_QCATCH(SPEC, DATA = SPP, VIS_MODEL = "APIS")
## entra lista de specs + planilha de especies, sai: qcatches das spp

spp_qcatch = function(spec, data = spp, vis_model = "apis") {

  # spp = read.csv("spp.csv") # importa especies

  QC = lapply(X = spec, FUN = qcatch, vis_model = vis_model)
  QC = bind_rows(QC)
  row.names(QC) = data$sp

  return(QC)
}

### SPP_SPACE(SPEC, DATA = SPP, VIS_MODEL = "APIS")
## entra lista de specs + planilha de especies, sai: colspace

spp_space = function(spec, data = spp, vis_model = "apis") {

  # spp = read.csv("spp.csv") # importa especies

      QC = lapply(X = spec, FUN = qcatch, vis_model = vis_model)
      QC = bind_rows(QC)
      row.names(QC) = data$sp

      if(vis_model == 'apis'| vis_model == 'bombus') {

        CS = colspace(QC, space = 'hexagon', qcatch = "Ei")
        return(CS)

      } else {

        CS = colspace(QC, space = 'tcs', qcatch = "Qi")
        return(CS)
      }
  }
