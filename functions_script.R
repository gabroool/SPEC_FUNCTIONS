## SPEC_FUNCTIONS, Gabo CoÃ¯mbra 2019-11-07

library(pavo)
library(tidyr)
library(dplyr)

############## GETSPEC_FULL()
### obtem arquivos de spec de uma pasta e retorna a curva mÃ©dia tratada
### getspec(), procspec() and aggspec() at once
## input: species name, structure name and file extension of spectral data
## output: mean reflectance for the species

getspec_full = function(sp, ext = 'txt', parte = "") {
  spec = getspec(
      where = if_else(parte == "",
                            false = paste(sp, parte, sep = "_"),
                            true = paste(sp)),
      ext = ext, lim = c(300, 700), decimal = ",")
  spec = procspec(spec, fixneg = 'zero', opt = 'smooth', span = 0.1)
  spec = procspec(spec, fixneg = 'zero', opt = c('min', 'max'))
  spec = aggspec(spec)
  names(spec) = c("wl", sp)
  return(spec)}


################################################ QUANTUM CATCHES

########################## VIA COLORSPACE ######################################

############ QCATCH_CS()
## input: spec
##output: quantum catches

qcatch_cs = function(spec, background = NULL, illum = 'D65') {

  #### APIS MELLIFERA

    vismodel_apis = vismodel(spec,
                           visual = 'apis',
                           qcatch = 'Ei',
                           relative = TRUE,
                           vonkries = TRUE,
                           achro = 'l',
                           bkg = background,
                           illum = illum)

  #### BOMBUS TERRESTRIS

    bombus = sensmodel(c(328, 428, 536)) # 'Bombus terrestris' (Peitsch et al. 1992)
    names(bombus) = c('wl', 's', 'm', 'l')

    vismodel_bombus = vismodel(spec,
                               visual = bombus,
                           qcatch = 'Ei',
                           relative = TRUE,
                           vonkries = TRUE,
                           achro = 'l',
                           bkg = background,
                           illum = illum)

    ### SEPHANOIDES SEPHANIODES

    sephanoides = sensmodel(c(370, 440, 508, 560)) # 'Sephanoides sephaniodes' (Herrera et al. 2008)
    names(sephanoides) = c("wl", "u", "s", "m", "l")

    vismodel_sephanoides = vismodel(spec, visual = sephanoides,
                           achro = 'l',
                           bkg = background, relative = TRUE,
                           illum = illum,
                           qcatch = 'Qi')

    ### AVERAGE VS AVIAN SYSTEM

    vismodel_avg.v = vismodel(spec, visual = "avg.v",
                           achro = 'l',
                           bkg = background, relative = TRUE,
                           illum = illum,
                           qcatch = 'Qi')

    ### AVERAGE UVS AVIAN SYSTEM

    vismodel_avg.uv = vismodel(spec, visual = "avg.uv",
                           achro = 'l',
                           bkg = background, relative = TRUE,
                           illum = illum,
                           qcatch = 'Qi')

    ### criar lista com todos os modelos
    vismodels = list(apis = vismodel_apis,
                     bombus = vismodel_bombus,
                     sephanoides = vismodel_sephanoides,
                     avg.v = vismodel_avg.v,
                     avg.uv = vismodel_avg.uv)

    return(vismodels)
}

################################################ QUANTUM CATCHES

##################### VIA NOISE-LIMITED MODEL #####################

############ QCATCH_NL()
## input: spec, background (spec do fundo)
## output: quantum catches

qcatch_nl = function(spec,
                     background = NULL,
                     illum = 'D65') {

  #### APIS MELLIFERA

  vismodel_apis = vismodel(spec,
                           visual = 'apis',
                           qcatch = 'fi', relative = FALSE,
                            vonkries = TRUE,
                           achro = 'l',
                          bkg = background,
                           illum = illum)

      #### BOMBUS TERRESTRIS

      bombus = sensmodel(c(328, 428, 536)) # 'Bombus terrestris' (Peitsch et al. 1992)
      names(bombus) = c('wl', 's', 'm', 'l')

      vismodel_bombus = vismodel(spec,
                                 visual = bombus,
                                 qcatch = 'fi',
                                 relative = FALSE,
                                 vonkries = TRUE,
                                 achro = 'l',
                                 bkg = background,
                                 illum = illum)

      ### SEPHANOIDES SEPHANIODES

      sephanoides = sensmodel(c(370, 440, 508, 560)) # 'Sephanoides sephaniodes' (Herrera et al. 2008)
      names(sephanoides) = c("wl", "u", "s", "m", "l")

      vismodel_sephanoides = vismodel(spec,
                                      visual = sephanoides,
                                      achro = 'l',
                                      bkg = background,
                                      relative = FALSE,
                                      illum = illum,
                                      qcatch = 'fi')

      ### AVERAGE VS AVIAN SYSTEM

      vismodel_avg.v = vismodel(spec, visual = "avg.v",
                                achro = 'l',
                                bkg = background, relative = FALSE,
                                illum = illum,
                                qcatch = 'fi')

      ### AVERAGE UVS AVIAN SYSTEM

      vismodel_avg.uv = vismodel(spec, visual = "avg.uv",
                                 achro = 'l',
                                 bkg = background,
                                 relative = FALSE,
                                 illum = illum,
                                 qcatch = 'fi')

      ### criar lista com todos os modelos
      vismodels = list(apis = vismodel_apis,
                       bombus = vismodel_bombus,
                       sephanoides = vismodel_sephanoides,
                       avg.v = vismodel_avg.v,
                       avg.uv = vismodel_avg.uv)
      
      return(vismodels)

}

############################################### CONTRASTES #################

#################  VIA COLORSPACES ###################

contrast_cs = function(spec, background = NULL, illum = 'D65') {

  #### APIS MELLIFERA

  vismodel_apis = vismodel(spec,
                           visual = 'apis',
                           qcatch = 'Ei',
                           relative = FALSE,
                           vonkries = TRUE,
                           achro = 'l',
                           bkg = background,
                           illum = illum)

  colspace_apis = colspace(vismodel_apis,
                           space = 'hexagon',
                           qcatch = "Ei")

  #### BOMBUS TERRESTRIS

  bombus = sensmodel(c(328, 428, 536)) # 'Bombus terrestris' (Peitsch et al. 1992)
  names(bombus) = c('wl', 's', 'm', 'l')

  vismodel_bombus = vismodel(spec,
                             visual = bombus,
                             qcatch = 'Ei',
                             relative = FALSE,
                             vonkries = TRUE,
                             achro = 'l',
                             bkg = background,
                             illum = illum)

  colspace_bombus = colspace(vismodel_bombus,
                             space = 'hexagon',
                             qcatch = "Ei")

  ### SEPHANOIDES SEPHANIODES

  sephanoides = sensmodel(c(370, 440, 508, 560)) # 'Sephanoides sephaniodes' (Herrera et al. 2008)
  names(sephanoides) = c("wl", "u", "s", "m", "l")

  vismodel_sephanoides = vismodel(spec, visual = sephanoides,
                                  achro = 'l',
                                  bkg = background, relative = TRUE,
                                  illum = illum,
                                  qcatch = 'Qi')

  colspace_sephanoides = colspace(vismodel_sephanoides,
                                  space = 'tcs',
                                  qcatch = "Qi")

  ### AVERAGE VS AVIAN SYSTEM

  vismodel_avg.v = vismodel(spec, visual = "avg.v",
                            achro = 'l',
                            bkg = background, relative = TRUE,
                            illum = illum,
                            qcatch = 'Qi')

  colspace_avg.v = colspace(vismodel_avg.v,
                                  space = 'tcs',
                                  qcatch = "Qi")

  ### AVERAGE UVS AVIAN SYSTEM

  vismodel_avg.uv = vismodel(spec, visual = "avg.uv",
                             achro = 'l',
                             bkg = background, relative = TRUE,
                             illum = illum,
                             qcatch = 'Qi')

  colspace_avg.uv = colspace(vismodel_avg.uv,
                            space = 'tcs',
                            qcatch = "Qi")

  ### criar data.frame com todos os modelos
  constrasts = data.frame(ACB_apis = vismodel_apis$lum,
                          CCB_apis = colspace_apis$r.vec,
                          ACB_bombus = vismodel_bombus$lum,
                          CCB_bombus = colspace_bombus$r.vec,
                          ACB_sephanoides = vismodel_sephanoides$lum,
                          CCB_sephanoides = colspace_sephanoides$r.vec,
                          ACB_avg.v = vismodel_avg.v$lum,
                          CCB_avg.v = colspace_avg.v$r.vec,
                          ACB_avg.uv = vismodel_avg.uv$lum,
                          CCB_avg.uv = colspace_avg.uv$r.vec) %>%
    return()
}


############################################### CONTRASTES #################

################ CONSTRASTES VIA COLDIST (NOISE-LIMITED) #############################

contrast_nl =  function(spec,
         background = NULL, ### spec do fundo, com coluna "spp_folhas"
         illum = 'D65') {

  spec = data.frame(spec, background = background$spp_folhas)

  vismodels = qcatch_nl(spec, background = background)

  ################## APIS MELLIFERA
  vismodel_apis =  vismodels$apis
  coldists_apis = coldist(vismodel_apis,
                          achromatic = TRUE,
                          n = c(1, 1, 1),
                   weber = c(0.13, 0.06, 0.12),
                   weber.achro = 0.12)
  contrasts = data.frame(CCB_apis = coldists_apis$dS,
                         ACB_apis = coldists_apis$dL)

  ###################### BOMBUS TERRESTRIS
  vismodel_bombus = vismodels$bombus
  coldists_bombus = coldist(vismodel_bombus,
                            achromatic = TRUE,
                            n = c(1, 1, 1),
                    weber = c(0.74, 0.67, 0.61),
                    weber.achro = 0.61)

  contrasts = data.frame(contrasts,
                         CCB_bombus = coldists_bombus$dS,
                         ACB_bombus = coldists_bombus$dL)

  ############## SEPHANOIDES SEPHANIODES
  vismodel_sephanoides = vismodels$sephanoides
  coldists_sephanoides = coldist(vismodel_sephanoides,
                                 achromatic = TRUE,
                                 n = c(1, 1, 1, 1),
                                 weber = c(0.05, 0.13, 0.22, 0.15),
                                 weber.achro = 0.15)

  contrasts = data.frame(contrasts,
                         CCB_sephanoides = coldists_sephanoides$dS,
                         ACB_sephanoides = coldists_sephanoides$dL)

  ######################### AVG.V
  vismodel_avg.v = vismodels$avg.v
  coldists_avg.v = coldist(vismodel_avg.v, achromatic = TRUE, n = c(1, 1, 1, 1))

  contrasts = data.frame(contrasts,
                         CCB_avg.v = coldists_avg.v$dS,
                         ACB_avg.v = coldists_avg.v$dL)

  ##################### AVG.UV
  vismodel_avg.uv = vismodels$avg.uv
  coldists_avg.uv = coldist(vismodel_avg.v, achromatic = TRUE, n = c(1, 1, 1, 1))

  contrasts = data.frame(contrasts,
                         CCB_avg.uv = coldists_avg.uv$dS,
                         ACB_avg.uv = coldists_avg.uv$dL)
return(contrasts)

}
