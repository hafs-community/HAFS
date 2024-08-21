#!/bin/bash

MACHINE_ID=unknown
case $(hostname -f) in

  adecflow0[12].acorn.wcoss2.ncep.noaa.gov)  MACHINE_ID=wcoss2 ;; ### acorn
  alogin0[1-3].acorn.wcoss2.ncep.noaa.gov)   MACHINE_ID=wcoss2 ;; ### acorn
  clogin0[1-9].cactus.wcoss2.ncep.noaa.gov)  MACHINE_ID=wcoss2 ;; ### cactus01-9
  clogin10.cactus.wcoss2.ncep.noaa.gov)      MACHINE_ID=wcoss2 ;; ### cactus10
  dlogin0[1-9].dogwood.wcoss2.ncep.noaa.gov) MACHINE_ID=wcoss2 ;; ### dogwood01-9
  dlogin10.dogwood.wcoss2.ncep.noaa.gov)     MACHINE_ID=wcoss2 ;; ### dogwood10


  hfe0[1-9]) MACHINE_ID=hera ;; ### hera01-9
  hfe1[0-2]) MACHINE_ID=hera ;; ### hera10-12
  hecflow01) MACHINE_ID=hera ;; ### heraecflow01


  fe[1-8]) MACHINE_ID=jet ;; ### jet01-8
  tfe[12]) MACHINE_ID=jet ;; ### tjet1-2

  Orion-login-[1-4].HPC.MsState.Edu) MACHINE_ID=orion ;; ### orion1-4

  Hercules-login-[1-4].HPC.MsState.Edu) MACHINE_ID=hercules ;; ### hercules1-4

  gaea6[1-8].ncrc.gov) MACHINE_ID=gaeaC6 ;; ### Gaea C6 61-68
esac
export ${MACHINE_ID}
