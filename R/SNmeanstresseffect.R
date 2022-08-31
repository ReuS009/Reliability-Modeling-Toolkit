# Stress-Life Diagram
# Developed by Reuel Smith, 2022

SN.meanstresseffect <- function(relationship,Sa,Sm,altvar){
  if(relationship == "Soderberg"){
    Sar <- Sa/(1 - (Sm/Sy))
  }
  if(relationship == "ModGoodman"){
    Sar <- Sa/(1 - (Sm/Su))
  }
  if(relationship == "Morrow"){
    Sar <- Sa/(1 - (Sm/sig_f))
  }
  if(relationship == "Gerber"){
    Sar <- Sa/(1 - ((Sm/Su)^2))
  }
  if(relationship == "SWT"){
    Sar <- Sa/sqrt(0.5*(1-R))
  }
  if(relationship == "Walker"){
    Sar <- Sa/((0.5*(1-R))^(1-gam))
  }
  return(Sar)
}
