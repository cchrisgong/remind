*** |  (C) 2006-2024 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/40_techpol/NPi2025/bounds.gms


*** Léa to do: technologies still missing are h2 to electricity and "coal to electricity"

*** Keep historical data until 2020 
*** AM the lowbound of solar and pv for 2030 to be taken from the NDCs (in GW), therefore multiplying by 0.001 for TW*
*** NPi bounds are only applied after 2020, as NPi scenarios should always have cm_startyear higher than 2020.
vm_cap.lo(t,regi,"spv","1")$(t.val gt 2025) = p40_TechBound(t,regi,"spv")*0.001;
vm_cap.lo(t,regi,"csp","1")$(t.val gt 2025) = p40_TechBound(t,regi,"csp")*0.001;   
vm_cap.lo(t,regi,"tnrs","1")$(t.val ge 2025) = p40_TechBound(t,regi,"tnrs")*0.001;
vm_cap.lo(t,regi_nucscen,"tnrs",rlf)$((t.val ge 2025) and (cm_nucscen eq 5)) = 0; !! we assume: Nucscen (limiting nuclear deployment) overrides NDC targets -> resetting lower bound to value defined at cm_nucscen switch
vm_cap.lo(t,regi,"hydro","1")$(t.val gt 2025) = p40_TechBound(t,regi,"hydro")*0.001;
vm_cap.lo(t,regi,"windon","1")$(t.val gt 2025) = p40_TechBound(t,regi,"windon")*0.001; 
vm_cap.lo(t,regi,"windoff","1")$(t.val gt 2025) = p40_TechBound(t,regi,"windoff")*0.001;
vm_cap.lo(t,regi,"wind","1")$(t.val gt 2025) = p40_TechBound(t,regi,"wind")*0.001;
*vm_cap.lo(t,regi,"elh2","1")$(t.val gt 2025) = p40_TechBound(t,regi,"elh2")*0.001;   
*vm_cap.lo(t,regi,"geohdr","1")$(t.val gt 2025) = p40_TechBound(t,regi,"geohdr")*0.001;  quick fix needs adjustments in the mrremind Inputdata generation including new IRENA
vm_cap.lo(t,regi,"geohdr","1")$( (t.val gt 2025) AND sameas(regi, "OAS")) = p40_TechBound(t,regi,"geohdr")*0.001;
*vm_cap.up(t,regi,"coalchp","1")$(t.val gt 2025) = p40_TechBound(t,regi,"coalchp")*0.001;  !!max bound for Canada use the netzero calculator, check coal phase out module
*vm_cap.lo(t,regi,"bioigcc","1")$(t.val gt 2025) = p40_TechBound(t,regi,"bioigcc")*0.001; !!regular bioenergy bound

vm_cap.fx("2020","IND","windon","1") = 0.039;
vm_cap.fx("2025","IND","windon","1") = 0.054;
vm_cap.fx("2020","IND","spv","1") = 0.037;
vm_cap.fx("2025","IND","spv","1") = 0.130;
vm_cap.fx("2015","IND","pc","1") = 0.199;
vm_cap.fx("2020","IND","pc","1") = 0.255;
vm_cap.fx("2025","IND","pc","1") = 0.260;
vm_cap.fx("2030","IND","pc","1") = 0.300;
vm_cap.fx("2015","IND","tnrs","1") = 0.0058;
vm_cap.fx("2020","IND","tnrs","1") = 0.0068;
vm_cap.fx("2025","IND","tnrs","1") = 0.01;
vm_cap.fx("2030","IND","tnrs","1") = 0.015;
vm_capFac.fx("2010","IND","pc") = 0.69;
vm_capFac.fx("2015","IND","pc") = 0.56;
vm_capFac.fx("2020","IND","pc") = 0.54;
vm_capFac.fx("2025","IND","pc") = 0.68;
vm_capFac.fx("2030","IND","pc") = 0.67;

display vm_cap.lo;

*** EOF ./modules/40_techpol/NPi2025/bounds.gms

