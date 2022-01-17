setwd("~/source/REMIND/")
source("library_import.R")

setwd("~/source/REMIND_integrate/remind/output/")
run_number = "REMIND9"

data.dir = paste0("./",run_number)
plot.dir = "./plots/"


library(readr)
# te = "bioftcrec"
te = "wind"
# te = "spv"
# te = "elh2"
color.mapping <- c("wind" = "#337fff", "spv" = "#337fff")

tech_lst = c(te)
vnum = "SSP2-PkBudg1020"

# gdx = paste0(run_number, "/ww_baseline.gdx")
gdx = paste0(run_number, "/", vnum, ".gdx")

igdx("/opt/gams/gams30.2_linux_x64_64_sfx")


reg = c("CHA")

########################################################
########################################################
capCost <- read.gdx(gdx, "vm_costTeCapital",field="l", factor = FALSE) %>% 
filter(all_regi %in% reg) %>%
filter(all_te %in%tech_lst) %>% 
select(period=ttot, value,tech=all_te,region=all_regi) %>% 
mutate(value = value * 1e3 * 1.2) 

barwidth = 1.5

p<-ggplot() +
  geom_col(data = capCost %>% filter(period > 2005 & period <2110, tech == te), aes(x = period-barwidth/2-0.1, y = value, fill = tech), position='stack', size = 1, width = barwidth) +
  scale_fill_manual(name = "Technology", values = color.mapping) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size= 14,face="bold")) +
  xlab("year") + ylab(paste0("Investment Cost ($/kW)")) +
  # coord_cartesian(xlim = c(0, max(vrN_DT_CAP_plot$iter)),ylim = c(0, ymax))+
  ggtitle(paste0(te, "   ", reg))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
  theme(aspect.ratio = .5)+
  facet_wrap(~region, nrow = 1, scales = 'free_y')

ggsave(filename = paste0(plot.dir, run_number, "_IC_", vnum, "_", te,".png"),  p,  width = 10, height =6, units = "in", dpi = 120)


########################################################
########################################################
cap <- read.gdx(gdx, "vm_cap",field="l", factor = FALSE) %>% 
  filter(all_regi %in% reg) %>%
  filter(all_te %in%tech_lst) %>% 
  select(period=tall, cap=value,tech=all_te,region=all_regi) %>% 
  mutate(cap = cap * 1e3) 

barwidth = 1.5

p<-ggplot() +
  geom_col(data = cap %>% filter(period > 2005 & period <2110, tech == te), aes(x = period-barwidth/2-0.1, y = cap, fill = tech), position='stack', size = 1, width = barwidth) +
  scale_fill_manual(name = "Technology", values = color.mapping) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size= 14,face="bold")) +
  xlab("year") + ylab(paste0("Capacity (GW)")) +
  # coord_cartesian(xlim = c(0, max(vrN_DT_CAP_plot$iter)),ylim = c(0, ymax))+
  ggtitle(paste0("REMIND ", reg))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
  theme(aspect.ratio = .5)+
  facet_wrap(~region, nrow = 1, scales = 'free_y')

ggsave(filename = paste0(plot.dir, run_number,"_cap_", vnum, "_", te,".png"),  p,  width = 13, height =6, units = "in", dpi = 120)

########################################################
sm_TWa_2_MWh = 8760000000
prodSe <- read.gdx(gdx, "vm_prodSe",field="l", factor = FALSE) %>% 
  filter(all_regi %in% reg) %>%
  filter(all_te == te) %>% 
  select(period=tall, value,tech=all_te,region=all_regi) %>% 
  mutate(value = value * sm_TWa_2_MWh / 1e6) 

barwidth = 1.5

p<-ggplot() +
  geom_col(data = prodSe, aes(x = period-barwidth/2-0.1, y = value, fill = tech), position='stack', size = 1, width = barwidth) +
  scale_fill_manual(name = "Technology", values = color.mapping) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size= 14,face="bold")) +
  xlab("year") + ylab(paste0("Generation (TWh)")) +
  # coord_cartesian(xlim = c(0, max(vrN_DT_CAP_plot$iter)),ylim = c(0, ymax))+
  ggtitle(paste0("REMIND ", reg))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
  theme(aspect.ratio = .5)+
  facet_wrap(~region, nrow = 1, scales = 'free_y')

ggsave(filename = paste0(plot.dir, run_number, "_prodSe_", vnum, "_", te,".png"),  p,  width = 13, height =6, units = "in", dpi = 120)

########################################################
sm_TWa_2_MWh = 8760000000
deltacap <- read.gdx(gdx, "vm_deltaCap",field="l", factor = FALSE) %>% 
  filter(all_regi %in% reg) %>%
  filter(all_te %in%tech_lst) %>% 
  select(period=tall, value,tech=all_te,region=all_regi) %>% 
  mutate(value = value * 1e3) 

barwidth = 1.5

p<-ggplot() +
  geom_col(data = deltacap %>% filter(period > 2005 & period <2110, tech == te), aes(x = period-barwidth/2-0.1, y = value, fill = tech), position='stack', size = 1, width = barwidth) +
  # scale_fill_manual(name = "Technology", values = color.mapping) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size= 14,face="bold")) +
  xlab("year") + ylab(paste0("added Capacity (GW)")) +
  # coord_cartesian(xlim = c(0, max(vrN_DT_CAP_plot$iter)),ylim = c(0, ymax))+
  ggtitle(paste0("REMIND ", reg))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
  theme(aspect.ratio = .5)+
  facet_wrap(~region, nrow = 1, scales = 'free_y')

ggsave(filename = paste0(plot.dir, run_number, "_deltaCap_", vnum, "_", te,".png"),  p,  width = 13, height =7, units = "in", dpi = 120)

########################################################
capfac <- list(cap, prodSe) %>%
  reduce(full_join) %>% 
  mutate(capfac = value *1e3 *1e2/ (cap * 8760)) 

barwidth = 1.5

p<-ggplot() +
  geom_col(data = capfac %>% filter(period > 2005 & period <2110, tech == te), aes(x = period-barwidth/2-0.1, y = capfac, fill = tech), position='stack', size = 1, width = barwidth) +
  scale_fill_manual(name = "Technology", values = color.mapping) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size= 14,face="bold")) +
  xlab("year") + ylab(paste0("Capacity Factor (%)")) +
  coord_cartesian(ylim = c(0, 50))+
  ggtitle(paste0("REMIND ", reg))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
  theme(aspect.ratio = .5)+
  facet_wrap(~region, nrow = 1, scales = 'free_y')

ggsave(filename = paste0(plot.dir, run_number, "_capfac_", vnum, "_", te,"ng"),  p,  width = 13, height =6, units = "in", dpi = 120)






###############################global deltacap#########################
sm_TWa_2_MWh = 8760000000
deltacap <- read.gdx(gdx, "vm_deltaCap",field="l", factor = FALSE) %>% 
  # filter(all_regi %in% reg) %>%
  filter(all_te %in%tech_lst) %>% 
  select(period=tall, value,tech=all_te,region=all_regi) %>% 
  mutate(value = value * 1e3) %>% 
  dplyr::group_by(period, tech) %>%
  dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
  dplyr::ungroup(period, tech) 

barwidth = 1.5

p<-ggplot() +
  geom_col(data = deltacap %>% filter(period > 2005 & period <2110, tech == te), aes(x = period-barwidth/2-0.1, y = value, fill = tech), position='stack', size = 1, width = barwidth) +
  # scale_fill_manual(name = "Technology", values = color.mapping) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size= 14,face="bold")) +
  xlab("year") + ylab(paste0("added Capacity (GW)")) +
  # coord_cartesian(xlim = c(0, max(vrN_DT_CAP_plot$iter)),ylim = c(0, ymax))+
  ggtitle(paste0("REMIND GLO" ))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
  theme(aspect.ratio = .5)

ggsave(filename = paste0(plot.dir, run_number, "_deltaCap_", vnum, "_", te,"global.png"),  p,  width = 13, height =7, units = "in", dpi = 120)















