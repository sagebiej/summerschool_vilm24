# Make some nice plots of the endowment
library(ggpubr)

hnv_sh <- ggplot(data=final_set) +
  geom_density(aes(x=HNV_share), fill="peru", alpha=0.7) +
  xlab("HNV Share within 15km (%)")


pa_sh <- ggplot(data=final_set) +
  geom_density(aes(x=PA_share), fill="darkslategray", alpha=0.7) +
  xlab("PA Share within 15km (%)")



hnv_sq <- ggplot(data=final_set) +
  geom_density(aes(x=HNV_SQ), fill="peru", alpha=0.7) +
  xlab("HNV Area within 15km (ha)")


pa_sq <- ggplot(data=final_set) +
  geom_density(aes(x=PA_SQ), fill="darkslategray", alpha=0.7) +
  xlab("PA Area within 15km (ha)")

ggarrange(hnv_sh, pa_sh, hnv_sq, pa_sq)

ggsave("Figures/simul_endow.png", width = 7, height = 5, dpi=300)


# Check correlation HNV and PA
ggplot(data=final_set) +
  geom_point(aes(x=HNV_share, y=PA_share)) +
  geom_smooth(aes(x=HNV_share, y=PA_share), method=lm, col="indianred", formula = y ~ x)

# Plot respondents and HNV distribution
map_hnv <- ggplot(data=final_set) +
  geom_sf(data=germany_sf) +
  geom_point(aes(x=lon, y=lat, color=HNV_share)) +
  scale_color_viridis_c()

map_pa <- ggplot(data=final_set) +
  geom_sf(data=germany_sf) +
  geom_point(aes(x=lon, y=lat, color=PA_share)) +
  scale_color_viridis_c()

ggarrange(map_hnv, map_pa, legend="bottom")

ggsave("Figures/endow_maps.png", width = 7, height = 5, dpi=300)             
