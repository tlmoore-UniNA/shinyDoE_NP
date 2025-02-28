# LIBRARIES =================================================================
# For web interface/shiny app
library(shiny)
library(shinydashboard)
# For data cleaning
library(dplyr)
library(data.table)
# For plotting/data viz
library(ggplot2)
library(plotly)

## Custom ggplot theme
theme_shiny <- function(){
	theme(
		panel.background=element_rect(fill="white", colour="#3b3b3b"),
		plot.background=element_rect(fill="#ecf0f5"),
		panel.grid.major=element_line(color="grey80"),
		panel.grid.minor=element_line(color="grey80")
	)
}


# DATA ======================================================================
## Load raw data ======
df <- read.csv("www/raw_data.csv")
db <- df
# Drop rows with NA
df <- na.omit(df)
df$plga_spec <- ifelse(df$plga_502 != 0, "PLGA 502H", "PLGA 504H")



## Summary data frames =====
# (p188, pei, plga_502, plga_504, fr_mlmin)
df_summ_p188 <- df |> group_by(p188) |>
	summarize(
		mean_size = mean(size_nm),
		sd_size = sd(size_nm),
		mean_pdi = mean(pdi),
		sd_pdi = sd(pdi),
		mean_zeta = mean(zeta_pot),
		sd_zeta = sd(zeta_pot)
	)
df_summ_pei <- df |> group_by(pei) |>
	summarize(
		mean_size = mean(size_nm),
		sd_size = sd(size_nm),
		mean_pdi = mean(pdi),
		sd_pdi = sd(pdi),
		mean_zeta = mean(zeta_pot),
		sd_zeta = sd(zeta_pot)
	)
df_summ_plga <- df |> group_by(plga_spec) |>
	summarize(
		mean_size = mean(size_nm),
		sd_size = sd(size_nm),
		mean_pdi = mean(pdi),
		sd_pdi = sd(pdi),
		mean_zeta = mean(zeta_pot),
		sd_zeta = sd(zeta_pot)
	)
df_summ_fr <- df |> group_by(fr_mlmin) |>
	summarize(
		mean_size = mean(size_nm),
		sd_size = sd(size_nm),
		mean_pdi = mean(pdi),
		sd_pdi = sd(pdi),
		mean_zeta = mean(zeta_pot),
		sd_zeta = sd(zeta_pot)
	)

## Nunzia's models ===========================================================
### Generating the mesh of test conditions -----------------------------------
n = 10 # number pf points to test in the mesh
factors_range = 2 # Factors along which a range of values will be tested
# i.e. non-binary, non-characteristic factors
# i.e.i.e. pei amount and flow rate

mesh <- matrix(ncol=factors_range, nrow=n)
# Fill the mesh
for (i in 1:factors_range){
	mesh[,i] <- seq(-1, 1, length.out=n)
}

# Make mesh a data frame
mesh <- data.frame(mesh)
# Give data frame column names
names(mesh) <- c("mod_pei", "mod_fr")
# Create an expanded mesh with every combination of factors
exp_mesh <- expand.grid(mesh)
len_exp_mesh <- nrow(exp_mesh) # length of expanded mesh
exp_mesh <- rbind(exp_mesh, exp_mesh)
# Create a column for the characteristics data - Kolliphor P-188
exp_mesh$mod_p188 <- c(rep(-1, times=len_exp_mesh), 
									 rep(1, times=len_exp_mesh))
len2_exp_mesh <- nrow(exp_mesh) # Get the new mesh length
fmesh <- rbind(exp_mesh, exp_mesh)
fmesh$mod_plga <- c(rep(-1, times=len2_exp_mesh), 
									 rep(1, times=len2_exp_mesh))

# Add converted, real values (not for modeling)
fmesh$pei <- 50*fmesh$mod_pei+75
fmesh$fr <- 0.5*fmesh$mod_fr+1
fmesh$plga_spec <- ifelse(fmesh$mod_plga == 1, "PLGA 502H", "PLGA 504H")
fmesh$p188 <- ifelse(fmesh$mod_p188 == 1, max(df$p188), min(df$p188))

### Factorize real data for modeling ---------------------------------------------
df$mod_pei <- 0.02*df$pei-1.5
df$mod_fr <- 2*df$fr_mlmin-2
df$mod_p188 <- ifelse(df$p188 == 0, -1, 1)
df$mod_plga <- ifelse(df$plga_spec == "PLGA 502H", 1, -1)


#### Model for size ======
#### Coefficients
b0 <- 69.2407434
b_pei <- -25.1646505
b_flow <- 5.1326631
b_p188 <- 4.1949715
b_plga <- -12.2641452
b_pei_flow <- -3.9728324
b_pei_p188 <- -1.8333980
b_pei_plga <- -14.9517834
b_flow_p188 <- 4.4287024
b_flow_plga <- -0.8744699
b_p188_plga <- -0.2693636
b_pei_sq <- 40.9621016
b_flow_sq <- -3.6950841

# Predictions
df$pred_size <- b0+b_pei*df$mod_pei+
				b_flow*df$mod_fr+
				b_p188*df$mod_p188+
				b_plga*df$mod_plga+
				b_pei_flow*df$mod_pei*df$mod_fr+
				b_pei_p188*df$mod_pei*df$mod_p188+
				b_pei_plga*df$mod_pei*df$mod_plga+
				b_flow_p188*df$mod_fr*df$mod_p188+
				b_flow_plga*df$mod_fr*df$mod_plga+
				b_p188_plga*df$mod_p188*df$mod_plga+
				b_pei_sq*df$mod_pei^2+
				b_flow_sq*df$mod_fr^2
# On mesh
fmesh$pred_size <- b0+b_pei*fmesh$mod_pei+
				b_flow*fmesh$mod_fr+
				b_p188*fmesh$mod_p188+
				b_plga*fmesh$mod_plga+
				b_pei_flow*fmesh$mod_pei*fmesh$mod_fr+
				b_pei_p188*fmesh$mod_pei*fmesh$mod_p188+
				b_pei_plga*fmesh$mod_pei*fmesh$mod_plga+
				b_flow_p188*fmesh$mod_fr*fmesh$mod_p188+
				b_flow_plga*fmesh$mod_fr*fmesh$mod_plga+
				b_p188_plga*fmesh$mod_p188*fmesh$mod_plga+
				b_pei_sq*mesh$mod_pei^2+
				b_flow_sq*mesh$mod_fr^2
fmesh$pred_size <- signif(fmesh$pred_size, digits=2)


#### Model for PdI ======
#### Coefficients
b0_pdi <- 0.110817612
b_pei_pdi <- 0.024123119
b_flow_pdi <- -0.00181048
b_p188_pdi <- 0.003891977
b_plga_pdi <- 0.003813195
b_pei_flow_pdi <- 0.002348323
b_pei_p188_pdi <- -0.001762106
b_pei_plga_pdi <- 0.019667453
b_flow_p188_pdi <- 0.003518566
b_flow_plga_pdi <- 0.000833855
b_p188_plga_pdi <- 0.005170303
b_pei_sq_pdi <- -0.012193886
b_flow_sq_pdi <- -0.00339098

# Predictions
df$pred_pdi <- b0_pdi+b_pei_pdi*df$mod_pei+
				b_flow_pdi*df$mod_fr+
				b_p188_pdi*df$mod_p188+
				b_plga_pdi*df$mod_plga+
				b_pei_flow_pdi*df$mod_pei*df$mod_fr+
				b_pei_p188_pdi*df$mod_pei*df$mod_p188+
				b_pei_plga_pdi*df$mod_pei*df$mod_plga+
				b_flow_p188_pdi*df$mod_fr*df$mod_p188+
				b_flow_plga_pdi*df$mod_fr*df$mod_plga+
				b_p188_plga_pdi*df$mod_p188*df$mod_plga+
				b_pei_sq_pdi*df$mod_pei^2+
				b_flow_sq_pdi*df$mod_fr^2
# On mesh
fmesh$pred_pdi <- b0_pdi+b_pei_pdi*fmesh$mod_pei+
				b_flow_pdi*fmesh$mod_fr+
				b_p188_pdi*fmesh$mod_p188+
				b_plga_pdi*fmesh$mod_plga+
				b_pei_flow_pdi*fmesh$mod_pei*fmesh$mod_fr+
				b_pei_p188_pdi*fmesh$mod_pei*fmesh$mod_p188+
				b_pei_plga_pdi*fmesh$mod_pei*fmesh$mod_plga+
				b_flow_p188_pdi*fmesh$mod_fr*fmesh$mod_p188+
				b_flow_plga_pdi*fmesh$mod_fr*fmesh$mod_plga+
				b_p188_plga_pdi*fmesh$mod_p188*fmesh$mod_plga+
				b_pei_sq_pdi*mesh$mod_pei^2+
				b_flow_sq_pdi*mesh$mod_fr^2
fmesh$pred_pdi <- signif(fmesh$pred_pdi, digits=4)


#### Model for zeta ======
#### Coefficients
b0_zeta <- 48.7637393	
b_pei_zeta <- 14.2730378	
b_flow_zeta <- -7.0224434	
b_p188_zeta <- -2.3552147	
b_plga_zeta <- -9.6543192	
b_pei_flow_zeta <- 3.7780133	
b_pei_p188_zeta <- 1.8829832	
b_pei_plga_zeta <- 9.2738257	
b_flow_p188_zeta <- -3.5483126	
b_flow_plga_zeta <- -3.4816556	
b_p188_plga_zeta <- -0.7091539	
b_pei_sq_zeta <- -4.2829744	
b_flow_sq_zeta <- -8.5820419

# Predictions
df$pred_zeta <- b0_zeta+b_pei_zeta*df$mod_pei+
				b_flow_zeta*df$mod_fr+
				b_p188_zeta*df$mod_p188+
				b_plga_zeta*df$mod_plga+
				b_pei_flow_zeta*df$mod_pei*df$mod_fr+
				b_pei_p188_zeta*df$mod_pei*df$mod_p188+
				b_pei_plga_zeta*df$mod_pei*df$mod_plga+
				b_flow_p188_zeta*df$mod_fr*df$mod_p188+
				b_flow_plga_zeta*df$mod_fr*df$mod_plga+
				b_p188_plga_zeta*df$mod_p188*df$mod_plga+
				b_pei_sq_zeta*df$mod_pei^2+
				b_flow_sq_zeta*df$mod_fr^2
# On mesh
fmesh$pred_zeta <- b0_zeta+b_pei_zeta*fmesh$mod_pei+
				b_flow_zeta*fmesh$mod_fr+
				b_p188_zeta*fmesh$mod_p188+
				b_plga_zeta*fmesh$mod_plga+
				b_pei_flow_zeta*fmesh$mod_pei*fmesh$mod_fr+
				b_pei_p188_zeta*fmesh$mod_pei*fmesh$mod_p188+
				b_pei_plga_zeta*fmesh$mod_pei*fmesh$mod_plga+
				b_flow_p188_zeta*fmesh$mod_fr*fmesh$mod_p188+
				b_flow_plga_zeta*fmesh$mod_fr*fmesh$mod_plga+
				b_p188_plga_zeta*fmesh$mod_p188*fmesh$mod_plga+
				b_pei_sq_zeta*mesh$mod_pei^2+
				b_flow_sq_zeta*mesh$mod_fr^2
fmesh$pred_zeta <- signif(fmesh$pred_zeta, digits=2)

