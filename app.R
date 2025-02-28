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

# UI =========================================================================
ui <- dashboardPage(
	## HEADER ===========================									
	dashboardHeader(title="shiny DoE NP"),
	## SIDEBAR ==========================
	dashboardSidebar(
		sidebarMenu(
			menuItem("Home", tabName="home", icon=icon("house")),
			menuItem("About", tabName="about", icon=icon("circle-info")),
			menuItem("Explore Data", tabName="explore", icon=icon("street-view")),
			menuItem("DoE Dashboard", tabName="dashboard", icon=icon("gauge-high"))
		) # End sidebarMenu
	), # End dashboardSidebar
	## BODY =============================
	dashboardBody(
	tabItems(
	### HOMEPAGE ========================
		tabItem(tabName="home",
			h2("Welcome to Shiny DoE NP"),
      h3("A Free Platform Employing Design of Experiments for
         Nanoparticle Production"),
      p("Shiny DoE NP is a free, web-based application aimed at
        streamlining the development of nanoparticles for the delivery
        of therapeutics.
        Specifically, in Version 1.0, we are implementing a model for
        providing the optimal production parameters for
        poly(lactide-co-glycolide) (PLGA) and poly(ethyleneimine) (PEI)
        composite nanoparticles."),
			h3("Acknowledgements"),
      p("Shiny DoE NP is currently developed and maintained by..."),
			h3("Contact Us"),
      p("If you want to collaborate or contribute to the project
        (with data), contact us at:",
        tags$a(href="mailto:xxxx@unina.it"),
        "xxx@unina.it")
		), # End home
	### ABOUT ===========================
		tabItem(tabName="about",
			h2("About shiny DoE NP")
		), # End about
	### EXPLORE DATA ====================
		tabItem(tabName="explore",
			navbarPage("Explore the underlying data",
				tabPanel("Scatter Matrix", icon=icon("chart-column"),
					fluidRow(
						column(3,
							plotlyOutput("expPlot_size_fr")
						), # End column
						column(3,
							plotlyOutput("expPlot_size_pei")
						), # End column
						column(3,
							plotlyOutput("expPlot_size_p188")
						), # End column
						column(3,
							plotlyOutput("expPlot_size_plga")
						) # End column
					), # End fluidRow
					## PdI PLOTS
					fluidRow(
						column(3,
							plotlyOutput("expPlot_pdi_fr")
						), # End column
						column(3,
							plotlyOutput("expPlot_pdi_pei")
						), # End column
						column(3,
							plotlyOutput("expPlot_pdi_p188")
						), # End column
						column(3,
							plotlyOutput("expPlot_pdi_plga")
						) # End column
					) # End fluidRow
				), # End tabPanel
				tabPanel("Database", icon=icon("database"),
					h2("DoE Database"),
					dataTableOutput("db") 
				) # End tabPanel
			## SIZE PLOTS
			) # End navbarPage
		), # End explore
	### DOE DASHBOARD ===================
		tabItem(tabName="dashboard",
			navbarPage("DoE Dashboard",
				tabPanel("Formulation Optimizer", icon=icon("flask-vial"),
					h3("Welcome to the formulation optimizer."),
					p("Input your desired nanoparticle attributes (e.g. size and 
						surface charge, and the model will return the best formulation."),
					fluidRow(#
						column(3,
							sliderInput("slider_diam",
								"Desired diameter (nm)",
								min=min(round(fmesh$pred_size)),
								max=max(round(fmesh$pred_size)),
								value=mean(fmesh$pred_size)
							)#, # End sliderInput
#							sliderInput("slider_zeta",
#								"Desired zeta potential (mV)",
#								min=min(round(fmesh$pred_zeta)),
#								max=max(round(fmesh$pred_zeta)),
#								value=mean(fmesh$pred_zeta)
#							) # End sliderInput
						) # End column
					) # End fluidRow
				), # End tabPanel	 
				tabPanel("Model Heatmaps", icon=icon("map-location"),
					h3("Heatmaps show the various values predicted by the models"),
					fluidRow(
						column(3,
							plotlyOutput("dashPlot_size_noP188_502h")
						), # End column
						column(3,
							plotlyOutput("dashPlot_size_P188_502h")
						), # End column
						column(3,
							plotlyOutput("dashPlot_size_noP188_504h")
						), # End column
						column(3,
							plotlyOutput("dashPlot_size_P188_504h")
						)
					) # End fluidRow
				) # End tabPanel
			) # End navbarPage
		) # End DoE dashboard
	) # End tabItems
	) # End dashboardBody
)

# SERVER =====================================================================
server <- function(input, output) {
## Custom ggplot theme
theme_shiny <- function(){
	theme(
		panel.background=element_rect(fill="white", colour="#3b3b3b"),
		plot.background=element_rect(fill="#ecf0f5"),
		panel.grid.major=element_line(color="grey80"),
		panel.grid.minor=element_line(color="grey80")
	)
}
				
## Load raw data ======
df <- read.csv("www/raw_data.csv")
db <- df
# Drop rows with NA
df <- na.omit(df)
df$plga_spec <- ifelse(df$plga_502 != 0, "PLGA 502H", "PLGA 504H")

## Summary data frames
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

## Explore data ==============================================================
### Database =====

output$db <- renderDataTable(db)

### Size plots =====
output$expPlot_size_fr <- renderPlotly({
	ggplotly(
		ggplot()+
			geom_point(data = df, 
				mapping=aes(x=fr_mlmin, y=size_nm),
				alpha=I(0.2))+
			geom_errorbar(data = df_summ_fr,
				mapping=aes(x=fr_mlmin, ymin=mean_size-sd_size, ymax=mean_size+sd_size),
				colour="#a21c26", width=0.05*(max(df_summ_fr$fr_mlmin)-min(df_summ_fr$fr_mlmin)))+
			geom_point(data = df_summ_fr,
				mapping=aes(x=fr_mlmin, y=mean_size), size=2, shape=21, fill="#a21c26", 
				colour="#3b3b3b")+
			scale_x_continuous(name="Syringe pump flow rate (ml/min)")+
			scale_y_continuous(name="Hydrodynamic Diameter (nm)")+
			theme_shiny()
	)
}) 
output$expPlot_size_pei <- renderPlotly({
	ggplotly(
		ggplot()+
			geom_point(data = df, 
				mapping=aes(x=pei, y=size_nm),
				alpha=I(0.2))+
			geom_errorbar(data = df_summ_pei,
				mapping=aes(x=pei, ymin=mean_size-sd_size, ymax=mean_size+sd_size),
				colour="#a21c26", width=0.05*(max(df_summ_pei$pei)-min(df_summ_pei$pei)))+
			geom_point(data = df_summ_pei,
				mapping=aes(x=pei, y=mean_size), size=2, shape=21, fill="#a21c26", 
				colour="#3b3b3b")+
			scale_x_continuous(name="Total amt. PEI (mg)")+
			scale_y_continuous(name="Hydrodynamic Diameter (nm)")+
			theme_shiny()
	)
}) 
output$expPlot_size_p188 <- renderPlotly({
	ggplotly(
		ggplot()+
			geom_point(data = df, 
				mapping=aes(x=p188, y=size_nm),
				alpha=I(0.2))+
			geom_errorbar(data = df_summ_p188,
				mapping=aes(x=p188, ymin=mean_size-sd_size, ymax=mean_size+sd_size),
				colour="#a21c26", width=0.05*(max(df_summ_p188$p188)-min(df_summ_p188$p188)))+
			geom_point(data = df_summ_p188,
				mapping=aes(x=p188, y=mean_size), size=2, shape=21, fill="#a21c26", 
				colour="#3b3b3b")+
			scale_x_continuous(name="Total amt. Kolliphor P-188 (mg)")+
			scale_y_continuous(name="Hydrodynamic Diameter (nm)")+
			theme_shiny()
	)
}) 
output$expPlot_size_plga <- renderPlotly({
	ggplotly(
		ggplot()+
			geom_point(data = df, 
				mapping=aes(x=plga_spec, y=size_nm),
				alpha=I(0.2))+
			geom_errorbar(data = df_summ_plga,
				mapping=aes(x=plga_spec, ymin=mean_size-sd_size, ymax=mean_size+sd_size),
				colour="#a21c26", width=0.05*(max(df_summ_p188$p188)-min(df_summ_p188$p188)))+
			geom_point(data = df_summ_plga,
				mapping=aes(x=plga_spec, y=mean_size), size=2, shape=21, fill="#a21c26", 
				colour="#3b3b3b")+
			scale_x_discrete(name="PLGA Identity")+
			scale_y_continuous(name="Hydrodynamic Diameter (nm)")+
			theme_shiny()
	)
}) 
### PdI plots =====
output$expPlot_pdi_fr <- renderPlotly({
	ggplotly(
		ggplot()+
			geom_point(data = df, 
				mapping=aes(x=fr_mlmin, y=pdi),
				alpha=I(0.2))+
			geom_errorbar(data = df_summ_fr,
				mapping=aes(x=fr_mlmin, ymin=mean_pdi-sd_pdi, ymax=mean_pdi+sd_pdi),
				colour="#a21c26", width=0.05*(max(df_summ_fr$fr_mlmin)-min(df_summ_fr$fr_mlmin)))+
			geom_point(data = df_summ_fr,
				mapping=aes(x=fr_mlmin, y=mean_pdi), size=2, shape=21, fill="#a21c26", 
				colour="#3b3b3b")+
			scale_x_continuous(name="Syringe pump flow rate (ml/min)")+
			scale_y_continuous(name="PdI")+
			theme_shiny()
	)
}) 
output$expPlot_pdi_pei <- renderPlotly({
	ggplotly(
		ggplot()+
			geom_point(data = df, 
				mapping=aes(x=pei, y=pdi),
				alpha=I(0.2))+
			geom_errorbar(data = df_summ_pei,
				mapping=aes(x=pei, ymin=mean_pdi-sd_pdi, ymax=mean_pdi+sd_pdi),
				colour="#a21c26", width=0.05*(max(df_summ_pei$pei)-min(df_summ_pei$pei)))+
			geom_point(data = df_summ_pei,
				mapping=aes(x=pei, y=mean_pdi), size=2, shape=21, fill="#a21c26", 
				colour="#3b3b3b")+
			scale_x_continuous(name="Total amt. PEI (mg)")+
			scale_y_continuous(name="PdI")+
			theme_shiny()
	)
}) 
output$expPlot_pdi_p188 <- renderPlotly({
	ggplotly(
		ggplot()+
			geom_point(data = df, 
				mapping=aes(x=p188, y=pdi),
				alpha=I(0.2))+
			geom_errorbar(data = df_summ_p188,
				mapping=aes(x=p188, ymin=mean_pdi-sd_pdi, ymax=mean_pdi+sd_pdi),
				colour="#a21c26", width=0.05*(max(df_summ_p188$p188)-min(df_summ_p188$p188)))+
			geom_point(data = df_summ_p188,
				mapping=aes(x=p188, y=mean_pdi), size=2, shape=21, fill="#a21c26", 
				colour="#3b3b3b")+
			scale_x_continuous(name="Total amt. Kolliphor P-188 (mg)")+
			scale_y_continuous(name="PdI")+
			theme_shiny()
	)
}) 
output$expPlot_pdi_plga <- renderPlotly({
	ggplotly(
		ggplot()+
			geom_point(data = df, 
				mapping=aes(x=plga_spec, y=pdi),
				alpha=I(0.2))+
			geom_errorbar(data = df_summ_plga,
				mapping=aes(x=plga_spec, ymin=mean_pdi-sd_pdi, ymax=mean_pdi+sd_pdi),
				colour="#a21c26", width=0.05*(max(df_summ_p188$p188)-min(df_summ_p188$p188)))+
			geom_point(data = df_summ_plga,
				mapping=aes(x=plga_spec, y=mean_pdi), size=2, shape=21, fill="#a21c26", 
				colour="#3b3b3b")+
			scale_x_discrete(name="PLGA Identity")+
			scale_y_continuous(name="PdI")+
			theme_shiny()
	)
}) 



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
fmesh$pred_size <- signif(fmesh$pred_size, digits=1)

#### Model for PdI ======

#### Model for zeta ======

### Dashboard plots ==========================================================
output$dashPlot_size_noP188_502h <- renderPlotly({
	ggplotly(
		ggplot(fmesh[which(fmesh$mod_p188 == -1 & fmesh$mod_plga == 1),],
				aes(pei, fr, fill=pred_size))+
			geom_raster()+
			scale_x_continuous(name="Total amt. PEI (mg)", expand=c(0,0))+
			scale_y_continuous(name="Syringe pump flow rate (ml/min)", expand=c(0,0))+
			scale_fill_viridis_c(limits=c(min(fmesh$pred_size),max(fmesh$pred_size)))+
			theme_shiny()+
			ggtitle("PLGA: 502H | No Kolliphor P-188")+
			theme(legend.background = element_rect(fill ="#ecf0f5"))+
			labs(fill="Predicted\nSize (nm)")
	)
}) 
output$dashPlot_size_P188_502h <- renderPlotly({
	ggplotly(
		ggplot(fmesh[which(fmesh$mod_p188 == 1 & fmesh$mod_plga == 1),],
				aes(pei, fr, fill=pred_size))+
			geom_raster()+
			scale_x_continuous(name="Total amt. PEI (mg)", expand=c(0,0))+
			scale_y_continuous(name="Syringe pump flow rate (ml/min)", expand=c(0,0))+
			scale_fill_viridis_c(limits=c(min(fmesh$pred_size),max(fmesh$pred_size)))+
			theme_shiny()+
			ggtitle("PLGA: 502H | With Kolliphor P-188")+
			theme(legend.background = element_rect(fill ="#ecf0f5"))+
			labs(fill="Predicted\nSize (nm)")
	)
}) 
output$dashPlot_size_noP188_504h <- renderPlotly({
	ggplotly(
		ggplot(fmesh[which(fmesh$mod_p188 == -1 & fmesh$mod_plga == -1),],
				aes(pei, fr, fill=pred_size))+
			geom_raster()+
			scale_x_continuous(name="Total amt. PEI (mg)", expand=c(0,0))+
			scale_y_continuous(name="Syringe pump flow rate (ml/min)", expand=c(0,0))+
			scale_fill_viridis_c(limits=c(min(fmesh$pred_size),max(fmesh$pred_size)))+
			theme_shiny()+
			ggtitle("PLGA: 504H | No Kolliphor P-188")+
			theme(legend.background = element_rect(fill ="#ecf0f5"))+
			labs(fill="Predicted\nSize (nm)")
	)
}) 
output$dashPlot_size_P188_504h <- renderPlotly({
	ggplotly(
		ggplot(fmesh[which(fmesh$mod_p188 == 1 & fmesh$mod_plga == -1),],
				aes(pei, fr, fill=pred_size))+
			geom_raster()+
			scale_x_continuous(name="Total amt. PEI (mg)", expand=c(0,0))+
			scale_y_continuous(name="Syringe pump flow rate (ml/min)", expand=c(0,0))+
			scale_fill_viridis_c(limits=c(min(fmesh$pred_size),max(fmesh$pred_size)))+
			theme_shiny()+
			ggtitle("PLGA: 504H | With Kolliphor P-188")+
			theme(legend.background = element_rect(fill ="#ecf0f5"))+
			labs(fill="Predicted\nSize (nm)")
	)
}) 

} # End server

# App =======================================================================
shinyApp(ui = ui, server = server)
