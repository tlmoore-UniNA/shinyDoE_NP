# LIBRARIES =================================================================
# For web interface/shiny app
library(shiny)
library(shinydashboard)
# For data cleaning
library(dplyr)
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
			menuItem("Explore Data", tabName="explore", icon=icon("map-location-dot")),
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
			## SIZE PLOTS
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
			), # End fluidRow
		), # End explore
	### DOE DASHBOARD ===================
		tabItem(tabName="dashboard",
			h2("DoE Dashboard")
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
### Size
#### Coefficients
b0 <- 69.2407434
pei <- -25.1646505
flow <- 5.1326631
p188 <- 4.1949715
plga <- -12.2641452
pei_flow <- -3.9728324
pei_p188 <- -1.8333980
pei_plga <- -14.9517834
flow_p188 <- 4.4287024
flow_plga <- -0.8744699
p188_plga <- -0.2693636
pei_sq <- 40.9621016
flow_sq <- -3.6950841

#### Data range
# pei <- c(25,75,125) = c(-1,0,1)

#### Model

} # End server

# App =======================================================================
shinyApp(ui = ui, server = server)
