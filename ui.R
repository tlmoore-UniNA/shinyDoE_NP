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
					## ZETA PLOTS
					fluidRow(
						column(3,
							plotlyOutput("expPlot_zeta_fr")
						), # End column
						column(3,
							plotlyOutput("expPlot_zeta_pei")
						), # End column
						column(3,
							plotlyOutput("expPlot_zeta_p188")
						), # End column
						column(3,
							plotlyOutput("expPlot_zeta_plga")
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
					DT::DTOutput("db") 
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
							), # End sliderInput
							sliderInput("slider_zeta",
								"Desired zeta potential (mV)",
								min=min(round(fmesh$pred_zeta)),
								max=max(round(fmesh$pred_zeta)),
								value=mean(fmesh$pred_zeta)
							) # End sliderInput
						), # End column
						column(3,
							plotlyOutput("dashPlot_corr_size")
						), # End column
						column(3,
							plotlyOutput("dashPlot_corr_zeta")
						), # End column
						column(3,
							plotlyOutput("dashPlot_corr_pdi")
						) # End column
					), # End fluidRow
          fluidRow(
					  DT::DTOutput("opt_form") 
          ) # End fluidRow
				), # End tabPanel	 
				tabPanel("Model Heatmaps", icon=icon("map-location"),
					h3("Heatmaps show the various values predicted by the models"),
          # Size
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
						), # End column
					), # End fluidRow
          # Zeta
					fluidRow(
						column(3,
							plotlyOutput("dashPlot_zeta_noP188_502h")
						), # End column
						column(3,
							plotlyOutput("dashPlot_zeta_P188_502h")
						), # End column
						column(3,
							plotlyOutput("dashPlot_zeta_noP188_504h")
						), # End column
						column(3,
							plotlyOutput("dashPlot_zeta_P188_504h")
						) # End column
					), # End fluidRow
          # PdI
					fluidRow(
						column(3,
							plotlyOutput("dashPlot_pdi_noP188_502h")
						), # End column
						column(3,
							plotlyOutput("dashPlot_pdi_P188_502h")
						), # End column
						column(3,
							plotlyOutput("dashPlot_pdi_noP188_504h")
						), # End column
						column(3,
							plotlyOutput("dashPlot_pdi_P188_504h")
						) # End column
					) # End fluidRow
				), # End tabPanel
        tabPanel("Prediction Scatter Plots", icon=icon("chart-simple"),
          fluidRow(
            column(6, 
              plotlyOutput("dashPlot_predExplorer1")
            ), # End column
            column(6, 
              plotlyOutput("dashPlot_predExplorer2")
            ) # End column
          ), # End fluidRow
          fluidRow(
            column(6, 
              plotlyOutput("dashPlot_predExplorer3")
            ), # End column
            column(6, 
              plotlyOutput("dashPlot_predExplorer4")
            ) # End column
          ) # End fluidRow
        ) # End tabPanel
			) # End navbarPage
		) # End DoE dashboard
	) # End tabItems
	) # End dashboardBody
)
