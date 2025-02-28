homePage <- tabItem(tabName = "home",
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
		(with data), contact us at:", tags$a(href="mailto:xxxx@unina.it"),
		"xxx@unina.it")
) # End homePage
