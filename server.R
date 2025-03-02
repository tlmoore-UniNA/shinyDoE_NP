# SERVER =====================================================================
server <- function(input, output) {


## Explore data ==============================================================
### Database =====
output$db <- DT::renderDT(db)

### Size plots =====
output$expPlot_size_fr <- renderPlotly({
	ggplotly(
		ggplot()+
			geom_point(data = df, 
				mapping=aes(x=fr_mlmin, y=size_nm),
				alpha=I(0.2), colour="#143b65")+
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
				alpha=I(0.2), colour="#143b65")+
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
				alpha=I(0.2), colour="#143b65")+
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
				alpha=I(0.2), colour="#143b65")+
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
				alpha=I(0.2), colour="#143b65")+
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
				alpha=I(0.2), colour="#143b65")+
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
				alpha=I(0.2), colour="#143b65")+
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
				alpha=I(0.2), colour="#143b65")+
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
### Zeta plots =====
output$expPlot_zeta_fr <- renderPlotly({
	ggplotly(
		ggplot()+
			geom_point(data = df, 
				mapping=aes(x=fr_mlmin, y=zeta_pot),
				alpha=I(0.2), colour="#143b65")+
			geom_errorbar(data = df_summ_fr,
				mapping=aes(x=fr_mlmin, ymin=mean_zeta-sd_zeta, ymax=mean_zeta+sd_zeta),
				colour="#a21c26", width=0.05*(max(df_summ_fr$fr_mlmin)-min(df_summ_fr$fr_mlmin)))+
			geom_point(data = df_summ_fr,
				mapping=aes(x=fr_mlmin, y=mean_zeta), size=2, shape=21, fill="#a21c26", 
				colour="#3b3b3b")+
			scale_x_continuous(name="Syringe pump flow rate (ml/min)")+
			scale_y_continuous(name="Zeta potential (mV)")+
			theme_shiny()
	)
}) 
output$expPlot_zeta_pei <- renderPlotly({
	ggplotly(
		ggplot()+
			geom_point(data = df, 
				mapping=aes(x=pei, y=zeta_pot),
				alpha=I(0.2), colour="#143b65")+
			geom_errorbar(data = df_summ_pei,
				mapping=aes(x=pei, ymin=mean_zeta-sd_zeta, ymax=mean_zeta+sd_zeta),
				colour="#a21c26", width=0.05*(max(df_summ_pei$pei)-min(df_summ_pei$pei)))+
			geom_point(data = df_summ_pei,
				mapping=aes(x=pei, y=mean_zeta), size=2, shape=21, fill="#a21c26", 
				colour="#3b3b3b")+
			scale_x_continuous(name="Total amt. PEI (mg)")+
			scale_y_continuous(name="Zeta potential (mV)")+
			theme_shiny()
	)
}) 
output$expPlot_zeta_p188 <- renderPlotly({
	ggplotly(
		ggplot()+
			geom_point(data = df, 
				mapping=aes(x=p188, y=zeta_pot),
				alpha=I(0.2), colour="#143b65")+
			geom_errorbar(data = df_summ_p188,
				mapping=aes(x=p188, ymin=mean_zeta-sd_zeta, ymax=mean_zeta+sd_zeta),
				colour="#a21c26", width=0.05*(max(df_summ_p188$p188)-min(df_summ_p188$p188)))+
			geom_point(data = df_summ_p188,
				mapping=aes(x=p188, y=mean_zeta), size=2, shape=21, fill="#a21c26", 
				colour="#3b3b3b")+
			scale_x_continuous(name="Total amt. Kolliphor P-188 (mg)")+
			scale_y_continuous(name="Zeta potential (mV)")+
			theme_shiny()
	)
}) 
output$expPlot_zeta_plga <- renderPlotly({
	ggplotly(
		ggplot()+
			geom_point(data = df, 
				mapping=aes(x=plga_spec, y=zeta_pot),
				alpha=I(0.2), colour="#143b65")+
			geom_errorbar(data = df_summ_plga,
				mapping=aes(x=plga_spec, ymin=mean_zeta-sd_zeta, ymax=mean_zeta+sd_zeta),
				colour="#a21c26", width=0.05*(max(df_summ_p188$p188)-min(df_summ_p188$p188)))+
			geom_point(data = df_summ_plga,
				mapping=aes(x=plga_spec, y=mean_zeta), size=2, shape=21, fill="#a21c26", 
				colour="#3b3b3b")+
			scale_x_discrete(name="PLGA Identity")+
			scale_y_continuous(name="Zeta potential (mV)")+
			theme_shiny()
	)
}) 




### Dashboard plots ==========================================================
#### OUTPUT FORMULATION TABLE =======================
opt_table <- reactive({
  df <- fmesh
  # Get offset from desired values
  df$size_abs_offset <- abs(df$pred_size-input$slider_diam)
  df$zeta_abs_offset <- abs(df$pred_zeta-input$slider_zeta)
  # Round column values
  df$pei <- round(df$pei, digits=2)
  df$fr <- round(df$fr, digits=2)

  # Order data by offset from predictions
  df <- df[with(df, order(df$size_abs_offset, df$zeta_abs_offset)), ]
  # Drop unnecessary columns
  df <- df[, !(names(df) %in% c("mod_pei", "mod_fr", "mod_p188", "mod_plga",
                                "size_abs_offset", "zeta_abs_offset"))]
  
  return(df)
})

output$opt_form <- DT::renderDT(
  return(opt_table())
)

#### VALIDATION PLOTS ===============================
output$dashPlot_corr_size <- renderPlotly({
	ggplotly(
		ggplot(df, aes(pred_size, size_nm))+
			geom_abline(colour="grey50", linetype=2)+
			geom_smooth(method=lm , color="#119ac3", 
				fill="#119ac3", se=TRUE)+
			geom_point(alpha=I(0.5), colour="#143b65")+
			scale_x_continuous(name="Predicted size (nm)", 
				limits=c(min(df$pred_size, df$size_nm),#
								 max(df$pred_size, df$size_nm)))+
			scale_y_continuous(name="Measured size (nm)", 
				limits=c(min(df$pred_size, df$size_nm),#
								 max(df$pred_size, df$size_nm)))+
      ggtitle(paste("r-squared =", rsq_size))+
			theme_shiny()
	)
})
output$dashPlot_corr_pdi <- renderPlotly({
	ggplotly(
		ggplot(df, aes(pred_pdi, pdi))+
			geom_abline(colour="grey50", linetype=2)+
			geom_smooth(method=lm , color="#119ac3", 
				fill="#119ac3", se=TRUE)+
			geom_point(alpha=I(0.5), colour="#143b65")+
			scale_x_continuous(name="Predicted PdI", 
				limits=c(min(df$pred_pdi, df$pdi),#
								 max(df$pred_pdi, df$pdi)))+
			scale_y_continuous(name="Measured PdI", 
				limits=c(min(df$pred_pdi, df$pdi),#
								 max(df$pred_pdi, df$pdi)))+
      ggtitle(paste("r-squared =", rsq_pdi))+
			theme_shiny()
	)
})
output$dashPlot_corr_zeta <- renderPlotly({
	ggplotly(
		ggplot(df, aes(pred_zeta, zeta_pot))+
			geom_abline(colour="grey50", linetype=2)+
			geom_smooth(method=lm , color="#119ac3", 
				fill="#119ac3", se=TRUE)+
			geom_point(alpha=I(0.5), colour="#143b65")+
			scale_x_continuous(name="Predicted zeta potential (mV)", 
				limits=c(min(df$pred_zeta, df$zeta_pot),#
								 max(df$pred_zeta, df$zeta_pot)))+
			scale_y_continuous(name="Measured zeta potential (mV)", 
				limits=c(min(df$pred_zeta, df$zeta_pot),#
								 max(df$pred_zeta, df$zeta_pot)))+
      ggtitle(paste("r-squared =", rsq_zeta))+
			theme_shiny()
  )
})

#### HEATMAPS =======================================
##### SIZE ==========================================
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

##### ZETA ==========================================
output$dashPlot_zeta_noP188_502h <- renderPlotly({
	ggplotly(
		ggplot(fmesh[which(fmesh$mod_p188 == -1 & fmesh$mod_plga == 1),],
				aes(pei, fr, fill=pred_zeta))+
			geom_raster()+
			scale_x_continuous(name="Total amt. PEI (mg)", expand=c(0,0))+
			scale_y_continuous(name="Syringe pump flow rate (ml/min)", expand=c(0,0))+
			scale_fill_viridis_c(option = "C",
        limits=c(min(fmesh$pred_zeta),max(fmesh$pred_zeta)))+
			theme_shiny()+
			ggtitle("PLGA: 502H | No Kolliphor P-188")+
			theme(legend.background = element_rect(fill ="#ecf0f5"))+
			labs(fill="Predicted Zeta\nPotential (nm)")
	)
}) 
output$dashPlot_zeta_P188_502h <- renderPlotly({
	ggplotly(
		ggplot(fmesh[which(fmesh$mod_p188 == 1 & fmesh$mod_plga == 1),],
				aes(pei, fr, fill=pred_zeta))+
			geom_raster()+
			scale_x_continuous(name="Total amt. PEI (mg)", expand=c(0,0))+
			scale_y_continuous(name="Syringe pump flow rate (ml/min)", expand=c(0,0))+
			scale_fill_viridis_c(option = "C",
        limits=c(min(fmesh$pred_zeta),max(fmesh$pred_zeta)))+
			theme_shiny()+
			ggtitle("PLGA: 502H | With Kolliphor P-188")+
			theme(legend.background = element_rect(fill ="#ecf0f5"))+
			labs(fill="Predicted Zeta\nPotential (nm)")
	)
}) 
output$dashPlot_zeta_noP188_504h <- renderPlotly({
	ggplotly(
		ggplot(fmesh[which(fmesh$mod_p188 == -1 & fmesh$mod_plga == -1),],
				aes(pei, fr, fill=pred_zeta))+
			geom_raster()+
			scale_x_continuous(name="Total amt. PEI (mg)", expand=c(0,0))+
			scale_y_continuous(name="Syringe pump flow rate (ml/min)", expand=c(0,0))+
			scale_fill_viridis_c(option = "C",
        limits=c(min(fmesh$pred_zeta),max(fmesh$pred_zeta)))+
			theme_shiny()+
			ggtitle("PLGA: 504H | No Kolliphor P-188")+
			theme(legend.background = element_rect(fill ="#ecf0f5"))+
			labs(fill="Predicted Zeta\nPotential (nm)")
	)
}) 
output$dashPlot_zeta_P188_504h <- renderPlotly({
	ggplotly(
		ggplot(fmesh[which(fmesh$mod_p188 == 1 & fmesh$mod_plga == -1),],
				aes(pei, fr, fill=pred_zeta))+
			geom_raster()+
			scale_x_continuous(name="Total amt. PEI (mg)", expand=c(0,0))+
			scale_y_continuous(name="Syringe pump flow rate (ml/min)", expand=c(0,0))+
			scale_fill_viridis_c(option = "C",
        limits=c(min(fmesh$pred_zeta),max(fmesh$pred_zeta)))+
			theme_shiny()+
			ggtitle("PLGA: 504H | With Kolliphor P-188")+
			theme(legend.background = element_rect(fill ="#ecf0f5"))+
			labs(fill="Predicted Zeta\nPotential (nm)")
	)
}) 

##### PDI ===========================================
output$dashPlot_pdi_noP188_502h <- renderPlotly({
	ggplotly(
		ggplot(fmesh[which(fmesh$mod_p188 == -1 & fmesh$mod_plga == 1),],
				aes(pei, fr, fill=pred_pdi))+
			geom_raster()+
			scale_x_continuous(name="Total amt. PEI (mg)", expand=c(0,0))+
			scale_y_continuous(name="Syringe pump flow rate (ml/min)", expand=c(0,0))+
			scale_fill_viridis_c(option = "F",
        limits=c(min(fmesh$pred_pdi),max(fmesh$pred_pdi)))+
			theme_shiny()+
			ggtitle("PLGA: 502H | No Kolliphor P-188")+
			theme(legend.background = element_rect(fill ="#ecf0f5"))+
			labs(fill="Predicted\nPdI (nm)")
	)
}) 
output$dashPlot_pdi_P188_502h <- renderPlotly({
	ggplotly(
		ggplot(fmesh[which(fmesh$mod_p188 == 1 & fmesh$mod_plga == 1),],
				aes(pei, fr, fill=pred_pdi))+
			geom_raster()+
			scale_x_continuous(name="Total amt. PEI (mg)", expand=c(0,0))+
			scale_y_continuous(name="Syringe pump flow rate (ml/min)", expand=c(0,0))+
			scale_fill_viridis_c(option = "F",
        limits=c(min(fmesh$pred_pdi),max(fmesh$pred_pdi)))+
			theme_shiny()+
			ggtitle("PLGA: 502H | With Kolliphor P-188")+
			theme(legend.background = element_rect(fill ="#ecf0f5"))+
			labs(fill="Predicted\nPdI (nm)")
	)
}) 
output$dashPlot_pdi_noP188_504h <- renderPlotly({
	ggplotly(
		ggplot(fmesh[which(fmesh$mod_p188 == -1 & fmesh$mod_plga == -1),],
				aes(pei, fr, fill=pred_pdi))+
			geom_raster()+
			scale_x_continuous(name="Total amt. PEI (mg)", expand=c(0,0))+
			scale_y_continuous(name="Syringe pump flow rate (ml/min)", expand=c(0,0))+
			scale_fill_viridis_c(option = "F",
        limits=c(min(fmesh$pred_pdi),max(fmesh$pred_pdi)))+
			theme_shiny()+
			ggtitle("PLGA: 504H | No Kolliphor P-188")+
			theme(legend.background = element_rect(fill ="#ecf0f5"))+
			labs(fill="Predicted\nPdI (nm)")
	)
}) 
output$dashPlot_pdi_P188_504h <- renderPlotly({
	ggplotly(
		ggplot(fmesh[which(fmesh$mod_p188 == 1 & fmesh$mod_plga == -1),],
				aes(pei, fr, fill=pred_pdi))+
			geom_raster()+
			scale_x_continuous(name="Total amt. PEI (mg)", expand=c(0,0))+
			scale_y_continuous(name="Syringe pump flow rate (ml/min)", expand=c(0,0))+
			scale_fill_viridis_c(option = "F",
        limits=c(min(fmesh$pred_pdi),max(fmesh$pred_pdi)))+
			theme_shiny()+
			ggtitle("PLGA: 504H | With Kolliphor P-188")+
			theme(legend.background = element_rect(fill ="#ecf0f5"))+
			labs(fill="Predicted\nPdI (nm)")
	)
}) 

output$dashPlot_predExplorer1 <- renderPlotly({
  ggplotly(
    ggplot(fmesh[which(fmesh$mod_p188 == -1 & fmesh$mod_plga == 1),], 
           aes(pred_size, pred_zeta, fill=pei))+
      geom_point(size=3, shape=21, alpha=I(0.85))+
			ggtitle("PLGA: 502H | No Kolliphor P-188")+
      scale_fill_viridis_c(option="G", 
        limits=c(min(fmesh$pei), max(fmesh$pei)))+
      scale_x_continuous(name="Predicted size (nm)",
                         limits=c(min(fmesh$pred_size), 
                                  max(fmesh$pred_size)))+
      scale_y_continuous(name="Predicted zeta potential (mV)",
                         limits=c(min(fmesh$pred_zeta), 
                                  max(fmesh$pred_zeta)))+
      theme_shiny()+
			theme(legend.background = element_rect(fill ="#ecf0f5"))
  )
})
output$dashPlot_predExplorer2 <- renderPlotly({
  ggplotly(
    ggplot(fmesh[which(fmesh$mod_p188 == 1 & fmesh$mod_plga == 1),], 
           aes(pred_size, pred_zeta, fill=pei))+
      geom_point(size=3, shape=21, alpha=I(0.85))+
			ggtitle("PLGA: 502H | With Kolliphor P-188")+
      scale_fill_viridis_c(option="G", 
        limits=c(min(fmesh$pei), max(fmesh$pei)))+
      scale_x_continuous(name="Predicted size (nm)",
                         limits=c(min(fmesh$pred_size), 
                                  max(fmesh$pred_size)))+
      scale_y_continuous(name="Predicted zeta potential (mV)",
                         limits=c(min(fmesh$pred_zeta), 
                                  max(fmesh$pred_zeta)))+
      theme_shiny()+
			theme(legend.background = element_rect(fill ="#ecf0f5"))
  )
})
output$dashPlot_predExplorer3 <- renderPlotly({
  ggplotly(
    ggplot(fmesh[which(fmesh$mod_p188 == -1 & fmesh$mod_plga == -1),], 
           aes(pred_size, pred_zeta, fill=pei))+
      geom_point(size=3, shape=21, alpha=I(0.85))+
			ggtitle("PLGA: 504H | No Kolliphor P-188")+
      scale_fill_viridis_c(option="G", 
        limits=c(min(fmesh$pei), max(fmesh$pei)))+
      scale_x_continuous(name="Predicted size (nm)",
                         limits=c(min(fmesh$pred_size), 
                                  max(fmesh$pred_size)))+
      scale_y_continuous(name="Predicted zeta potential (mV)",
                         limits=c(min(fmesh$pred_zeta), 
                                  max(fmesh$pred_zeta)))+
      theme_shiny()+
			theme(legend.background = element_rect(fill ="#ecf0f5"))
  )
})
output$dashPlot_predExplorer4 <- renderPlotly({
  ggplotly(
    ggplot(fmesh[which(fmesh$mod_p188 == 1 & fmesh$mod_plga == -1),], 
           aes(pred_size, pred_zeta, fill=pei))+
      geom_point(size=3, shape=21, alpha=I(0.85))+
			ggtitle("PLGA: 504H | With Kolliphor P-188")+
      scale_fill_viridis_c(option="G", 
        limits=c(min(fmesh$pei), max(fmesh$pei)))+
      scale_x_continuous(name="Predicted size (nm)",
                         limits=c(min(fmesh$pred_size), 
                                  max(fmesh$pred_size)))+
      scale_y_continuous(name="Predicted zeta potential (mV)",
                         limits=c(min(fmesh$pred_zeta), 
                                  max(fmesh$pred_zeta)))+
      theme_shiny()+
			theme(legend.background = element_rect(fill ="#ecf0f5"))
  )
})

} # End server
