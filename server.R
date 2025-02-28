# SERVER =====================================================================
server <- function(input, output) {


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




### Dashboard plots ==========================================================
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
			theme_shiny()
	)
})

#### HEATMAPS =======================================
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
