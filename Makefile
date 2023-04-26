
all: 
.PHONY: Ex2D

## 1. ________________________________________________________________
Ex2D:
	./manage_SHEEP.R -l Ex2D [ layout [ layout_color layout_tools ] sheet [ sheet_correlation_matrix sheet_diagnostic_station sheet_diagnostic_region ] panel [ panel_correlation_matrix panel_foot panel_hydrograph panel_diagnostic_criteria panel_station_info panel_region_info panel_mini_map panel_spaghetti ] ] -b
