# Description of R/ folder and .R files

This folder contains custom functions or R scripts used for this project’s analyses, such as in the docs/ folder. Each R script is split up into sections (e.g. with RStudio’s ‘Insert Section’ with Ctrl-Shift-R) by utility or purpose. You can use the ‘Document Outline’ (Ctrl-Shift-O) to see the sections and functions.



```bash 
├── 01_port_data_collector.py
├── 02_generate_city_port_combinations.R
├── 03_combine_difference_maps.R
├── 04_calculate_shortest_distance.R
├── funs
│   ├── 00_funs.R    # Contains utility functions migrated from previous projects.
│   ├── assign_points_to_cities.R 
│   ├── calculate_network_distance.R
│   ├── create_bimodal_network.R   
│   ├── create_transfer_edges.R  
│   ├── geneAbbr.R
│   └── province_map.R
├── main.qmd
├── process_city_harbor_distances.R
└── README.md
```