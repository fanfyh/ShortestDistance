# ShortestDistance:

TODO : Give a brief description of what your project is about

This project...

# Brief description of folder and file contents

TODO: As project evolves, add brief description of what is inside the
data, docs and R folders.

The following folders contain:

```bash
├├── data        # data processed 
│   ├── all_cities_with_distance.csv
│   ├── all_ports_data.csv
│   ├── city_port_combinations.Rdata
│   ├── merged_sf_city.Rdata
│   └── README.md
├── data-raw    # raw data, manual changes are prohibited
│   ├── 城市经纬度        # Points for each city
│   ├── 2000年道路交通图  # sources: http://geodata.pku.edu.cn
│   ├── 2000-map        # sources: Taobao
│   ├── 2020-Waterway   # sources: Taobao
│   ├── gadm            # City area map 
│   ├── README.md
│   └── tidy_citycode_1990_2019.csv
├── DESCRIPTION
├── docs
│   └── README.md
├── R
│   ├── 01_port_data_collector.py  # collect port location info
│   ├── 02_generate_city_port_combinations.R  # generate potential combinations based on province
│   ├── 03_combine_difference_maps.R          # !todo: combine diffenrence maps toegher
│   ├── 04_calculate_shortest_distance.R      # based by united maps    
│   ├── funs
│   ├── main_files
│   ├── main.html
│   ├── main.qmd                              # 
│   ├── process_city_harbor_distances.R       # [depreciated] calculated linear distances
│   └── README.md
├── README_files
│   └── libs
├── README.html
├── README.md
├── ShortestDistance.Rproj
└── TODO.md
```
# Installing project R package dependencies

If dependencies have been managed by using
`usethis::use_package("packagename")` through the `DESCRIPTION` file,
installing dependencies is as easy as opening the
`ShortestDistance.Rproj` file and running this command in the console:

```         
# install.packages("pak")
pak::pak()
```

You'll need to have remotes installed for this to work.

# Resource

For more information on this folder and file workflow and setup, check
out the [prodigenr](https://rostools.github.io/prodigenr) online
documentation.
