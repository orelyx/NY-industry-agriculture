# title: "synthesizeNAICScodes.R"
# author: "Eric Koski"
# date: "12/10/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 
# Synthesize 1-digit codes for 'Agriculture' (1), 'Mining, Utilities, and Construction' (2), 
# 'Manufacturing' (3), 'Trade, Transportation and Warehousing' (4). 
NAICS_Addrows1dig <- 
  tibble(
    NAICS = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    NAICSname = c(
      "Agriculture",   "Mining, Utilities, and Construction",
      "Manufacturing", "Trade, Transportation, and Warehousing",
      "General services: Information, Financial, Real Estate, Professional, Management, Waste Management",
      "Education and Training; Health Care and Social Assistance",
      "Arts, Entertainment, and Recreation; Accommodation and Food Services", 
      "Other Services", 
      "Public Administration and Government"
    ),
    NAICSdescr = c(
      "The Agriculture, Forestry, Fishing and Hunting sector comprises establishments primarily engaged in growing crops, raising animals, harvesting timber, and harvesting fish and other animals from a farm, ranch, or their natural habitats.\r\n\r\nThe establishments in this sector are often described as farms, ranches, dairies, greenhouses, nurseries, orchards, or hatcheries.  A farm may consist of a single tract of land or a number of separate tracts which may be held under different tenures.  For example, one tract may be owned by the farm operator and another rented.  It may be operated by the operator alone or with the assistance of members of the household or hired employees, or it may be operated by a partnership, corporation, or other type of organization. When a landowner has one or more tenants, renters, croppers, or managers, the land operated by each is considered a farm.",
      "The Mining, Quarrying, and Oil and Gas Extraction sector comprises establishments that extract naturally occurring mineral solids, such as coal and ores; liquid minerals, such as crude petroleum; and gases, such as natural gas.  The term mining is used in the broad sense to include quarrying, well operations, beneficiating (e.g., crushing, screening, washing, and flotation), and other preparation customarily performed at the mine site, or as a part of mining activity.\r\n\r\nThe Utilities sector comprises establishments engaged in the provision of the following utility services: electric power, natural gas, steam supply, water supply, and sewage removal.  Within this sector, the specific activities associated with the utility services provided vary by utility: electric power includes generation, transmission, and distribution; natural gas includes distribution; steam supply includes provision and/or distribution; water supply includes treatment and distribution; and sewage removal includes collection, treatment, and disposal of waste through sewer systems and sewage treatment facilities.\r\n\r\nThe Construction sector comprises establishments primarily engaged in the construction of buildings or engineering projects (e.g., highways and utility systems).  Establishments primarily engaged in the preparation of sites for new construction and establishments primarily engaged in subdividing land for sale as building sites also are included in this sector.  Construction work done may include new work, additions, alterations, or maintenance and repairs.  Activities of these establishments generally are managed at a fixed place of business, but they usually perform construction activities at multiple project sites.  Production responsibilities for establishments in this sector are usually specified in (1) contracts with the owners of construction projects (prime contracts) or (2) contracts with other construction establishments (subcontracts).",
      "The Manufacturing sector comprises establishments engaged in the mechanical, physical, or chemical transformation of materials, substances, or components into new products.  The assembling of component parts of manufactured products is considered manufacturing, except in cases where the activity is appropriately classified in Sector 23, Construction.\r\n\r\nEstablishments in the Manufacturing sector are often described as plants, factories, or mills and characteristically use power-driven machines and material handling equipment.  However, establishments that transform materials or substances into new products by hand or in the worker's home and those engaged in selling to the general public products made on the same premises from which they are sold, such as bakeries, candy stores, and custom tailors, may also be included in this sector.  The materials, substances, or components transformed by manufacturing establishments are raw materials that are products of agriculture, forestry, fishing, mining, or quarrying as well as products of other manufacturing establishments.  The new product of a manufacturing establishment may be finished in the sense that it is ready for utilization or consumption, or it may be semi-finished to become an input for an establishment engaged in further manufacturing.\r\n\r\nThe subsectors in the Manufacturing sector generally reflect distinct production processes related to material inputs, production equipment, and employee skills. In the machinery area, where assembling is a key activity, parts and accessories for manufactured products are classified in the industry of the finished manufactured item when they are made for separate sale.  For example, a replacement refrigerator door would be classified with refrigerators and an attachment for a piece of metalworking machinery would be classified with metalworking machinery.  However, components, input from other manufacturing establishments, are classified based on the production function of the component manufacturer.", 
      "The Retail Trade sector comprises establishments engaged in retailing merchandise, generally without transformation, and rendering services incidental to the sale of merchandise.  The retailing process is the final step in the distribution of merchandise; retailers are, therefore, organized to sell merchandise in small quantities to the general public.\r\n\r\n
The Wholesale Trade sector comprises establishments engaged in wholesaling merchandise, generally without transformation, and rendering services incidental to the sale of merchandise.  The merchandise described in this sector includes the outputs of agriculture, mining, manufacturing, and certain information industries, such as publishing.\r\n\r\nThe Transportation and Warehousing sector includes industries providing transportation of passengers and cargo, warehousing and storage for goods, scenic and sightseeing transportation, and support activities related to modes of transportation.  Establishments in these industries use transportation equipment or transportation related facilities as a productive asset.  The type of equipment depends on the mode of transportation.  The modes of transportation are air, rail, water, road, and pipeline.\r\n\r\nThe Transportation and Warehousing sector distinguishes three basic types of activities: subsectors for each mode of transportation, a subsector for warehousing and storage, and a subsector for establishments providing support activities for transportation.  In addition, there are subsectors for establishments that provide passenger transportation for scenic and sightseeing purposes, postal services, and courier services.",
"General services: Information, Financial, Real Estate, Professional, Management, Waste Management",
"Education and Training; Health Care and Social Assistance",
"Arts, Entertainment, and Recreation; Accommodation and Food Services", 
"Other Services", 
"Public Administration and Government"),
origin = "synthesized"
  )

# Synthesize 2-digit codes for Manufacturing (31..33); Retail Trade (44, 45), and 
# Transportation (48); and for Postal, courier, and express delivery; Warehousing and 
# Storage (49)
NAICS_Addrows2dig <- 
  tibble(NAICS = c(31, 32, 33, 44, 45, 48, 49), 
         NAICSname = c(
           "Manufacturing (Food, Beverages, Tobacco, Textiles, Apparel, Leather)",   #31
           str_c("Manufacturing (Wood, Paper, Printing, Petroleum, Coal products, ", #32
                 "Chemicals, Plastic and Rubber products, Minerals)"),
           str_c("Manufacturing (Metals, Machinery, Electronics, Transportation, ",  #33
                 "Furniture, Miscellaneous"),
           str_c("Retail Trade (Consumer Goods, Food and Beverages, Health and ",    #44
                 "Personal Care, Home and Garden, Clothing, Luxury; Filling Stations)"),
           str_c("Retail Trade (Leisure Goods, Publications, General Merchandise, ", #45
                 "Miscellaneous Retailers, Nonstore Retailers)"),
           "General Transportation (Passenger and Freight)",                         #48
           str_c("Postal, Courier, and Express Delivery; Storage")                   #49
         ),
         NAICSdescr = c(
           "Manufacturing (Food, Beverages, Tobacco, Textiles, Apparel, Leather)",   #31
           str_c("Manufacturing (Wood, Paper, Printing, Petroleum, Coal products, ", #32 
                 "Chemicals, Plastic and Rubber products, Minerals)"),
           str_c("Manufacturing (Metals, Machinery, Electronics, Transportation, ",  #33
                 "Furniture, Miscellaneous"),
           str_c("Retail Trade (Consumer Goods, Food and Beverages, Health and ",    #44
                 "Personal Care, Home and Garden, Clothing, Luxury; Filling Stations)"),
           str_c("Retail Trade (Leisure Goods, Publications, General Merchandise, ", #45
                 "Miscellaneous Retailers, Nonstore Retailers)"),
           "General Transportation (Passenger and Freight)",                         #48
           str_c("Postal, Courier, and Express Delivery; Warehousing and Storage")), #49
         origin = "synthesized")

NAICS_Addrows3dig <- 
  tibble(NAICS = 119, 
         NAICSname = "Agriculture (unclassified)", 
         NAICSdescr = "Agriculture (unclassified)",
         origin = "synthesized")

NAICS_Addrows4dig <- 
  tibble(NAICS = c(1191, 2369, 2378, 2388),
         NAICSname = c("Agriculture (unclassified)", 
                       "Building Construction (unclassified)",
                       "Heavy and Civil Engineering Construction (unclassified)",
                       "Specialty Trade Contractors (unclassified)"),
         NAICSdescr = c("Agriculture (unclassified)",
                        "Building Construction (unclassified)",
                        "Heavy and Civil Engineering Construction (unclassified)",
                        "Specialty Trade Contractors (unclassified)"),
         origin = "synthesized")

NAICS_Addrows5dig <- 
  tibble(NAICS = c(11910, 23690, 23780, 23880),
         NAICSname = c("Agriculture (unclassified)", 
                       "Building Construction (unclassified)",
                       "Heavy and Civil Engineering Construction (unclassified)",
                       "Specialty Trade Contractors (unclassified)"),
         NAICSdescr = c("Agriculture (unclassified)",
                        "Building Construction (unclassified)",
                        "Heavy and Civil Engineering Construction (unclassified)",
                        "Specialty Trade Contractors (unclassified)"),
         origin = "synthesized")

NAICS_Addrows6dig <- 
  tibble(NAICS = c(119100, 236900, 237800, 238800),
         NAICSname = c("Agriculture (unclassified)", 
                       "Building Construction (unclassified)",
                       "Heavy and Civil Engineering Construction (unclassified)",
                       "Specialty Trade Contractors (unclassified)"),
         NAICSdescr = c("Agriculture (unclassified)",
                        "Building Construction (unclassified)",
                        "Heavy and Civil Engineering Construction (unclassified)",
                        "Specialty Trade Contractors (unclassified)"),
         origin = "synthesized")
