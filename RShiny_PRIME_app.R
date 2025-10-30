#******************************************************************************.
# World Health Organization
# Aquarius Population Health
# Project: 290_WHO_HPV_tool_interface
# Last updated: 24/10/25
# Code description: 
#  (1) Build the RShiny WHO HPV Decision Tool
#******************************************************************************.

#====================
# Set directory
#====================

# Get the script location
this_path <- tryCatch({
  # When sourcing a script
  dirname(sys.frame(1)$ofile)
}, error = function(e) {
  # When running interactively in RStudio
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    dirname(rstudioapi::getActiveDocumentContext()$path)
  } else {
    getwd()  # fallback
  }
})

base_path <- normalizePath(this_path, winslash = "/")

# Set working directory
setwd(base_path)

#===============================
# Packages
#===============================

.ensure_pkgs <- function(pkgs, github = NULL, pinned = list()) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing)) install.packages(missing, dependencies = TRUE)
  
  # Handle pinned CRAN versions
  if (length(pinned)) {
    if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
    for (p in names(pinned)) {
      if (!requireNamespace(p, quietly = TRUE) || packageVersion(p) != pinned[[p]]) {
        remotes::install_version(p, version = pinned[[p]], repos = "http://cran.us.r-project.org")
      }
    }
  }
  
  # GitHub installs (only if not present)
  if (!is.null(github)) {
    if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
    for (repo in github) {
      pkgname <- sub(".*/", "", repo)
      if (!requireNamespace(pkgname, quietly = TRUE)) {
        remotes::install_github(repo, upgrade = "never")
      }
    }
  }
  
  invisible(lapply(pkgs, require, character.only = TRUE))
}

cran_pkgs <- c(
  # Shiny + UI
  "shiny","bslib","shinyWidgets","htmltools","DT",
  
  # Data wrangling
  "dplyr","tidyr","readr","purrr","stringr","data.table",
  
  # Plotting
  "ggplot2","ggtext","ggpattern","ggnewscale","scales","plotly",
  
  # Modeling / PRIME deps
  "rjags","foreach","lhs","stats","prevalence",
  
  # Reporting
  "rmarkdown","knitr","kableExtra","gridExtra","tinytex","bsicons",
  
  # Excel export
  "writexl"
)

github_pkgs <- c("lshtm-vimc/prime")

# Ensure packages, pinning wbstats to 1.0.4
.ensure_pkgs(cran_pkgs, github = github_pkgs, pinned = list("wbstats" = "1.0.4"))

#===============================
# TinyTeX bootstrap for PDF
#===============================
if (requireNamespace("tinytex", quietly = TRUE)) {
  if (!tinytex::is_tinytex()) try(tinytex::install_tinytex(), silent = TRUE)
  
  # Pre-install common LaTeX packages so first render is fast
  pkgs_needed <- c("multirow","wrapfig","colortbl","pdflscape",
                   "tabu","varwidth","threeparttable","threeparttablex",
                   "environ","trimspaces","ulem","makecell")
  try(tinytex::tlmgr_install(pkgs_needed), silent = TRUE)
}

#-------------------------------.
# Libraries
#-------------------------------.
library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)

library(dplyr)
library(tidyr)
library(data.table)

library(ggplot2)
library(ggtext)
library(ggpattern)
library(ggnewscale)
library(scales)

library(stringr)
library(readr)
library(purrr)

library(rmarkdown)
library(knitr)
library(kableExtra)
library(gridExtra)

library(rjags)     # needs JAGS installed
library(foreach)
library(wbstats)
library(lhs)
library(prevalence)
library(prime)
library(bsicons)


  # Load PRIME new functions
  source("PRIME_functions.R")
  
  # Map the WHO logo folder (www) to /assets
  addResourcePath("assets", file.path(base_path, "www"))
  

#===============================
# Prepare data inputs dataset
#===============================

  # Import dataset with all default values
  data.tool <- read.csv("data/data.tool.csv", stringsAsFactors = FALSE)
  
  # Vaccine price and costs per dose (already per dose in data.tool)
  data.tool <- data.tool %>%
    mutate(
      vac_price = vac_price,
      vac_delivery = vac_delivery
    )
  
  # Add assumption about vaccine effectiveness, coverage (default:90%), target age (default: 9)
  data.tool <- data.tool %>%
    mutate(
      eff_one = 100,
      eff_two = 100,
      cov = 90,
      agevac = 9
    )
  
  # Add cohort size variable
    # Parameters
    agevac <- 9  # default value
    agecohort <- 9  # default value
    year_now <- as.numeric(format(Sys.time(), "%Y"))   # e.g. 2025
    year_born <- year_now - agevac                     # e.g. 2016
    
    # Build default cohort dataset
    default_cohorts <- data.pop %>%
      filter(age_from == agecohort, year == (year_born + agecohort)) %>%
      transmute(
        iso3   = country_code,
        cohort = round(value, 0)   # no decimals
      )
    
    # Merge with your data.tool
    data.tool <- data.tool %>%
      left_join(default_cohorts, by = "iso3")
  
  


#====================
# UI
#====================

ui <- 
    
  #----------------------------------------
  # Formatting
  #----------------------------------------
    
  # Aligning elements in the nav panel
  tagList(
  
  # CSS
    
    # Custom CSS for DT tables
    tags$head(
      tags$style(HTML("
    /* Header cells */
    table.dataTable thead th {
      background-color: #ffffff !important;  /* force white */
      font-weight: bold !important;          
      color: #5a5a5a;                        
      border-bottom: 2px solid #ddd !important;
    }
    
    /* Footer cells (if any) */
    table.dataTable tfoot th {
      background-color: #ffffff !important;
    }

    /* Remove vertical borders */
    table.dataTable tbody td, 
    table.dataTable thead th {
      border-left: none !important;
      border-right: none !important;
    }

    /* Horizontal row separators only */
    table.dataTable tbody td {
      border-top: 1px solid #eee !important;
      background-color: #ffffff !important;  /* force white rows */
      color: #5a5a5a;
      font-size: 0.9rem;
    }

    table.dataTable tbody tr {
      background-color: #ffffff !important;  /* remove striping */
    }
      "))
      
    ),
    
  # Format for graph cards  
  tags$head(
    tags$style(HTML("
    .graph-card .tab-content {
      height: 500px;       /* fixed height for all cards */
      overflow-y: auto;    /* scroll if content is taller */
    }
    "))
  ),
    
  # Format for the nav bar
  tags$head(
    tags$style(HTML("
      /* Keep navbar items on one line and vertically centered */
      .navbar-nav { flex-wrap: nowrap; }
      .navbar .nav-link, .navbar .nav-item {
        display: flex;
        align-items: center;
      }
      /* Tidy the logo size/alignment */
      .navbar .nav-item img {
        height: 40px;            /* adjust as needed */
        margin-left: 8px;
        vertical-align: middle;
      }
    "))
  ),
    
    #--------------------------------------
    # Main layout for the app ----.
    #--------------------------------------
    page_navbar(
    fillable = FALSE, # controlling how items grow/shrink when browser different sizes
    window_title = "WHO HPV tool",
    lang = "en",
    
    theme = bs_theme(bootswatch = "cerulean"),
    id = "nav",
    navbar_options = navbar_options(
      bg = "white", # background navbar colour
      collapsible = TRUE # collapse tabs on smaller screens
    ),

    #--------------------------------------
    # Homepage ----.
    #--------------------------------------
    
    # This tab is the homepage of the app.
    nav_panel("Home",
              
              #---------------------------------------------
              # Banner with background image + overlay text 
              #---------------------------------------------
              div(
                style = "position: relative; width: 100%; height: 300px; overflow: hidden;",
                
                # Banner image
                tags$img(
                  src = "assets/vaccine.jpg",
                  style = "width: 100%; height: 100%; object-fit: cover;"
                ),
                
                # Overlay text container (aligned left)
                div(
                  style = "
                  position: absolute; 
                  top: 50%; 
                  left: 60px; 
                  transform: translateY(-50%);
                  text-align: left; 
                  background: rgba(0,0,0,0.4); 
                  padding: 20px 30px; 
                  border-radius: 8px;
                ",
                  
                  h1("Welcome to WHO HPV PRIME",
                     style = "font-weight: bold; font-size: 36px; margin-bottom: 10px; color: white;"),
                  
                  h3("Explore HPV vaccination impacts across countries with evidence using PRIME",
                     style = "font-weight: normal; font-size: 20px; margin-bottom: 20px; color: white;"),
                  
                  # Button: Go to PRIME (navigation)
                  actionButton(
                    inputId = "goto_tool_btn",
                    label = tagList(bs_icon("bar-chart-fill"), "Go to PRIME"),
                    class = "btn btn-light btn-lg",
                    style = "border-radius: 6px; font-size: 16px; color: black;"
                  )
                ),
                # Attribution block (bottom right)
                div(
                  style = "
                  position: absolute; 
                  bottom: 20px; 
                  right: 40px; 
                  display: flex; 
                  flex-direction: column; 
                  align-items: flex-start; 
                  gap: 6px;
                  background: rgba(255,255,255,0.8); 
                  padding: 8px 12px; 
                  border-radius: 6px;
                ",
                  p("App developed by:", 
                    style = "margin: 0; color: #333; font-size: 13px; text-align: left;"
                  ),
                  tags$img(
                    src = "assets/APH_logo_long.png",
                    style = "height: 40px;"
                  )
                )
                
              ),
              
              #--------------------------------
              # Pill-list tabset below banner
              #--------------------------------
              div(
                style = "margin-top: 40px; padding: 0 40px;",
                
                navset_pill_list(
                  id = "home_pills",
                  widths = c(3, 9),   # 3-col pill list on the left, 9-col content on the right
                  
                  #------------------------------------------------------------------
                  # 1) Main menu: About PRIME → sub-tabs (PRIME, Inputs, Outputs)
                  #------------------------------------------------------------------
                  nav_menu(
                    "About PRIME",
                    
                    nav_panel(
                      "Overall description",
                      div(
                        class = "p-3",
                        h3("Overall description", style = "margin-bottom: 10px;"),
                        # Section: About the PRIME
                        p("Papillomavirus Rapid Interface for Modelling and Economics (PRIME) is a static modelling tool designed to estimate the health and economic impact of HPV vaccination in girls before sexual debut, specifically for cervical cancer prevention. It evaluates health outcomes such as reductions in cases, deaths, and disability-adjusted life years (DALYs) and economic outcomes such as vaccine delivery and cervical cancer treatment costs for both single-age and multi-age cohorts."),
                        p("PRIME was endorsed by the WHO Immunization and Vaccines Implementation Research Advisory Committee to provide conservative estimates of health impact and cost-effectiveness."),
                        p("The model uses default inputs from global databases and publications, but these may not be available or suitable for all countries. Customizing inputs with local data —ideally in collaboration with experts such as epidemiologists, health economists, and policymakers — is strongly recommended."),
                        p("The PRIME tool is not designed for complex analyses such as:"),
                        tags$ul(
                          tags$li("Evaluating herd immunity and cross-protection"),
                          tags$li("Vaccination in multiple age cohorts"),
                          tags$li("Male vaccination"),
                          tags$li("Comparing vaccines with different valencies"),
                          tags$li("Assessing vaccine impact on cervical screening programmes")
                        ),
                        p("Although this app is intended only for vaccinating a single age cohort of girls, vaccinating multiple age cohorts is expected to result in a substantially faster impact compared to single cohort vaccination. Therefore, WHO recommends vaccinating multiple age cohorts when feasible. The impact estimated by this app for a single age cohort should be considered conservative."),
                        p("This app is intended to be user-friendly for non-modellers, including country program managers and decision-makers in low- and middle-income countries. Its simplicity supports local model adaptation and evidence-based policy decisions, especially where capacity for developing complex models is limited."),
                        p("PRIME relies on global datasets rather than country-specific data. Therefore, if a country’s results suggest HPV vaccination is not cost-effective, this result should prompt further investigation rather than be taken as a reason to reject vaccination. Also, its results are better suited for understanding regional trends rather than informing national policy directly. However, it can serve as a useful starting point to guide local data collection and support more contextualized, evidence-based decision-making."),
                        p(
                          "Further details can be found in the peer-reviewed publications describing PRIME:"),
                          p("Jit M, Brisson M, Portnoy A, Hutubessy R. Cost-effectiveness of female human papillomavirus vaccination in 179 countries: a PRIME modelling study. ",
                          em("Lancet Global Health"), " 2014; 2(7):e406. Available at: ",
                          a("https://doi.org/10.1016/s2214-109x(14)70237-2",
                            href = "https://doi.org/10.1016/s2214-109x(14)70237-2",
                            target = "_blank"
                          ),
                          ),
                          p("Abbas KM, van Zandvoort K, Brisson M, Jit M. Effects of updated demography, disability weights, and cervical cancer burden on estimates of human papillomavirus vaccination impact at the global, regional, and national levels: a PRIME modelling study. ",
                            em("Lancet Global Health"), " 2020 Apr;8(4):e536–44. Available at: ",
                            a("https://doi.org/10.1016/S2214-109X(20)30022-X",
                              href = "https://doi.org/10.1016/S2214-109X(20)30022-X",
                              target = "_blank"
                            ),
                          ),
                        p("This app has been developed using the PRIME R package version 2.0.18 from the Modelling and Economic Evaluation of Vaccines group (MEEV), London School of Hygiene & Tropical Medicine. Source code available at: ",
                          a("https://github.com/lshtm-vimc/prime",
                            href = "https://github.com/lshtm-vimc/prime",
                            target = "_blank"
                          ))
                      )
                    ),
                    
                    nav_panel(
                      "Inputs",
                      div(
                        class = "p-3",
                        h3("Data inputs", style = "margin-bottom: 10px;"),
                        p("The table below lists the data parameters required by PRIME. In the app, each parameter is automatically populated with default values from global databases, position papers, and published studies. These default values can also be overridden manually by the user."),
                        p("Further information can be found in the peer-reviewed publication describing PRIME, avaiable ", 
                          a("here.",
                            href = "https://doi.org/10.1016/s2214-109x(14)70237-2",
                            target = "_blank"
                          )),
                        tags$table(
                          class = "table table-striped table-bordered",
                          style = "width: 100%;",
                          
                          tags$thead(
                            tags$tr(
                              tags$th("Parameter", style = "width: 25%;"),
                              tags$th("Description", style = "width: 55%;"),
                              tags$th("Reference for default values", style = "width: 20%;")
                            )
                          ),
                          
                          tags$tbody(
                            tags$tr(
                              tags$td("Proportion of cervical cancer cases due to HPV 16/18 (%)"),
                              tags$td("The proportion of cervical cancer cases diagnosed in the base year that are caused by HPV 16 or 18 infection."),
                              tags$td("Serrano et al., 2012")
                              ),
                            tags$tr(
                              tags$td("Vaccination coverage (%)"),
                              tags$td("The expected proportion of girls in the relevant age group who will receive the full course of the vaccine (either 1 or 2 doses)."),
                              tags$td("The global target for cervical cancer elimination was used in default. Users are encouraged to override the default with any national target.")
                            ),
                            tags$tr(
                              tags$td("Target age group (age)"),
                              tags$td("The age at which HPV vaccines are routinely given."),
                              tags$td("The minimum age of WHO recommended primary target population for HPV vaccination is used as default.")
                            ),
                            tags$tr(
                              tags$td("Cohort size at vaccination age (number of females)"),
                              tags$td("The number of females in the country at the age at which routine vaccination is given (based on the age in “target age group”)."),
                              tags$td("UN World Population Prospects, 2024")
                            ),
                            tags$tr(
                              tags$td("Vaccine price per dose (USD)"),
                              tags$td("The procurement cost to purchase enough vaccines to vaccinate one girl with one dose."),
                              tags$td("For every grouping of countries with fixed price agreements exist, the lowest price is presented as default. When ranges are available, the median prices paid in the market are the default.")
                            ),
                            tags$tr(
                              tags$td("Vaccine delivery cost per dose for one-dose scenario (USD)"),
                              tags$td("The cost of delivering and administering enough vaccines to vaccinate one girl with one dose."),
                              tags$td("Yeung et al., 2025")
                            ),
                            tags$tr(
                              tags$td("Vaccine delivery cost per dose for two-dose scenario (USD)"),
                              tags$td("The cost of delivering and administering enough vaccines to vaccinate one girl with two doses."),
                              tags$td("Yeung et al., 2025")
                            ),
                            tags$tr(
                              tags$td("Vaccine efficacy for one-dose scenario (%)"),
                              tags$td(HTML("The proportionate reduction in risk of cervical cancers due to HPV 16/18 in vaccinees. Protection level provided by a single dose of the HPV vaccine.<br/>
                                            Current evidence suggests that a single dose has comparable efficacy and duration of protection as a two-dose schedule.")),
                              tags$td(HTML("WHO, 2022<br/>
                                           SAGE, 2022"))
                            ),
                            tags$tr(
                              tags$td("Vaccine efficacy for two-dose scenario (%)"),
                              tags$td("The proportionate reduction in risk of cervical cancers due to HPV 16/18 in vaccinees. Protection level provided by two doses of the HPV vaccine. This should normally be 95-100%."),
                              tags$td(HTML("WHO, 2022<br/>
                                           SAGE, 2022"))
                            ),
                            tags$tr(
                              tags$td("Cervical cancer treatment cost, per episode over lifetime (USD)"),
                              tags$td("The cost on average to treat a woman with cervical cancer, from diagnosis to death."),
                              tags$td("2017")
                            ),
                            tags$tr(
                              tags$td("Disability weight for cancer diagnosis (scale from 0 to 1)"),
                              tags$td("Severity of health loss when diagnosed with cervical cancer, on a scale from 0 (full health) to 1 (equivalent to death). Advice from a health economist is recommended before altering this parameter."),
                              tags$td(HTML("Salomon et al., 2015<br/>
                                           James et al., 2017"))
                            ),
                            tags$tr(
                              tags$td("Disability weight for non-terminal cancer sequelae (scale from 0 to 1)"),
                              tags$td("Severity of health loss from cervical cancer in cases that are not fatal, on a scale from 0 (full health) to 1 (equivalent to death). Advice from a health economist is recommended before altering this parameter."),
                              tags$td(HTML("Salomon et al., 2015<br/>
                                           James et al., 2017"))
                            ),
                            tags$tr(
                              tags$td("Disability weight for terminal cancer (scale from 0 to 1)"),
                              tags$td("Severity of health loss in the final phase of fatal cervical cancer, on a scale from 0 (full health) to 1 (equivalent to death). Advice from a health economist is recommended before altering this parameter."),
                              tags$td(HTML("Salomon et al., 2015<br/>
                                           James et al., 2017"))
                            ),
                            tags$tr(
                              tags$td("Discount rate for health costs (%)"),
                              tags$td("The rate representing society’s preference for consumption and health gains in the present rather than in the future. It is used to reduce the value of future healthcare costs compared to present costs."),
                              tags$td("Torres et al., 2003")
                            ),
                            tags$tr(
                              tags$td("Discount rate for health outcomes (%)"),
                              tags$td("The rate representing society’s preference for consumption and health gains in the present rather than in the future. It is used to reduce the value of future health benefits compared to present benefits."),
                              tags$td("Torres et al., 2003")
                            ),
                            tags$tr(
                              tags$td("Cost-effectiveness threshold (USD)"),
                              tags$td("Maximum amount considered acceptable to spend per unit of health gained (e.g., per DALY averted). By default, PRIME uses GDP per capita data from the World Bank as the cost-effectiveness threshold. 0.5 time of GDP per capita is used as default."),
                              tags$td("World Bank Group, 2024")
                            )
                          )
                        )

                      )
                    ),
                    
                    nav_panel(
                      "Outputs",
                      div(
                        class = "p-3",
                        h3("Model outputs", style = "margin-bottom: 10px;"),
                        p("The table below provides information to help interpret PRIME outputs."),
                        p("Further information can be found in the peer-reviewed publication describing PRIME, available ", 
                          a("here.",
                            href = "https://doi.org/10.1016/s2214-109x(14)70237-2",
                            target = "_blank"
                          )),
                        tags$table(
                          class = "table table-striped table-bordered",
                          style = "width: 100%;",
                          tags$thead(
                            tags$tr(
                              tags$th("Outputs", style = "width: 30%;"),
                              tags$th("Description")
                            )
                          ),
                          tags$tbody(
                            tags$tr(
                              tags$td("Cohort size at birth (female)"),
                              tags$td("The number of female newborns in the country in the base year.")
                            ),
                            tags$tr(
                              tags$td("Cohort size at vaccination age (female)"),
                              tags$td("The number of females in the country at the age at which routine vaccination is given (based on the age in 'Target age group').")
                            ),
                            tags$tr(
                              tags$td("Vaccine procurement cost"),
                              tags$td("The vaccine procurement cost of vaccinating a single age cohort in the base year.")
                            ),
                            tags$tr(
                              tags$td("Vaccine delivery cost"),
                              tags$td("The vaccine delivery cost of vaccinating a single age cohort in the base year.")
                            ),
                            tags$tr(
                              tags$td("Cost of vaccination including delivery costs"),
                              tags$td("The total cost of vaccinating a single age cohort in the base year.")
                            ),
                            tags$tr(
                              tags$td("Treatment costs saved"),
                              tags$td("The treatment costs eventually averted due to cervical cancer cases prevented by vaccinating a single age cohort in the base year.")
                            ),
                            tags$tr(
                              tags$td("Net cost"),
                              tags$td("The net (incremental) cost of vaccinating a single age cohort in the base year. This equals the cost of vaccination minus the treatment costs saved.")
                            ),
                            tags$tr(
                              tags$td("Cervical cancers prevented"),
                              tags$td("The number of cervical cancers eventually averted by vaccinating a single age cohort in the base year.")
                            ),
                            tags$tr(
                              tags$td("Deaths prevented"),
                              tags$td("The number of deaths eventually averted due to cervical cancer cases prevented by vaccinating a single age cohort in the base year.")
                            ),
                            tags$tr(
                              tags$td("Life years saved"),
                              tags$td("The number of life year losses eventually averted due to cervical cancer cases prevented by vaccinating a single age cohort in the base year.")
                            ),
                            tags$tr(
                              tags$td("Nonfatal DALYs averted"),
                              tags$td("The number of DALY losses eventually averted due to cervical cancer cases prevented by vaccinating a single age cohort in the base year.")
                            ),
                            tags$tr(
                              tags$td("Total DALYs averted"),
                              tags$td("The total number of DALY losses eventually averted (including life years saved) due to cervical cancer cases prevented by vaccinating a single age cohort in the base year.")
                            ),
                            tags$tr(
                              tags$td("Incremental cost per cervical cancer prevented"),
                              tags$td("The net (incremental) cost of vaccination divided by the number of cervical cancers eventually averted by vaccinating a single age cohort in the base year.")
                            ),
                            tags$tr(
                              tags$td("Incremental cost per life saved"),
                              tags$td("The net (incremental) cost of vaccination divided by the number of deaths eventually averted due to cervical cancer cases prevented by vaccinating a single age cohort in the base year.")
                            ),
                            tags$tr(
                              tags$td("Incremental cost per life year saved"),
                              tags$td("The net (incremental) cost of vaccination divided by the number of life years eventually saved due to cervical cancer cases prevented by vaccinating a single age cohort in the base year.")
                            ),
                            tags$tr(
                              tags$td("Incremental cost per total DALYs prevented"),
                              tags$td("The net (incremental) cost of vaccination divided by the number of total DALYs eventually averted due to cervical cancer cases prevented by vaccinating a single age cohort in the base year.")
                            )
                          )
                        )

                      )
                    )
                  ),
                  #-------------------------------------------------------
                  # 2) Main tab: How to navigate the app
                  #-------------------------------------------------------
                  nav_panel(
                    "How to navigate the app",
                    div(
                      class = "p-3",
                      h3("How to navigate the app", style = "margin-bottom: 10px;"),
                      
                      p("This app is built in R Shiny and includes two main pages within PRIME:"),
                      tags$ul(
                        tags$li(strong("Parameters")),
                        tags$li(strong("Results"))
                      ),
                      p("Navigation between these pages is sequential: users must first complete the ",
                        strong("Parameters"), " page before moving to the ", strong("Results"), " page."
                      ),
                      
                      h5("Parameters page"),
                      p("On the ", strong("Parameters"), " page, start by using the ", strong("Select country"),
                        " drop-down menu to choose your country of interest. Once a country is selected, the input fields are automatically populated with default values, drawing on global databases, position papers, and published studies."
                      ),
                      p("These default values may not always represent the most appropriate data for your country. If you have access to additional or more accurate information, it is strongly recommended that you update the inputs. To update the inputs, enter the new values into the ",
                        strong("Override value"), " cells."
                      ),
                      p("Any customization of inputs or activities to collect suitable data should ideally be undertaken in consultation with epidemiologists, health economists, clinicians, policy makers, and other experts familiar with cervical cancer data in the country."
                      ),
                      p("When entering data, it is important to ensure consistency in the use of units (for example, percentages or currency) and to confirm that values correspond to the same population group and year. If needed, the ",
                        strong("Reset to default values"), " button restores all inputs in the Override column to their original defaults."
                      ),
                      p("For further details on the data inputs required for PRIME, see the section About PRIME on the Home page."),
                      p("With ",
                        strong("Select comparison,"), "users can choose the vaccination strategies to compare. ",
                        "The comparison will apply to all results sections and includes the following options:"
                      ),
                      tags$ul(
                        tags$li("No vaccination vs. One-dose"),
                        tags$li("No vaccination vs. Two-dose"),
                        tags$li("No vaccination vs. One-dose vs. Two-dose"),
                        tags$li("One-dose vs. Two-dose")
                      ),
                      
                      p("After reviewing and editing the parameters, click ", strong("Run PRIME"),
                        " to execute the model. The calculation will take a few seconds, and once complete, a message will appear stating ",
                        em("Model run completed!"), " along with an option to proceed to the Results page."
                      ),
                      
                      h5("Results page"),
                      p("Results are grouped into three sections:"),
                      tags$ul(
                        tags$li(strong("Health impact:"), "cervical cancer cases prevented, deaths prevented, and DALYs averted."),
                        tags$li(strong("Economic impact:"), "vaccination costs, treatment costs saved, and net cost."),
                        tags$li(strong("Cost-effectiveness:"), "a scatter plot showing the trade-off between net costs and benefits of HPV vaccination strategies, along with cost-effectiveness thresholds.")
                      ),
                      p("Navigation between these results sections is done using the top buttons on each page."),
                      p("For further details on the model outputs, see the section About PRIME on the Home page."),
                      
                      p("At the top right of the ", strong("Results"), " page, two download options are available:"),
                      tags$ul(
                        tags$li(strong("Download PDF report:"), "generates a PDF version of the results displayed in the app."),
                        tags$li(strong("Download data:"), "generates an Excel file containing all model outputs.")
                      )
                    )
                  ),
                  
                  #-------------------------------------------------------
                  # 3) Main tab: About cervical cancer & HPV vaccination
                  #-------------------------------------------------------
                  nav_panel(
                    "About cervical cancer and HPV vaccination",
                    div(
                      class = "p-3",
                      h3("About cervical cancer and HPV vaccination", style = "margin-bottom: 10px;"),
                      p("Cervical cancer is the fourth most common cancer among women globally, with approximately 660,000 new cases and 350,000 deaths in 2022. Most cases can be prevented through effective primary prevention (human papillomavirus [HPV] vaccination) and secondary prevention (screening and treatment of precancerous lesions)."),
                      p("Cervical cancer is primarily caused by persistent infection with HPV. All currently licensed HPV vaccines are highly effective in preventing infection with HPV types 16 and 18, which together account for about 70% of cervical cancer cases worldwide."),
                      p("The primary target group for HPV vaccination in most countries is adolescent girls aged 9–14. According to the WHO Position Paper on HPV Vaccines (December 2022), the recommended vaccination schedules are:"),
                      tags$ul(
                        tags$li("One or two doses for girls aged 9–14"),
                        tags$li("One or two doses for girls and women aged 15–20"),
                        tags$li("Two doses with a six-month interval for women aged over 21"),
                        tags$li("At least two doses, and preferably three, for individuals known to be immunocompromised or living with HIV")
                      )                      )
                  ),
                  
                  #-------------------------------------------------------
                  # 4) Main tab: List of abbreviations
                  #-------------------------------------------------------
                  nav_panel(
                    "List of abbreviations",
                    div(
                      class = "p-3",
                      h3("List of abbreviations", style = "margin-bottom: 10px;"),
                      p("Quick reference for abbreviations used in this app."),
                      
                      tags$table(
                        class = "table table-striped table-bordered",
                        style = "width: 100%;",
                        tags$thead(
                          tags$tr(
                            tags$th("Abbreviation", style = "width: 25%;"),
                            tags$th("Full form")
                          )
                        ),
                        tags$tbody(
                          tags$tr(
                            tags$td("PRIME"),
                            tags$td("Papillomavirus Rapid Interface for Modelling and Economics")
                          ),
                          tags$tr(
                            tags$td("HPV"),
                            tags$td("Human Papillomavirus")
                          ),
                          tags$tr(
                            tags$td("WHO"),
                            tags$td("World Health Organization")
                          ),
                          tags$tr(
                            tags$td("DALY"),
                            tags$td("Disability-Adjusted Life Year")
                          ),
                          tags$tr(
                            tags$td("USD"),
                            tags$td("United States Dollar")
                          ),
                          tags$tr(
                            tags$td("LSHTM"),
                            tags$td("London School of Hygiene & Tropical Medicine")
                          ),
                          tags$tr(
                            tags$td("MEEV"),
                            tags$td("Modelling and Economic Evaluation of Vaccines group")
                          ),
                          
                        )
                      )
                    )
                  ),

                  
                )
              ),
              
              
    ),
    
    #--------------------------------------
    # PRIME Tool page ----.
    #--------------------------------------
    
    # This tab for the PRIME tool visualization.
    nav_panel("PRIME",
              navset_tab(
                id = "main_tabs",

                #-----------------------------
                # Parameters page ----.
                #-----------------------------
                nav_panel("Parameters",
                          value = "parameters_tab",
                          
                          #----------------------------------------------
                          # Select country, comparison & Run PRIME button
                          #----------------------------------------------
                          div(
                            style = "display: flex; justify-content: center; align-items: center; gap: 20px; margin-top: 20px;",
                            
                            # Left side: stacked rows for country + comparison
                            div(
                              style = "display: flex; flex-direction: column; gap: 15px;",
                              
                              # Row for country
                              div(
                                style = "display: flex; align-items: baseline; gap: 38px;",
                                strong("Select country:"),
                                selectInput(
                                  "country", NULL,
                                  choices = c("", sort(unique(data.tool$Country))),
                                  selected = "",
                                  width = "400px"
                                )
                              ),
                              
                              # Row for comparison
                              div(
                                style = "display: flex; align-items: baseline; gap: 10px;",
                                strong("Select comparison:"),
                                selectInput(
                                  "comparison", NULL,
                                  choices = c(
                                    "No vaccination vs. One-dose",
                                    "No vaccination vs. Two-dose",
                                    "No vaccination vs. One-dose vs. Two-dose",
                                    "One-dose vs. Two-dose"
                                  ),
                                  selected = "No vaccination vs. One-dose vs. Two-dose",
                                  width = "400px"
                                )
                              )
                            ),
                            
                            # Right side: Run PRIME button
                            div(
                              tags$head(
                                tags$style(HTML("
                                button#run_prime.btn.btn-success:active {
                                  background-color: #343a40 !important;
                                  background-image: none !important;
                                  color: white !important;
                                }
                              "))
                              ),
                              actionButton("run_prime", "Run PRIME", class = "btn-success")
                            )
                          ),
                          
                          
                          #--------------------------------------
                          # Parameters table:
                          #--------------------------------------
                          div(
                            style = "max-width: 1000px; margin: 0 auto;",  # controls width and centers it
                          
                          card(
                            card_body(
                              fillable = FALSE,
                              div(
                                style = "display: flex; justify-content: space-between; align-items: center;",
                                
                                # Left side
                                div(
                                strong("Parameters"),
                                tooltip(
                                  bs_icon("info-circle"),
                                  HTML("<br>This table shows the parameters used in PRIME. 
                                    To learn more about PRIME, refer to Home and More information pages in the app.
                                    <br><br>You can customize the model by adjusting the input fields. After reviewing and editing the parameters, click <b>Run PRIME<b> to execute the model. The calculation will take a few seconds, and once complete, a message will appear stating Model run completed! along with an option to proceed to the Results page. <br><br>"),
                                  placement = "right")
                                ),
                                # Right side
                                tags$head(
                                  tags$style(HTML("
                                  button#run_reset.btn.btn-secondary:active {
                                    background-color: #343a40 !important;
                                    background-image: none !important;
                                    color: white !important;
                                  }
                                  button#run_reset.btn.btn-secondary:hover {
                                    border-color: #d3d3d3 !important;
                                  }
                                "))
                                ),
                                actionButton("run_reset", "Reset to default values", class = "btn-secondary btn-sm")
                              )
                            ),
                            
                            #-------------------------------------------
                            # Population and targets inputs:
                            #-------------------------------------------
                            card_body(
                              fillable = FALSE,
                              div(
                                style = "
                                display: flex;
                                justify-content: space-between;
                                align-items: baseline;
                                width: 100%;
                                border-bottom: 1px solid #ddd;
                                padding-bottom: 4px;
                                margin-bottom: 6px;
                                background-color: #46AEE9;
                                padding: 8px 12px;
                                border-radius: 4px;
                                color: white;
                              ",
                                
                                # Left side: Section title
                                strong("I. Population and targets"),
                                
                                # Right side: two headers
                                div(
                                  style = "display: flex; gap: 40px; justify-content: flex-end; min-width: 250px;",
                                  strong("Default value"),
                                  strong("Override value")
                                )
                              )
                            ),
                            
                            card_body(
                              fillable = TRUE,
                              # Outer flex-column to stack rows
                              div(
                                style = "
                                display: flex;
                                flex-direction: column;
                                gap: 1px;                        /* space between rows */
                                width: 100%;
                              ",
                                # — Row 1: Proportion of cervical cancer cases due to HPV 16/18 —
                                div(
                                  style = "display: flex;align-items: baseline;justify-content: space-between;width: 100%;",
                                  tags$span("Proportion of cervical cancer cases due to HPV 16/18 (%):",
                                            tooltip(
                                              bs_icon("info-circle"),
                                              HTML("The proportion of cervical cancer cases diagnosed in the base year that are caused by HPV 16 or 18 infection."),
                                              placement = "right"
                                            ),
                                  ),
                                  div(
                                    style = "display: flex; align-items: baseline; gap: 10px;",
                                    uiOutput("ui_hpv1618_prop"),
                                    numericInput(
                                      "hpv1618_prop", NULL,
                                      value = "", min = 0, max = 100, step = 0.1,
                                      width = "100px"
                                    )
                                  )
                                ),
                                # — Row 2: Coverage —
                                div(
                                  style = "display: flex;align-items: baseline;justify-content: space-between;width: 100%;",
                                  # Left label
                                  tags$span("Vaccination coverage (%):",
                                            tooltip(
                                              bs_icon("info-circle"),
                                              HTML("The expected proportion of girls in the relevant age group who will receive the full course of the vaccine (either 1 or 2 doses)."),
                                              placement = "right"
                                            ),
                                  ),

                                  # Right group
                                  div(
                                    style = "display: flex; align-items: baseline; gap: 10px;",
                                    uiOutput("ui_cov"),
                                    numericInput(
                                      "cov", NULL,
                                      value = "", min = 0, max = 100,
                                      width = "100px"
                                    )
                                  )
                                ),
                                
                                # — Row 3: Target age group —
                                div(
                                  style = "display: flex;align-items: baseline;justify-content: space-between;width: 100%;",
                                  # Left label
                                  tags$span("Target age group (age):",
                                            tooltip(
                                              bs_icon("info-circle"),
                                              HTML("The age at which HPV vaccines are routinely given."),
                                              placement = "right"
                                            ),
                                  ),
                                  # Right group
                                  div(
                                    style = "display: flex; align-items: baseline; gap: 10px;",
                                    uiOutput("ui_agevac"),
                                    numericInput(
                                      "agevac", NULL,
                                      value = "", 
                                      width = "100px"
                                    )
                                  )
                                ),
                                # — Row 4: Cohort size at vaccination age (female) —
                                div(
                                  style = "display: flex;align-items: baseline;justify-content: space-between;width: 100%;",
                                  # Left label
                                  tags$span("Cohort size at vaccination age (number of females):",
                                            tooltip(
                                              bs_icon("info-circle"),
                                              HTML("The number of females in the country at the age at which routine vaccination is given (based on the age in “target age group”)."),
                                              placement = "right"
                                            ),
                                  ),
                                  # Right group
                                  div(
                                    style = "display: flex; align-items: baseline; gap: 10px;",
                                    uiOutput("ui_cohort"),
                                    numericInput(
                                      "cohort", NULL,
                                      value = "", min = 0, max = 100000000,
                                      width = "100px"
                                    )
                                  )
                                )
                              )
                            ),
                            
                            #-------------------------------------------
                            # Vaccination costs and efficacy inputs
                            #-------------------------------------------
                            card_body(
                              fillable = FALSE,
                              div(
                                style = "
                                display: flex;
                                justify-content: space-between;
                                align-items: baseline;
                                width: 100%;
                                border-bottom: 1px solid #ddd;
                                padding-bottom: 4px;
                                margin-bottom: 6px;
                                background-color: #46AEE9;
                                padding: 8px 12px;
                                border-radius: 4px;
                                color: white;
                              ",
                                
                                # Left side: Section title
                                strong("II. Vaccination costs and efficacy"),
                                
                                # Right side: two headers
                                div(
                                  style = "display: flex; gap: 40px; justify-content: flex-end; min-width: 250px;",
                                  strong("Default value"),
                                  strong("Override value")
                                )
                              )
                            ),
                            
                            card_body(
                              fillable = TRUE,
                              # Outer flex-column to stack rows
                              div(
                                style = "
                                display: flex;
                                flex-direction: column;
                                gap: 1px;                        /* space between rows */
                                width: 100%;
                              ",
                                
                                # — Row 1: Vaccine price —
                                div(
                                  style = "display: flex;align-items: baseline;justify-content: space-between;width: 100%;",
                                  # Left label
                                  tags$span("Vaccine price per dose (USD):",
                                            tooltip(
                                              bs_icon("info-circle"),
                                              HTML("The procurement cost to purchase enough vaccines to vaccinate one girl with one dose."),
                                              placement = "right"
                                            ),
                                  ),
                                  # Right group
                                  div(
                                    style = "display: flex; align-items: baseline; gap: 10px;",
                                    uiOutput("ui_vac_price"),
                                    numericInput(
                                      "vac_price", NULL,
                                      value = "", min = 0, max = 1000, step = 0.01,
                                      width = "100px"
                                    )
                                  )
                                ),
                                
                                # — Row 2.1: Vaccine delivery cost —
                                div(
                                  style = "display: flex; align-items: baseline; justify-content: space-between; width: 100%;",
                                  # Left label
                                  tags$span("Vaccine delivery cost per dose for one-dose scenario (USD):",
                                            tooltip(
                                              bs_icon("info-circle"),
                                              HTML("The cost of delivering and administering enough vaccines to vaccinate one girl with one dose."),
                                              placement = "right"
                                            ),          
                                  ),
                                  # Right group
                                  div(
                                    style = "display: flex; align-items: baseline; gap: 10px;",
                                    uiOutput("ui_vac_delivery"),
                                    numericInput(
                                      "vac_delivery", NULL,
                                      value = "", min = 0, max = 1000, step = 0.01,
                                      width = "100px"
                                    )
                                  )
                                ),
                                # — Row 2.2: Vaccine delivery cost —
                                div(
                                  style = "display: flex; align-items: baseline; justify-content: space-between; width: 100%;",
                                  # Left label
                                  tags$span("Vaccine delivery cost per dose for two-dose scenario (USD):",
                                            tooltip(
                                              bs_icon("info-circle"),
                                              HTML("The cost of delivering and administering enough vaccines to vaccinate one girl with two doses."),
                                              placement = "right"
                                            ),          
                                  ),
                                  # Right group
                                  div(
                                    style = "display: flex; align-items: baseline; gap: 10px;",
                                    uiOutput("ui_vac_delivery2"),
                                    numericInput(
                                      "vac_delivery2", NULL,
                                      value = "", min = 0, max = 1000, step = 0.01,
                                      width = "100px"
                                    )
                                  )
                                ),
                                # — Row 3: Effectiveness for one-dose scenario —
                                div(
                                  style = "display: flex;align-items: baseline;justify-content: space-between;width: 100%;",
                                  tags$span("Vaccine efficacy for one-dose scenario (%):",
                                            tooltip(
                                              bs_icon("info-circle"),
                                              HTML("The proportionate reduction in risk of cervical cancers due to HPV 16/18 in vaccinees. Protection level provided by a single dose of the HPV vaccine.<br/><br/>
                                                   Current evidence suggests that <b>a single dose has comparable efficacy and duration of protection as a two-dose schedule.<b>"),
                                              placement = "right"
                                            ),          
                                  ),
                                  div(
                                    style = "display: flex; align-items: baseline; gap: 10px;",
                                    uiOutput("ui_eff_one"),
                                    numericInput(
                                      "eff_one", NULL,
                                      value = "", min = 0, max = 100,
                                      step = 1,
                                      width = "100px"
                                    )
                                  )
                                ),
                                # — Row 4: Effectiveness for two-dose scenario —
                                div(
                                  style = "display: flex;align-items: baseline;justify-content: space-between;width: 100%;",
                                  tags$span("Vaccine efficacy for two-dose scenario (%):",
                                            tooltip(
                                              bs_icon("info-circle"),
                                              HTML("The proportionate reduction in risk of cervical cancers due to HPV 16/18 in vaccinees. Protection level provided by two doses of the HPV vaccine. This should normally be 95-100%."),
                                              placement = "right"
                                            ),          
                                  ),
                                  div(
                                    style = "display: flex; align-items: baseline; gap: 10px;",
                                    uiOutput("ui_eff_two"),
                                    numericInput(
                                      "eff_two", NULL,
                                      value = "", min = 0, max = 100,
                                      step = 1,
                                      width = "100px"
                                    )
                                  )
                                )
                                
                              )
                            ),
                            
                            #-------------------------------------------
                            # Health impact and treatment costs inputs
                            #-------------------------------------------
                            card_body(
                              fillable = FALSE,
                              div(
                                style = "
                                display: flex;
                                justify-content: space-between;
                                align-items: baseline;
                                width: 100%;
                                border-bottom: 1px solid #ddd;
                                padding-bottom: 4px;
                                margin-bottom: 6px;
                                background-color: #46AEE9;
                                padding: 8px 12px;
                                border-radius: 4px;
                                color: white;
                              ",
                                
                                # Left side: Section title
                                strong("III.  Treatment costs and disability weight"),
                                
                                # Right side: two headers
                                div(
                                  style = "display: flex; gap: 40px; justify-content: flex-end; min-width: 250px;",
                                  strong("Default value"),
                                  strong("Override value")
                                )
                              )
                            ),
                            
                            card_body(
                              fillable = TRUE,
                              # Outer flex-column to stack rows
                              div(
                                style = "
                                display: flex;
                                flex-direction: column;
                                gap: 1px;                        /* space between rows */
                                width: 100%;
                              ",
                                
                                # — Row 1: Cervical cancer treatment cost —
                                div(
                                  style = "display: flex;align-items: baseline;justify-content: space-between;width: 100%;",
                                  # Left label
                                  tags$span("Cervical cancer treatment cost, per episode over lifetime (USD):",
                                            tooltip(
                                              bs_icon("info-circle"),
                                              HTML("The cost on average to treat a woman with cervical cancer, from diagnosis to death."),
                                              placement = "right"
                                            ),        
                                  ),
                                  # Right group
                                  div(
                                    style = "display: flex; align-items: baseline; gap: 10px;",
                                    uiOutput("ui_cecx_cost"),
                                    numericInput(
                                      "cecx_cost", NULL,
                                      value = "", min = 0, max = 100000, step = 0.01,
                                      width = "100px"
                                    )
                                  )
                                ),
                                
                                # — Row 2: Disability weight Dx —
                                div(
                                  style = "display: flex;align-items: baseline;justify-content: space-between;width: 100%;",
                                  # Left label
                                  tags$span("Disability weight for cancer diagnosis (scale from 0 to 1):",
                                            tooltip(
                                              bs_icon("info-circle"),
                                              HTML("Severity of health loss when diagnosed with cervical cancer, on a scale from 0 (full health) to 1 (equivalent to death). Advice from a health economist is recommended before altering this parameter."),
                                              placement = "right"
                                            ),        
                                  ),
                                  # Right group
                                  div(
                                    style = "display: flex; align-items: baseline; gap: 10px;",
                                    uiOutput("ui_dw_dx"),
                                    numericInput(
                                      "dw_dx", NULL,
                                      value = "", min = 0, max = 1,
                                      step = 0.01,
                                      width = "100px"
                                    )
                                  )
                                ),
                                
                                # — Row 3: Disability weight for non-terminal cancer sequelae —
                                div(
                                  style = "display: flex;align-items: baseline;justify-content: space-between;width: 100%;",
                                  tags$span("Disability weight for non-terminal cancer sequelae (scale from 0 to 1):",
                                            tooltip(
                                              bs_icon("info-circle"),
                                              HTML("Severity of health loss from cervical cancer in cases that are not fatal, on a scale from 0 (full health) to 1 (equivalent to death). Advice from a health economist is recommended before altering this parameter."),
                                              placement = "right"
                                            ),    
                                  ),
                                  div(
                                    style = "display: flex; align-items: baseline; gap: 10px;",
                                    uiOutput("ui_dw_seq"),
                                    numericInput(
                                      "dw_seq", NULL,
                                      value = "", min = 0, max = 1,
                                      step = 0.01,
                                      width = "100px"
                                    )
                                  )
                                ),
                                
                                # — Row 4: Disability weight for terminal cancer —
                                div(
                                  style = "display: flex;align-items: baseline;justify-content: space-between;width: 100%;",
                                  tags$span("Disability weight for terminal cancer (scale from 0 to 1):",
                                            tooltip(
                                              bs_icon("info-circle"),
                                              HTML("Severity of health loss in the final phase of fatal cervical cancer, on a scale from 0 (full health) to 1 (equivalent to death). Advice from a health economist is recommended before altering this parameter."),
                                              placement = "right"
                                            ),          
                                  ),
                                  div(
                                    style = "display: flex; align-items: baseline; gap: 10px;",
                                    uiOutput("ui_dw_term"),
                                    numericInput(
                                      "dw_term", NULL,
                                      value = "", min = 0, max = 1,
                                      step = 0.01,
                                      width = "100px"
                                    )
                                  )
                                )
                              )
                            ),
                            
                            
                            #-------------------------------------------
                            # Economic evaluation criteria
                            #-------------------------------------------
                            card_body(
                              fillable = FALSE,
                              div(
                                style = "
                                display: flex;
                                justify-content: space-between;
                                align-items: baseline;
                                width: 100%;
                                border-bottom: 1px solid #ddd;
                                padding-bottom: 4px;
                                margin-bottom: 6px;
                                background-color: #46AEE9;
                                padding: 8px 12px;
                                border-radius: 4px;
                                color: white;
                              ",
                                
                                # Left side: Section title
                                strong("IV. Economic evaluation settings"),
                                
                                # Right side: two headers
                                div(
                                  style = "display: flex; gap: 40px; justify-content: flex-end; min-width: 250px;",
                                  strong("Default value"),
                                  strong("Override value")
                                )
                              )
                            ),

                            card_body(
                              fillable = TRUE,
                              # Outer flex-column to stack rows
                              div(
                                style = "
                                display: flex;
                                flex-direction: column;
                                gap: 1px;                        /* space between rows */
                                width: 100%;
                              ",
                                
                                # — Row 1: Discounting for health costs —
                                div(
                                  style = "display: flex;align-items: baseline;justify-content: space-between;width: 100%;",
                                  # Left label
                                  tags$span("Discount rate for health costs (%):",
                                            tooltip(
                                              bs_icon("info-circle"),
                                              HTML("The rate representing society’s preference for consumption and health gains in the present rather than in the future. It is used to reduce the value of future healthcare costs compared to present costs."),
                                              placement = "right"
                                            ),          
                                  ),
                                  # Right group
                                  div(
                                    style = "display: flex; align-items: baseline; gap: 10px;",
                                    uiOutput("ui_disc.cost"),
                                    numericInput(
                                      "disc.cost", NULL,
                                      value = "", min = 0, max = 100, step = 0.1,
                                      width = "100px"
                                    )
                                  )
                                ),
                                
                                # — Row 2: Discounting for health outcomes  —
                                div(
                                  style = "display: flex; align-items: baseline; justify-content: space-between; width: 100%;",
                                  # Left label
                                  tags$span("Discount rate for health outcomes (%):",
                                            tooltip(
                                              bs_icon("info-circle"),
                                              HTML("The rate representing society’s preference for consumption and health gains in the present rather than in the future. It is used to reduce the value of future health benefits compared to present benefits."),
                                              placement = "right"
                                            ),          
                                  ),
                                  # Right group
                                  div(
                                    style = "display: flex; align-items: baseline; gap: 10px;",
                                    uiOutput("ui_disc.ben"),
                                    numericInput(
                                      "disc.ben", NULL,
                                      value = "", min = 0, max = 100, step = 0.1,
                                      width = "100px"
                                    )
                                  )
                                ),
                                
                                # — Row 3: Cost-effectiveness threshold —
                                div(
                                  style = "display: flex; align-items: baseline; justify-content: space-between; width: 100%;",
                                  # Left label
                                  tags$span("Cost-effectiveness threshold (USD):",
                                            tooltip(
                                              bs_icon("info-circle"),
                                              HTML("Maximum amount considered acceptable to spend per unit of health gained (e.g., per DALY averted). By default, PRIME will show 0.5 time GDP per capita data from the World Bank as the cost-effectiveness threshold.<br><br>
                                                   Use the box to enter an additional threshold value."),
                                              placement = "right"
                                            ),          
                                  ),
                                  # Right group
                                  div(
                                    style = "display: flex; align-items: baseline; gap: 10px;",
                                    numericInput(
                                      "ce_thr", NULL,
                                      value = "", min = 0, max = 1000000,
                                      width = "100px"
                                    )
                                  )
                                ),
                              )
                            ),
                            #--------------------------------------.
                          )
                          )
                ),

                #-----------------------------
                # Results page ----.
                #-----------------------------
                nav_panel("Results",
                          value = "results_page",
  
                          #--------------------------------------
                          # Formatting
                          #--------------------------------------
                          tags$head(
                            tags$style(HTML("
                            .results-header {
                              display: flex;
                              justify-content: space-between;
                              align-items: flex-start;
                              margin-top: 25px;
                              margin-bottom: 25px;
                            }
                            .country-name {
                              color: #006285;         /* Dark blue */
                              margin-top: 10px;
                            }
                            .top-right-buttons {
                              display: flex;
                              gap: 10px;              /* space between buttons */
                            }
                            /* Normal state */
                            a#downloadPDF.btn.btn-info,
                            a#downloadData.btn.btn-info {
                              background-color: #1482d7 !important;
                              border-color: #1482d7 !important;
                              color: white !important;
                              background-image: none !important;
                            }
                            /* Hover state */
                            a#downloadPDF.btn.btn-info:hover,
                            a#downloadData.btn.btn-info:hover {
                              background-color: #e0e0e0 !important;
                              border-color: #e0e0e0 !important;
                              color: black !important;
                              background-image: none !important;
                            }
                            /* Active/pressed state */
                            a#downloadPDF.btn.btn-info:active,
                            a#downloadData.btn.btn-info:active,
                            a#downloadPDF.btn.btn-info:focus:active,
                            a#downloadData.btn.btn-info:focus:active {
                              background-color: #343a40 !important;
                              border-color: #343a40 !important;
                              color: white !important;
                              background-image: none !important;
                            }
                            "))
                          ),
                          
                          #--------------------------------------
                          # Titles + Download buttons
                          #--------------------------------------
                          tags$div(
                            class = "results-header",
                            # Left: Titles
                            tags$div(
                              tags$div(h2("HPV vaccination impact results by PRIME")),
                              tags$div(h5(textOutput("selected_country"), class = "country-name")),

                              # Select comparison:
                              #--------------------.
                              tags$div(
                                style = "display: flex; align-items: baseline; gap: 10px;",
                                
                                # Selected option text (keeps your styling)
                                tags$h5(
                                  textOutput("chosen_comparison", inline = TRUE),
                                  style = "color: #006285; margin-top: 5px; margin-bottom: 5px;"
                                )
                              )
                              
                            ),
                            # Right: Buttons
                            tags$div(
                              class = "top-right-buttons",
                              tags$a(
                                id = "downloadPDF",
                                class = "btn btn-info btn-sm shiny-download-link",
                                href = "", target = "_blank", download = NA,
                                bs_icon("file-earmark-pdf-fill"), # icon
                                "Download PDF report"
                              ),
                              downloadButton("downloadData", "Download data", class = "btn btn-info btn-sm")
                            )
                          ),


                          #--------------------------------------
                          # Results tabs
                          #--------------------------------------
                          navset_tab(
                            id = "results_tabs",
                            
                            #--------------------------------------
                            # Health impact tab
                            #--------------------------------------
                            nav_panel("Health impact",
                                      value = "health_tab",
                                      br(),

                                        #------------------------------
                                        # Value boxes:
                                        #------------------------------
                                        layout_column_wrap(
                                          width = "80px",
                                          uiOutput("vaccines_delivered_box"),
                                          uiOutput("females_vaccinated_box")
                                        ),
                                        
                                        #------------------------------
                                        # Graphs:
                                        #------------------------------
                                        # Format
                                        tags$head(
                                          tags$style(HTML("
                                          .three-cards {
                                            display: flex;
                                            gap: 20px;        /* space between cards */
                                          }
                                          .three-cards > div {
                                            flex: 1;          /* all cards equal width */
                                          }
                                          "))
                                        ),
                                        
                                        # The three cards
                                        div(
                                          class = "three-cards",
                                          
                                          #------------------------------
                                          # Graph1: CeCx cases prevented
                                          #------------------------------
                                          div(
                                          class = "graph-card",
                                          navset_card_pill( 
                                            nav_panel("Chart",
                                                      div(
                                                        div(
                                                          textOutput("plot1_title"),
                                                          style = "font-weight:bold; margin-bottom:0;"
                                                        ),
                                                        div(
                                                          textOutput("plot1_subtitle"),
                                                          style = "margin-top:0; margin-bottom:20px;"
                                                        ),
                                                        plotOutput("gph_cases_prevented", height = "300px")
                                                      )
                                            ), 
                                            nav_panel("Table", 
                                                      card_body(
                                                      div(  
                                                        div(
                                                          textOutput("tab1_title"),
                                                          style = "font-weight:bold; margin-bottom:0;"
                                                        ),
                                                        div(
                                                          textOutput("tab1_subtitle"),
                                                          style = "margin-top:0; margin-bottom:20px;"
                                                        ),
                                                        dataTableOutput("tab_cases_prevented")
                                                      ))
                                            ), 
                                            nav_panel("Supporting information", 
                                                      h5("What does the chart show?"),
                                                      p("The chart shows the estimated number of cervical cancer cases that would be prevented under each selected vaccination strategy. 
                                                        These estimates are based on model outputs from PRIME, which project the impact of HPV vaccination on disease incidence over time."),
                                                      h5("How do I interpret the chart?"),
                                                      p("Higher values indicate a greater reduction in cervical cancer cases due to vaccination. 
                                                         Comparing strategies allows you to see which approach is projected to prevent the most cases.")
                                            ),
                                            # Footer for the navset card
                                            card_footer(
                                              downloadLink(
                                                outputId = "download_plot1",
                                                label = tagList(bs_icon("bar-chart-fill"), "Save chart (PNG)"),
                                                class = "btn-link",
                                                style = "background:none; border:none; color:#2FA4E7; font-size:0.9rem; padding:0;"
                                              ),
                                              downloadLink(
                                                outputId = "download_table1",
                                                label = tagList(bs_icon("file-spreadsheet-fill"), "Download data (Excel)"),
                                                class = "btn-link",
                                                style = "background:none; border:none; color:#2FA4E7; font-size:0.9rem; padding:0; margin-left:15px;"
                                              )
                                            )
                                          )),
                                          
                                          #------------------------------
                                          # Graph2: Deaths prevented
                                          #------------------------------
                                          div(
                                            class = "graph-card",
                                            navset_card_pill( 
                                              nav_panel("Chart",
                                                        div(
                                                          div(
                                                            textOutput("plot2_title"),
                                                            style = "font-weight:bold; margin-bottom:0;"
                                                          ),
                                                          div(
                                                            textOutput("plot2_subtitle"),
                                                            style = "margin-top:0; margin-bottom:20px;"
                                                          ),
                                                          plotOutput("gph_deaths_prevented", height = "300px")
                                                        )
                                              ), 
                                              nav_panel("Table", 
                                                        card_body(
                                                          div(  
                                                            div(
                                                              textOutput("tab2_title"),
                                                              style = "font-weight:bold; margin-bottom:0;"
                                                            ),
                                                            div(
                                                              textOutput("tab2_subtitle"),
                                                              style = "margin-top:0; margin-bottom:20px;"
                                                            ),
                                                            dataTableOutput("tab_deaths_prevented")
                                                        ))
                                              ), 
                                              nav_panel("Supporting information", 
                                                        h5("What does the chart show?"),
                                                        p("The chart shows the estimated number of cervical cancer deaths that would be prevented under each selected vaccination strategy. 
                                                        These estimates are derived from PRIME model projections of how HPV vaccination reduces the risk of developing and dying from cervical cancer."),
                                                        h5("How do I interpret the chart?"),
                                                        p("Higher values mean fewer deaths are expected to occur due to the protection provided by vaccination. 
                                                        Comparing strategies highlights which approach has the greatest potential to save lives.")
                                              ),
                                              # Footer for the navset card
                                              card_footer(
                                                downloadLink(
                                                  outputId = "download_plot2",
                                                  label = tagList(bs_icon("bar-chart-fill"), "Save chart (PNG)"),
                                                  class = "btn-link",
                                                  style = "background:none; border:none; color:#2FA4E7; font-size:0.9rem; padding:0;"
                                                ),
                                                downloadLink(
                                                  outputId = "download_table2",
                                                  label = tagList(bs_icon("file-spreadsheet-fill"), "Download data (Excel)"),
                                                  class = "btn-link",
                                                  style = "background:none; border:none; color:#2FA4E7; font-size:0.9rem; padding:0; margin-left:15px;"
                                                )
                                              )
                                            )),
                                          
                                          #------------------------------
                                          # Graph3: DALYs prevented
                                          #------------------------------
                                          div(
                                            class = "graph-card",
                                            navset_card_pill( 
                                              nav_panel("Chart",
                                                        div(
                                                          div(
                                                            textOutput("plot3_title"),
                                                            style = "font-weight:bold; margin-bottom:0;"
                                                          ),
                                                          div(
                                                            textOutput("plot3_subtitle"),
                                                            style = "margin-top:0; margin-bottom:20px;"
                                                          ),
                                                          plotOutput("gph_dalys_prevented", height = "300px")
                                                        )
                                              ), 
                                              nav_panel("Table", 
                                                        card_body(
                                                          div(  
                                                            div(
                                                              textOutput("tab3_title"),
                                                              style = "font-weight:bold; margin-bottom:0;"
                                                            ),
                                                            div(
                                                              textOutput("tab3_subtitle"),
                                                              style = "margin-top:0; margin-bottom:20px;"
                                                            ),
                                                            dataTableOutput("tab_dalys_prevented")
                                                        ))
                                              ), 
                                              nav_panel("Supporting information", 
                                                        h5("What does the chart show?"),
                                                        p("The chart shows the estimated number of Disability-Adjusted Life Years (DALYs) that would be prevented under each selected vaccination strategy. 
                                                        DALYs combine the burden from both premature death and years lived with disability, providing a measure of total health impact."),
                                                        h5("How do I interpret the chart?"),
                                                        p("Higher values indicate a greater overall health benefit from vaccination, reflecting both reduced mortality and improved quality of life. 
                                                        Comparing strategies shows which approach yields the largest total health gains.")
                                              ),
                                              # Footer for the navset card
                                              card_footer(
                                                downloadLink(
                                                  outputId = "download_plot3",
                                                  label = tagList(bs_icon("bar-chart-fill"), "Save chart (PNG)"),
                                                  class = "btn-link",
                                                  style = "background:none; border:none; color:#2FA4E7; font-size:0.9rem; padding:0;"
                                                ),
                                                downloadLink(
                                                  outputId = "download_table3",
                                                  label = tagList(bs_icon("file-spreadsheet-fill"), "Download data (Excel)"),
                                                  class = "btn-link",
                                                  style = "background:none; border:none; color:#2FA4E7; font-size:0.9rem; padding:0; margin-left:15px;"
                                                )
                                              )
                                            ))
                                          
                                        ),
                                        
                                                
                            ),
                            
                            #--------------------------------------
                            # Economic impact tab
                            #--------------------------------------
                            nav_panel("Economic impact",
                                      value = "econ_tab",
                                      br(),
                                      
                                        #------------------------------
                                        # Value boxes:
                                        #------------------------------
                                        layout_column_wrap(
                                          width = "80px",
                                          uiOutput("vaccines_delivered_box_econ"),
                                          uiOutput("females_vaccinated_box_econ")
                                        ),
                                        
                                        #------------------------------
                                        # Graphs:
                                        #------------------------------
                                        # Format
                                        tags$head(
                                          tags$style(HTML("
                                          .three-cards {
                                            display: flex;
                                            gap: 20px;        /* space between cards */
                                          }
                                          .three-cards > div {
                                            flex: 1;          /* all cards equal width */
                                          }
                                          "))
                                        ),
                                        
                                        # The three cards
                                        div(
                                          class = "three-cards",
                                          
                                          #------------------------------
                                          # Graph4: Cost of vacc
                                          #------------------------------
                                          div(
                                            class = "graph-card",
                                            navset_card_pill( 
                                              nav_panel("Chart",
                                                        div(
                                                          div(
                                                            textOutput("plot4_title"),
                                                            style = "font-weight:bold; margin-bottom:0;"
                                                          ),
                                                          tags$div(
                                                            textOutput("plot4_subtitle"),
                                                            style = "margin-top:0; margin-bottom:20px;"
                                                          ),
                                                          plotOutput("gph_cost_vacc", height = "350px")
                                                        )
                                              ), 
                                              nav_panel("Table", 
                                                        card_body(
                                                          div(  
                                                            div(
                                                              textOutput("tab4_title"),
                                                              style = "font-weight:bold; margin-bottom:0;"
                                                            ),
                                                            tags$div(
                                                              textOutput("tab4_subtitle"),
                                                              style = "margin-top:0; margin-bottom:20px;"
                                                            ),
                                                            dataTableOutput("tab_cost_vacc")
                                                        ))
                                              ), 
                                              nav_panel("Supporting information", 
                                                        h5("What does the chart show?"),
                                                        p("The chart shows the estimated cost of implementing each vaccination strategy, including both vaccine procurement and delivery costs (including operational and administrative costs). 
                                                        These estimates are based on model outputs from PRIME."),
                                                        h5("How do I interpret the chart?"),
                                                        p("Higher values indicate greater program expenditures. 
                                                         Comparing strategies highlights how the total vaccination cost changes depending on the number of doses and scale of delivery.")
                                              ),
                                              # Footer for the navset card
                                              card_footer(
                                                downloadLink(
                                                  outputId = "download_plot4",
                                                  label = tagList(bs_icon("bar-chart-fill"), "Save chart (PNG)"),
                                                  class = "btn-link",
                                                  style = "background:none; border:none; color:#2FA4E7; font-size:0.9rem; padding:0;"
                                                ),
                                                downloadLink(
                                                  outputId = "download_table4",
                                                  label = tagList(bs_icon("file-spreadsheet-fill"), "Download data (Excel)"),
                                                  class = "btn-link",
                                                  style = "background:none; border:none; color:#2FA4E7; font-size:0.9rem; padding:0; margin-left:15px;"
                                                )
                                              )
                                            )),
                                          
                                          #------------------------------
                                          # Graph5: Tx costs saved
                                          #------------------------------
                                          div(
                                            class = "graph-card",
                                            navset_card_pill( 
                                              nav_panel("Chart",
                                                        div(
                                                          div(
                                                            textOutput("plot5_title"),
                                                            style = "font-weight:bold; margin-bottom:0;"
                                                          ),
                                                          tags$div(
                                                            textOutput("plot5_subtitle"),
                                                            style = "margin-top:0; margin-bottom:20px;"
                                                          ),
                                                          plotOutput("gph_cost_tx", height = "300px")
                                                        )
                                              ), 
                                              nav_panel("Table", 
                                                        card_body(
                                                          div(  
                                                            div(
                                                              textOutput("tab5_title"),
                                                              style = "font-weight:bold; margin-bottom:0;"
                                                            ),
                                                            tags$div(
                                                              textOutput("tab5_subtitle"),
                                                              style = "margin-top:0; margin-bottom:20px;"
                                                            ),
                                                          dataTableOutput("tab_cost_tx")
                                                        ))
                                              ), 
                                              nav_panel("Supporting information", 
                                                        h5("What does the chart show?"),
                                                        p("The chart shows the healthcare costs that could be avoided through vaccination, due to fewer cases of cervical cancer requiring diagnosis, treatment, or palliative care. 
                                                        These estimates are based on PRIME model projections and are presented as discounted costs, expressed in present value in the current year."),
                                                        h5("How do I interpret the chart?"),
                                                        p("Higher values mean larger savings for the health system. 
                                                        Comparing strategies illustrates which vaccination approaches lead to the greatest reduction in future treatment costs.")
                                              ),
                                              # Footer for the navset card
                                              card_footer(
                                                downloadLink(
                                                  outputId = "download_plot5",
                                                  label = tagList(bs_icon("bar-chart-fill"), "Save chart (PNG)"),
                                                  class = "btn-link",
                                                  style = "background:none; border:none; color:#2FA4E7; font-size:0.9rem; padding:0;"
                                                ),
                                                downloadLink(
                                                  outputId = "download_table5",
                                                  label = tagList(bs_icon("file-spreadsheet-fill"), "Download data (Excel)"),
                                                  class = "btn-link",
                                                  style = "background:none; border:none; color:#2FA4E7; font-size:0.9rem; padding:0; margin-left:15px;"
                                                )
                                              )
                                            )),
                                          
                                          #------------------------------
                                          # Graph6: Incremental cost
                                          #------------------------------
                                          div(
                                            class = "graph-card",
                                            navset_card_pill( 
                                              nav_panel("Chart",
                                                        div(
                                                          div(
                                                            textOutput("plot6_title"),
                                                            style = "font-weight:bold; margin-bottom:0;"
                                                          ),
                                                          tags$div(
                                                            textOutput("plot6_subtitle"),
                                                            style = "margin-top:0; margin-bottom:20px;"
                                                          ),
                                                          plotOutput("gph_cost_incre", height = "300px")
                                                        )
                                              ), 
                                              nav_panel("Table", 
                                                        card_body(
                                                          div(  
                                                            div(
                                                              textOutput("tab6_title"),
                                                              style = "font-weight:bold; margin-bottom:0;"
                                                            ),
                                                            tags$div(
                                                              textOutput("tab6_subtitle"),
                                                              style = "margin-top:0; margin-bottom:20px;"
                                                            ),
                                                          dataTableOutput("tab_cost_incre")
                                                        ))
                                              ), 
                                              nav_panel("Supporting information", 
                                                        h5("What does the chart show?"),
                                                        p("The chart shows the net additional cost of vaccination after accounting for savings from reduced cervical cancer treatment. 
                                                        It represents the balance between program spending and treatment costs avoided.These estimates are derived from PRIME model outputs and are expressed in present value terms, discounted to the current year."),
                                                        h5("How do I interpret the chart?"),
                                                        p("Values above zero indicate that vaccination costs exceed the savings from treatment avoided, while values closer to zero (or negative, if applicable) suggest a more cost-neutral or cost-saving strategy. 
                                                        Comparing strategies helps identify which option provides the most favorable balance of costs and savings.")
                                              ),
                                              # Footer for the navset card
                                              card_footer(
                                                downloadLink(
                                                  outputId = "download_plot6",
                                                  label = tagList(bs_icon("bar-chart-fill"), "Save chart (PNG)"),
                                                  class = "btn-link",
                                                  style = "background:none; border:none; color:#2FA4E7; font-size:0.9rem; padding:0;"
                                                ),
                                                downloadLink(
                                                  outputId = "download_table6",
                                                  label = tagList(bs_icon("file-spreadsheet-fill"), "Download data (Excel)"),
                                                  class = "btn-link",
                                                  style = "background:none; border:none; color:#2FA4E7; font-size:0.9rem; padding:0; margin-left:15px;"
                                                )
                                              )
                                            ))
                                          
                                        ),
                                      

                            ),
                            
                            
                            #------------------------------------
                            # Cost-effectiveness frontier
                            #------------------------------------
                            nav_panel("Cost-effectiveness",
                                      value = "ce_frontier_tab",
                            
                                        #------------------------------
                                        # Side bar
                                        #------------------------------
                                        # Add CSS to set sidebar background to white
                                        tags$head(
                                          tags$style(HTML("
                                          .sidebar {
                                            background-color: white !important;
                                          }
                                          "))
                                        ),
                                        
                                        layout_sidebar(
                                          sidebar = sidebar(
                                            width = "350px",
                                            
                                            # Dropdown
                                            selectInput(
                                              "eff_indicator", strong("Select effectiveness indicator"),
                                              choices = c(
                                                "Cervical cancer cases prevented",
                                                "Cervical cancer deaths prevented",
                                                "Cervical cancer DALYs prevented"
                                              ),
                                              width = "300px"
                                            ),
                                            
                                            # Checkboxes
                                            checkboxGroupInput(
                                              inputId = "summary_ce_thresholds",
                                              label = strong("Select cost-effectiveness threshold:"),
                                              choices = c(
                                                "0.25x GDP per capita", 
                                                "0.5x GDP per capita", 
                                                "1x GDP per capita", 
                                                "User's threshold"
                                              ),
                                              selected = c("0.5x GDP per capita")  # default
                                            ),
                                            
                                            # Link to information about health impact
                                            br(),
                                            actionLink(
                                              inputId = "show_ce_frontier_modal",
                                              label = tagList(
                                                icon("info-circle", style = "color: #1482d7;"),
                                                span("About cost-effectiveness")
                                              ),
                                              style = "color: #1482d7; text-decoration: underline; font-size: 0.95em; margin-left: 5px;"
                                            )
                                          ),
                                        
                                        #------------------------------
                                        # CE frontier graph
                                        #------------------------------
                                        
                                        # Title + Subtitle
                                        div(
                                          style = "margin-bottom:20px; line-height:1.2;",  # control vertical spacing
                                          div("Costs and benefits of HPV vaccination strategies",
                                              style = "font-weight:bold; margin:5; margin-top:20px;"),
                                          div("Y-axis: Net cost (Million USD, discounted) versus X-axis: Effectiveness (cases, deaths, or DALYs prevented)",
                                              style = "margin-top:5px; margin-bottom:0;"),
                                          tags$div(
                                            textOutput("ce_baseline"),
                                            style = "margin-top:5px; margin-bottom:0;")
                                        ),
                                          
                                          ## Graph 
                                           div(
                                              plotOutput("ce_graph", height = "500px", width = "100%"), 
                                              style = "flex:2;"
                                            ),
                                            
                                          
                                      )
                            ) # nav panel

                          )
                )
              )
    ),
    
    
    #--------------------------------------
    # Additional information page ----.
    #--------------------------------------
    
      # Right-aligned tab including additional information of the app.
      nav_spacer(),
      nav_panel(
      title = tagList("More information"),
      
      # References
      h3("References"),
      p("Abbas KM, van Zandvoort K, Brisson M, Jit M. Effects of updated demography, disability weights, and cervical cancer burden on estimates of human papillomavirus vaccination impact at the global, regional, and national levels: a PRIME modelling study. Lancet Glob Health. 2020 Apr;8(4):e536–44"),
      p("World Bank Group. World Bank Open Data. 2024 [cited 2025 Sept 23]. World Bank Open Data. Available from: https://data.worldbank.org"),
      p("Jit M, Brisson M, Portnoy A, Hutubessy R. Cost-effectiveness of female human papillomavirus vaccination in 179 countries: a PRIME modelling study. Lancet Glob Health. 2014 July;2(7):e406-414"),
      p("James SL, Abate D, Abate KH, Abay SM, Abbafati C, Abbasi N, et al. Global, regional, and national incidence, prevalence, and years lived with disability for 354 diseases and injuries for 195 countries and territories, 1990–2017: a systematic analysis for the Global Burden of Disease Study 2017. The Lancet [Internet]. 2018 [cited 2025 Sept 23];392(10159):1789–858. Available from: https://linkinghub.elsevier.com/retrieve/pii/S0140673618322797"),
      p("Salomon JA, Haagsma JA, Davis A, De Noordhout CM, Polinder S, Havelaar AH, et al. Disability weights for the Global Burden of Disease 2013 study. The Lancet Global Health [Internet]. 2015 [cited 2025 Sept 23];3(11):e712–23. Available from: https://linkinghub.elsevier.com/retrieve/pii/S2214109X15000698"),
      p("Yeung K, Boyle D, Cook A, Jit M, Bloem P, Hutubessy R, et al. Extrapolating recurrent costs to deliver human papillomavirus vaccine in low-income and lower-middle-income countries. 2025"),
      p("UNICEF. HPV vaccine prices [Internet]. 2025. (UNICEF Supply Division). Available from: https://www.unicef.org/supply/media/23891/file/HPV-vaccine-prices-26062025.pdf"),
      p("Serrano B, Alemany L, Tous S, Bruni L, Clifford GM, Weiss T, et al. Potential impact of a nine-valent vaccine in human papillomavirus related cervical disease. Infectious Agents and Cancer [Internet]. 2012 Dec 29 [cited 2025 Sept 23];7(1):38. Available from: https://doi.org/10.1186/1750-9378-7-38"),
      p("WHO. Human papillomavirus vaccines: WHO position paper, December 2022 [Internet]. 2022 [cited 2025 Sept 23]. Available from: https://www.who.int/publications/i/item/who-wer9750-645-672"),
      p("SAGE. Strategic Advisory Group of Experts (SAGE) Working Group on potential contribution of HPV vaccines and immunization towards  cervical cancer elimination [Internet]. 2022 Mar. Available from: https://cdn.who.int/media/docs/default-source/immunization/position_paper_documents/human-papillomavirus-(hpv)/hpv-background-document--report-march-2022.pdf"),
      br(),
      
      # Acknowledgements
      h3("Acknowledgements"),
      p("PRIME development and subsequent updates were supported by WHO, Gavi, the Vaccine Alliance, and the Gates Foundation. The PRIME model was developed by Mark Jit, Marc Brisson, Allison Portnoy and Raymond Hutubessy, and later updated by Kaja Abbas."),
      p("This app was developed by ",
        a("Aquarius Population Health",
          href = "https://aquariusph.com/",
          target = "_blank"
        ),"with support and guidance from Karene Yeung and Paul Bloem."
      ),
      
      br(),
      
      # Relevant links
      h3("Links to related materials"),
      p(a("Cervical cancer",
          href = "https://www.who.int/health-topics/cervical-cancer",
          target = "_blank"
        )),
      p(a("Human papillomavirus vaccines: WHO position paper, December 2022",
          href = "https://www.who.int/publications/i/item/who-wer9750-645-672",
          target = "_blank"
      )),
      p(a("HPV Vaccine Introduction Clearing House",
          href = "https://www.who.int/teams/immunization-vaccines-and-biologicals/diseases/human-papillomavirus-vaccines-(HPV)/hpv-clearing-house",
          target = "_blank"
      )),
      p(a("Cost-effectiveness of female human papillomavirus vaccination in 179 countries: a PRIME modelling study",
          href = "https://doi.org/10.1016/S2214-109X(14)70237-2",
          target = "_blank"
      )),
      p(a("Effects of updated demography, disability weights, and cervical cancer burden on estimates of human papillomavirus vaccination impact at the global, regional, and national levels: a PRIME modelling study",
          href = "https://doi.org/10.1016/S2214-109X(20)30022-X",
          target = "_blank"
      )),
      br(),
      
      # WHO contact for this tool
      h3("Contact us"),
      p("Further information and advice on using PRIME can be obtained by contacting ",
        a("vaccines@who.int",
          href = "mailto:vaccines@who.int",
          target = "_blank"
        )),
      br(),
      
      # Disclaimer
      h3("Disclaimer"),
      p("WHO makes no guarantees that the information provided by this tool is error-free or suitable for specific uses. Results generated by the tool depend entirely on the assumptions and data entered by users and do not represent official WHO findings. Users are solely responsible for the outcomes they produce, and WHO advises seeking independent expert advice before acting on any information from the tool. WHO is not liable for any losses or damages resulting from errors, omissions, or misinterpretations in the tool’s content."),
      p("Details of the methodology and assumptions of the PRIME model have been extensively described in ",
        a("Jit et al., 2014",
          href = "https://doi.org/10.1016/S2214-109X(14)70237-2",
          target = "_blank"
        ),"and",
        a("Abbas et al., 2020.",
          href = "https://doi.org/10.1016/S2214-109X(20)30022-X",
          target = "_blank"
        ))
    ),
    
    #--------------------------------------
    # Logo at the top main panel ----.
    #--------------------------------------    
    # WHO logo 
      nav_item(
      tags$img(src = "assets/WHO-EN-C-H.png", style = "height:60px; margin-right:10px; vertical-align:middle;")
      )
    )
  )
  







#====================
# SERVER
#====================

  server <- function(input, output, session) {
  
    #--------------------------------------
    # Homepage ----.
    #--------------------------------------
    # When Go to the Tool is clicked -> navigate to Parameters tab
    observeEvent(input$goto_tool_btn, {
      updateTabsetPanel(session, inputId = "nav", selected = "PRIME")
      updateTabsetPanel(session, inputId = "main_tabs", selected = "parameters_tab")
    })
    
    #-----------------------------
    # Parameters page ----.
    #-----------------------------    
    # Pull the row when a country is chosen
    defaults_row <- reactive({
      req(input$country)   # stops here if country == ""
      row <- data.tool[data.tool$Country == input$country, , drop = FALSE]
      validate(need(nrow(row) >= 1, "No data for that country"))
      row[1, ]
    })
    
        # Function for formatting the display-only default boxes
        fmt_val <- function(x, digits = NULL, percent = FALSE, na_txt = "\u2014") {
          if (is.null(x) || is.na(x)) return(tags$em(na_txt))
          y <- x
          if (!is.null(digits)) y <- round(y, digits)
          txt <- format(y, big.mark = ",", scientific = FALSE, trim = TRUE,
                        nsmall = if (is.null(digits)) 0 else digits)
          if (percent) paste0(txt, "%") else txt
        }
        
        # Function that produces the right-hand default box
        render_default_box <- function(value, digits = NULL, percent = FALSE) {
          r <- defaults_row()
          div(
            style = "width: 100px; margin: 1px; padding: 1px;",
            fmt_val(r[[value]], digits = digits, percent = percent)
          )
        }
        
        # Function to match fmt_val() numeric rounding for numericInput
        fmt_val_num <- function(x, digits = NULL) {
          if (is.null(x) || is.na(x)) return(NA_real_)
          if (!is.null(digits)) round(x, digits) else x
        }
        
        # Function to update numeric inputs
        update_all_inputs <- function(row) {
          updateNumericInput(session, "hpv1618_prop", value = fmt_val_num(row$hpv1618_prop))
          updateNumericInput(session, "cov",          value = fmt_val_num(row$cov, digits = NULL))
          updateNumericInput(session, "agevac",       value = fmt_val_num(row$agevac))
          updateNumericInput(session, "cohort",       value = fmt_val_num(row$cohort))
          
          updateNumericInput(session, "vac_price",    value = fmt_val_num(row$vac_price, digits = 2))
          updateNumericInput(session, "vac_delivery", value = fmt_val_num(row$vac_delivery, digits = 2))
          updateNumericInput(session, "vac_delivery2", value = fmt_val_num(row$vac_delivery2, digits = 2))
          updateNumericInput(session, "eff_one",      value = fmt_val_num(row$eff_one))
          updateNumericInput(session, "eff_two",      value = fmt_val_num(row$eff_two))
          
          updateNumericInput(session, "cecx_cost",    value = fmt_val_num(row$cecx_cost, digits = 2))
          updateNumericInput(session, "dw_dx",        value = fmt_val_num(row$dw_dx, digits = 2))
          updateNumericInput(session, "dw_seq",       value = fmt_val_num(row$dw_seq, digits = 2))
          updateNumericInput(session, "dw_term",      value = fmt_val_num(row$dw_term, digits = 2))
          
          updateNumericInput(session, "disc.cost",    value = fmt_val_num(row$disc.cost, digits = 1))
          updateNumericInput(session, "disc.ben",     value = fmt_val_num(row$disc.ben, digits = 1))
          updateNumericInput(session, "ce_thr", value = NA)
        }
        
    # Update when country changes
    observeEvent(input$country, {
      update_all_inputs(defaults_row())
    })
        
    # Update when reset button clicked
    observeEvent(input$run_reset, {
      update_all_inputs(defaults_row())
    })    
        
    # Display defaults for each field
    
    ## I. Population and targeting
    output$ui_hpv1618_prop <- renderUI(render_default_box("hpv1618_prop", percent = TRUE))
    output$ui_cov          <- renderUI(render_default_box("cov", percent = TRUE))
    output$ui_agevac       <- renderUI(render_default_box("agevac"))
    output$ui_cohort       <- renderUI(render_default_box("cohort"))
    
    ## II. Vaccination costs and effectiveness
    output$ui_vac_price    <- renderUI(render_default_box("vac_price", digits = 2))
    output$ui_vac_delivery <- renderUI(render_default_box("vac_delivery", digits = 2))
    output$ui_vac_delivery2 <- renderUI(render_default_box("vac_delivery2", digits = 2))
    output$ui_eff_one <- renderUI(render_default_box("eff_one", percent = TRUE))
    output$ui_eff_two <- renderUI(render_default_box("eff_two", percent = TRUE))
    
    ## III. Health impact & treatment costs
    output$ui_cecx_cost <- renderUI(render_default_box("cecx_cost", digits = 2))
    # Disability weights are 0–1; show to 2 decimals
    output$ui_dw_dx    <- renderUI(render_default_box("dw_dx",   digits = 2))
    output$ui_dw_seq   <- renderUI(render_default_box("dw_seq",  digits = 2))
    output$ui_dw_term  <- renderUI(render_default_box("dw_term", digits = 2))
    
    ## IV. Economic evaluation criteria
    output$ui_disc.cost <- renderUI(render_default_box("disc.cost", percent = TRUE))
    output$ui_disc.ben  <- renderUI(render_default_box("disc.ben",  percent = TRUE))

    
    
    #-----------------------------
    # Run PRIME button ----.
    #-----------------------------
    # Reactive value to store results
    results <- reactiveVal(NULL)
    confirmed_country <- reactiveVal(NULL)
    cancel_run <- reactiveVal(FALSE)
    
    # Countries with no WB GDP/capita available
    countries_missing_gdp <- c("CUB","ERI","PRK","SSD","VEN","YEM")
    
    # ---- Function to add new indicators ----.
    add_indicators <- function(df) {
      # Extract needed values
      life_years     <- df[df$variable == "Life years saved", ]
      nonfatal_dalys <- df[df$variable == "Nonfatal DALYs prevented", ]
      net_cost       <- df[df$variable == "Net cost", ]
      cases_prev     <- df[df$variable == "CeCx prevented", ]
      
      # Calculate new indicators
      total_dalys <- data.frame(
        variable     = "Total DALYs prevented",
        undiscounted = life_years$undiscounted + nonfatal_dalys$undiscounted,
        discounted   = life_years$discounted   + nonfatal_dalys$discounted
      )
      
      cost_case <- data.frame(
        variable     = "Cost/case prevented",
        undiscounted = ifelse(cases_prev$undiscounted > 0,
                              net_cost$undiscounted / cases_prev$undiscounted,
                              NA),
        discounted   = ifelse(cases_prev$discounted > 0,
                              net_cost$discounted / cases_prev$discounted,
                              NA)
      )
      
      # Append new rows
      df <- rbind(df, total_dalys, cost_case)
      return(df)
    }
    
    # Helper that performs the run (keeps UI flow intact)
    do_run_prime <- function(iso_code) {
      
      # --- Step 1: show a modal with initial message ---
      showModal(modalDialog(
        title = "Running PRIME model",
        div("Please wait while the model runs..."),
        br(),
        tags$div(class = "loader"),
        tags$style("
      .loader {
        border: 8px solid #f3f3f3;
        border-top: 8px solid #1C87C9;
        border-radius: 50%;
        width: 60px;
        height: 60px;
        animation: spin 1s linear infinite;
        margin: auto;
      }
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    "),
        footer = NULL,
        easyClose = FALSE
      ))
    
      # --- Step 2: run the model ---
      withProgress(message = "Running PRIME model...", value = 0, {
        # One-dose scenario
        incProgress(0.5, detail = "Running one-dose scenario...")
        res_one <- new_RunCountry(
          iso_code,
          analyseCosts = TRUE,
          discounting  = TRUE,
          cov          = input$cov/100,
          agevac       = input$agevac,
          cohort       = input$cohort,
          vaceff_beforesexdebut = input$eff_one/100,
          vac_price    = input$vac_price,
          vac_delivery = input$vac_delivery,
          hpv1618_prop = input$hpv1618_prop,
          cecx_cost    = input$cecx_cost,
          ce_threshold = ifelse(is.na(input$ce_thr), -1, input$ce_thr),
          ce_threshold_gdp_mult = -1,
          dw_dx        = input$dw_dx,
          dw_seq       = input$dw_seq,
          dw_term      = input$dw_term,
          disc.cost    = input$disc.cost/100,
          disc.ben     = input$disc.ben/100,
          one_dose     = 1
        )
        res_one <- add_indicators(res_one)
        
        # Two-dose scenario
        incProgress(1, detail = "Running two-dose scenario...")
        res_two <- new_RunCountry(
          iso_code,
          analyseCosts = TRUE,
          discounting  = TRUE,
          cov          = input$cov/100,
          agevac       = input$agevac,
          cohort       = input$cohort,
          vaceff_beforesexdebut = input$eff_two/100,
          vac_price    = input$vac_price,
          vac_delivery = input$vac_delivery2,
          hpv1618_prop = input$hpv1618_prop,
          cecx_cost    = input$cecx_cost,
          ce_threshold = ifelse(is.na(input$ce_thr), -1, input$ce_thr),
          ce_threshold_gdp_mult = -1,
          dw_dx        = input$dw_dx,
          dw_seq       = input$dw_seq,
          dw_term      = input$dw_term,
          disc.cost    = input$disc.cost/100,
          disc.ben     = input$disc.ben/100,
          one_dose     = 0
        )
        res_two <- add_indicators(res_two)
        
        # Save results to reactiveVal
        results(list(one = res_one, two = res_two))
      })
      
      # --- Step 2.5: freeze the country name for the title *after* success ---
      confirmed_country(isolate(input$country))
      
      # Step 3: replace modal with success + navigation button
      removeModal()
      showModal(modalDialog(
        title = "Model run completed!",
        div("The PRIME model has finished running."),
        br(),
        actionButton("go_results", "Go to results page", class = "btn-primary"),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    # Run model when button is clicked
    observeEvent(input$run_prime, {
      
      # --- Step 0: check for country selection ---
      if (is.null(input$country) || input$country == "") {
        showModal(modalDialog(
          title = "Unable to run PRIME",
          div("Please select a country before running PRIME."),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return()
      }
      
      # --- Step 0: check for invalid input ---
      if (!is.null(input$eff_one) && !is.null(input$eff_two) && input$eff_one > input$eff_two) {
        showModal(modalDialog(
          title = "Invalid parameter setting",
          div("One-dose effectiveness cannot be greater than two-dose effectiveness."),
          br(),
          div("Please adjust the values before running PRIME."),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return()  # stop here, do not run the model
      }
      
      # Get iso3 code from data.tool based on user selection
      iso_code <- data.tool$iso3[data.tool$Country == input$country]
      
      # NEW: warn if country lacks WB GDP & CE threshold not provided
      ce_thr_missing <- is.null(input$ce_thr) || is.na(input$ce_thr) || identical(input$ce_thr, "")
      needs_confirm  <- iso_code %in% countries_missing_gdp && ce_thr_missing
      
      if (needs_confirm) {
        showModal(modalDialog(
          title = "No GDP-based threshold available",
          tagList(
            p("For the selected country, there is no GDP per capita in the World Bank dataset to use as a cost-effectiveness (CE) threshold."),
            p("We suggest entering a CE threshold in the parameters table."),
            br(),
            strong("Do you want to continue without a CE threshold?")
          ),
          easyClose = FALSE,
          footer = tagList(
            actionButton("confirm_run_without_thr", "Yes", class = "btn-primary"),
            modalButton("No, go back")  # closes the modal and lets the user edit parameters
          )
        ))
        return()  # wait for user's choice
      }
      
      # If no confirmation needed, just run
      do_run_prime(iso_code)
    })
    
    # Handler for the confirmation modal (run anyway)
    observeEvent(input$confirm_run_without_thr, {
      removeModal()
      iso_code <- data.tool$iso3[data.tool$Country == isolate(input$country)]
      do_run_prime(iso_code)
    })
    
    # Go to results page
    observeEvent(input$go_results, {
      updateTabsetPanel(session, "main_tabs", selected = "results_page")
      updateTabsetPanel(session, "results_tabs", selected = "health_tab")
      removeModal()
    })

    #-----------------------------
    # Results page ----.
    #-----------------------------    
    
    # Country title
    output$selected_country <- renderText({
      cc <- confirmed_country()
      if (is.null(cc) || is.na(cc) || identical(cc, "")) {
        "[Please select country and run PRIME in the previous page to see results]"
      } else {
        cc
      }
    })
    
    #Selected comparison text
    output$chosen_comparison <- renderText({
      req(input$comparison)
      input$comparison
    })
    
        #-----------------------
        # Download PDF report
        #-----------------------
        output$downloadPDF <- downloadHandler(
          filename = function() {
            paste0("HPV_vaccination_impact_report_", input$country, "_", Sys.Date(), ".pdf")
          },
          content = function(file) {
            # Copy the report template to a temporary directory
            tempReport <- file.path(tempdir(), "PRIME_report.Rmd")
            file.copy("PRIME_report.Rmd", tempReport, overwrite = TRUE)
            
            # Logo path
            logo_file <- normalizePath("www/WHO-EN-C-H.png", winslash = "/")
            
            # Extract values from results()
            res <- results()
            
            # Vaccines delivered
            vaccines_one <- if (!is.null(res$one)) {
              round(res$one[res$one$variable == "Vac cohort size", "undiscounted"])
            } else NA
            
            vaccines_two <- if (!is.null(res$two)) {
              round(res$two[res$two$variable == "Vac cohort size", "undiscounted"] * 2)
            } else NA
            
            # Females vaccinated
            females_one <- if (!is.null(res$one)) {
              round(res$one[res$one$variable == "Vac cohort size", "undiscounted"])
            } else NA
            
            females_two <- if (!is.null(res$two)) {
              round(res$two[res$two$variable == "Vac cohort size", "undiscounted"])
            } else NA
            
            # Health impact data
            df_cases  <- make_df(results(), "CeCx prevented")
            df_deaths <- make_df(results(), "Deaths prevented")
            df_dalys  <- make_df(results(), "Total DALYs prevented")
            
            # Economic impact data
            df_vacc_cost <- make_df(results(), "Vaccine cost")
            df_costs_saved <- make_df(results(), "Costs saved")
            df_net_cost <- make_df(results(), "Net cost")
            
            # CE data
            df_cost_case   <- make_df(results(), "Cost/case prevented")
            df_cost_death  <- make_df(results(), "Cost/death prevented")
            df_cost_dalys  <- make_df(results(), "Cost/DALY prevented")
            
            # GDP per capita (from results)
            gdp_cap <- results()$one %>%
              dplyr::filter(variable == "GDP/capita") %>%
              dplyr::pull(discounted) %>%
              as.numeric()
            
            # Selected thresholds (from user input, or hard-code if needed)
            selected_thresholds <- input$summary_ce_thresholds   # e.g. c("0.25x GDP per capita", "0.5x GDP per capita", "1x GDP per capita")
            
            # User-defined threshold
            user_thr <- as.numeric(input$ce_thr)
            
            # Parameters table
            params_table <- data.frame(
              Section = c(
                "I. Population and targets",
                "I. Population and targets",
                "I. Population and targets",
                "I. Population and targets",
                "II. Vaccination costs and efficacy",
                "II. Vaccination costs and efficacy",
                "II. Vaccination costs and efficacy",
                "II. Vaccination costs and efficacy",
                "III. Treatment cost and disability weight",
                "III. Treatment cost and disability weight",
                "III. Treatment cost and disability weight",
                "III. Treatment cost and disability weight",
                "IV. Economic evaluation settings",
                "IV. Economic evaluation settings",
                "IV. Economic evaluation settings"
              ),
              Parameter = c(
                "Proportion of cervical cancer cases due to HPV 16/18 (%)",
                "Vaccination coverage (%)",
                "Target age group (age)",
                "Cohort size at vaccination age (females)",
                "Vaccine price per dose (USD)",
                "Vaccine delivery cost per dose (USD)",
                "Efficacy, one-dose scenario (%)",
                "Efficacy, two-dose scenario (%)",
                "Cervical cancer treatment cost per episode (USD)",
                "Disability weight - diagnosis",
                "Disability weight - non-terminal sequelae",
                "Disability weight - terminal cancer",
                "Discount rate for health costs (%)",
                "Discount rate for health outcomes (%)",
                "Cost-effectiveness threshold (USD)"
              ),
              Value = c(
                paste0(input$hpv1618_prop, "%"),
                paste0(input$cov, "%"),
                input$agevac,
                format(input$cohort, big.mark = ","),
                input$vac_price,
                input$vac_delivery,
                paste0(input$eff_one, "%"),
                paste0(input$eff_two, "%"),
                format(input$cecx_cost, big.mark = ","),
                input$dw_dx,
                input$dw_seq,
                input$dw_term,
                paste0(input$disc.cost, "%"),
                paste0(input$disc.ben, "%"),
                ifelse(is.na(input$ce_thr), "N/A", input$ce_thr)
              )
            )
            
            
            # Render the report with parameters
            rmarkdown::render(
              input = tempReport,
              output_file = file,
              params = list(
                country      = input$country,
                comparison   = input$comparison,
                logo         = logo_file,
                vaccines_one = vaccines_one,
                vaccines_two = vaccines_two,
                females_one  = females_one,
                females_two  = females_two,
                df_cases     = df_cases,
                df_deaths    = df_deaths,
                df_dalys     = df_dalys,
                df_vacc_cost = df_vacc_cost,
                df_costs_saved = df_costs_saved,
                df_net_cost  = df_net_cost,
                df_cost_case   = df_cost_case,
                df_cost_death  = df_cost_death,
                df_cost_dalys  = df_cost_dalys,
                gdp_cap        = gdp_cap,
                ce_thresholds  = selected_thresholds,
                ce_user_thr    = user_thr,
                params_table = params_table,
                eff_indicator = input$eff_indicator
              ),
              envir = new.env(parent = globalenv())
            )
          }
        )
        
        #-----------------------
        # Download data
        #-----------------------
        output$downloadData <- downloadHandler(
          filename = function() {
            paste0("prime_results_", input$country, "_", Sys.Date(), ".xlsx")
          },
          content = function(file) {
            res <- results()
            if (is.null(res)) {
              stop("No results available. Please run the model first.")
            }
            
            # Convert to data.frames with Scenario column
            res_one <- cbind(Scenario = "One-dose", as.data.frame(res$one))
            res_two <- cbind(Scenario = "Two-dose", as.data.frame(res$two))
            
            # Variables to remove
            drop_vars <- c("CE at 1xGDP/capita?", "CE at 3xGDP/capita?", "Cut-off price")
            
            # Filter them out
            res_one <- res_one[!res_one$variable %in% drop_vars, ]
            res_two <- res_two[!res_two$variable %in% drop_vars, ]
            
            # Desired variable order
            var_order <- c(
              "Cohort size",
              "Vac cohort size",
              "Vaccine procurement cost",
              "Vaccine delivery cost",
              "Vaccine cost",
              "Costs saved",
              "Net cost",
              "CeCx prevented",
              "Deaths prevented",
              "Life years saved",
              "Nonfatal DALYs prevented",
              "Total DALYs prevented",
              "Cost/case prevented",
              "Cost/death prevented",
              "Cost/life year saved",
              "Cost/DALY prevented",
              "GDP/capita"
            )
            
            # Apply order separately for each scenario
            res_one$variable <- factor(res_one$variable, levels = var_order)
            res_one <- res_one[order(res_one$variable), ]
            
            res_two$variable <- factor(res_two$variable, levels = var_order)
            res_two <- res_two[order(res_two$variable), ]
            
            # Bind in sequence: One-dose first, then Two-dose
            res_all <- rbind(res_one, res_two)
            
            # Write to Excel
            writexl::write_xlsx(res_all, path = file)
          }
        )
        
        #--------------------------------------
        # Results tabs
        #--------------------------------------
                        #-------------------------
                        # Define limits for plots
                        #-------------------------
                        
                        ## General plot1-2
                        shared_general_max <- reactive({
                          req(results())
                          res <- results()
                          
                          # --- Values for general1 ---
                          val1_one <- if (!is.null(res$one)) round(res$one[variable == "Vac cohort size", undiscounted]) else NA
                          val1_two <- if (!is.null(res$two)) round(res$two[variable == "Vac cohort size", undiscounted] * 2) else NA
                          vals1 <- c(val1_one, val1_two) / 1e3   # in thousands
                          
                          # --- Values for general2 ---
                          val2_one <- if (!is.null(res$one)) round(res$one[variable == "Vac cohort size", undiscounted]) else NA
                          val2_two <- if (!is.null(res$two)) round(res$two[variable == "Vac cohort size", undiscounted]) else NA
                          vals2 <- c(val2_one, val2_two) / 1e3   # in thousands
                          
                          # --- Shared max ---
                          max_val <- max(c(vals1, vals2), na.rm = TRUE)
                          max_val * 1.3
                        })
                    
                        ## Plot1-3
                        shared_health_limits <- reactive({
                          req(results())
                          dfs <- list(
                            make_df(results(), "CeCx prevented"),
                            make_df(results(), "Deaths prevented"),
                            make_df(results(), "Total DALYs prevented")
                          )
                          
                          all_vals <- unlist(lapply(dfs, function(df) df$Value))
                          max_val <- max(all_vals, na.rm = TRUE)
                          
                          c(0, max_val * 1.3)   # always start at 0, scale top
                        })
                        
                        ## Plot4-6
                        shared_econ_limits <- reactive({
                          req(results())
                          
                          dfs <- list(
                            make_df(results(), "Vaccine cost"),
                            make_df(results(), "Costs saved"),
                            make_df(results(), "Net cost")
                          )
                          
                          # Collect values (convert to millions)
                          all_vals <- unlist(lapply(dfs, function(df) df$Value / 1e6))
                          all_vals <- all_vals[!is.na(all_vals)]
                          
                          if (length(all_vals) == 0) return(c(0, 1))  # fallback
                          
                          min_val <- min(all_vals, na.rm = TRUE)
                          max_val <- max(all_vals, na.rm = TRUE)
                          
                          # --- Case 1: all positive ---
                          if (min_val >= 0) {
                            return(c(0, max_val * 1.3))
                          }
                          # --- Case 2: all negative ---
                          else if (max_val <= 0) {
                            return(c(min_val * 1.3, 0))
                          }
                          # --- Case 3: mix of + and – ---
                          else {
                            return(c(min_val * 1.3, max_val * 1.3))
                          }
                        })
                        
                        ## Plot7-9
                        shared_costeffect_limits <- reactive({
                          req(results())
                          
                          dfs <- list(
                            make_df(results(), "Cost/case prevented"),
                            make_df(results(), "Cost/death prevented"),
                            make_df(results(), "Cost/DALY prevented")
                          )
                          
                          # Collect values from all datasets
                          all_vals <- unlist(lapply(dfs, function(df) df$Value))
                          all_vals <- all_vals[!is.na(all_vals)]
                          
                          # --- Add thresholds ---
                          gdp_cap <- results()$one %>%
                            dplyr::filter(variable == "GDP/capita") %>%
                            dplyr::pull(discounted) %>%
                            as.numeric()
                          
                          user_thr <- as.numeric(input$ce_thr)
                          
                          thr_vals <- c(
                            0.25 * gdp_cap,
                            0.5  * gdp_cap,
                            1.0  * gdp_cap,
                            if (!is.na(user_thr) && user_thr > 0) user_thr else NA
                          )
                          
                          thr_vals <- thr_vals[!is.na(thr_vals)]
                          
                          # Combine data values + thresholds
                          all_vals <- c(all_vals, thr_vals)
                          
                          if (length(all_vals) == 0) return(c(0, 1))  # fallback
                          
                          min_val <- min(all_vals, na.rm = TRUE)
                          max_val <- max(all_vals, na.rm = TRUE)
                          
                          # --- Case 1: all positive ---
                          if (min_val >= 0) {
                            return(c(0, max_val * 1.3))
                          }
                          # --- Case 2: all negative ---
                          else if (max_val <= 0) {
                            return(c(min_val * 1.3, 0))
                          }
                          # --- Case 3: mix of + and – ---
                          else {
                            return(c(min_val * 1.3, max_val * 1.3))
                          }
                        })
                        
                        
              #--------------------------------------
              # Health impact tab
              #--------------------------------------
                    #----------------
                    # Value boxes
                    #----------------
                    renderStrategies <- function(selected, data) {
                      out <- list()
                      for (strategy in selected) {
                        out <- append(out, list(
                          tags$div(
                            style = "display: flex; align-items: baseline",
                            tags$span(
                              style = "flex: 1; font-size: 0.5em; text-align: left; margin-left: 5px;", 
                              strategy
                            ),
                            tags$span(
                              style = "min-width: 40px; font-size: 0.7em; font-weight: bold; text-align: right; margin-right: 30px;", 
                              data[[strategy]]
                            )
                          )
                        ))
                      }
                      do.call(tagList, out)
                    }
    
                    output$vaccines_delivered_box <- renderUI({
                      res <- results()
                      
                      # Extract values
                      val_one <- if (!is.null(res$one)) round(res$one[variable == "Vac cohort size", undiscounted]) else NA
                      val_two <- if (!is.null(res$two)) round(res$two[variable == "Vac cohort size", undiscounted] * 2) else NA
                      
                      # Base data
                      data_list <- list(
                        "No vaccination"       = "-",
                        "One-dose schedule" = format(val_one, big.mark = ","),
                        "Two-dose schedule" = format(val_two, big.mark = ",")
                      )
                      
                      # Filter based on comparison
                      comparison <- input$comparison
                      if (comparison == "No vaccination vs. One-dose") {
                        data_list <- data_list[c("No vaccination", "One-dose vaccination")]
                      } else if (comparison == "No vaccination vs. Two-dose") {
                        data_list <- data_list[c("No vaccination", "Two-dose vaccination")]
                      } else if (comparison == "One-dose vs. Two-dose") {
                        # Keep both One-dose and Two-dose
                        data_list <- data_list[c("One-dose vaccination", "Two-dose vaccination")]
                      } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                        # Keep all (do nothing)
                      }
                      
                      value_box(
                        title = paste0(input$country, ": Number of vaccines delivered"),
                        showcase = bs_icon("prescription2"),
                        theme = NULL,
                        style = "background-color: #F5F5F5; color: black;",
                        renderStrategies(selected = names(data_list), data = data_list)
                      )
                    })
                    
                    
                    output$females_vaccinated_box <- renderUI({
                      res <- results()
                      
                      # Extract values
                      val_one <- if (!is.null(res$one)) round(res$one[variable == "Vac cohort size", undiscounted]) else NA
                      val_two <- if (!is.null(res$two)) round(res$two[variable == "Vac cohort size", undiscounted]) else NA
                      
                      # Base data
                      data_list <- list(
                        "No vaccination"       = "-",
                        "One-dose vaccination" = format(val_one, big.mark = ","),
                        "Two-dose vaccination" = format(val_two, big.mark = ",")
                      )
                      
                      # Filter based on comparison
                      comparison <- input$comparison
                      if (comparison == "No vaccination vs. One-dose") {
                        data_list <- data_list[c("No vaccination", "One-dose vaccination")]
                      } else if (comparison == "No vaccination vs. Two-dose") {
                        data_list <- data_list[c("No vaccination", "Two-dose vaccination")]
                      } else if (comparison == "One-dose vs. Two-dose") {
                        data_list <- data_list[c("One-dose vaccination", "Two-dose vaccination")]
                      } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                        # Keep all (do nothing)
                      }
                      
                      value_box(
                        title = paste0(input$country, ": Number of females vaccinated"),
                        showcase = bs_icon("person-standing-dress"),
                        theme = NULL,
                        style = "background-color: #F5F5F5; color: black;",
                        renderStrategies(selected = names(data_list), data = data_list)
                      )
                    })
                    
                    #----------------
                    # Plots
                    #----------------
                    # Function to build df for plots
                    make_df <- function(results, indicator) {
                      df_one <- as.numeric(results$one[results$one$variable == indicator, "discounted"])
                      df_two <- as.numeric(results$two[results$two$variable == indicator, "discounted"])
                      data.frame(
                        Scenario = c("No vaccination", "One-dose", "Two-dose"),
                        Value    = c(0, df_one, df_two)
                      )
                    }
                    # Custom ggplot theme
                    my_custom_theme <- function(angle = 0, label_size = 13) {
                      theme_minimal(base_size = 15) +
                        theme(
                          legend.position = "none",
                          axis.text.x     = element_text(size = label_size, angle = angle, 
                                                         hjust = ifelse(angle == 0, 0.5, 1)),
                          axis.line.x     = element_line(color = "grey70", size = 0.5),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank()
                        )
                    }
                    # Shared fill scale
                    my_fill_scale <- scale_fill_manual(values = c(
                      "No vaccination" = "#999999",
                      "One-dose"       = "#74C0E3",
                      "Two-dose"       = "#0094D3"
                    ))
                    
                    # Shared y-axis scale
                    #my_y_scale <- scale_y_continuous(
                      #labels = scales::comma,
                      #expand = expansion(mult = c(0, 0.08))
                    #)
                    
                    # Function to make subtitles
                    make_subtitle <- function(base, comparison) {
                      if (comparison %in% c("No vaccination vs. One-dose",
                                            "No vaccination vs. Two-dose",
                                            "No vaccination vs. One-dose vs. Two-dose")) {
                        paste(base, "no vaccination")
                        
                      } else if (comparison == "One-dose vs. Two-dose") {
                        paste(base, "no vaccination")
                        
                      } else {
                        base
                      }
                    }
                    # Function to make subtitles (for CE graph)
                    make_subtitle_ce <- function(base, comparison) {
                      if (comparison %in% c("No vaccination vs. One-dose",
                                            "No vaccination vs. Two-dose",
                                            "No vaccination vs. One-dose vs. Two-dose")) {
                        paste(base, "No vaccination")
                        
                      } else if (comparison == "One-dose vs. Two-dose") {
                        paste(base, "One-dose strategy")
                        
                      } else {
                        base
                      }
                    }
                    
                        #------------------------------
                        # Graph1: CeCx cases prevented
                        #------------------------------
                        #Title:
                        output$plot1_title <- renderText({
                          paste0(input$country, ": Cervical cancer cases prevented, ", format(Sys.Date(), "%Y"))
                        })
                        #Title:
                        output$tab1_title <- renderText({
                          paste0(input$country, ": Cervical cancer cases prevented, ", format(Sys.Date(), "%Y"))
                        })
                        
                        # Chart subtitle
                        output$plot1_subtitle <- renderText({
                          make_subtitle("Number of cases prevented compared to", input$comparison)
                        })
                        # Table subtitle
                        output$tab1_subtitle <- renderText({
                          make_subtitle("Number of cases prevented compared to", input$comparison)
                        })

                        #Graph:
                        #-------------------------.
                        plot1_reactive <- reactive({
                          req(results())
                          
                          # build base df
                          df <- make_df(results(), "CeCx prevented")
                          
                          comparison <- input$comparison
                          
                          if (comparison == "No vaccination vs. One-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "One-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. Two-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "Two-dose"), ]
                            
                          } else if (comparison == "One-dose vs. Two-dose") {
                            # keep both scenarios, no difference calculation
                            df <- df[df$Scenario %in% c("One-dose", "Two-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                            # keep all scenarios
                            df <- df
                          }
                          
                          # dynamic angle/label size depending on width
                          angle <- ifelse(session$clientData$output_gph_cases_prevented_width < 300, 30, 0)
                          label_size <- ifelse(session$clientData$output_gph_cases_prevented_width < 300, 12, 13)
                          
                          ggplot(df, aes(x = Scenario, y = Value, fill = Scenario)) +
                            geom_col(width = 0.7) +
                            geom_text(aes(label = scales::comma(round(Value, 1))),
                                      vjust = -0.5, size = 4) +
                            scale_y_continuous(
                              labels = scales::comma,
                              expand = expansion(mult = c(0, 0.08)),
                              limits = shared_health_limits()
                            ) +
                            my_fill_scale +
                            labs(x = "Vaccination strategy", y = NULL) +
                            my_custom_theme(angle, label_size) +
                            theme(
                              axis.title.x = element_text(size = 13, color = "grey30", margin = margin(t = 10))
                            )
                        })
                        
                        ## Graph in the app
                        output$gph_cases_prevented <- renderPlot({
                          plot1_reactive()
                        })
                        
                        ## Graph in the download button
                        output$download_plot1 <- downloadHandler(
                          filename = function() {
                            paste0(input$country, "_cervical_cancer_cases_prevented.png")
                          },
                          content = function(file) {
                            # Build the plot with title + 2 subtitles
                            p <- plot1_reactive() +
                              labs(
                                title = input$country,                                                                         # main title (country)
                                subtitle = paste0("Number of cervical cancer cases prevented, ", format(Sys.Date(), "%Y"))     # subtitle 1
                              ) +
                              theme(
                                plot.title    = element_text(hjust = 0, size = 16, face = "bold"),
                                plot.subtitle = element_text(hjust = 0, size = 14, face = "plain"),
                              )
                            
                            ggsave(file, plot = p, width = 7, height = 5, dpi = 300)
                          }
                        )

                        #Table:
                        #-------------------------.
                        table1_data <- reactive({
                          req(results())
                          
                          df <- make_df(results(), "CeCx prevented")
                          
                          comparison <- input$comparison
                          
                          if (comparison == "No vaccination vs. One-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "One-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. Two-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "Two-dose"), ]
                            
                          } else if (comparison == "One-dose vs. Two-dose") {
                            # keep both scenarios, no difference calculation
                            df <- df[df$Scenario %in% c("One-dose", "Two-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                            # keep all scenarios
                            df <- df
                          }
                          
                          # Format with commas, no decimals
                          df$Value <- formatC(df$Value, format = "f", digits = 0, big.mark = ",")
                          
                          df
                        })
                        
                        ## Table in the app
                        output$tab_cases_prevented <- DT::renderDataTable({
                          df <- table1_data()
                          
                          DT::datatable(
                            df,
                            rownames = FALSE,
                            selection = "none",
                            options = list(
                              dom = "t",
                              ordering = FALSE,
                              pageLength = nrow(df),
                              columnDefs = list(
                                list(className = "dt-left", targets = 0),
                                list(className = "dt-right", targets = 1)
                              )
                            ),
                            class = "cell-border hover"
                          )
                        })
                        
                        ## Table in the download button
                        output$download_table1 <- downloadHandler(
                          filename = function() {
                            paste0(input$country, "_cervical_cancer_cases_prevented.xlsx")
                          },
                          content = function(file) {
                            # Export table1_data() to Excel
                            writexl::write_xlsx(table1_data(), path = file)
                          }
                        )
                        
                        #------------------------------
                        # Graph2: Deaths prevented
                        #------------------------------
                        #Title:
                        output$plot2_title <- renderText({
                          paste0(input$country, ": Cervical cancer deaths prevented, ", format(Sys.Date(), "%Y"))
                        })
                        #Title:
                        output$tab2_title <- renderText({
                          paste0(input$country, ": Cervical cancer deaths prevented, ", format(Sys.Date(), "%Y"))
                        })
                        # Chart subtitle
                        output$plot2_subtitle <- renderText({
                          make_subtitle("Number of deaths prevented compared to", input$comparison)
                        })
                        # Table subtitle
                        output$tab2_subtitle <- renderText({
                          make_subtitle("Number of deaths prevented compared to", input$comparison)
                        })
                        
                        #Graph:
                        #-------------------------.
                        plot2_reactive <- reactive({
                          req(results())
                          
                          # build base df
                          df <- make_df(results(), "Deaths prevented")
                          
                          comparison <- input$comparison
                          
                          if (comparison == "No vaccination vs. One-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "One-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. Two-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "Two-dose"), ]
                            
                          } else if (comparison == "One-dose vs. Two-dose") {
                            # keep both scenarios, no difference calculation
                            df <- df[df$Scenario %in% c("One-dose", "Two-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                            # keep all scenarios
                            df <- df
                          }
                          
                          # dynamic angle/label size depending on width
                          angle <- ifelse(session$clientData$output_gph_deaths_prevented_width < 300, 30, 0)
                          label_size <- ifelse(session$clientData$output_gph_deaths_prevented_width < 300, 12, 13)
                          
                          ggplot(df, aes(x = Scenario, y = Value, fill = Scenario)) +
                            geom_col(width = 0.7) +
                            geom_text(aes(label = scales::comma(round(Value, 1))),
                                      vjust = -0.5, size = 4) +
                            scale_y_continuous(
                              labels = scales::comma,
                              expand = expansion(mult = c(0, 0.08)),
                              limits = shared_health_limits()
                            ) +
                            my_fill_scale +
                            labs(x = "Vaccination strategy", y = NULL) +
                            my_custom_theme(angle, label_size) +
                            theme(
                              axis.title.x = element_text(size = 13, color = "grey30", margin = margin(t = 10))
                            )
                        })
                        
                        ## Graph in the app
                        output$gph_deaths_prevented <- renderPlot({
                          plot2_reactive()
                        })
                        
                        ## Graph in the download button
                        output$download_plot2 <- downloadHandler(
                          filename = function() {
                            paste0(input$country, "_cervical_cancer_deaths_prevented.png")
                          },
                          content = function(file) {
                            # Build the plot with title + subtitle
                            p <- plot2_reactive() +
                              labs(
                                title = input$country,                                                                      # main title (country)
                                subtitle = paste0("Number of cervical cancer deaths prevented, ", format(Sys.Date(), "%Y")) # subtitle 1
                              ) +
                              theme(
                                plot.title    = element_text(hjust = 0, size = 16, face = "bold"),
                                plot.subtitle = element_text(hjust = 0, size = 14, face = "plain"),
                              )
                            
                            ggsave(file, plot = p, width = 7, height = 5, dpi = 300)
                          }
                        )
                        
                        #Table:
                        #-------------------------.
                        table2_data <- reactive({
                          req(results())
                          
                          df <- make_df(results(), "Deaths prevented")
                          
                          comparison <- input$comparison
                          
                          if (comparison == "No vaccination vs. One-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "One-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. Two-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "Two-dose"), ]
                            
                          } else if (comparison == "One-dose vs. Two-dose") {
                            # keep both scenarios, no difference calculation
                            df <- df[df$Scenario %in% c("One-dose", "Two-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                            # keep all scenarios
                            df <- df
                          }
                          
                          # Format with commas, no decimals
                          df$Value <- formatC(df$Value, format = "f", digits = 0, big.mark = ",")
                          
                          df
                        })
                        
                        ## Table in the app
                        output$tab_deaths_prevented <- DT::renderDataTable({
                          df <- table2_data()
                          
                          DT::datatable(
                            df,
                            rownames = FALSE,
                            selection = "none",
                            options = list(
                              dom = "t",
                              ordering = FALSE,
                              pageLength = nrow(df),
                              columnDefs = list(
                                list(className = "dt-left", targets = 0),
                                list(className = "dt-right", targets = 1)
                              )
                            ),
                            class = "cell-border hover"
                          )
                        })
                        
                        ## Table in the download button
                        output$download_table2 <- downloadHandler(
                          filename = function() {
                            paste0(input$country, "_cervical_cancer_deaths_prevented.xlsx")
                          },
                          content = function(file) {
                            writexl::write_xlsx(table2_data(), path = file)
                          }
                        )
                        
                        #------------------------------
                        # Graph3: DALYs prevented
                        #------------------------------
                        #Title:
                        output$plot3_title <- renderText({
                          paste0(input$country, ": DALYs prevented, ", format(Sys.Date(), "%Y"))
                        })
                        #Title:
                        output$tab3_title <- renderText({
                          paste0(input$country, ": DALYs prevented, ", format(Sys.Date(), "%Y"))
                        })
                        # Chart subtitle
                        output$plot3_subtitle <- renderText({
                          make_subtitle("Number of DALYs prevented compared to", input$comparison)
                        })
                        # Table subtitle
                        output$tab3_subtitle <- renderText({
                          make_subtitle("Number of DALYs prevented compared to", input$comparison)
                        })
                        
                        #Graph:
                        #-------------------------.
                        plot3_reactive <- reactive({
                          req(results())
                          
                          # build base df
                          df <- make_df(results(), "Total DALYs prevented")
                          
                          comparison <- input$comparison
                          
                          if (comparison == "No vaccination vs. One-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "One-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. Two-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "Two-dose"), ]
                            
                          } else if (comparison == "One-dose vs. Two-dose") {
                            # keep both scenarios, no difference calculation
                            df <- df[df$Scenario %in% c("One-dose", "Two-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                            # keep all scenarios
                            df <- df
                          }
                          
                          # dynamic angle/label size depending on width
                          angle <- ifelse(session$clientData$output_gph_dalys_prevented_width < 300, 30, 0)
                          label_size <- ifelse(session$clientData$output_gph_dalys_prevented_width < 300, 12, 13)
                          
                          ggplot(df, aes(x = Scenario, y = Value, fill = Scenario)) +
                            geom_col(width = 0.7) +
                            geom_text(aes(label = scales::comma(round(Value, 1))),
                                      vjust = -0.5, size = 4) +
                            scale_y_continuous(
                              labels = scales::comma,
                              expand = expansion(mult = c(0, 0.08)),
                              limits = shared_health_limits()
                            ) +
                            my_fill_scale +
                            labs(x = "Vaccination strategy", y = NULL) +
                            my_custom_theme(angle, label_size) +
                            theme(
                              axis.title.x = element_text(size = 13, color = "grey30", margin = margin(t = 10))
                            )
                        })
                        
                        ## Graph in the app
                        output$gph_dalys_prevented <- renderPlot({
                          plot3_reactive()
                        })
                        
                        ## Graph in the download button
                        output$download_plot3 <- downloadHandler(
                          filename = function() {
                            paste0(input$country, "_cervical_cancer_dalys_prevented.png")
                          },
                          content = function(file) {
                            # Build the plot with title + subtitle
                            p <- plot3_reactive() +
                              labs(
                                title = input$country,                                                     # main title (country)
                                subtitle = paste0("Number of DALYs prevented, ", format(Sys.Date(), "%Y")) # subtitle 1
                              ) +
                              theme(
                                plot.title    = element_text(hjust = 0, size = 16, face = "bold"),
                                plot.subtitle = element_text(hjust = 0, size = 14, face = "plain"),
                              )
                            
                            ggsave(file, plot = p, width = 7, height = 5, dpi = 300)
                          }
                        )
                        
                        #Table:
                        #-------------------------.
                        table3_data <- reactive({
                          req(results())
                          
                          df <- make_df(results(), "Total DALYs prevented")
                          
                          comparison <- input$comparison
                          
                          if (comparison == "No vaccination vs. One-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "One-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. Two-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "Two-dose"), ]
                            
                          } else if (comparison == "One-dose vs. Two-dose") {
                            # keep both scenarios, no difference calculation
                            df <- df[df$Scenario %in% c("One-dose", "Two-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                            # keep all scenarios
                            df <- df
                          }
                          
                          # Format with commas, no decimals
                          df$Value <- formatC(df$Value, format = "f", digits = 0, big.mark = ",")
                          
                          df
                        })
                        
                        ## Table in the app
                        output$tab_dalys_prevented <- DT::renderDataTable({
                          df <- table3_data()
                          
                          DT::datatable(
                            df,
                            rownames = FALSE,
                            selection = "none",
                            options = list(
                              dom = "t",
                              ordering = FALSE,
                              pageLength = nrow(df),
                              columnDefs = list(
                                list(className = "dt-left", targets = 0),
                                list(className = "dt-right", targets = 1)
                              )
                            ),
                            class = "cell-border hover"
                          )
                        })
                        
                        ## Table in the download button
                        output$download_table3 <- downloadHandler(
                          filename = function() {
                            paste0(input$country, "_dalys_prevented.xlsx")
                          },
                          content = function(file) {
                            writexl::write_xlsx(table3_data(), path = file)
                          }
                        )
                        
              #--------------------------------------
              # Economic impact tab
              #--------------------------------------
                    #----------------
                    # Value boxes
                    #----------------
                    output$vaccines_delivered_box_econ <- renderUI({
                      res <- results()
                      
                      # Extract values
                      val_one <- if (!is.null(res$one)) round(res$one[variable == "Vac cohort size", undiscounted]) else NA
                      val_two <- if (!is.null(res$two)) round(res$two[variable == "Vac cohort size", undiscounted] * 2) else NA
                      
                      # Base data
                      data_list <- list(
                        "No vaccination"       = "-",
                        "One-dose schedule" = format(val_one, big.mark = ","),
                        "Two-dose schedule" = format(val_two, big.mark = ",")
                      )
                      
                      # Filter based on comparison
                      comparison <- input$comparison
                      if (comparison == "No vaccination vs. One-dose") {
                        data_list <- data_list[c("No vaccination", "One-dose vaccination")]
                      } else if (comparison == "No vaccination vs. Two-dose") {
                        data_list <- data_list[c("No vaccination", "Two-dose vaccination")]
                      } else if (comparison == "One-dose vs. Two-dose") {
                        data_list <- data_list[c("One-dose vaccination", "Two-dose vaccination")]
                      } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                        # Keep all (do nothing)
                      }
                      
                      value_box(
                        title = paste0(input$country, ": Number of vaccines delivered"),
                        showcase = bs_icon("prescription2"),
                        theme = NULL,
                        style = "background-color: #F5F5F5; color: black;",
                        renderStrategies(selected = names(data_list), data = data_list)
                      )
                    })
                    
                    output$females_vaccinated_box_econ <- renderUI({
                    res <- results()
                    
                    # Extract values
                    val_one <- if (!is.null(res$one)) round(res$one[variable == "Vac cohort size", undiscounted]) else NA
                    val_two <- if (!is.null(res$two)) round(res$two[variable == "Vac cohort size", undiscounted]) else NA
                    
                    # Base data
                    data_list <- list(
                      "No vaccination"       = "-",
                      "One-dose vaccination" = format(val_one, big.mark = ","),
                      "Two-dose vaccination" = format(val_two, big.mark = ",")
                    )
                    
                    # Filter based on comparison
                    comparison <- input$comparison
                    if (comparison == "No vaccination vs. One-dose") {
                      data_list <- data_list[c("No vaccination", "One-dose vaccination")]
                    } else if (comparison == "No vaccination vs. Two-dose") {
                      data_list <- data_list[c("No vaccination", "Two-dose vaccination")]
                    } else if (comparison == "One-dose vs. Two-dose") {
                      data_list <- data_list[c("One-dose vaccination", "Two-dose vaccination")]
                    } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                      # Keep all (do nothing)
                    }
                    
                    value_box(
                      title = paste0(input$country, ": Number of females vaccinated"),
                      showcase = bs_icon("person-standing-dress"),
                      theme = NULL,
                      style = "background-color: #F5F5F5; color: black;",
                      renderStrategies(selected = names(data_list), data = data_list)
                    )
                  })

                    #----------------
                    # Plots
                    #----------------
                        #------------------------------
                        # Graph4: Cost of vacc
                        #------------------------------
                        #Title:
                        output$plot4_title <- renderText({
                          paste0(input$country, ": Cost of vaccination, ", format(Sys.Date(), "%Y"))
                        })
                        #Title:
                        output$tab4_title <- renderText({
                          paste0(input$country, ": Cost of vaccination, ", format(Sys.Date(), "%Y"))
                        })
                        # Chart subtitle
                        output$plot4_subtitle <- renderText({
                          base <- paste0(
                            "Total cost of vaccination (procurement + delivery) (Million ",
                            format(Sys.Date(), "%Y"), " USD) compared to"
                          )
                          make_subtitle(base, input$comparison)
                        })
                        # Table subtitle
                        output$tab4_subtitle <- renderText({
                          base <- paste0(
                            "Total cost of vaccination (procurement + delivery) (Million ",
                            format(Sys.Date(), "%Y"), " USD) compared to"
                          )
                          make_subtitle(base, input$comparison)
                        })
                        
                        #Graph:
                        #-------------------------.
                        plot4_reactive <- reactive({
                          req(results())
                          
                          # Build data frames for procurement and delivery
                          df_proc <- make_df(results(), "Vaccine procurement cost")
                          df_proc$Component <- "Procurement"
                          
                          df_del <- make_df(results(), "Vaccine delivery cost")
                          df_del$Component <- "Delivery"
                          
                          # Combine into long dataset
                          df_long <- dplyr::bind_rows(df_proc, df_del)
                          
                          # Map strategy names
                          strategy_map <- c(
                            "No vaccination"        = "No vaccination",
                            "One-dose vaccination"  = "One-dose",
                            "Two-dose vaccination"  = "Two-dose"
                          )
                          df_long$Scenario <- dplyr::recode(df_long$Scenario, !!!strategy_map)
                          
                          # Convert values to millions 
                          df_long$Value <- df_long$Value / 1e6
                          
                          # Apply comparison logic
                          comparison <- input$comparison
                          
                          if (comparison == "No vaccination vs. One-dose") {
                            df_long <- df_long[df_long$Scenario %in% c("No vaccination", "One-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. Two-dose") {
                            df_long <- df_long[df_long$Scenario %in% c("No vaccination", "Two-dose"), ]
                            
                          } else if (comparison == "One-dose vs. Two-dose") {
                            df_long <- df_long[df_long$Scenario %in% c("One-dose", "Two-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                            # keep all scenarios
                            df_long <- df_long
                          }
                          
                          # Compute totals for labels (already in millions) 
                          df_totals <- df_long %>%
                            dplyr::group_by(Scenario) %>%
                            dplyr::summarise(total = sum(Value, na.rm = TRUE), .groups = "drop")
                          
                          # Responsiveness
                          angle <- ifelse(session$clientData$output_gph_cost_vacc_width < 300, 30, 0)
                          label_size <- ifelse(session$clientData$output_gph_cost_vacc_width < 300, 12, 13)
                          
                          ggplot(df_long, aes(x = Scenario, y = Value, 
                                              fill = Scenario, pattern = Component)) +
                            geom_col_pattern(
                              width = 0.7,
                              color  = "white",            
                              pattern_colour = "white",    
                              pattern_fill   = NA,         
                              pattern_density = 0.2,
                              pattern_spacing = 0.03,
                              pattern_size   = 0.15,        
                              pattern_key_scale_factor = 0.6
                            ) +
                            # Labels inside stacked blocks (with white background)
                            geom_label(
                              aes(label = ifelse(Scenario == "No vaccination" | Value == 0, "", 
                                                 scales::label_number(accuracy = 0.1)(Value))),
                              position = position_stack(vjust = 0.5),
                              size = 4, color = "black",
                              fill = "white",       # white background
                              label.size = 0        # remove border
                            ) +
                            # Totals above stacked bars
                            geom_text(data = df_totals,
                                      aes(x = Scenario, y = total, 
                                          label = scales::label_number(accuracy = 0.1)(total)),
                                      vjust = -0.5, size = 4, inherit.aes = FALSE) +
                            scale_y_continuous(
                              labels = scales::label_number(accuracy = 0.1),   # values already in millions
                              expand = expansion(mult = c(0, 0.08)),
                              limits = shared_econ_limits()                   # consistent shared econ limits
                            ) +
                            labs(x = "Vaccination strategy", y = NULL) +
                            my_custom_theme(angle, label_size) +
                            theme(
                              axis.title.x = element_text(size = 13, color = "grey30", margin = margin(t = 10)),
                              legend.position = "bottom",
                              legend.text = element_text(size = 13, color = "grey30"),
                              legend.title = element_text(size = 13, color = "grey30", face = "plain"),
                              legend.margin = margin(t = 5, b = 5),
                              legend.box.margin = margin(t = 5, b = 0),
                              plot.margin = margin(5, 20, 0, 20)
                            ) +
                            scale_fill_manual(values = c(
                              "No vaccination" = "#999999",
                              "One-dose"       = "#74C0E3",
                              "Two-dose"       = "#0094D3"
                            )) +
                            scale_pattern_manual(values = c(
                              "Procurement" = "stripe", 
                              "Delivery"    = "circle"
                            )) +
                            guides(fill = "none", pattern = guide_legend(title = "Cost component"))                          
                        })
                        
                        ## Graph in the app
                        output$gph_cost_vacc <- renderPlot({
                          plot4_reactive()
                        })
                        
                        ## Graph in the download button
                        output$download_plot4 <- downloadHandler(
                          filename = function() {
                            paste0(input$country, "_cost_vaccination.png")
                          },
                          content = function(file) {
                            p <- plot4_reactive() +
                              labs(
                                title = input$country,
                                subtitle = paste0("Cost of vaccination (procurement + delivery) (Million ", 
                                                  format(Sys.Date(), "%Y"), " USD)")
                              ) +
                              theme(
                                plot.title    = element_text(hjust = 0, size = 16, face = "bold"),
                                plot.subtitle = element_text(hjust = 0, size = 14, face = "plain")
                              )
                            
                            ggsave(file, plot = p, width = 7, height = 5, dpi = 300)
                          }
                        )
                        
                        # Table:
                        #-------------------------.
                        table4_data <- reactive({
                          req(results())
                          
                          # Build data frames
                          df_proc <- make_df(results(), "Vaccine procurement cost")
                          df_del  <- make_df(results(), "Vaccine delivery cost")
                          df_tot  <- make_df(results(), "Vaccine cost")   # already total from results
                          
                          # Merge into one table by Scenario
                          df <- dplyr::left_join(df_proc, df_del, by = "Scenario", suffix = c("_proc", "_del")) %>%
                            dplyr::left_join(df_tot, by = "Scenario") %>%
                            dplyr::rename(
                              Procurement = Value_proc,
                              Delivery    = Value_del,
                              Total       = Value
                            )
                          
                          comparison <- input$comparison
                          
                          if (comparison == "No vaccination vs. One-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "One-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. Two-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "Two-dose"), ]
                            
                          } else if (comparison == "One-dose vs. Two-dose") {
                            df <- df[df$Scenario %in% c("One-dose", "Two-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                            # keep all scenarios
                            df <- df
                          }
                          
                          # Convert to millions with one decimal
                          df <- df %>%
                            dplyr::mutate(
                              Procurement = formatC(Procurement / 1e6, format = "f", digits = 1, big.mark = ","),
                              Delivery    = formatC(Delivery / 1e6, format = "f", digits = 1, big.mark = ","),
                              Total       = formatC(Total / 1e6, format = "f", digits = 1, big.mark = ",")
                            )
                          
                          df
                        })
                        
                        ## Table in the app
                        output$tab_cost_vacc <- DT::renderDataTable({
                          df <- table4_data()
                          
                          DT::datatable(
                            df,
                            rownames = FALSE,
                            selection = "none",
                            options = list(
                              dom = "t",
                              ordering = FALSE,
                              pageLength = nrow(df),
                              columnDefs = list(
                                list(className = "dt-left",  targets = 0),
                                list(className = "dt-right", targets = 1:3)
                              )
                            ),
                            class = "cell-border hover"
                          )
                        })
                        
                        ## Table in the download button
                        output$download_table4 <- downloadHandler(
                          filename = function() {
                            paste0(input$country, "_cost_vaccination.xlsx")
                          },
                          content = function(file) {
                            writexl::write_xlsx(table4_data(), path = file)
                          }
                        )
                        
                        #------------------------------
                        # Graph5: Tx costs saved
                        #------------------------------
                        #Title:
                        output$plot5_title <- renderText({
                          paste0(input$country, ": Treatment costs saved, ", format(Sys.Date(), "%Y"))
                        })
                        #Title:
                        output$tab5_title <- renderText({
                          paste0(input$country, ": Treatment costs saved, ", format(Sys.Date(), "%Y"))
                        })
                        # Chart subtitle
                        output$plot5_subtitle <- renderText({
                          base <- paste0(
                            "Total healthcare costs of cancer treatment avoided (Million ",
                            format(Sys.Date(), "%Y"), " USD) compared to"
                          )
                          make_subtitle(base, input$comparison)
                        })
                        # Table subtitle
                        output$tab5_subtitle <- renderText({
                          base <- paste0(
                            "Total healthcare costs of cancer treatment avoided (Million ",
                            format(Sys.Date(), "%Y"), " USD) compared to"
                          )
                          make_subtitle(base, input$comparison)
                        })
                        
                        #Graph:
                        #-------------------------.
                        plot5_reactive <- reactive({
                          req(results())
                          
                          # build base df
                          df <- make_df(results(), "Costs saved")
                          
                          # Convert to millions
                          df$Value <- df$Value / 1e6
                          
                          comparison <- input$comparison
                          
                          if (comparison == "No vaccination vs. One-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "One-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. Two-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "Two-dose"), ]
                            
                          } else if (comparison == "One-dose vs. Two-dose") {
                            # keep both scenarios, no difference calculation
                            df <- df[df$Scenario %in% c("One-dose", "Two-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                            # keep all scenarios
                            df <- df
                          }
                          
                          # dynamic angle/label size depending on width
                          angle <- ifelse(session$clientData$output_gph_cost_tx_width < 300, 30, 0)
                          label_size <- ifelse(session$clientData$output_gph_cost_tx_width < 300, 12, 13)
                          
                          ggplot(df, aes(x = Scenario, y = Value, fill = Scenario)) +
                            geom_col(width = 0.7) +
                            # Labels in millions with one decimal
                            geom_text(aes(label = scales::label_number(accuracy = 0.1)(Value)),
                                      vjust = -0.5, size = 4) +
                            # Y axis also in millions
                            scale_y_continuous(
                              labels = scales::label_number(accuracy = 0.1),   # no scale needed, already in millions
                              expand = expansion(mult = c(0, 0.08)),
                              limits = shared_econ_limits()
                            ) +
                            labs(x = "Vaccination strategy", y = NULL) +
                            my_custom_theme(angle, label_size) +
                            theme(
                              axis.title.x = element_text(size = 13, color = "grey30", margin = margin(t = 10))
                            ) +
                            my_fill_scale
                        })
                        
                        ## Graph in the app
                        output$gph_cost_tx <- renderPlot({
                          plot5_reactive()
                        })
                        
                        ## Graph in the download button
                        output$download_plot5 <- downloadHandler(
                          filename = function() {
                            paste0(input$country, "_tx_costs_saved.png")
                          },
                          content = function(file) {
                            # Build the plot with title + subtitle
                            p <- plot5_reactive() +
                              labs(
                                title = input$country,                                                                      # main title (country)
                                subtitle = paste0("Treatment costs saved (Million ", format(Sys.Date(), "%Y"), " USD)")             # subtitle 1
                              ) +
                              theme(
                                plot.title    = element_text(hjust = 0, size = 16, face = "bold"),
                                plot.subtitle = element_text(hjust = 0, size = 14, face = "plain"),
                              )
                            
                            ggsave(file, plot = p, width = 7, height = 5, dpi = 300)
                          }
                        )
                        
                        #Table:
                        #-------------------------.
                        table5_data <- reactive({
                          req(results())
                          
                          df <- make_df(results(), "Costs saved")
                          
                          comparison <- input$comparison
                          
                          if (comparison == "No vaccination vs. One-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "One-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. Two-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "Two-dose"), ]
                            
                          } else if (comparison == "One-dose vs. Two-dose") {
                            # keep both scenarios, no difference calculation
                            df <- df[df$Scenario %in% c("One-dose", "Two-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                            # keep all scenarios
                            df <- df
                          }
                          
                          # Format values in millions, one decimal place
                          df$Value <- formatC(df$Value / 1e6, format = "f", digits = 1, big.mark = ",")
                          
                          df
                        })
                        
                        ## Table in the app
                        output$tab_cost_tx <- DT::renderDataTable({
                          df <- table5_data()
                          
                          DT::datatable(
                            df,
                            rownames = FALSE,
                            selection = "none",
                            options = list(
                              dom = "t",
                              ordering = FALSE,
                              pageLength = nrow(df),
                              columnDefs = list(
                                list(className = "dt-left", targets = 0),
                                list(className = "dt-right", targets = 1)
                              )
                            ),
                            class = "cell-border hover"
                          )
                        })
                        
                        ## Table in the download button
                        output$download_table5 <- downloadHandler(
                          filename = function() {
                            paste0(input$country, "_tx_costs_saved.xlsx")
                          },
                          content = function(file) {
                            writexl::write_xlsx(table5_data(), path = file)
                          }
                        )
                        
                        #------------------------------
                        # Graph6: Incremental cost
                        #------------------------------
                        #Title:
                        output$plot6_title <- renderText({
                          paste0(input$country, ": Incremental cost, ", format(Sys.Date(), "%Y"))
                        })
                        #Title:
                        output$tab6_title <- renderText({
                          paste0(input$country, ": Incremental cost, ", format(Sys.Date(), "%Y"))
                        })
                        #Subtitle:
                        output$plot6_subtitle <- renderText({
                          base <- paste0(
                            "Net cost (vaccination cost - treatment cost saved) (Million ",
                            format(Sys.Date(), "%Y"), " USD) compared to"
                          )
                          make_subtitle(base, input$comparison)
                        })
                        #Subtitle:
                        output$tab6_subtitle <- renderText({
                          base <- paste0(
                            "Net cost (vaccination cost - treatment cost saved) (Million ",
                            format(Sys.Date(), "%Y"), " USD) compared to"
                          )
                          make_subtitle(base, input$comparison)
                        })
                        
                        #Graph:
                        #-------------------------.
                        plot6_reactive <- reactive({
                          req(results())
                          
                          # build base df
                          df <- make_df(results(), "Net cost")
                          
                          # Convert to millions 
                          df$Value <- df$Value / 1e6
                          
                          comparison <- input$comparison
                          
                          if (comparison == "No vaccination vs. One-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "One-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. Two-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "Two-dose"), ]
                            
                          } else if (comparison == "One-dose vs. Two-dose") {
                            # keep both scenarios, no difference calculation
                            df <- df[df$Scenario %in% c("One-dose", "Two-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                            # keep all scenarios
                            df <- df
                          }
                          
                          # dynamic angle/label size depending on width
                          angle <- ifelse(session$clientData$output_gph_cost_incre_width < 300, 30, 0)
                          label_size <- ifelse(session$clientData$output_gph_cost_incre_width < 300, 12, 13)
                          
                          ggplot(df, aes(x = Scenario, y = Value, fill = Scenario)) +
                            geom_col(width = 0.7) +
                            # Labels in millions with one decimal
                            geom_text(aes(label = scales::label_number(accuracy = 0.1)(Value)),
                                      vjust = -0.5, size = 4) +
                            # Y axis also in millions
                            scale_y_continuous(
                              labels = scales::label_number(accuracy = 0.1),   # no scale, already millions
                              expand = expansion(mult = c(0, 0.08)),
                              limits = shared_econ_limits()
                            ) +
                            labs(x = "Vaccination strategy", y = NULL) +
                            my_custom_theme(angle, label_size) +
                            theme(
                              axis.title.x = element_text(size = 13, color = "grey30", margin = margin(t = 10))
                            ) +
                            my_fill_scale
                        })
                        
                        ## Graph in the app
                        output$gph_cost_incre <- renderPlot({
                          plot6_reactive()
                        })
                        
                        ## Graph in the download button
                        output$download_plot6 <- downloadHandler(
                          filename = function() {
                            paste0(input$country, "_incremental_cost.png")
                          },
                          content = function(file) {
                            # Build the plot with title + subtitle
                            p <- plot6_reactive() +
                              labs(
                                title = input$country,                                                                                               # main title (country)
                                subtitle = paste0("Net cost (vaccination cost – treatment cost saved) (Million ", format(Sys.Date(), "%Y"), " USD)") # subtitle 1
                              ) +
                              theme(
                                plot.title    = element_text(hjust = 0, size = 16, face = "bold"),
                                plot.subtitle = element_text(hjust = 0, size = 14, face = "plain"),
                              )
                            
                            ggsave(file, plot = p, width = 7, height = 5, dpi = 300)
                          }
                        )
                        
                        #Table:
                        #-------------------------.
                        table6_data <- reactive({
                          req(results())
                          
                          df <- make_df(results(), "Net cost")
                          
                          comparison <- input$comparison
                          
                          if (comparison == "No vaccination vs. One-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "One-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. Two-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "Two-dose"), ]
                            
                          } else if (comparison == "One-dose vs. Two-dose") {
                            # keep both scenarios, no difference calculation
                            df <- df[df$Scenario %in% c("One-dose", "Two-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                            # keep all scenarios
                            df <- df
                          }
                          
                          # Format values in millions, one decimal place
                          df$Value <- formatC(df$Value / 1e6, format = "f", digits = 1, big.mark = ",")
                          
                          df
                        })
                        
                        ## Table in the app
                        output$tab_cost_incre <- DT::renderDataTable({
                          df <- table6_data()
                          
                          DT::datatable(
                            df,
                            rownames = FALSE,
                            selection = "none",
                            options = list(
                              dom = "t",
                              ordering = FALSE,
                              pageLength = nrow(df),
                              columnDefs = list(
                                list(className = "dt-left", targets = 0),
                                list(className = "dt-right", targets = 1)
                              )
                            ),
                            class = "cell-border hover"
                          )
                        })
                        
                        ## Table in the download button
                        output$download_table6 <- downloadHandler(
                          filename = function() {
                            paste0(input$country, "_incremental_cost.xlsx")
                          },
                          content = function(file) {
                            writexl::write_xlsx(table6_data(), path = file)
                          }
                        )
                        
              #--------------------------------------
              # Cost-effectiveness frontier tab
              #--------------------------------------
                        #-------------------
                        # About CE
                        #-------------------
                        # Information links (located in sidebars) 
                        observeEvent(input$show_ce_frontier_modal, {
                        showModal(
                          modalDialog(
                            title = "Cost-effectiveness frontier",
                            easy_close = TRUE,
                            footer = modalButton("Close"),
                            size = "l",
                            tagList(
                              p("This section shows modelled estimates of the costs and benefits of HPV vaccination strategies."),
                              p("The graph displays the cost-effectiveness of each strategy: 
                               the Y-axis shows net costs (in million USD, discounted), 
                               while the X-axis shows the effectiveness of vaccination (measured in cases, deaths, or DALYs prevented)."),
                              p("Each point represents a vaccination strategy. The point at the origin (0,0) is the main comparator. All other points show how vaccination strategies compare to this baseline."), 
                              p("Being more to the right means greater health impact (more cases, deaths, or DALYs prevented) while being more upwards means higher costs (in million USD, discounted to the present).
                                Points that appear below the horizontal line at zero indicate strategies that save money overall (i.e., the costs of vaccination are outweighed by the treatment costs avoided)."),
                              p("The threshold line illustrates the best trade-off between costs and health gains. 
                               Points lying on or below this line represent the best value for money, this means that the strategy that give the greatest health gains for their cost.
                               Strategies that lie above the threshold are considered less cost-effective, as they cost more for the health benefit they provide compared to the threshold."),
                              p("Users can apply different cost-effectiveness thresholds. These are benchmarks that help assess whether vaccination strategies 
                               represent good value for money in their country context."),
                              p("All estimates are based on PRIME model outputs and are presented in present value terms, discounted to the current year.")
                            )
                          )
                        )
                        })
                        
                        #------------------
                        # CE frontier graph
                        #------------------
                        
                        # Chart subtitle
                        output$ce_baseline <- renderText({
                          make_subtitle_ce("Baseline comparator (positioned at origin):", input$comparison)
                        })
                        
                        # Map between UI labels and variable names
                        eff_map <- list(
                          "Cervical cancer cases prevented"  = "CeCx prevented",
                          "Cervical cancer deaths prevented" = "Deaths prevented",
                          "Cervical cancer DALYs prevented"  = "Total DALYs prevented"
                        )
                        
                        # Function to build dataframe
                        make_ce_df <- function(results, eff_indicator) {
                          df_one <- as.data.frame(results$one)
                          df_two <- as.data.frame(results$two)
                          
                          # Effectiveness
                          eff_one  <- df_one[df_one$variable == eff_indicator, "discounted"]
                          eff_two  <- df_two[df_two$variable == eff_indicator, "discounted"]
                          
                          # Net cost
                          cost_one <- df_one[df_one$variable == "Net cost", "discounted"]
                          cost_two <- df_two[df_two$variable == "Net cost", "discounted"]
                          
                          data.frame(
                            Scenario      = c("No vaccination", "One-dose", "Two-dose"),
                            Effectiveness = c(0, as.numeric(eff_one), as.numeric(eff_two)),
                            NetCost       = c(0, as.numeric(cost_one), as.numeric(cost_two))
                          )
                        }
                        
                        # Reactive wrapper for CE data (place this BEFORE output$ce_graph)
                        ce_reactive <- reactive({
                          req(results(), input$eff_indicator)
                          indicator_var <- eff_map[[input$eff_indicator]]   # maps UI label -> variable name
                          make_ce_df(results(), indicator_var)
                        })
                        
                        ## Render CE scatter with thresholds
                        output$ce_graph <- renderPlot({
                          df <- ce_reactive()
                          
                          # Convert NetCost to millions
                          df$NetCost <- df$NetCost / 1e6
                          
                          # Apply comparison filter/logic
                          comparison <- input$comparison
                          
                          if (comparison == "No vaccination vs. One-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "One-dose"), ]
                            
                          } else if (comparison == "No vaccination vs. Two-dose") {
                            df <- df[df$Scenario %in% c("No vaccination", "Two-dose"), ]
                            
                          } else if (comparison == "One-dose vs. Two-dose") {
                            eff_one <- df$Effectiveness[df$Scenario == "One-dose"]
                            eff_two <- df$Effectiveness[df$Scenario == "Two-dose"]
                            
                            cost_one <- df$NetCost[df$Scenario == "One-dose"]
                            cost_two <- df$NetCost[df$Scenario == "Two-dose"]
                            
                            df <- data.frame(
                              Scenario      = c("One-dose", "Two-dose"),
                              Effectiveness = c(0, eff_two - eff_one),
                              NetCost       = c(0, cost_two - cost_one)
                            )
                            
                          } else if (comparison == "No vaccination vs. One-dose vs. Two-dose") {
                            # keep all scenarios
                            df <- df
                          }
                          
                          # Fix x-axis maximum at 1,000 only for the incremental One-dose vs Two-dose view
                          x_max <- NULL
                          if (comparison == "One-dose vs. Two-dose") {
                            x_max <- 1000
                          }
                          
                          # GDP per capita value (in millions)
                          gdp_cap <- results()$one %>%
                            dplyr::filter(variable == "GDP/capita") %>%
                            dplyr::pull(discounted) %>%
                            as.numeric() / 1e6
                          
                          # Get thresholds selected by user (include ce_thr for user threshold)
                          selected <- input$summary_ce_thresholds
                          
                          # Map labels to numeric multipliers
                          thr_map <- list(
                            "0.25x GDP per capita" = 0.25,
                            "0.5x GDP per capita"  = 0.50,
                            "1x GDP per capita"    = 1.00,
                            "User's threshold"     = suppressWarnings(as.numeric(input$ce_thr))
                          )
                          thr_selected <- thr_map[selected]
                          
                          # Colors for scenarios and thresholds
                          scenario_cols <- c(
                            "No vaccination" = "#999999",
                            "One-dose"       = "#74C0E3",
                            "Two-dose"       = "#0094D3"
                          )
                          threshold_cols <- c(
                            "0.25x GDP per capita" = "#00A44F",
                            "0.5x GDP per capita"  = "orange",
                            "1x GDP per capita"    = "red",
                            "User's threshold"     = "#F05937"
                          )
                          
                          # Base plot: axes, theme, reference lines
                          p <- ggplot(df, aes(x = Effectiveness, y = NetCost)) +
                            geom_line(aes(group = 1), linetype = "dashed", color = "grey50") +
                            scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                                               name   = "Net cost (Million USD, discounted)") +
                            scale_x_continuous(labels = scales::comma, name = input$eff_indicator) +
                            theme_minimal(base_size = 13) +
                            theme(
                              panel.grid.major = element_line(color = "grey80", size = 0.3),
                              panel.grid.minor = element_line(color = "grey80", size = 0.2),
                              axis.line = element_line(color = "transparent"),
                              legend.position = "bottom",
                              legend.title = element_text(size = 13),
                              legend.text  = element_text(size = 13)
                            ) +
                            geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
                            geom_vline(xintercept = 0, color = "black", linewidth = 0.4)
                          
                          # -------------------------
                          # Legend 1: SCENARIOS (dots)
                          # -------------------------
                          p <- p +
                            geom_point(aes(color = Scenario), size = 4, show.legend = TRUE) +
                            scale_color_manual(
                              name   = NULL,
                              values = scenario_cols,
                              breaks = c("No vaccination", "One-dose", "Two-dose"),
                              guide  = guide_legend(
                                order = 1,
                                override.aes = list(linetype = "blank", shape = 16, size = 4)
                              )
                            )
                          
                          # Start a NEW color scale for thresholds
                          p <- p + ggnewscale::new_scale_color()
                          
                          # -------------------------
                          # Legend 2: THRESHOLDS (lines)
                          # -------------------------
                          if (length(thr_selected) > 0) {
                            thr_df <- data.frame(
                              Threshold = names(thr_selected),
                              slope     = as.numeric(thr_selected) * gdp_cap,
                              intercept = 0
                            )
                            thr_df <- thr_df[is.finite(thr_df$slope), , drop = FALSE]
                            
                            if (nrow(thr_df) > 0) {
                              p <- p +
                                geom_abline(
                                  data = thr_df,
                                  aes(slope = slope, intercept = intercept, color = Threshold),
                                  linewidth = 1,
                                  show.legend = TRUE
                                ) +
                                scale_color_manual(
                                  name   = NULL,
                                  values = threshold_cols,
                                  breaks = names(threshold_cols),
                                  guide  = guide_legend(
                                    order = 2,
                                    override.aes = list(shape = NA, linetype = 1, size = 1.5)
                                  )
                                )
                            }
                          }
                          
                          # Apply fixed x-axis max only for the incremental comparison
                          if (!is.null(x_max)) {
                            p <- p + coord_cartesian(xlim = c(0, x_max))
                          }
                          
                          # Overlay points again ON TOP of lines (no legend for this overlay)
                          p <- p + ggnewscale::new_scale_color() +
                            geom_point(aes(color = Scenario), size = 4, show.legend = FALSE) +
                            scale_color_manual(values = scenario_cols, guide = "none")
                          
                          p
                        })
                        
        
                        
    }
    
    

#====================
# RUN APP
#====================
  
shinyApp(ui, server)

  
  
  
  
  
  
