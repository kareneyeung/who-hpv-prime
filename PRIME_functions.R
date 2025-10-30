#******************************************************************************.
# World Health Organization
# Aquarius Population Health
# Project: WHO_PRIME_app
# Last updated: 25/08/25
# Code description: 
#  (1) Modify PRIME functions to include user's inputs for the app.
#******************************************************************************.

#========================
# New functions creation
#========================
  #================================
  # New coding for analyseCosts:
  #================================
  
  analyseCosts_breakdown <- function(
    results, 
    
    # NEW: vaccine costs breakdown
    vaccine_cost_price,
    vaccine_cost_delivery,
    #----------------------------.
    
    gdp_per_capita) 
  {
    if (sum(!sapply(ls(), function(x) {
      checkSize(get(x))
    })) > 0) {
      stop("Not all values have the required length")
    }
    results[, "vaccinated"] <- results[, vaccinated] * results[, cohort_size]
    results[, "immunized"] <- results[, immunized] * results[, cohort_size]
    results[, "inc.cecx"] <- results[, inc.cecx] * results[, cohort_size]
    results[, "mort.cecx"] <- results[, mort.cecx] * results[, cohort_size]
    results[, "lifey"] <- results[, lifey] * results[, cohort_size]
    results[, "disability"] <- results[, disability] * results[, cohort_size]
    results[, "cost.cecx"] <- results[, cost.cecx] * results[, cohort_size]
    
    # NEW: add 2 new rows to variable list
    #---------------------------------------------------------
    costvariables <- c("Cohort size", 
                       "Vac cohort size",
                       "Vaccine procurement cost",      # NEW
                       "Vaccine delivery cost",         # NEW
                       "Vaccine cost", 
                       "Costs saved", 
                       "Net cost", "CeCx prevented", 
                       "Deaths prevented", 
                       "Life years saved", 
                       "Nonfatal DALYs prevented", 
                       "Cost/death prevented", 
                       "Cost/life year saved", 
                       "Cost/DALY prevented", 
                       "GDP/capita", 
                       "CE at 1xGDP/capita?", 
                       "CE at 3xGDP/capita?", 
                       "Cut-off price")
    #---------------------------------------------------------
    costeffect <- data.table(variable = costvariables)
    types <- unique(results[, type])
    for (d in types) {
      costeffect[, d] <- numeric(length(costvariables))
    }
    vaccinated <- 0
    a <- -1
    while (vaccinated == 0) {
      a <- a + 1
      vaccinated <- results[scenario == "post-vaccination" & 
                              type == "undiscounted" & age == a, vaccinated]
    }
    agevac <- a
    cohort_size <- 0
    a <- -1
    while (cohort_size == 0) {
      a <- a + 1
      cohort_size <- results[scenario == "post-vaccination" & 
                               type == "undiscounted" & age == a, cohort_size]
    }
    agecohort <- a
    aggregated <- dtAggregate(results, "age", id.vars = c("scenario", "type"))
    difference <- aggregated[scenario == "pre-vaccination"]
    difference[, "scenario"] <- "difference"
    difference[, "cohort_size"] <- abs(aggregated[scenario == 
                                                    "pre-vaccination", cohort_size] - aggregated[scenario == 
                                                                                                   "post-vaccination", cohort_size])
    difference[, "vaccinated"] <- abs(aggregated[scenario == 
                                                   "pre-vaccination", vaccinated] - aggregated[scenario == 
                                                                                                 "post-vaccination", vaccinated])
    difference[, "immunized"] <- abs(aggregated[scenario == "pre-vaccination", 
                                                immunized] - aggregated[scenario == "post-vaccination", 
                                                                        immunized])
    difference[, "inc.cecx"] <- aggregated[scenario == "pre-vaccination", 
                                           inc.cecx] - aggregated[scenario == "post-vaccination", 
                                                                  inc.cecx]
    difference[, "mort.cecx"] <- aggregated[scenario == "pre-vaccination", 
                                            mort.cecx] - aggregated[scenario == "post-vaccination", 
                                                                    mort.cecx]
    difference[, "lifey"] <- aggregated[scenario == "pre-vaccination", 
                                        lifey] - aggregated[scenario == "post-vaccination", lifey]
    difference[, "disability"] <- aggregated[scenario == "pre-vaccination", 
                                             disability] - aggregated[scenario == "post-vaccination", 
                                                                      disability]
    difference[, "cost.cecx"] <- aggregated[scenario == "pre-vaccination", 
                                            cost.cecx] - aggregated[scenario == "post-vaccination", 
                                                                    cost.cecx]
    for (d in types) {
      costeffect[variable == "Cohort size", d] <- cohort_size
      costeffect[variable == "Vac cohort size", d] <- results[age == 
                                                                agevac & scenario == "post-vaccination" & type == 
                                                                d, vaccinated]
      # NEW: split vaccine costs
      #--------------------------------
      vac_cohort <- unlist(costeffect[variable == "Vac cohort size", d, with = FALSE], use.names = FALSE)
      costeffect[variable == "Vaccine procurement cost", d] <- vac_cohort * vaccine_cost_price
      costeffect[variable == "Vaccine delivery cost",    d] <- vac_cohort * vaccine_cost_delivery
      
      # keep total vaccine cost row as the sum 
      total_vac_cost <- unlist(costeffect[variable == "Vaccine procurement cost", d, with = FALSE], use.names = FALSE) +
        unlist(costeffect[variable == "Vaccine delivery cost",    d, with = FALSE], use.names = FALSE)
      costeffect[variable == "Vaccine cost", d] <- total_vac_cost
      #--------------------------------
      
      costeffect[variable == "Costs saved", d] <- difference[type == 
                                                               d, cost.cecx]
      costeffect[variable == "Net cost", d] <- unlist(costeffect[variable == 
                                                                   "Vaccine cost", d, with = FALSE], use.names = FALSE) - 
        unlist(costeffect[variable == "Costs saved", d, with = FALSE], 
               use.names = FALSE)
      costeffect[variable == "CeCx prevented", d] <- difference[type == 
                                                                  d, inc.cecx]
      costeffect[variable == "Deaths prevented", d] <- difference[type == 
                                                                    d, mort.cecx]
      costeffect[variable == "Life years saved", d] <- difference[type == 
                                                                    d, lifey]
      costeffect[variable == "Nonfatal DALYs prevented", d] <- difference[type == 
                                                                            d, disability]
      costeffect[variable == "Cost/death prevented", d] <- unlist(costeffect[variable == 
                                                                               "Net cost", d, with = FALSE], use.names = FALSE)/unlist(costeffect[variable == 
                                                                                                                                                    "Deaths prevented", d, with = FALSE], use.names = FALSE)
      costeffect[variable == "Cost/life year saved", d] <- unlist(costeffect[variable == 
                                                                               "Net cost", d, with = FALSE], use.names = FALSE)/unlist(costeffect[variable == 
                                                                                                                                                    "Life years saved", d, with = FALSE], use.names = FALSE)
      costeffect[variable == "Cost/DALY prevented", d] <- unlist(costeffect[variable == 
                                                                              "Net cost", d, with = FALSE], use.names = FALSE)/(unlist(costeffect[variable == 
                                                                                                                                                    "Life years saved", d, with = FALSE], use.names = FALSE) + 
                                                                                                                                  unlist(costeffect[variable == "Nonfatal DALYs prevented", 
                                                                                                                                                    d, with = FALSE], use.names = FALSE))
      costeffect[variable == "GDP/capita", d] <- gdp_per_capita
      costeffect[variable == "CE at 1xGDP/capita?", d] <- as.numeric(unlist(costeffect[variable == 
                                                                                         "Cost/DALY prevented", d, with = FALSE], use.names = FALSE) < 
                                                                       gdp_per_capita)
      costeffect[variable == "CE at 3xGDP/capita?", d] <- as.numeric(unlist(costeffect[variable == 
                                                                                         "Cost/DALY prevented", d, with = FALSE], use.names = FALSE) < 
                                                                       (gdp_per_capita * 3))
      costeffect[variable == "Cut-off price", d] <- (unlist(costeffect[variable == 
                                                                         "Costs saved", d, with = FALSE], use.names = FALSE) + 
                                                       (unlist(costeffect[variable == "Life years saved", 
                                                                          d, with = FALSE], use.names = FALSE) + unlist(costeffect[variable == 
                                                                                                                                     "Nonfatal DALYs prevented", d, with = FALSE], 
                                                                                                                        use.names = FALSE)) * gdp_per_capita)/unlist(costeffect[variable == 
                                                                                                                                                                                  "Vac cohort size", d, with = FALSE], use.names = FALSE)
      costeffect[, d] <- round(unlist(costeffect[, d, with = FALSE], 
                                      use.names = FALSE), 2)
    }
    return(costeffect)
  }
  

  #================================
  # New coding for new_RunCohort:
  #================================  
  
  new_RunCohort <- function (
    lifetab, 
    cohort, 
    incidence, 
    mortality_cecx, 
    prevalence, 
    agevac, 
    coverage, 
    campaigns, 
    vaccine_efficacy_nosexdebut, 
    vaccine_efficacy_sexdebut, 
    cost_cancer, discounting = FALSE, 
    disc.cost = 0.03, 
    disc.ben = 0.03, 
    country_iso3 = NULL, 
    run_country = FALSE, 
    disability.weights = "gbd_2017",
    
    # NEW: new arguments in the function:
    #-------------------------------.
    dw_dx   = -1,  # dw diagnosis
    dw_seq  = -1,  # dw non-terminal sequelae (control)
    dw_term = -1   # dw terminal
    #-------------------------------.
  ) 
  {
    if (sum(!sapply(ls(), function(x) {
      checkSize(get(x))
    })) > 0) {
      stop("Not all values have the required length")
    }
    ages <- lifetab[, age]
    lexp <- lifetab[, ex]
    if (disability.weights == "gbd_2001") {
      stratum <- data.global[iso3 == country_iso3, `WHO Mortality Stratum`]
      daly.canc.seq <- data.disability_weights[Source == "gbd_2001" & 
                                                 WHO_MortalityStratum == stratum, Mid]
      diag <- data.disability_weights[Source == disability.weights & 
                                        Sequela == "diagnosis", Mid]
      terminal <- data.disability_weights[Source == disability.weights & 
                                            Sequela == "terminal", Mid]
      # NEW: adjustment for dw
      #-----------------------------
      if (!is.null(dw_dx)   && dw_dx   != -1) diag          <- as.numeric(dw_dx)
      if (!is.null(dw_seq)  && dw_seq  != -1) daly.canc.seq <- as.numeric(dw_seq)    
      if (!is.null(dw_term) && dw_term != -1) terminal      <- as.numeric(dw_term)
      #-----------------------------
      
      control <- daly.canc.seq * data.disability_weights[Source == 
                                                           disability.weights & Sequela == "control" & WHO_MortalityStratum == 
                                                           "E", Duration]
      daly.canc.nonfatal <- diag + control
      daly.canc.fatal <- diag + terminal
      yld <- ((incidence - mortality_cecx) * daly.canc.nonfatal) + 
        (mortality_cecx * daly.canc.fatal)
      if (discounting) {
        daly.canc.nonfatal.disc <- diag + daly.canc.seq * 
          (1/(1 + disc.ben) + 1/(1 + disc.ben)^2 + 1/(1 + 
                                                        disc.ben)^3 + 1/(1 + disc.ben)^4)
        daly.canc.fatal.disc <- diag + terminal * 1/(1 + 
                                                       disc.ben)
        yld.disc <- ((incidence - mortality_cecx) * daly.canc.nonfatal.disc) + 
          (mortality_cecx * daly.canc.fatal.disc)
      }
    }
    else if (disability.weights == "gbd_2017") {
      dw = list(diag = data.disability_weights[Source == disability.weights & 
                                                 Sequela == "diagnosis", Mid], control = data.disability_weights[Source == 
                                                                                                                   disability.weights & Sequela == "control", Mid], 
                metastatic = data.disability_weights[Source == disability.weights & 
                                                       Sequela == "metastatic", Mid], terminal = data.disability_weights[Source == 
                                                                                                                           disability.weights & Sequela == "terminal", Mid])
      cecx_duration = list(diag = data.disability_weights[Source == 
                                                            disability.weights & Sequela == "diagnosis", Duration], 
                           metastatic = data.disability_weights[Source == disability.weights & 
                                                                  Sequela == "metastatic", Duration], terminal = data.disability_weights[Source == 
                                                                                                                                           disability.weights & Sequela == "terminal", Duration])
      # NEW: adjustment for dw
      #-----------------------------
      if (!is.null(dw_dx)   && dw_dx   != -1) dw$diag     <- as.numeric(dw_dx)
      if (!is.null(dw_seq)  && dw_seq  != -1) dw$control  <- as.numeric(dw_seq)   # non-terminal sequelae
      if (!is.null(dw_term) && dw_term != -1) dw$terminal <- as.numeric(dw_term)
      #-----------------------------
      
      yld <- (incidence * dw$diag * cecx_duration$diag) + 
             (prevalence * dw$control) + 
             (mortality_cecx * ((dw$metastatic * cecx_duration$metastatic) + (dw$terminal * cecx_duration$terminal)))
      if (discounting) {
        yld.disc <- yld
      }
    }
    if (discounting) {
      disc.cost.yr <- rep(0, length(ages))
      disc.ben.yr <- rep(0, length(ages))
      disc.cost.yr[1:(which(ages == agevac))] <- 1
      disc.ben.yr[1:(which(ages == agevac))] <- 1
      for (a in ages[which(ages > agevac)]) {
        disc.cost.yr[which(ages == a)] <- 1/(1 + disc.cost)^(a - 
                                                               agevac)
        disc.ben.yr[which(ages == a)] <- 1/(1 + disc.ben)^(a - 
                                                             agevac)
      }
      lexp.disc <- (1 - exp(-1 * disc.ben * lexp))/disc.ben
    }
    coverage <- ageCoverage(ages, coverage, vaccine_efficacy_nosexdebut, 
                            vaccine_efficacy_sexdebut, campaigns, lifetab, cohort, 
                            agevac, country_iso3 = country_iso3)
    out.pre <- data.table(age = ages, cohort_size = cohort * 
                            lifetab[, lx.adj], vaccinated = rep(0, length(ages)), 
                          immunized = rep(0, length(ages)), inc.cecx = incidence, 
                          mort.cecx = mortality_cecx, lifey = mortality_cecx * 
                            lexp, disability = yld, cost.cecx = incidence * cost_cancer, 
                          prev.cecx = prevalence)
    out.post <- out.pre
    out.post <- out.post * (1 - coverage[, effective_coverage])
    out.post[, "age"] <- ages
    out.post[, "cohort_size"] <- cohort * lifetab[, lx.adj]
    out.post[, "vaccinated"] <- coverage[, coverage]
    out.post[, "immunized"] <- coverage[, effective_coverage]
    if (discounting) {
      out.pre.disc <- out.pre
      out.pre.disc[, "inc.cecx"] <- out.pre.disc[, inc.cecx] * 
        disc.ben.yr
      out.pre.disc[, "mort.cecx"] <- out.pre.disc[, mort.cecx] * 
        disc.ben.yr
      out.pre.disc[, "prev.cecx"] <- out.pre.disc[, prev.cecx] * 
        disc.ben.yr
      out.pre.disc[, "lifey"] <- out.pre[, mort.cecx] * lexp.disc * 
        disc.ben.yr
      out.pre.disc[, "disability"] <- yld.disc * disc.ben.yr
      out.pre.disc[, "cost.cecx"] <- out.pre[, cost.cecx] * 
        disc.cost.yr
      out.post.disc <- out.pre.disc
      out.post.disc <- out.pre.disc * (1 - coverage[, effective_coverage])
      out.post.disc[, "age"] <- ages
      out.post.disc[, "cohort_size"] <- cohort * lifetab[, 
                                                         lx.adj]
      out.post.disc[, "vaccinated"] <- coverage[, coverage]
      out.post.disc[, "immunized"] <- coverage[, effective_coverage]
      out.pre.disc[, "scenario"] <- "pre-vaccination"
      out.pre.disc[, "type"] <- "discounted"
      out.post.disc[, "scenario"] <- "post-vaccination"
      out.post.disc[, "type"] <- "discounted"
    }
    out.pre[, "scenario"] <- "pre-vaccination"
    out.pre[, "type"] <- "undiscounted"
    out.post[, "scenario"] <- "post-vaccination"
    out.post[, "type"] <- "undiscounted"
    if (discounting) {
      out.pre <- rbindlist(list(out.pre, out.pre.disc))
      out.post <- rbindlist(list(out.post, out.post.disc))
    }
    out <- rbindlist(list(out.pre, out.post))
    out <- out[, c("scenario", "type", "age", "cohort_size", 
                   "vaccinated", "immunized", "inc.cecx", "mort.cecx", "lifey", 
                   "disability", "cost.cecx", "prev.cecx"), with = FALSE]
    return(out)
  }
  
  
  #================================
  # New coding for new_RunCountry:
  #================================
  
  new_RunCountry <- function (
    country_iso3,
    vaceff_beforesexdebut = 1,
    vaceff_aftersexdebut = 0,
    cov = 1,
    agevac = 10,
    agecohort = 10,
    cohort = -1,
    canc.inc = "2020",
    sens = -1,
    unwpp_mortality = TRUE,
    year_born = -1,
    year_vac = -1,
    campaigns = -1,
    analyseCosts = FALSE,
    canc.cost = "unadj",
    discounting = FALSE,
    disc.cost = 0.03,
    disc.ben = 0.03,
    run_batch = FALSE,
    psadat = -1,
    disability.weights = "gbd_2017",
    wb.indicator = "NY.GDP.PCAP.PP.CD",
    wb.year = 2024,
    vaccine = "4vHPV",
    
    # NEW: new arguments in the function
    #-----------------------------------.
      # Vaccine price and delivery cost
      vac_price     = -1,
      vac_delivery  = -1,

      # Proportion of cervical cancer cases that are due to HPV16/18
      hpv1618_prop  = -1,
    
      # Cervical cancer treatment cost
      cecx_cost     = -1,
    
      # Cost-effectiveness threshold
      ce_threshold            = -1,   # absolute threshold in US$ (same units as Cost/DALY)
      ce_threshold_gdp_mult   = -1,    # e.g., 0.5 means 0.5 × GDP/capita
    
      # Disability weights (dw)
      dw_dx   = -1, # Dw for cancer diagnosis
      dw_seq  = -1, # Dw for non-terminal cancer sequelae
      dw_term = -1, # Dw for terminal cancer
    
      # One-dose scenario
      one_dose = 0  # Dummy variable (1 or 0) to consider one-dose scenario
    
    #-----------------------------------.

    )
  {
    if (sum(!sapply(ls(), function(x) {
      checkSize(get(x))
    })) > 0) {
      stop("Not all values have the required length")
    }
    if (year_vac != -1 & year_born != -1) {
      if (year_vac - agevac != year_born) {
        stop(paste0("Year of vaccination (", year_vac, ") and age of vaccination (",
                    agevac, ") do not correspond with chosen birthcohort (",
                    year_born, "). Please change accordingly or omit 'year_born' or 'year_vac'."))
      }
    }
    else if (year_vac == -1 & year_born != -1) {
      year_vac <- year_born + agevac
    }
    else if (year_vac != -1 & year_born == -1) {
      year_born <- year_vac - agevac
    }
    else if (year_vac == -1 & year_born == -1) {
      year_vac <- as.numeric(format(Sys.time(), format = "%Y"))
      year_born <- year_vac - agevac
    }
    ages <- as.numeric(colnames(data.incidence)[!grepl("\\D",
                                                       colnames(data.incidence))])
    ages <- ages[!is.na(ages)]
    if ((country_iso3 %in% c("XK")) | ((country_iso3 == "PSE") &
                                       (canc.inc == "2012"))) {
      proxy <- TRUE
      country_iso3 <- switch(country_iso3, XK = "ALB", PSE = "JOR",
                             country_iso3)
    }
    else {
      proxy <- FALSE
    }
    
    # NEW: adjustment for hpv1618_prop
    #----------------------------------
    if (vaccine == "4vHPV") {
      p1618 <- if (hpv1618_prop!=-1) hpv1618_prop/100 else data.hpv_distribution[iso3 == country_iso3, hpv_4v]/100 
    }
    else if (vaccine == "9vHPV") {
      p1618 <- if (hpv1618_prop!=-1) hpv1618_prop/100 else data.hpv_distribution[iso3 == country_iso3, hpv_9v]/100
    }
    #----------------------------------
    
    if (canc.inc == "2020") {
      inc <- unlist(data.incidence2020[iso3 == country_iso3,
                                       as.character(ages), with = F], use.names = F) * p1618
      mort.cecx <- unlist(data.mortcecx2020[iso3 == country_iso3,
                                            as.character(ages), with = F], use.names = F) * p1618
      cecx_5y_prev <- unlist(data.cecx_5y_prevalence2020[iso3 ==
                                                           country_iso3, as.character(ages), with = F], use.names = F) *
        p1618
    }
    else if (canc.inc == "2018") {
      inc <- unlist(data.incidence[iso3 == country_iso3, as.character(ages),
                                   with = F], use.names = F) * p1618
      mort.cecx <- unlist(data.mortcecx[iso3 == country_iso3,
                                        as.character(ages), with = F], use.names = F) * p1618
      cecx_5y_prev <- unlist(data.cecx_5y_prevalence[iso3 ==
                                                       country_iso3, as.character(ages), with = F], use.names = F) *
        p1618
    }
    else if (canc.inc == "2012") {
      inc <- unlist(data.incidence2012[iso3 == country_iso3,
                                       as.character(ages), with = F], use.names = F) * p1618
      mort.cecx <- unlist(data.mortcecx2012[iso3 == country_iso3,
                                            as.character(ages), with = F], use.names = F) * p1618
      cecx_5y_prev <- unlist(data.cecx_5y_prevalence[iso3 ==
                                                       country_iso3, as.character(ages), with = F], use.names = F) *
        p1618
      cecx_5y_prev <- cecx_5y_prev * 0
    }
    else if (canc.inc == "2008") {
    }
    inc[which(is.na(inc))] <- 0
    mort.cecx[which(is.na(mort.cecx))] <- 0
    cecx_5y_prev[which(is.na(cecx_5y_prev))] <- 0
    if (proxy) {
      proxy <- FALSE
      country_iso3 <- switch(country_iso3, ALB = "XK", JOR = "PSE",
                             country_iso3)
    }
    if (cohort == -1) {
      if ((year_born + agecohort) <= 2100) {
        cohort <- data.pop[country_code == country_iso3 &
                             year == (year_born + agecohort) & age_from ==
                             agecohort, value]
      }
      else {
        cohort <- data.pop[country_code == country_iso3 &
                             year == 2100 & age_from == agecohort, value]
      }
      if (length(cohort) == 0) {
        cohort <- 1
      }
      else if (proxy) {
        cohort <- switch(country_iso3, ALB = cohort * 1824000/2774000,
                         JOR = cohort * 4170000/6459000, cohort)
      }
    }
    if ((country_iso3 %in% c("XK", "PSE")) & (unwpp_mortality ==
                                              FALSE)) {
      proxy <- TRUE
      country_iso3 <- switch(country_iso3, XK = "ALB", PSE = "JOR",
                             country_iso3)
    }
    else {
      proxy <- FALSE
    }
    if (!unwpp_mortality) {
      mort.all <- unlist(data.mortall[iso3 == country_iso3,
                                      as.character(ages), with = F], use.names = F)
      lifetab <- lifeTable(qx = mort.all, agecohort)
    }
    else {
      mx <- numeric(length(ages))
      for (a in ages) {
        if (year_born + a > max(data.mortall.unwpp.nqx[country_code ==
                                                       country_iso3, year])) {
          lookup.yr <- max(data.mortall.unwpp.nqx[country_code ==
                                                    country_iso3, year])
        }
        else if (year_born + a < min(data.mortall.unwpp.nqx[country_code ==
                                                            country_iso3, year])) {
          lookup.yr <- min(data.mortall.unwpp.nqx[country_code ==
                                                    country_iso3, year])
        }
        else {
          lookup.yr <- year_born + a
        }
        mortality <- data.mortall.unwpp.nqx[(country_code ==
                                               country_iso3) & (age_from <= a) & (age_to >=
                                                                                    a) & (year - (lookup.yr) < 1) & (year - (lookup.yr) >
                                                                                                                       -5), list(value, age_from, age_to)]
        if (nrow(mortality) < 1) {
          mortality <- 1
        }
        else {
          age_interval <- (mortality$age_to - mortality$age_from +
                             1)
          mortality <- (mortality$value/(1 - 0.5 * mortality$value))/age_interval
        }
        mx[which(ages == a)] <- mortality
      }
      lifetab <- lifeTable(mx = mx, agecohort = agecohort)
    }
    if (proxy) {
      proxy <- FALSE
      country_iso3 <- switch(country_iso3, ALB = "XK", JOR = "PSE",
                             country_iso3)
    }
    if (country_iso3 %in% c("XK", "PSE")) {
      proxy <- TRUE
      country_iso3 <- switch(country_iso3, XK = "ALB", PSE = "JOR",
                             country_iso3)
    }
    else {
      proxy <- FALSE
    }

    # NEW: adjustment for vac_price & vac_delivery
    #----------------------------------------------
    cost_price        <- if (vac_price!=-1)    vac_price    else monetary_to_number(data.global[iso3 == country_iso3,`Vaccine price (USD) [4]`])
    cost_delivery     <- if (vac_delivery!=-1) vac_delivery else monetary_to_number(data.global[iso3 == country_iso3, `Vaccine delivery/ operational/ admin costs (USD) [5]`])

    dose_factor   <- if (isTRUE(one_dose == 1)) 0.5 else 1.0
    cost_price    <- cost_price    * dose_factor
    cost_delivery <- cost_delivery * dose_factor
    
    cost.vac <- cost_price + cost_delivery

    # NEW: adjustment for cecx_cost
    #-----------------------------------------
    if (canc.cost == "unadj"  ) {
      cost.canc <- if (cecx_cost!=-1)  cecx_cost  else  data.costcecx[iso3 == country_iso3, cancer_cost]
    }
    else if (canc.cost == "adj" ) {
      cost.canc <- if (cecx_cost!=-1)  cecx_cost  else  data.costcecx[iso3 == country_iso3, cancer_cost_adj]
    }
    #-----------------------------------------
    
    if ((length(cost.vac) == 0) & (analyseCosts == FALSE)) {
      cost.vac <- 0
    }
    if ((length(cost.canc) == 0) & (analyseCosts == FALSE)) {
      cost.canc <- 0
    }
    if (proxy) {
      proxy <- FALSE
      country_iso3 <- switch(country_iso3, ALB = "XK", JOR = "PSE",
                             country_iso3)
    }
    if (is.list(psadat)) {
      if ("incidence" %in% names(psadat)) {
        inc <- inc * psadat[["incidence"]]
      }
      if ("mortality" %in% names(psadat)) {
        mort.cecx <- mort.cecx * psadat[["mortality"]]
      }
      if ("vaccine_efficacy" %in% names(psadat)) {
        vaceff <- vaceff * psadat[["vaccine_efficacy"]]
      }
      if ("vaccine_cost" %in% names(psadat)) {
        cost.vac <- cost.vac * psadat[["vaccine_cost"]]
      }
      if ("cancer_cost" %in% names(psadat)) {
        cost.canc <- cost.canc * psadat[["cancer_cost"]]
      }
      if ("discounting_cost" %in% names(psadat)) {
        disc.cost <- disc.cost * psadat[["discounting_cost"]]
      }
      if ("discounting_ben" %in% names(psadat)) {
        disc.ben <- disc.ben * psadat[["discounting_ben"]]
      }
    }
    result_cohort <- new_RunCohort(lifetab = lifetab, cohort = cohort,
                               incidence = inc, mortality_cecx = mort.cecx, prevalence = cecx_5y_prev,
                               agevac = agevac, coverage = cov, campaigns = campaigns,
                               vaccine_efficacy_nosexdebut = vaceff_beforesexdebut,
                               vaccine_efficacy_sexdebut = vaceff_aftersexdebut, cost_cancer = cost.canc,
                               discounting = discounting, disc.cost = disc.cost, disc.ben = disc.ben,
                               country_iso3 = country_iso3, run_country = TRUE, disability.weights = disability.weights,
                               
                               # NEW: adjustment for dw
                               #-------------------
                               dw_dx   = dw_dx,
                               dw_seq  = dw_seq,
                               dw_term = dw_term
                               )
    
    # NEW: adjustment for ce_threshold & ce_threshold_gdp_mult
    #----------------------------------------------------------
    if (analyseCosts) {
      gdp_per_capita <- wb(country = country_iso3, indicator = wb.indicator,
                           startdate = wb.year, enddate = wb.year)$value
      
      # If no data (numeric(0)), use 0
      if (length(gdp_per_capita) == 0) {
        gdp_per_capita <- 0
      }
      
      # Run PRIME's cost analysis
      out <- analyseCosts_breakdown(results              = result_cohort,
                                    vaccine_cost_price   = cost_price,     
                                    vaccine_cost_delivery= cost_delivery, 
                                    gdp_per_capita       = gdp_per_capita)
      
      # Work on a plain data.frame to keep indexing simple
      is_dt <- data.table::is.data.table(out)
      df <- if (is_dt) as.data.frame(out) else out
      
      # Locate the Cost/DALY row
      i <- match("Cost/DALY prevented", df$variable)
      if (!is.na(i)) {
        cols <- setdiff(names(df), "variable")
        vals <- as.numeric(df[i, cols, drop = FALSE])
        
        # Resolve thresholds (-1 means not provided)
        thr_abs <- if (!is.null(ce_threshold) && ce_threshold != -1) as.numeric(ce_threshold) else NA_real_
        thr_gdp <- if (!is.null(ce_threshold_gdp_mult) && ce_threshold_gdp_mult != -1)
          as.numeric(ce_threshold_gdp_mult) * as.numeric(gdp_per_capita) else NA_real_
        
        # Create a new 1/0 decision row
        make_row <- function(df, label, thr, cols, vals) {
          if (is.na(thr)) return(NULL)
          new <- df[0, , drop = FALSE]   # empty row with same types/cols
          new[1, ] <- NA
          new$variable[1] <- label
          new[1, cols] <- as.numeric(vals < thr)
          new
        }
        
        r_abs <- make_row(df, sprintf("CE at abs threshold (%.2f)?", thr_abs), thr_abs, cols, vals)
        r_gdp <- make_row(
          df,
          sprintf("CE at %.2fx GDP/capita (%.2f)?", ce_threshold_gdp_mult, thr_gdp),
          thr_gdp, cols, vals
        )
        
        add <- Filter(Negate(is.null), list(r_abs, r_gdp))
        if (length(add)) df <- do.call(rbind, list(df, add[[1]], if (length(add) > 1) add[[2]] else NULL))
      } else {
        warning("Could not locate 'Cost/DALY prevented' row in output.")
      }
      
      # Return with original class
      return(if (is_dt) data.table::as.data.table(df) else df)
      #---------------------------------------------------------
    }
    else {
      return(result_cohort)
    }
  }

  

