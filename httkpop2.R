
# Source code for physiology, part of EPA's HEM population generator module
# Designed and written by WGG at ICF, based on httkpop code developed at EPA 
# Last modified on September 15, 2017


gen_height_weight = function(hbw_dt,specs) {
  mean_logh <- g <- gender <- r <- reth <- height_spline <- NULL
  age_months <- mean_logbw <- weight_spline <- hw_kde <- nkde <- NULL
  id <- weight <- NULL
  logbw_resid <- height <- logh_resid <- NULL
  hbw_dt <- data.table::copy(hbw_dt)
  hbw_dt[, `:=`(age, age_years)]
  hbw_dt[, `:=`(age_years, pmin(age_years,79))]
  hbw_dt[, `:=`(age_months,pmin(age_months,79*12))]    # cap age at 79 years
  hbw_dt[, `:=`(mean_logh,  predict(spline_heightweight[g == gender & r == reth, height_spline][[1]], x = age_months)$y), by = list(gender, reth)]
  hbw_dt[, `:=`(mean_logbw, predict(spline_heightweight[g == gender & r == reth, weight_spline][[1]], x = age_months)$y), by = list(gender, reth)]
  spline_kde <- spline_heightweight[, list(g, r, hw_kde, nkde)]
  setnames(spline_kde, c("g", "r"), c("gender", "reth"))
  hbw_dt[, `:=`(id, 1:nrow(hbw_dt))]
  hbw_dt <- merge(hbw_dt, spline_kde, by = c("gender", "reth"))
  hbw_dt[, `:=`(q.nkde, get.randoms("nkde",nrow(hbw_dt),specs$seeds,specs$var.list,0))] 
  hbw_dt[, `:=`(q.hw1,  get.randoms("hw1", nrow(hbw_dt),specs$seeds,specs$var.list,0))] 
  hbw_dt[, `:=`(q.hw2,  get.randoms("hw2", nrow(hbw_dt),specs$seeds,specs$var.list,0))]
  hbw_dt[, `:=`(c("logbw_resid", "logh_resid"), as.data.frame(hw_kde[[1]]$x[sampleq(unique(nkde), hw_kde[[1]]$w, q.nkde), ] 
                                                              + bi_norm_cor(cbind(q.hw1,q.hw2), mean=c(0, 0), sigma=hw_kde[[1]]$H))), by = list(gender, reth)]
  hbw_dt[, `:=`(weight, pmin(exp(mean_logbw + logbw_resid),160))]   # cap at 160 kg
  hbw_dt[, `:=`(height, pmin(exp(mean_logh + logh_resid),225))]     # cap at 225 cm
  hbw_dt[, `:=`(id, NULL)]
  hbw_dt[, `:=`(hw_kde, NULL)]
  hbw_dt[, `:=`(nkde, NULL)]
  hbw_dt[, `:=`(q.nkde,NULL)]
  hbw_dt[, `:=`(q.hw1,NULL)]
  hbw_dt[, `:=`(q.hw2,NULL)]
  return(hbw_dt)
}


sampleq = function(x,p,q) {
  cp.x <- cumul.prob(p)
  sel.x <- 1+findInterval(q,cp.x)
  return(sel.x)
}



tissue_masses_flows = function (tmf_dt) 
{
  id <- mass_mean <- height_ref <- height <- mass_ref <- tissue <- NULL
  gender <- age_years <- age_months <- weight <- bonemass_mean <- NULL
  BSA <- mass_dist <- mass <- mass_cv <- flow_mean <- flow_ref <- NULL
  flow_frac <- flow <- flow_cv <- CO <- Adipose <- Bone <- NULL
  org_mass_sum <- Blood <- Other_mass <- Adipose_mass <- NULL
  org_flow_check <- CO_flow <- weight_adj <- BSA_adj <- NULL
  million.cells.per.gliver <- NULL
  tmf_dt <- copy(tmf_dt)
  tmf_dt[, `:=`(id, 1:nrow(tmf_dt))]
  tmp_dt <- merge(tmf_dt, mcnally_dt, by = "gender", allow.cartesian = TRUE)
  tmp_dt[, `:=`(mass_mean, tissue_scale(height_ref = height_ref, height_indiv = height, tissue_mean_ref = mass_ref))]
  tmp_dt[tissue == "Brain", `:=`(mass_mean, brain_mass(gender = gender, age_years = age_years))]
  tmp_dt[tissue == "Bone", `:=`(mass_mean, bone_mass_age(age_years = age_years, 
                                                         age_months = age_months, height = height, weight = weight, gender = gender))]
  bone_mass_mean <- tmp_dt[tissue == "Bone", list(id, mass_mean)]
  setnames(bone_mass_mean, "mass_mean", "bonemass_mean")
  tmp_dt <- merge(tmp_dt, bone_mass_mean, by = ("id"))
  tmp_dt[tissue == "Skeleton", `:=`(mass_mean, bonemass_mean/0.5)]
  tmp_dt[, `:=`(bonemass_mean, NULL)]
  rm(bone_mass_mean)
  tmp_dt[, `:=` (q.norm, get.randoms("norm",nrow(tmp_dt),g$seeds,g$var.list,0))] 
  tmp_dt[, `:=` (q.logn, get.randoms("logn",nrow(tmp_dt),g$seeds,g$var.list,0))] 
  tmp_dt[tissue == "Muscle", `:=`(mass_mean, skeletal_muscle_mass(smm = mass_mean, age_years = age_years, height = height, gender = gender))]
  tmp_dt[tissue == "Liver" & age_years <= 18, `:=`(mass_mean, liver_mass_children(height = height, weight = weight, gender = gender))]
  tmp_dt[tissue == "Kidney" & age_years <= 18, `:=`(mass_mean, kidney_mass_children(weight = weight, height = height, gender = gender))]
  tmp_dt[tissue == "Pancreas" & age_years <= 18, `:=`(mass_mean, pancreas_mass_children(height = height, weight = weight, gender = gender))]
  tmp_dt[tissue == "Spleen" & age_years <= 18, `:=`(mass_mean, spleen_mass_children(height = height, weight = weight, gender = gender))]
  tmp_dt[tissue == "Lung" & age_years <= 18, `:=`(mass_mean, lung_mass_children(height = height, weight = weight, gender = gender))]
  tmp_dt[, `:=`(BSA, body_surface_area(BW = weight, H = height, age_years = age_years))]
  tmp_dt[tissue == "Skin", `:=`(mass_mean, skin_mass_bosgra(BSA = BSA))]
  tmp_dt[tissue == "Blood", `:=`(mass_mean, blood_weight(BSA = BSA/(100^2), gender = gender))]
  tmp_dt[tissue == "Blood" & mass_mean < 0.2, `:=`(mass_mean, blood_mass_correct(blood_mass = mass_mean, age_months = age_months, 
                                                                                 age_years = age_years, gender = gender, weight = weight))]
  tmp_dt[mass_dist == "Normal", `:=`(mass, truncnorm::qtruncnorm(q.norm, a = 0, mean = mass_mean, sd = mass_cv * mass_mean))]
  tmp_dt[mass_dist == "Log-normal", `:=`(mass, exp(rnorm(q.logn, mean = log(mass_mean), sd = sqrt(log(mass_cv^2 + 1)))))]
  tmp_dt[tissue == "CO", `:=`(flow_mean, tissue_scale(height_ref = height_ref, height_indiv = height,
                                                      tissue_mean_ref = 1.05 * flow_ref) * (1 - max(0, 0.005 * (age_years - 25))))]
  CO_flow_mean <- tmp_dt[tissue == "CO", list(id, flow_mean)]
  setnames(CO_flow_mean, "flow_mean", "CO_flow_mean")
  tmp_dt <- merge(tmp_dt, CO_flow_mean, by = "id", allow.cartesian = TRUE)
  tmp_dt[tissue != "CO", `:=`(flow_mean, flow_frac * CO_flow_mean)]
  tmp_dt[, `:=` (q.flow, get.randoms("flow",nrow(tmp_dt),g$seeds,g$var.list,0))] 
  tmp_dt[tissue != "CO" & tissue != "Lung" & !is.na(flow_mean),`:=`(flow, truncnorm::qtruncnorm(q.flow, a=0, mean=flow_mean, sd=flow_cv*flow_mean))]
  tmp_dt[tissue == "Lung", `:=`(flow, flow_frac * CO_flow_mean)]
  tmp_dt[tissue == "CO", `:=`(flow, CO_flow_mean)]
  mass_cast <- data.table::dcast.data.table(tmp_dt, id ~ tissue, value.var = "mass")
  mass_cast[, `:=`(CO, NULL)]
  mass_cast[, `:=`(Adipose, NULL)]
  mass_cast[, `:=`(Bone, NULL)]
  setnames(mass_cast, names(mass_cast)[names(mass_cast) != "id"], paste(names(mass_cast)[names(mass_cast) != "id"], "mass", sep = "_"))
  mass_cast[, `:=`(org_mass_sum, Reduce("+", .SD)), .SDcols = grep(x = names(mass_cast), pattern = "mass", value = TRUE)]
  flow_cast <- data.table::dcast.data.table(tmp_dt, id ~ tissue, value.var = "flow")
  flow_cast[, `:=`(Blood, NULL)]
  flow_cast[, `:=`(Bone, NULL)]
  setnames(flow_cast, names(flow_cast)[names(flow_cast) != "id"], paste(names(flow_cast)[names(flow_cast) != "id"], "flow", sep = "_"))
  tmf_dt <- merge(tmf_dt, mass_cast, by = "id")
  tmf_dt <- merge(tmf_dt, flow_cast, by = "id")
  tmf_dt[, `:=`(Other_mass, (0.033 + 0.014) * weight)]
  tmf_dt[, `:=`(org_mass_sum, org_mass_sum + Other_mass)]
  tmf_dt[, `:=`(q.adip, get.randoms("adip",nrow(tmf_dt),g$seeds,g$var.list,0))] 
  #tmf_dt[(weight - org_mass_sum) > 1, `:=`(Adipose_mass, exp(rnorm(tmp_dt$q.adip, mean = log(weight-org_mass_sum), sd = sqrt(log(0.42^2 + 1)))))]
  tmf_dt[, `:=`(Adipose_mass, exp(rnorm(q.adip, mean = log(pmax(1,weight-org_mass_sum)), sd = sqrt(log(0.42^2 + 1)))))]
  tmf_dt[(weight - org_mass_sum) <= 1, `:=`(Adipose_mass, 0)]
  tmf_dt[, `:=`(org_flow_check, Reduce("+", .SD)), .SDcols = names(flow_cast)[!(names(flow_cast) %in% c("CO_flow", "id"))]]
  tmf_dt[, `:=`(org_flow_check, org_flow_check/CO_flow)]
  tmf_dt[, `:=`(weight_adj, org_mass_sum + Adipose_mass)]
  tmf_dt[, `:=`(BSA_adj, body_surface_area(BW = weight_adj, H = height, age_years = age_years))]
  mu <- log(10^(-0.66 * log10(tmf_dt[, age_years]) + 3.1))
  mu[tmf_dt[, age_years < 20]] <- log(10^(-0.66 * log10(19) + 3.1))
  sigma.total <- ((log(444) - log(99))/2 + (log(99) - log(23))/2)/2
  Fval <- qf(0.012/2, df1 = 1, df2 = 26, lower.tail = FALSE)
  R2 <- Fval/(1 + Fval)
  sigma <- sqrt((1 - R2) * sigma.total^2)
  tmf_dt[, `:=`(million.cells.per.gliver, exp(rnorm(n = nrow(tmf_dt), mean = mu, sd = sigma)))]
  setnames(tmf_dt, c("Kidney_mass", "Kidney_flow", "CO_flow"), c("Kidneys_mass", "Kidneys_flow", "CO"))
  tmf_dt[, `:=`(id, NULL)]
  # tmf_dt[, `:=`(org_mass_sum, NULL)]
  return(tmf_dt)
}




blood_mass_correct = function (blood_mass, age_months, age_years, gender, weight) 
{
  blood_mass[age_months < 3] <- 83.3 * weight[age_months < 3]/1000/1.06
  blood_mass[is_in_inclusive(age_months, c(3, 6))] <- 87 * weight[is_in_inclusive(age_months, c(3, 6))]/1000/1.06
  blood_mass[is_in_inclusive(age_months, c(7, 72))] <- 80 * weight[is_in_inclusive(age_months, c(7, 72))]/1000/1.06
  blood_mass[is_in_inclusive(age_months, c(73, 120))] <- 75 * weight[is_in_inclusive(age_months, c(73, 120))]/1000/1.06
  blood_mass[is_in_inclusive(age_months, c(121, 15 * 12))] <- 71 * weight[is_in_inclusive(age_months, c(121, 15 * 12))]/1000/1.06
  blood_mass[age_years > 15 & gender == "Male"] <- 71 * weight[age_years > 15 & gender == "Male"]/1000/1.06
  blood_mass[age_years > 15 & gender == "Female"] <- 70 * weight[age_years > 15 & gender == "Female"]/1000/1.06
  return(blood_mass)
}



is_in_inclusive = function (x, lims) 
{
  if (is.numeric(x)) {
    if (length(x) != 1) {
      if (is.matrix(lims)) {
        if (length(x) != nrow(lims)) {
          stop("x must either be length 1 or the same length as nrow(lims) if lims is a matrix")
        }
      }
    }
  }
  else stop("x must be a numeric vector")
  if (is.numeric(lims)) {
    if (length(lims) == 2) {
      a <- lims[1]
      b <- lims[2]
    }
    else stop("If lims is a numeric vector, it must be length 2.")
  }
  else if (is.matrix(lims)) {
    if (ncol(lims) == 2) {
      if (nrow(lims) > 0) {
        a <- lims[, 1]
        b <- lims[, 2]
      }
      else {
        return(logical(0))
      }
    }
    else stop("If lims is a matrix, it must have two columns.")
  }
  else stop("lims must be either a 2-element numeric vector or a 2-column numeric matrix")
  if (!is.numeric(a) & is.numeric(b)) {
    print("a = ")
    print(a)
    stop("The lower limit was not numeric")
  }
  if (!is.numeric(b) & is.numeric(a)) {
    print("b = ")
    print(b)
    stop("The upper limit was not numeric")
  }
  if (!is.numeric(a) & !is.numeric(b)) {
    print("a = ")
    print(a)
    print("b = ")
    print(b)
    stop("The upper and lower limits were not numeric")
  }
  return((x - a) * (b - x) >= 0)
}




tissue_scale = function (height_ref, height_indiv, tissue_mean_ref) 
{
  tissue.mean.indiv <- ((height_indiv/height_ref)^(3/4)) * tissue_mean_ref
  return(tissue.mean.indiv)
}



brain_mass = function (gender, age_years) 
{
  B <- rep(NA, length(gender))
  B[gender == "Male"] <- 0.405
  B[gender == "Female"] <- 0.373
  brain.mean.mass <- B * ((3.68 - 2.68 * exp(-age_years/0.89)) * exp(-age_years/629))
  return(brain.mean.mass)
}



bone_mass_age = function (age_years, age_months, height, weight, gender) 
{
  bmc <- rep(NA, length(age_years))
  fmale <- function(a) {
    return(0.89983 + (2.99019 - 0.89983)/(1 + exp((14.17081 - a)/1.58179)))
  }
  bmc[gender == "Male" & age_years > 1] <- fmale(a = age_years[gender == "Male" & age_years > 1])
  ffemale <- function(a) {
    return(0.74042 + (2.14976 - 0.74042)/(1 + exp((12.35466 - a)/1.3575)))
  }
  bmc[gender == "Female" & age_years > 1] <- ffemale(a = age_years[gender == "Female" & age_years > 1])
  f_inf <- function(a.m, wt, ht) {
    return((77.24 + 24.94 * wt + 0.21 * (a.m * 30.4375) - 1.889 * ht)/1000)
  }
  bmc[age_years <= 1] <- f_inf(a.m = age_months[age_years <= 1], wt = weight[age_years <= 1], ht = height[age_years <= 1])
  bmc[gender=="Female" & age_years>=50] <- bmc[gender=="Female" & age_years>=50] - 0.0056*age_years[gender=="Female" & age_years>=50]
  bmc[gender=="Male" & age_years>=50]   <- bmc[gender=="Male"   & age_years>=50] - 0.0019*age_years[gender=="Male"   & age_years>=50]
  bonemass <- bmc/0.65
  return(bonemass)
}



skeletal_muscle_mass = function (smm, age_years, height, gender) 
{
  smm[gender=="Male"   & age_years>18] <- smm[gender=="Male"   & age_years>18] - 0.001*age_years[gender=="Male"   & age_years>18]^2
  smm[gender=="Female" & age_years>18] <- smm[gender=="Female" & age_years>18] - 0.001*age_years[gender=="Female" & age_years>18]^2
  smm[gender=="Male"  & age_years<=18] <- skeletal_muscle_mass_children(gender = "Male", 
                                                                        age_years = age_years[gender == "Male" & age_years <= 18])
  smm[gender == "Female" & age_years <= 18] <- skeletal_muscle_mass_children(gender = "Female", 
                                                                             age_years = age_years[gender == "Female" & age_years <= 18])
  return(smm)
}



skeletal_muscle_mass_children = function (gender, age_years) 
{
  if (gender == "Male") {
    C1 <- 12.4
    C2 <- 11
    C3 <- 0.45
    C4 <- 19.7
    C5 <- 0.85
    C6 <- 13.7
  }
  else if (gender == "Female") {
    C1 <- 7
    C2 <- 6.5
    C3 <- 0.55
    C4 <- 13
    C5 <- 0.75
    C6 <- 11.5
  }
  smm <- (C1/(1 + (C2 * exp(-C3 * age_years)))) + (C4/(1 + exp(-C5 * (age_years - C6))))
  return(smm)
}



liver_mass_children = function (height, weight, gender) 
{
  lm <- rep(NA, length(gender))
  lm[gender == "Male"] <- (576.9 * height[gender == "Male"]/100 + 
                             8.9 * weight[gender == "Male"] - 159.7)/1000
  lm[gender == "Female"] <- (674.3 * height[gender == "Female"]/100 + 
                               6.5 * weight[gender == "Female"] - 214.4)/1000
  return(lm)
}



kidney_mass_children = function (weight, height, gender) 
{
  km <- rep(NA, length(weight))
  km[gender == "Male"] <- (10.24 * (height[gender == "Male"]/100) * sqrt(weight[gender == "Male"]) + 7.85) + 
    (9.88 * (height[gender == "Male"]/100) * sqrt(weight[gender == "Male"]) + 7.2)
  km[gender == "Female"] <- (10.65 * (height[gender == "Female"]/100) * sqrt(weight[gender == "Female"]) + 6.11) + 
    (9.88 * (height[gender == "Female"]/100) * sqrt(weight[gender == "Female"]) + 6.55)
  return(km/1000)
}



pancreas_mass_children = function (height, weight, gender) 
{
  pm <- rep(NA, length(gender))
  pm[gender == "Male"] <- (7.46 * height[gender == "Male"]/100 * sqrt(weight[gender == "Male"]) - 0.79)/1000
  pm[gender == "Female"] <- (7.92 * height[gender == "Female"]/100 * sqrt(weight[gender == "Female"]) - 2.09)/1000
  return(pm)
}



spleen_mass_children = function (height, weight, gender) 
{
  sm <- rep(NA, length(gender))
  sm[gender == "Male"] <- (8.75 * height[gender == "Male"]/100 * sqrt(weight[gender == "Male"]) + 11.06)/1000
  sm[gender == "Female"] <- (9.36 * height[gender == "Female"]/100 * sqrt(weight[gender == "Female"]) + 7.98)/1000
  return(sm)
}



lung_mass_children = function (height, weight, gender) 
{
  lm <- rep(NA, length(gender))
  lm[gender == "Male"] <- ((29.08 * height[gender == "Male"]/100 * sqrt(weight[gender == "Male"]) + 11.06) + 
                             (35.47 * height[gender == "Male"]/100 * sqrt(weight[gender == "Male"]) + 5.53))/1000
  lm[gender == "Female"] <- ((31.46 * height[gender == "Female"]/100 * sqrt(weight[gender == "Female"]) + 1.43) + 
                               (35.3 * height[gender == "Female"]/100 * sqrt(weight[gender == "Female"]) + 1.53))/1000
  return(lm)
}



body_surface_area = function (BW, H, age_years) 
{
  bsa <- rep(NA, length(BW))
  bsa[age_years >= 18] <- sqrt(BW[age_years >= 18] * H[age_years >= 18]/3600)
  bsa[age_years < 18] <- 0.024265 * BW[age_years < 18]^0.5378 * H[age_years < 18]^0.3964
  return(bsa * (100^2))
}



skin_mass_bosgra = function (BSA) 
{
  wskin <- exp(1.64 * (BSA/100^2) - 1.93)
  return(wskin)
}



blood_weight = function (BSA, gender) 
{
  bw <- rep(NA, length(gender))
  bw[gender == "Male"] <- 3.33 * BSA[gender == "Male"] - 0.81
  bw[gender == "Female"] <- 2.66 * BSA[gender == "Female"] - 0.46
  return(bw)
}


estimate_gfr = function (gfrtmp.dt) 
{
  id <- age_years <- log_serum_creat <- sc_spline <- age_months <- NULL
  sc_kde <- serum_creat <- gfr_est <- gender <- reth <- BSA_adj <- NULL
  gfrtmp.dt <- data.table::copy(gfrtmp.dt)
  gfrtmp.dt[, `:=`(id, 1:nrow(gfrtmp.dt))]
  gfrtmp.tmp <- merge(gfrtmp.dt, spline_serumcreat, by = c("gender", 
                                                           "reth"))
  if (gfrtmp.tmp[, any(age_years >= 12)]) {
    gfrtmp.tmp[age_years >= 12, `:=`(log_serum_creat, predict(sc_spline[[1]], x = age_months)$y +
                                       rfun(n = sum(age_years >= 12), fhat = sc_kde[[1]])), by = list(gender, reth)]
    gfrtmp.dt <- merge(gfrtmp.dt, gfrtmp.tmp[, list(id, log_serum_creat)], by = "id")
    gfrtmp.dt[, `:=`(id, NULL)]
    gfrtmp.dt[age_years >= 12, `:=`(serum_creat, exp(log_serum_creat))]
    gfrtmp.dt[, `:=`(log_serum_creat, NULL)]
    gfrtmp.dt[age_years >= 18, `:=`(gfr_est, ckd_epi_eq(scr = serum_creat, gender = gender, reth = reth, age_years = age_years))]
  }
  gfrtmp.dt[age_years < 18, `:=`(gfr_est, estimate_gfr_ped(BSA = BSA_adj/(100^2)))]
  return(gfrtmp.dt)
}


estimate_gfr_ped = function (BSA) 
{
  gfr <- (-6.1604 * BSA^2) + (99.054 * BSA) - 17.74
  gfr <- gfr/(BSA/1.73)
  return(gfr)
}


rfun = function (n, fhat) 
{
  tmp <- fhat$x[sample(seq_along(fhat$x), size = n, replace = TRUE, prob = fhat$w)] + fhat$h * rnorm(n = n, mean = 0, sd = 1)
  return(tmp)
}


ckd_epi_eq = function (scr, gender, reth, age_years) 
{
  kappa <- rep(NA, length(scr))
  kappa[gender == "Female"] <- 0.7
  kappa[gender == "Male"] <- 0.9
  alph <- rep(NA, length(scr))
  alph[gender == "Female"] <- -0.329
  alph[gender == "Male"] <- -0.411
  genfact <- rep(1, length(scr))
  genfact[gender == "Female"] <- 1.018
  rethfact <- rep(1, length(scr))
  rethfact[reth == "Non-Hispanic Black"] <- 1.159
  gfr.est <- 141 * pmin(scr/kappa, 1)^alph * pmax(scr/kappa, 1)^(-1.209) * 0.993^age_years * genfact * rethfact
  return(gfr.est)
}


estimate_hematocrit = function (hcttmp_dt)   {
  id <- age_years <- log_hematocrit <- hct_spline <- age_months <- NULL
  hct_kde <- hematocrit <- gender <- reth <- NULL
  hcttmp_dt <- data.table::copy(hcttmp_dt)
  hcttmp_dt[, `:=`(id, 1:nrow(hcttmp_dt))]
  hcttmp_tmp <- merge(hcttmp_dt, spline_hematocrit, by = c("gender", 
                                                           "reth"))
  if (hcttmp_tmp[, any(age_years >= 1)]) {
    hcttmp_tmp[age_years >= 1, `:=`(log_hematocrit, predict(hct_spline[[1]], x = age_months)$y + 
                                      rfun(n = sum(age_years >= 1), fhat = hct_kde[[1]])), by = list(gender, reth)]
    hcttmp_dt <- merge(hcttmp_dt, hcttmp_tmp[, list(id, log_hematocrit)], by = "id")
    hcttmp_dt[, `:=`(id, NULL)]
    hcttmp_dt[age_years >= 1, `:=`(hematocrit, exp(log_hematocrit))]
  }
  if (nrow(hcttmp_dt[age_years < 1, ]) > 0) {
    hcttmp_dt[age_years < 1, `:=`(hematocrit, hematocrit_infants(age_months = age_months))]
  }
  hcttmp_dt[, `:=`(log_hematocrit, NULL)]
  return(hcttmp_dt)
}



hematocrit_infants = function (age_months) {
  loghct <- rep(NA, length(age_months))
  if (length(loghct[age_months < 1]) > 0) {
    sig.tmp <- (log(49) - log(31))/2
    mu.tmp <- log(49) - sig.tmp
    loghct[age_months < 1] <- rnorm(n = length(loghct[age_months < 1]), mean = mu.tmp, sd = sig.tmp)
  }
  if (length(loghct[age_months >= 1 & age_months <= 6]) > 0) {
    sig.tmp <- (log(42) - log(29))/2
    mu.tmp <- log(42) - sig.tmp
    loghct[age_months>=1 & age_months<=6] <- rnorm(n=length(loghct[age_months>=1 & age_months<=6]), mean=mu.tmp, sd=sig.tmp)
  }
  if (length(loghct[age_months > 6 & age_months < 12]) > 0) {
    sig.tmp <- (log(38) - log(33))/2
    mu.tmp <- log(38) - sig.tmp
    loghct[age_months>6 & age_months<12] <- rnorm(n=length(loghct[age_months>6 & age_months<12]), mean=mu.tmp, sd=sig.tmp)
  }
  hct <- exp(loghct)
  return(hct)
}



adjust_weight = function(y) {
  adj <- y$weight / y$weight_adj
  y[, `:=`(Blood_mass, Blood_mass*adj)]
  y[, `:=`(Brain_mass, Brain_mass*adj)]
  y[, `:=`(Gonads_mass, Gonads_mass*adj)]
  y[, `:=`(Heart_mass, Heart_mass*adj)]
  y[, `:=`(Kidneys_mass, Kidneys_mass*adj)]
  y[, `:=`(Large_intestine_mass, Large_intestine_mass*adj)]
  y[, `:=`(Liver_mass, Liver_mass*adj)]
  y[, `:=`(Lung_mass, Lung_mass*adj)]
  y[, `:=`(Muscle_mass, Muscle_mass*adj)]
  y[, `:=`(Pancreas_mass, Pancreas_mass*adj)]
  y[, `:=`(Skeleton_mass, Skeleton_mass*adj)]
  y[, `:=`(Skin_mass, Skin_mass*adj)]
  y[, `:=`(Small_intestine_mass, Small_intestine_mass*adj)]
  y[, `:=`(Spleen_mass, Spleen_mass*adj)]
  y[, `:=`(Stomach_mass, Stomach_mass*adj)]
  y[, `:=`(Adipose_mass, Adipose_mass*adj)]
  y[, `:=`(Other_mass, Other_mass*adj)]
  y[, `:=`(Adipose_flow, Adipose_flow*adj)]
  y[, `:=`(Brain_flow, Brain_flow*adj)]
  y[, `:=`(Gonads_flow, Gonads_flow*adj)]
  y[, `:=`(Heart_flow, Heart_flow*adj)]
  y[, `:=`(Kidneys_flow, Kidneys_flow*adj)]
  y[, `:=`(Large_intestine_flow, Large_intestine_flow*adj)]
  y[, `:=`(Liver_flow, Liver_flow*adj)]
  y[, `:=`(Lung_flow, Lung_flow*adj)]
  y[, `:=`(Muscle_flow, Muscle_flow*adj)]
  y[, `:=`(Pancreas_flow, Pancreas_flow*adj)]
  y[, `:=`(Skeleton_flow, Skeleton_flow*adj)]
  y[, `:=`(Skin_flow, Skin_flow*adj)]
  y[, `:=`(Small_intestine_flow, Small_intestine_flow*adj)]
  y[, `:=`(Spleen_flow, Spleen_flow*adj)]
  y[, `:=`(Stomach_flow, Stomach_flow*adj)]
  return(y)
}


