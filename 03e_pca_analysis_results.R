pacman::p_load(here,
               tidyverse,
               plotly, 
               MASS,
               GGally,
               scales,
               ggpubr,
               legendry,
               FactoMineR,
               factoextra,
               missMDA,
               pROC,
               janitor,
               corrplot,
               gtsummary,
               patchwork,
               gridExtra,
               gt,
               grid,
               png)

pca_analysis <- function(cty = "moz", hhmbr = "", prctl = 80, name = "daily_5"){
  
  full_names <- c("Mozambique", "India", "Pakistan", "Guatemala")
  names(full_names) <- c("moz", "ind", "pak", "gt")
  country = full_names[cty][1]
  outcomes = c("resp_outlier", "air_outlier", "ent_outlier")
  
  contacts_daily <- read.csv(paste0(here(),"/data/",cty,"/", cty, hhmbr, "_outlier_resp_air_ent.csv")) %>%
    mutate(avg_daily_resp_contacts = as.numeric(avg_daily_resp_contacts),
           avg_daily_ent_contacts = as.numeric(avg_daily_ent_contacts))%>%
    rename(sex = participant_sex,
           site = study_site) %>%
    mutate(age = factor(participant_age, levels = c("30-39y",
                                                    "<6mo",
                                                    "6-11mo",
                                                    "1-4y",
                                                    "5-9y", 
                                                    "10-19y",
                                                    "20-29y",
                                                    "40-59y",
                                                    "60+y"))) %>%
    mutate(occupation = factor(occupation, levels = c("Unemployed outside home", 
                                                      "Student",
                                                      "Semiskilled / skilled labor",
                                                      "Semiprofessional / professional"))) %>%
    mutate(hh_size_cat = factor(hh_size_cat, levels = c("[0,2]",
                                                        "(2,5]",
                                                        "(5,50]"))) %>%
    mutate(resp_outlier = case_when(prctl == 90 ~ daily_resp_q90_outlier,
                                    prctl == 80 ~ daily_resp_q80_outlier,
                                    prctl == 75 ~ daily_resp_q75_outlier,
                                    .default = daily_resp_q50_outlier)) %>%
    mutate(air_outlier = case_when(prctl == 90 ~ daily_air_q90_outlier,
                                   prctl == 80 ~ daily_air_q80_outlier,
                                   prctl == 75 ~ daily_air_q75_outlier,
                                   .default = daily_air_q50_outlier)) %>%
    mutate(ent_outlier = case_when(prctl == 90 ~ daily_ent_q90_outlier,
                                   prctl == 80 ~ daily_ent_q80_outlier,
                                   prctl == 75 ~ daily_ent_q75_outlier,
                                   .default = daily_ent_q50_outlier)) %>%
    mutate(resp_outlier = factor(resp_outlier, 
                                 levels = c(0, 1),
                                 labels = c("Non-outlier", "Outlier"))) %>%
    mutate(air_outlier = factor(air_outlier, 
                                 levels = c(0, 1),
                                 labels = c("Non-outlier", "Outlier"))) %>%
    mutate(ent_outlier = factor(ent_outlier, 
                                levels = c(0, 1),
                                labels = c("Non-outlier", "Outlier")))
  
   contacts_daily %>%
    tabyl(resp_outlier, air_outlier, ent_outlier) %>%  # 3-way cross-tab
    adorn_totals(where = c("row", "col")) 
   
   contacts_daily %>%
     count(resp_outlier, air_outlier, ent_outlier, name = "n") %>%       # count combinations
     # group_by(site) %>%                                 # group by site
     mutate(percent_within_site = 100 * n / sum(n)) %>% # compute % within each site
     ungroup()   
  
  # Select variables for FAMD
  famd_data <- contacts_daily %>% dplyr::select(age, sex, site, hh_size, occupation)
  # Recreate a clean copy
  famd_data <- contacts_daily %>% 
    dplyr::select(age, sex, site, hh_size, occupation) %>%
    droplevels() %>% 
    as.data.frame()
  
  # Ensure factors are properly encoded
  famd_data$sex <- as.factor(famd_data$sex)
  famd_data$site <- as.factor(famd_data$site)
  famd_data$occupation <- as.factor(famd_data$occupation)
  
  # Estimate number of components for imputation
  # (this prevents overfitting)
  ncp_est <- estim_ncpFAMD(famd_data)
  
  # Impute missing data if you need to:
  tryCatch(
    expr = {
      famd_imputed <- imputeFAMD(famd_data, ncp = ncp_est$ncp)
      famd_data <- famd_imputed$completeObs
    },
    error = function(e){
      print("just using regular data")
    },
    warning = function(w){
      message("just using regular data")
    },
    finally = {
      print("just using regular data")
    }
  ) 
  
  # Run FAMD on the imputed data
  pca_res <- FAMD(famd_data, graph = FALSE)
  
  # --- Step 1: Run PCA on demographic variables only ---
  # Use FAMD (Factor Analysis of Mixed Data) since predictors are mixed numeric + categorical
  # pca_res <- FAMD(contacts_daily %>% dplyr::select(age, sex, site, hh_size, occupation), graph = FALSE)
  
  # Examine variance explained
  fviz_screeplot(pca_res, addlabels = TRUE)
  
  # --- Step 2: Extract PCA coordinates ---
  pc_scores <- as.data.frame(pca_res$ind$coord)
  df_pca <- cbind(contacts_daily, pc_scores)
  
  
  # --- Step 3: Visualize individuals colored by droplet outlier status -------
    
    ind.p <- fviz_pca_ind(pca_res, geom = "point", col.ind = unlist(contacts_daily$resp_outlier), addEllipses = TRUE)
    p1 <- ggpubr::ggpar(ind.p,
                        title = "Principal Component Analysis",
                        subtitle = "Droplet Outlier Contact ~ Age + Sex + Site + HH Size + Occupation",
                        caption = "Source: factoextra",
                        xlab = "PC1", ylab = "PC2",
                        legend.title = "Outlier Contact", legend.position = "top",
                        ggtheme = theme_minimal(), palette = c("cornflowerblue", "salmon"),
                        font.main = c(20,"bold","black"),
                        font.submain = c(20, "plain","black"),
                        font.tickslab = 20
    )

    # --- Step 4: Logistic regression using PCs as predictors ---
    # Choose number of PCs explaining Variance based on results for this
    summary(pca_res)
    log_mod <- glm(resp_outlier ~ Dim.1 + Dim.2 + Dim.3 + Dim.4 + Dim.5, 
                   data = df_pca, family = binomial)
    p2 <- tbl_regression(log_mod,
                         title = "Droplet Regression using Principal Components") %>%
      as_gt()
    
    gtsave(p2, filename = "tbl_regression_output.png")
    
    # Load the image as a grob
    p2 <- rasterGrob(readPNG("tbl_regression_output.png"), interpolate = TRUE)
    
    # Now 'img_grob' can be used in grid layouts with other grobs
    # For example, to display it:
    # grid.newpage()
    # grid.draw(img_grob)
    
    toselect <- summary(log_mod)$coeff[-1,4] < 0.05 
    dims <- c(1:5)[toselect]
    
    # --- Step 5: Interpretation ---
    # - Check which PCs significantly predict outlier status
    # - Use loadings to interpret what demographic dimensions each PC represents
    p3 <- fviz_contrib(pca_res, choice = "var", axes = dims,) 
    p3 <- ggpubr::ggpar(p3, font.main = c(20,"bold","black"),
                    font.submain = c(20, "plain","black"),
                    font.tickslab = 20
    )
    
    png(paste0("figs/",cty, "_", hhmbr,"_pca_resp.png"), width=7000, height=5000, res=300)
    grid.arrange(p1, p2, p3, layout_matrix = rbind(c(1, 1), c(2, 3)))
    dev.off()
    
    # --- Step 3b: Visualize individuals colored by airborne outlier status -------
    
    ind.p <- fviz_pca_ind(pca_res, geom = "point", col.ind = unlist(contacts_daily$air_outlier), addEllipses = TRUE)
    p1 <- ggpubr::ggpar(ind.p,
                        title = "Principal Component Analysis",
                        subtitle = "Airborne Outlier Contact ~ Age + Sex + Site + HH Size + Occupation",
                        caption = "Source: factoextra",
                        xlab = "PC1", ylab = "PC2",
                        legend.title = "Outlier Contact", legend.position = "top",
                        ggtheme = theme_minimal(), palette = c("cornflowerblue", "salmon"),
                        font.main = c(20,"bold","black"),
                        font.submain = c(20, "plain","black"),
                        font.tickslab = 20
    )
    
    # --- Step 4b: Logistic regression using PCs as predictors ---
    # Choose number of PCs explaining Variance based on results for this
    summary(pca_res)
    log_mod <- glm(air_outlier ~ Dim.1 + Dim.2 + Dim.3 + Dim.4 + Dim.5, 
                   data = df_pca, family = binomial)
    p2 <- tbl_regression(log_mod,
                         title = "Airborne Regression using Principal Components") %>%
      as_gt()
    
    gtsave(p2, filename = "tbl_regression_output.png")
    
    # Load the image as a grob
    p2 <- rasterGrob(readPNG("tbl_regression_output.png"), interpolate = TRUE)
    
    # Now 'img_grob' can be used in grid layouts with other grobs
    # For example, to display it:
    # grid.newpage()
    # grid.draw(img_grob)
    
    toselect <- summary(log_mod)$coeff[-1,4] < 0.05 
    dims <- c(1:5)[toselect]
    
    # --- Step 5b: Interpretation ---
    # - Check which PCs significantly predict outlier status
    # - Use loadings to interpret what demographic dimensions each PC represents
    p3 <- fviz_contrib(pca_res, choice = "var", axes = dims,) 
    p3 <- ggpubr::ggpar(p3, font.main = c(20,"bold","black"),
                        font.submain = c(20, "plain","black"),
                        font.tickslab = 20
    )
    
    png(paste0("figs/",cty, "_", hhmbr,"_pca_air.png"), width=7000, height=5000, res=300)
    grid.arrange(p1, p2, p3, layout_matrix = rbind(c(1, 1), c(2, 3)))
    dev.off()
    
    # --- Step 3c: Visualize individuals colored by enteric outlier status -------
    
    ind.p <- fviz_pca_ind(pca_res, geom = "point", col.ind = unlist(contacts_daily$ent_outlier), addEllipses = TRUE)
    p1 <- ggpubr::ggpar(ind.p,
                        title = "Principal Component Analysis",
                        subtitle = "Enteric Outlier Contact ~ Age + Sex + Site + HH Size + Occupation",
                        caption = "Source: factoextra",
                        xlab = "PC1", ylab = "PC2",
                        legend.title = "Outlier Contact", legend.position = "top",
                        ggtheme = theme_minimal(), palette = c("cornflowerblue", "salmon"),
                        font.main = c(20,"bold","black"),
                        font.submain = c(20, "plain","black"),
                        font.tickslab = 20
    )
    
    # --- Step 4b: Logistic regression using PCs as predictors ---
    # Choose number of PCs explaining Variance based on results for this
    summary(pca_res)
    log_mod <- glm(ent_outlier ~ Dim.1 + Dim.2 + Dim.3 + Dim.4 + Dim.5, 
                   data = df_pca, family = binomial)
    p2 <- tbl_regression(log_mod,
                         title = "Enteric Regression using Principal Components") %>%
      as_gt()
    
    gtsave(p2, filename = "tbl_regression_output.png")
    
    # Load the image as a grob
    p2 <- rasterGrob(readPNG("tbl_regression_output.png"), interpolate = TRUE)
    
    # Now 'img_grob' can be used in grid layouts with other grobs
    # For example, to display it:
    # grid.newpage()
    # grid.draw(img_grob)
    
    toselect <- summary(log_mod)$coeff[-1,4] < 0.05 
    dims <- c(1:5)[toselect]
    
    # --- Step 5b: Interpretation ---
    # - Check which PCs significantly predict outlier status
    # - Use loadings to interpret what demographic dimensions each PC represents
    p3 <- fviz_contrib(pca_res, choice = "var", axes = dims,) 
    p3 <- ggpubr::ggpar(p3, font.main = c(20,"bold","black"),
                        font.submain = c(20, "plain","black"),
                        font.tickslab = 20
    )
    
    png(paste0("figs/",cty, "_", hhmbr,"_pca_ent.png"), width=7000, height=5000, res=300)
    grid.arrange(p1, p2, p3, layout_matrix = rbind(c(1, 1), c(2, 3)))
    dev.off()
    
  
}  