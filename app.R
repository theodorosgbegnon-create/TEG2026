# ==============================================================================
# APPLICATION : ThermoTEG & Électrofiltre - Maria-Gléta 2 (Version Expert)
# ==============================================================================

library(shiny)
library(shinydashboard)
library(tidyverse)

# Références de calculs (Module TEG Industriel)
REF_TEG <- list(
  t_chaude_ref = 271.44,
  t_froide_ref = 30.0,
  delta_t_ref = 241.44,
  p_nominale_w = 14.04,
  rendement_thermique = 0.055 # 5.5% d'efficacité de conversion Seebeck
)

# ==========================================
# 2. INTERFACE UTILISATEUR (UI)
# ==========================================
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Outil d'Analyse ThermoTEG", titleWidth = 350),
  
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("⚙️ Paramètres du Site & TEG", tabName = "inputs", icon = icon("sliders-h")),
      menuItem("⚡ Impact Énergétique", tabName = "energie", icon = icon("bolt")),
      menuItem("🍃 Impact Environnemental & PM", tabName = "environnement", icon = icon("leaf")),
      menuItem("🔥 Valorisation Thermique", tabName = "thermique", icon = icon("fire")),
      menuItem("💰 Rentabilité Économique", tabName = "economie", icon = icon("money-bill-wave"))
    )
  ),
  
  dashboardBody(
    # Style CSS personnalisé pour harmoniser l'affichage
    tags$head(tags$style(HTML("
      .box-header { font-weight: bold; }
      .vbox-text { font-size: 18px !important; }
    "))),
    
    tabItems(
      # --- ONGLET 1 : CONFIGURATION ---
      tabItem(tabName = "inputs",
              fluidRow(
                box(title = "🌡️ Profil Thermique des Fumées", width = 6, status = "primary", solidHeader = TRUE,
                    numericInput("t_max", "Température MAXIMALE des fumées (°C)", value = 280, min = 0),
                    numericInput("t_moy", "Température MOYENNE des fumées (°C)", value = 230, min = 0),
                    numericInput("t_min", "Température MINIMALE des fumées (°C)", value = 170, min = 0),
                    numericInput("t_froide", "Température de la FACE FROIDE du TEG (°C)", value = 35, min = 0)
                ),
                box(title = "⚙️ Configuration de la Matrice TEG", width = 6, status = "primary", solidHeader = TRUE,
                    numericInput("num_modules", "Nombre total de modules TEG à installer :", value = 6000, min = 1),
                    numericInput("debit_fumees", "Débit volumique des fumées (Nm³/h)", value = 120000, min = 1),
                    helpText("Le débit volumique sert à calculer la masse totale de poussières et la chaleur évacuée par la cheminée.")
                )
              ),
              fluidRow(
                box(title = "💨 Mesures des Particules Fines (µg/Nm³)", width = 12, status = "warning", solidHeader = TRUE,
                    fluidRow(
                      column(6, 
                             h4(strong("📍 Mesures SUR le site (Émissions)")),
                             numericInput("pm10_site", "Concentration PM10 sur site (µg/Nm³)", value = 48000, min = 0),
                             numericInput("pm25_site", "Concentration PM2.5 sur site (µg/Nm³)", value = 18000, min = 0)
                      ),
                      column(6, 
                             h4(strong("🏡 Mesures HORS du site (Ambiant / Témoin)")),
                             numericInput("pm10_hors", "Concentration PM10 hors site (µg/Nm³)", value = 35, min = 0),
                             numericInput("pm25_hors", "Concentration PM2.5 hors site (µg/Nm³)", value = 12, min = 0)
                      )
                    )
                )
              )
      ),
      
      # --- ONGLET 2 : ENERGIE ---
      tabItem(tabName = "energie",
              fluidRow(
                box(title = "📅 Production Électrique Journalière (kWh / jour)", width = 6, status = "info", solidHeader = TRUE,
                    plotOutput("plot_elec_jour")
                ),
                box(title = "⏳ Production Électrique Annuelle (MWh / an)", width = 6, status = "info", solidHeader = TRUE,
                    plotOutput("plot_elec_an")
                )
              ),
              fluidRow(
                box(title = "💡 Note explicative", width = 12, status = "warning",
                    p("La puissance est calculée selon la loi de Seebeck quadratique proportionnelle au gradient de température (ΔT) établi entre la face chaude (fumées) et la face froide.")
                )
              )
      ),
      
      # --- ONGLET 3 : ENVIRONNEMENT ---
      tabItem(tabName = "environnement",
              fluidRow(
                valueBoxOutput("vbox_pm10_capt", width = 6),
                valueBoxOutput("vbox_pm25_capt", width = 6)
              ),
              fluidRow(
                box(title = "📊 Comparatif des Émissions : Sans Système vs Avec Système (Électrofiltre)", width = 12, status = "success", solidHeader = TRUE,
                    plotOutput("plot_pm_compare", height = "500px"),
                    br(),
                    p(strong("Hypothèses d'efficacité de l'électrofiltre alimenté par les TEG :"), "90% d'abattement pour les PM10 et 80% pour les PM2.5.")
                )
              )
      ),
      
      # --- ONGLET 4 : THERMIQUE ---
      tabItem(tabName = "thermique",
              fluidRow(
                box(title = "🔥 Flux de Chaleur Capturée et Utilisée par le Système (kW)", width = 6, status = "danger", solidHeader = TRUE,
                    plotOutput("plot_chaleur_utile")
                ),
                box(title = "📉 Chaleur Perdue/Gaspillée en l'ABSENCE du système (Scénario de Référence)", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("plot_chaleur_perdue_scenarios")
                )
              ),
              fluidRow(
                box(title = "📅 Pertes Thermiques Évitées (Énergie Cumulée)", width = 12, status = "warning", solidHeader = TRUE,
                    fluidRow(
                      column(6, h4(strong("Pertes Journalières sans TEG :")), plotOutput("plot_chaleur_perdue_jour")),
                      column(6, h4(strong("Pertes Annuelles sans TEG :")), plotOutput("plot_chaleur_perdue_an"))
                    )
                )
              )
      ),
      
      # --- ONGLET 5 : ECONOMIE ---
      tabItem(tabName = "economie",
              fluidRow(
                box(title = "💰 Saisie des Paramètres Économiques (FCFA)", width = 4, status = "danger", solidHeader = TRUE,
                    numericInput("cost_per_module", "Prix d'un SEUL module TEG (FCFA)", value = 18000, min = 0),
                    numericInput("kwh_price", "Prix d'un Kilowatt-heure électrique (FCFA/kWh)", value = 90, min = 0)
                ),
                box(title = "📈 Indicateurs Financiers Globaux", width = 8, status = "success", solidHeader = TRUE,
                    valueBoxOutput("vbox_capex_expert", width = 6),
                    valueBoxOutput("vbox_tri_expert", width = 6),
                    column(12, plotOutput("plot_cashflow_expert"))
                )
              )
      )
    )
  )
)

# ==========================================
# 3. LOGIQUE SERVEUR (SERVER)
# ==========================================
server <- function(input, output, session) {
  
  # --- CALCULS INTERNES REACTIFS ---
  calculs_base <- reactive({
    n_mods <- input$num_modules
    tf <- input$t_froide
    
    # Établissement des gradients thermiques (ΔT)
    dt_min <- max(0, input$t_min - tf)
    dt_moy <- max(0, input$t_moy - tf)
    dt_max <- max(0, input$t_max - tf)
    
    # Calcul des puissances unitaires puis globales (kW)
    p_unit <- function(dt) REF_TEG$p_nominale_w * (dt / REF_TEG$delta_t_ref)^2
    
    p_tot_min <- (p_unit(dt_min) * n_mods) / 1000
    p_tot_moy <- (p_unit(dt_moy) * n_mods) / 1000
    p_tot_max <- (p_unit(dt_max) * n_mods) / 1000
    
    # Chaleur extraite / captée (kW) = Puissance électrique / Rendement Seebeck
    q_capt_min <- p_tot_min / REF_TEG$rendement_thermique
    q_capt_moy <- p_tot_moy / REF_TEG$rendement_thermique
    q_capt_max <- p_tot_max / REF_TEG$rendement_thermique
    
    # Chaleur totale perdue à la cheminée SANS système (Hypothèse cp air = 1.05 kJ/kg.K, rho = 1.2 kg/m³)
    # Q_perdue = Debit * rho * cp * (T_fumee - T_ambiant)
    masse_fumees_kg_h <- input$debit_fumees * 1.2
    calculer_p_thermique <- function(t_fumee) (masse_fumees_kg_h * 1.05 * (t_fumee - 30)) / 3600
    
    q_perdue_brute_min <- calculer_p_thermique(input$t_min)
    q_perdue_brute_moy <- calculer_p_thermique(input$t_moy)
    q_perdue_brute_max <- calculer_p_thermique(input$t_max)
    
    list(
      p_min = p_tot_min, p_moy = p_tot_moy, p_max = p_tot_max,
      q_capt_min = q_capt_min, q_capt_moy = q_capt_moy, q_capt_max = q_capt_max,
      q_perdue_brute_min = q_perdue_brute_min, q_perdue_brute_moy = q_perdue_brute_moy, q_perdue_brute_max = q_perdue_brute_max
    )
  })
  
  # --- GRAPHIQUES ÉNERGÉTIQUES ---
  output$plot_elec_jour <- renderPlot({
    c <- calculs_base()
    df <- data.frame(
      Regime = factor(c("Minimal", "Moyen", "Maximal"), levels = c("Minimal", "Moyen", "Maximal")),
      Energie_Jour = c(c$p_min * 24, c$p_moy * 24, c$p_max * 24)
    )
    ggplot(df, aes(x = Regime, y = Energie_Jour, fill = Regime)) +
      geom_bar(stat = "identity", width = 0.5, color = "black") +
      scale_fill_manual(values = c("#9ec5fe", "#0d6efd", "#0a58ca")) +
      labs(x = "", y = "Énergie produite (kWh / jour)") + theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  output$plot_elec_an <- renderPlot({
    c <- calculs_base()
    df <- data.frame(
      Regime = factor(c("Minimal", "Moyen", "Maximal"), levels = c("Minimal", "Moyen", "Maximal")),
      Energie_An = c(c$p_min * 8760 / 1000, c$p_moy * 8760 / 1000, c$p_max * 8760 / 1000)
    )
    ggplot(df, aes(x = Regime, y = Energie_An, fill = Regime)) +
      geom_bar(stat = "identity", width = 0.5, color = "black") +
      scale_fill_manual(values = c("#e1bec7", "#d63384", "#b11d6b")) +
      labs(x = "", y = "Énergie produite (MWh / an)") + theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  # --- GRAPH ENVIRONNEMENT : PM (CORRIGÉ & SÉCURISÉ) ---
  output$plot_pm_compare <- renderPlot({
    # 1. Protection contre les valeurs vides au démarrage
    req(input$pm10_site, input$pm25_site, input$debit_fumees)
    
    debit <- as.numeric(input$debit_fumees)
    
    # Calcul des flux en grammes par heure (g/h)
    pm10_initial_g_h <- (as.numeric(input$pm10_site) * debit) / 1000000
    pm25_initial_g_h <- (as.numeric(input$pm25_site) * debit) / 1000000
    
    # 2. Construction ultra-sécurisée du DataFrame avec des Facteurs explicites
    df_pm <- data.frame(
      Particule = factor(
        c("PM10", "PM10", "PM10", "PM2.5", "PM2.5", "PM2.5"),
        levels = c("PM10", "PM2.5")
      ),
      Scenario = factor(
        c("Sans Système (Libéré)", "Avec Système (Capturé)", "Avec Système (Résiduel Rejeté)",
          "Sans Système (Libéré)", "Avec Système (Capturé)", "Avec Système (Résiduel Rejeté)"),
        levels = c("Sans Système (Libéré)", "Avec Système (Capturé)", "Avec Système (Résiduel Rejeté)")
      ),
      Valeur = c(
        pm10_initial_g_h, pm10_initial_g_h * 0.90, pm10_initial_g_h * 0.10,
        pm25_initial_g_h, pm25_initial_g_h * 0.80, pm25_initial_g_h * 0.20
      )
    )
    
    # 3. Génération du graphique avec geom_col (plus stable que geom_bar)
    ggplot(df_pm, aes(x = Particule, y = Valeur, fill = Scenario)) +
      geom_col(position = position_dodge(width = 0.75), color = "black", width = 0.7) +
      # Association explicite Couleur <-> Texte pour éviter les bugs d'affichage
      scale_fill_manual(values = c(
        "Sans Système (Libéré)"          = "#dc3545",  # Rouge
        "Avec Système (Capturé)"         = "#198754",  # Vert
        "Avec Système (Résiduel Rejeté)" = "#ffc107"   # Jaune
      )) +
      labs(
        x = "Type de Particules", 
        y = "Flux de matière particulaire (g / heure)", 
        fill = "Légende de l'Impact :"
      ) +
      theme_minimal(base_size = 14) + 
      theme(
        legend.position = "top",
        panel.grid.major.x = element_blank() # Épouse proprement le style moderne
      )
  })
  
  # --- GRAPH COUTEUR & PERTES THERMIQUES ---
  output$plot_chaleur_utile <- renderPlot({
    c <- calculs_base()
    df <- data.frame(
      Regime = factor(c("Minimal", "Moyen", "Maximal"), levels = c("Minimal", "Moyen", "Maximal")),
      Chaleur = c(c$q_capt_min, c$q_capt_moy, c$q_capt_max)
    )
    ggplot(df, aes(x = Regime, y = Chaleur, fill = Regime)) +
      geom_bar(stat = "identity", width = 0.5, fill = "#fd7e14", color = "black") +
      labs(x = "", y = "Chaleur valorisée par Seebeck (kW)") + theme_minimal(base_size = 14)
  })
  
  output$plot_chaleur_perdue_scenarios <- renderPlot({
    c <- calculs_base()
    df <- data.frame(
      Regime = factor(c("Minimal", "Moyen", "Maximal"), levels = c("Minimal", "Moyen", "Maximal")),
      Chaleur_Gaspillee = c(c$q_perdue_brute_min, c$q_perdue_brute_moy, c$q_perdue_brute_max)
    )
    ggplot(df, aes(x = Regime, y = Chaleur_Gaspillee)) +
      geom_bar(stat = "identity", width = 0.5, fill = "#6f42c1", color = "black") +
      labs(x = "", y = "Chaleur purement perdue sans système (kW)") + theme_minimal(base_size = 14)
  })
  
  output$plot_chaleur_perdue_jour <- renderPlot({
    c <- calculs_base()
    df <- data.frame(
      Statut = factor(c("Sans Système (Pertes)", "Avec Système (Pertes résiduelles)"), 
                      levels = c("Sans Système (Pertes)", "Avec Système (Pertes résiduelles)")),
      Valeur = c(c$q_perdue_brute_moy * 24, (c$q_perdue_brute_moy - c$q_capt_moy) * 24)
    )
    ggplot(df, aes(x = Statut, y = Valeur, fill = Statut)) +
      geom_bar(stat = "identity", width = 0.4, color = "black") +
      scale_fill_manual(values = c("#dc3545", "#198754")) +
      labs(x = "", y = "Énergie thermique rejetée (kWh / jour)") + 
      theme_minimal(base_size = 14) + theme(legend.position = "none")
  })
  
  output$plot_chaleur_perdue_an <- renderPlot({
    c <- calculs_base()
    df <- data.frame(
      Statut = factor(c("Sans Système (Pertes)", "Avec Système (Pertes résiduelles)"), 
                      levels = c("Sans Système (Pertes)", "Avec Système (Pertes résiduelles)")),
      Valeur = c(c$q_perdue_brute_moy * 8760 / 1000, ((c$q_perdue_brute_moy - c$q_capt_moy) * 8760) / 1000)
    )
    ggplot(df, aes(x = Statut, y = Valeur, fill = Statut)) +
      geom_bar(stat = "identity", width = 0.4, color = "black") +
      scale_fill_manual(values = c("#dc3545", "#198754")) +
      labs(x = "", y = "Énergie thermique rejetée (MWh / an)") + 
      theme_minimal(base_size = 14) + theme(legend.position = "none")
  })
  
  # --- ÉCONOMIE EXPERT ---
  output$vbox_capex_expert <- renderValueBox({
    cost <- as.numeric(input$num_modules) * as.numeric(input$cost_per_module)
    valueBox(paste0(format(cost, big.mark=" "), " F CFA"), "Investissement Global (CAPEX)", icon = icon("wallet"), color = "red")
  })
  
  output$vbox_tri_expert <- renderValueBox({
    cost <- as.numeric(input$num_modules) * as.numeric(input$cost_per_module)
    gain_annuel <- calculs_base()$p_moy * 8760 * input$kwh_price
    tri <- if(gain_annuel > 0) cost / gain_annuel else Inf
    valueBox(paste0(round(tri, 1), " ans"), "Délai de Retour sur Investissement", icon = icon("hourglass-half"), color = "green")
  })
  
  output$plot_cashflow_expert <- renderPlot({
    cost <- as.numeric(input$num_modules) * as.numeric(input$cost_per_module)
    gain_annuel <- calculs_base()$p_moy * 8760 * input$kwh_price
    years <- 0:15
    cf <- -cost + (years * gain_annuel)
    df <- data.frame(Annee = years, CashFlow = cf)
    
    ggplot(df, aes(x = Annee, y = CashFlow)) +
      geom_line(color = "#198754", size = 1.5) + 
      geom_point(color = "#198754", size = 3) +
      geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
      scale_y_continuous(labels = function(x) paste0(format(x, big.mark=" "), " F")) +
      labs(x = "Années d'exploitation", y = "Bénéfice net cumulé (FCFA)") + 
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)