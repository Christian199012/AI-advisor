library(shiny)
library(bslib)
library(htmltools)
library(markdown)

# ============================================================
# MASTERDATASET – Produktion, Hälsa, Fruktsamhet, Rekrytering
# ============================================================

reference_sweden <- data.frame(
  production_system = c("Konventionell", "Ekologisk"),
  ecm = c(11400, 9800),
  milk_kg = c(10600, 9200),
  fat_pct = c(4.25, 4.35),
  protein_pct = c(3.45, 3.50),
  scc = c(200000, 210000),
  days_calving_first_insem = c(86, 88),
  days_calving_last_insem  = c(121, 124),
  calving_interval_months  = c(13.1, 13.3),
  inseminations_per_preg   = c(1.8, 1.9),
  age_first_calving_months = c(27.0, 27.5),
  percent_treated_total    = c(1.0, 1.2),
  percent_culled_fertility = c(4.94, 5.10)
)

reference_breed_extended <- data.frame(
  breed = rep(c("SLB", "SRB", "Jersey", "Korsning"), 2),
  production_system = rep(c("Konventionell", "Ekologisk"), each = 4),
  ecm = c(11600,10400,9600,10500, 10000,9000,8800,9500),
  milk_kg = c(11000,10000,8500,10200, 9500,8800,8000,9000),
  fat_pct = c(4.20,4.35,5.10,4.30, 4.30,4.40,5.20,4.40),
  protein_pct = c(3.55,3.50,3.90,3.55, 3.60,3.55,4.00,3.60),
  scc = c(210000,190000,170000,200000, 220000,200000,180000,210000),
  days_calving_first_insem = c(87,84,88,85, 90,88,92,89),
  days_calving_last_insem  = c(123,117,127,116, 128,122,132,120),
  calving_interval_months  = c(13.1,13.0,13.5,13.0, 13.3,13.2,13.7,13.2),
  inseminations_per_preg   = c(1.85,1.80,1.86,1.75, 1.95,1.90,1.92,1.85),
  age_first_calving_months = c(26.5,27.1,27.1,26.9, 27.0,27.5,27.6,27.2)
)

reference_counties <- data.frame(
  county = rep(c(
    "Stockholm","Uppsala","Södermanland","Östergötland","Jönköping",
    "Kronoberg","Kalmar","Gotland","Blekinge","Skåne","Halland",
    "Västra Götaland","Värmland","Örebro","Västmanland","Dalarna",
    "Gävleborg","Västernorrland","Jämtland","Västerbotten","Norrbotten"
  ), each = 2),
  production_system = rep(c("Konventionell","Ekologisk"), times = 21),
  ecm = c(
    11470,10100,11066,9700,10953,9600,11296,9900,11195,9800,
    11134,9750,11573,10150,11834,10400,11484,10050,11438,10000,
    11836,10350,11557,10100,11063,9700,11200,9800,11509,10000,
    11491,9950,10793,9400,10813,9450,10943,9550,11205,9800,
    10937,9600
  ),
  fat_pct = c(
    4.43,4.55,4.44,4.56,4.46,4.58,4.36,4.48,4.36,4.48,
    4.45,4.58,4.39,4.52,4.29,4.42,4.36,4.48,4.12,4.25,
    4.29,4.42,4.33,4.45,4.39,4.52,4.58,4.70,4.49,4.60,
    4.44,4.55,4.36,4.48,4.33,4.45,4.47,4.60,4.41,4.53,
    4.28,4.40
  ),
  protein_pct = c(
    3.75,3.82,3.71,3.78,3.70,3.77,3.64,3.70,3.64,3.70,
    3.69,3.75,3.67,3.73,3.62,3.68,3.74,3.80,3.54,3.62,
    3.62,3.70,3.60,3.67,3.62,3.70,3.77,3.85,3.71,3.78,
    3.67,3.74,3.64,3.70,3.61,3.67,3.71,3.78,3.70,3.77,
    3.63,3.70
  )
)

# ============================================================
# Hjälpfunktioner
# ============================================================

clamp <- function(x, low = 0, high = 100) max(low, min(high, x))

status_from_diff <- function(diff_pct, good_if_higher = TRUE) {
  if (is.na(diff_pct)) return("yellow")
  if (good_if_higher) {
    if (diff_pct >= -2) return("green")
    if (diff_pct >= -7) return("yellow")
    return("red")
  } else {
    if (diff_pct <= 0) return("green")
    if (diff_pct <= 10) return("yellow")
    return("red")
  }
}

make_bullets <- function(items) {
  if (length(items) == 0) tags$ul(tags$li("Inga punkter"))
  else tags$ul(lapply(items, tags$li))
}

# ============================================================
# AI‑liknande rådgivningstext (Växa‑stil)
# ============================================================

make_ai_text <- function(a, input) {
  txt <- c()

  txt <- c(txt, "### Sammanfattning")
  txt <- c(txt, sprintf(
    "Besättningen jämförs mot referenser för **%s**, **%s** och rasen **%s**. 
    Den totala besättningsscoren är **%d av 100**, vilket ger en tydlig bild av nuläget.",
    input$county, input$production_system, input$breed, a$herd_score
  ))

  txt <- c(txt, "")
  txt <- c(txt, "### Styrkor")
  txt <- c(txt, if (length(a$strengths)==0) "- Inga tydliga styrkor." else paste0("- ", a$strengths))

  txt <- c(txt, "")
  txt <- c(txt, "### Riskområden")
  txt <- c(txt, if (length(a$risks)==0) "- Inga större riskområden." else paste0("- ", a$risks))

  txt <- c(txt, "")
  txt <- c(txt, "### Prioriterade åtgärder")
  txt <- c(txt, if (length(a$actions)==0) "- Inga specifika åtgärder." else paste0("- ", a$actions))

  paste(txt, collapse = "\n")
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("AI Besättningsanalys"),

  sidebarLayout(
    sidebarPanel(
      selectInput("county", "Län", choices = c("Hela landet", unique(reference_counties$county))),
      selectInput("production_system", "Produktionssystem", choices = c("Konventionell", "Ekologisk")),
      selectInput("breed", "Ras", choices = c("SLB","SRB","Jersey","Korsning")),
      numericInput("herd_ecm", "ECM", 10200),
      numericInput("fat_pct", "Fetthalt", 4.2),
      numericInput("protein_pct", "Proteinhalt", 3.55),
      numericInput("heifer_ecm", "ECM förstakalvare", 8900),
      numericInput("older_ecm", "ECM äldre kor", 10900),
      numericInput("scc", "Tankcelltal", 230000),
      numericInput("calving_interval", "Kalvningsintervall", 13.6),
      numericInput("inseminations_per_preg", "Insem./dräktighet", 1.9),
      numericInput("age_first_calving", "Inkalvningsålder", 28),
      numericInput("n_cows", "Antal kor", 120),
      actionButton("analyze", "Analysera")
    ),

    mainPanel(
      h3("Resultat"),
      uiOutput("summary"),
      h3("Styrkor"), uiOutput("strengths"),
      h3("Riskområden"), uiOutput("risks"),
      h3("Åtgärder"), uiOutput("actions"),
      h3("AI‑rådgivning"), uiOutput("ai_text")
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  analysis <- eventReactive(input$analyze, {

    # Hämta referenser
    county_row <- if (input$county=="Hela landet")
      reference_sweden[reference_sweden$production_system==input$production_system,]
    else
      reference_counties[
        reference_counties$county==input$county &
          reference_counties$production_system==input$production_system,]

    breed_row <- reference_breed_extended[
      reference_breed_extended$breed==input$breed &
        reference_breed_extended$production_system==input$production_system,]

    # Skillnader
    ecm_diff_pct <- 100*(input$herd_ecm - county_row$ecm)/county_row$ecm
    fat_diff_pct <- 100*(input$fat_pct - county_row$fat_pct)/county_row$fat_pct
    protein_diff_pct <- 100*(input$protein_pct - county_row$protein_pct)/county_row$protein_pct
    heifer_gap <- input$heifer_ecm - input$older_ecm
    scc_diff_pct <- 100*(input$scc - breed_row$scc)/breed_row$scc

    # Status
    production_status <- status_from_diff(ecm_diff_pct, TRUE)
    composition_status <- if (fat_diff_pct>=-2 && protein_diff_pct>=-2) "green" else "red"
    heifer_status <- if (heifer_gap>=-800) "green" else "red"
    scc_status <- status_from_diff(scc_diff_pct, FALSE)

    # Score
    herd_score <- round(mean(c(
      clamp(100 + ecm_diff_pct*3),
      clamp(100 + (fat_diff_pct+protein_diff_pct)),
      clamp(100 - max(0,scc_diff_pct)*2),
      clamp(100 + (heifer_gap+800)/12)
    )))

    # Styrkor / risker / åtgärder
    strengths <- c()
    risks <- c()
    actions <- c()

    if (production_status=="green") strengths <- c(strengths,"Bra ECM‑nivå.")
    else risks <- c(risks,"ECM under referens.")

    if (composition_status=="green") strengths <- c(strengths,"Bra mjölksammansättning.")
    else risks <- c(risks,"Fett/protein under referens.")

    if (heifer_status=="green") strengths <- c(strengths,"Förstakalvare i bra balans.")
    else risks <- c(risks,"Förstakalvare ligger lågt.")

    if (scc_status=="green") strengths <- c(strengths,"Gynnsamt tankcelltal.")
    else risks <- c(risks,"Högt tankcelltal.")

    if ("ECM under referens." %in% risks) actions <- c(actions,"Följ upp foderstat och tidig laktation.")
    if ("Högt tankcelltal." %in% risks) actions <- c(actions,"Se över mjölkningsrutiner och hygien.")

    list(
      herd_score = herd_score,
      strengths = strengths,
      risks = risks,
      actions = actions,
      summary = sprintf("Besättningen har en total score på %d av 100.", herd_score)
    )
  })

  output$summary <- renderUI(analysis()$summary)
  output$strengths <- renderUI(make_bullets(analysis()$strengths))
  output$risks <- renderUI(make_bullets(analysis()$risks))
  output$actions <- renderUI(make_bullets(analysis()$actions))

  output$ai_text <- renderUI({
    a <- analysis()
    HTML(markdownToHTML(text = make_ai_text(a, input), fragment.only = TRUE))
  })
}

shinyApp(ui, server)
