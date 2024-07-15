ui = dashboardPage(
  dashboardHeader(title = "PhenotypeR"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "CodelistDiagnostics",
        tabName = "codelist_diagnostics",
        menuSubItem(
          text = "Code use",
          tabName = "code_use"
        ),
        menuSubItem(
          text = "Index events",
          tabName = "index_events"
        ),
        menuSubItem(
          text = "Orphan concepts",
          tabName = "orphan_codes"
        )
      ),
      menuItem(
        text = "Cohort diagnostics",
        tabName = "cohort_diagnostics",
        menuSubItem(
          text = "Cohort counts",
          tabName = "cohort_counts"
        ),
        menuSubItem(
          text = "Cohort attrition",
          tabName = "cohort_attrition"
        ),
        menuSubItem(
          text = "Cohort overlap",
          tabName = "cohort_overlap"
        ),
        menuSubItem(
          text = "Cohort timing",
          tabName = "cohort_timing"
        ),
        menuSubItem(
          text = "Incidence",
          tabName = "incidence"
        ),
        menuSubItem(
          text = "Prevalence",
          tabName = "prevalence"
        ),
        menuSubItem(
          text = "Cohort characteristics",
          tabName = "cohort_characteristics"
        ),
        menuSubItem(
          text = "Large scale characteristics",
          tabName = "lsc"
        )
      )
    )
  ),

  # ## body ----
  dashboardBody(
    tabItems(
      # code_counts ----
      tabItem(
        tabName = "code_use",
        gt_output("gt_achilles_code_count") %>%
          withSpinner()
      ),
      # code_counts ----
      tabItem(
        tabName = "index_events",
        gt_output("gt_index_events") %>%
          withSpinner()
      ),
      # orphan codes ----
      tabItem(
        tabName = "orphan_codes",
        gt_output("gt_orphan_codes") %>%
          withSpinner()
      ),
      # Cohort counts ----
      tabItem(
        tabName = "cohort_counts",
        gt_output("gt_cohort_counts") %>%
          withSpinner()
      ),
      # Cohort attrition ----
      tabItem(
        tabName = "cohort_attrition",
          div(
            style = "display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(
              inputId = "attrition_cohort_name",
              label = "Cohort",
              choices = cohort_names,
              selected = cohort_names[1],
              options = list(`actions-box` = TRUE,
                             size = 10,
                             `selected-text-format` = "count > 3"),
              multiple = TRUE
            )
          ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table",
            gt_output("gt_cohort_attrition") %>%
              withSpinner()
            ),
          tabPanel(
            "Plot",
            grVizOutput("gg_cohort_attrition")
            )
        )
      ),
      # Cohort overlap ----
      tabItem(
        tabName = "cohort_overlap",
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table",
        gt_output("gt_cohort_overlap") %>%
          withSpinner()),
        tabPanel(
          "Plot",
          plotlyOutput("gg_cohort_overlap") %>%
            withSpinner())
        )
      ),
      # Cohort timing ----
      tabItem(
        tabName = "cohort_timing",
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table",
        gt_output("gt_cohort_timing") %>%
          withSpinner()),
        tabPanel(
          "Plot",
          plotlyOutput("gg_cohort_timing") %>%
            withSpinner())
        )
      )
      # end ----
    )
  )
)
