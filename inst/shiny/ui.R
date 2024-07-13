ui = dashboardPage(
  dashboardHeader(title = "PhenotypeR"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Background",
        tabName = "background"),
      menuItem(
        text = "Databases",
        tabName = "cdm_summary"),
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
          tabName = "orphan_concepts"
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
          text = "Cohort timinig",
          tabName = "cohort_timining"
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
      # code_counts ----
      tabItem(
        tabName = "orphan_codes",
        gt_output("gt_orphan_codes") %>%
          withSpinner()
      ),
      # Cohort overlap ----
      tabItem(
        tabName = "cohort_overlap"
      )
      # end ----
    )
  )
)
