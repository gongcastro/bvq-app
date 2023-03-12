server <- function(input, output) {
    
    output$responses_age_plot <- renderPlot({
        participants %>%
            filter(between(age, input$participants_age[1], input$participants_age[2]),
                   between(time_stamp, input$participants_time_stamp[1], input$participants_time_stamp[2]),
                   lp %in% input$participants_lp) %>%
            mutate(age = floor(age)) %>%
            count(age) %>%
            ggplot(aes(age, n)) +
            geom_col(fill = clrs[4], colour = "white") +
            geom_text(aes(label = n), size = 3, position = position_nudge(y = 1)) +
            scale_colour_manual(values = clrs) +
            theme(panel.grid.major.x = element_blank())
    })
    
    output$responses_date_plot <- renderPlot({
        participants %>%
            filter(between(age, input$participants_age[1], input$participants_age[2]),
                   between(time_stamp, input$participants_time_stamp[1], input$participants_time_stamp[2]),
                   lp %in% input$participants_lp) %>%
            count(time_stamp, lp) %>%
            group_by(lp) %>%
            mutate(n = cumsum(n)) %>%
            ungroup() %>%
            ggplot(aes(time_stamp, n, colour = lp)) +
            geom_line(aes(group = lp), linewidth = 1) +
            scale_colour_manual(values = clrs[c(1, 4, 5)])
    }, res = 100)
    
    output$items_plot_n_phon <- renderPlot({
        items %>%
            filter(between(lv, input$items_lv[1], input$items_lv[2]),
                   class %in% input$items_class,
                   between(n_phon, input$items_n_phon[1], input$items_n_phon[2]),
                   between(n_syll, input$items_n_syll[1], input$items_n_syll[2]),
                   semantic_category %in% input$items_semantic_category) |>
            select(te, language, n_phon) |> 
            add_count(language, name = "n_total") |> 
            count(language, n_phon, n_total) |> 
            ungroup() |> 
            ggplot(aes(n_phon, n/n_total, fill = language)) +
            facet_wrap(~ language) +
            geom_col(colour = "white") +
            geom_text(aes(label = n),
                      position = position_nudge(y = 0.05),
                      size = 2.5) +
            labs(x = "Length (phonemes)",
                 y = "Number of words",
                 colour = "Language",
                 fill = "Language") +
            scale_colour_manual(values = clrs[c(1, 4)]) +
            scale_fill_manual(values = clrs[c(1, 4)]) +
            scale_x_continuous(breaks = 1:max(items$n_phon)) +
            scale_y_continuous(labels = percent) +
            theme(legend.position = "none")
        
    }, res = 100)
    
    output$items_plot_n_syll <- renderPlot({
        items %>%
            filter(between(lv, input$items_lv[1], input$items_lv[2]),
                   class %in% input$items_class,
                   between(n_phon, input$items_n_phon[1], input$items_n_phon[2]),
                   between(n_syll, input$items_n_syll[1], input$items_n_syll[2]),
                   semantic_category %in% input$items_semantic_category) |>
            select(te, language, n_syll) |> 
            add_count(language, name = "n_total") |> 
            count(language, n_syll, n_total) |> 
            ungroup() |> 
            ggplot(aes(n_syll, n/n_total, fill = language)) +
            facet_wrap(~ language) +
            geom_col(colour = "white") +
            geom_text(aes(label = n),
                      position = position_nudge(y = 0.05),
                      size = 2.5) +
            labs(x = "Number of syllables",
                 y = "Number of words",
                 colour = "Language",
                 fill = "Language") +
            scale_colour_manual(values = clrs[c(1, 4)]) +
            scale_fill_manual(values = clrs[c(1, 4)]) +
            scale_x_continuous(breaks = 1:max(items$n_syll)) +
            scale_y_continuous(labels = percent) +
            theme(legend.position = "none")
        
    }, res = 100)
    
    output$items_plot_lv <- renderPlot({
        items %>%
            filter(between(lv, input$items_lv[1], input$items_lv[2]),
                   class %in% input$items_class,
                   semantic_category %in% input$items_semantic_category) |>
            select(te, lv) |> 
            ggplot(aes(lv)) +
            stat_slab(colour = "white",
                      fill = clrs[2]) +
            labs(x = "Cognateness (Levenshtein similarity)",
                 y = "Proportion of words") +
            scale_x_continuous(labels = percent) +
            scale_y_continuous(labels = percent) +
            theme(legend.position = "none")
        
    }, res = 100)
    
    output$items_plot_class <- renderPlot({
        items %>%
            filter(between(lv, input$items_lv[1], input$items_lv[2]),
                   class %in% input$items_class,
                   between(n_phon, input$items_n_phon[1], input$items_n_phon[2]),
                   between(n_syll, input$items_n_syll[1], input$items_n_syll[2]),
                   semantic_category %in% input$items_semantic_category) |>
            select(te, class) |> 
            add_count(name = "n_total") |>
            count(class, n_total) |> 
            ggplot(aes(class, n/n_total)) +
            geom_col(colour = "white",
                     fill = clrs[5]) +
            geom_text(aes(label = n),
                      position = position_nudge(y = 0.05),
                      size = 2.5) +
            labs(x = "Grammatical class",
                 y = "Proportion of words") +
            scale_y_continuous(labels = percent) +
            theme(legend.position = "none",
                  axis.title.x = element_blank())
        
    }, res = 100)
    
    output$items_plot_semantic_category <- renderPlot({
        items %>%
            filter(between(lv, input$items_lv[1], input$items_lv[2]),
                   class %in% input$items_class,
                   between(n_phon, input$items_n_phon[1], input$items_n_phon[2]),
                   between(n_syll, input$items_n_syll[1], input$items_n_syll[2]),
                   semantic_category %in% input$items_semantic_category) |>
            select(te, class, semantic_category) |> 
            add_count(name = "n_total") |>
            count(semantic_category, class, n_total) |> 
            mutate(prop = n/n_total) |> 
            ggplot(aes(reorder(semantic_category, desc(prop)), prop,
                       fill = class)) +
            geom_col() +
            geom_text(aes(label = n),
                      position = position_nudge(y = 0.01),
                      size = 2.5) +
            labs(x = "Semantic category",
                 y = "Proportion of words") +
        scale_x_discrete(labels = label_wrap(10)) + 
            scale_y_continuous(labels = percent) +
            scale_fill_manual(values = clrs[c(1, 3, 5)]) +
            theme(legend.position = "left",
                  legend.justification = c(1, 0),
                  legend.title = element_blank(),
                  panel.grid = element_blank(),
                  panel.grid.major.y = element_line(colour = "grey",
                                                    linetype = "dotted"),
                  axis.title.x = element_blank())
        
    }, res = 100)
    
    output$items_table <- renderDT({
        items %>%
            filter(between(lv, input$items_lv[1], input$items_lv[2]),
                   class %in% input$items_class,
                   between(n_phon, input$items_n_phon[1], input$items_n_phon[2]),
                   between(n_syll, input$items_n_syll[1], input$items_n_syll[2]),
                   semantic_category %in% input$items_semantic_category) |>
            select(te, language, label, ipa, lv, class, semantic_category) |> 
            mutate(label = paste0(label, " (/", ipa, "/)")) |> 
            pivot_wider(id_cols = c(te, class, semantic_category, lv),
                        names_from = language,
                        values_from = label,
                        names_repair = make_clean_names) |> 
            drop_na() |> 
            relocate(te,
                     class,
                     semantic_category,
                     ends_with("_catalan"), 
                     ends_with("_spanish"), 
                     class, 
                     semantic_category) |> 
            datatable(rownames = FALSE,
                      selection = "single",
                      filter = "top",
                      colnames = c("TE" = 1,
                                   "Class" = 2,
                                   "Semantic" = 3,
                                   "Cognateness" = 4,
                                   "Catalan" = 5,
                                   "Spanish" = 6),
                      extensions = "ColReorder",
                      options = list(lengthMenu = seq(10, 100, 10),
                                     colReorder = TRUE,
                                     autoWidth = TRUE,
                                     columnDefs = list(list(width = "25px", targets = c(0, 1, 3)),
                                                       list(width = "100px", targets = 2),
                                                       list(width = "300px", targets = 4:5)))) |> 
            formatPercentage("Cognateness", 1)
    })
    
    output$model_draws <- renderPlot({
        posterior |> 
            collect() |> 
            ggplot(aes(.value/4, .variable_name)) +
            geom_vline(xintercept = 0,
                       colour = "grey",
                       linetype = "dotted") +
            stat_slab(colour = "white",
                      fill = clrs[1],
                      linewidth = 0.75) +
            stat_interval(position = position_nudge(y = -0.2),
                          .width = c(0.95, 0.89, 0.67, 0.50),
                          linewidth = 2) +
            labs(x = "Posterior regression coefficient",
                 y = "Predictor",
                 colour = "Confidence level") +
            scale_colour_manual(values = rev(clrs[c(2, 3, 4, 5)])) +
            scale_x_continuous(labels = percent) +
            theme(axis.title = element_blank(),
                  legend.position = "top")
    }, res = 100)
    
    output$trajectories_plot <- renderPlot({
        rescale_age <- function(x){
            age_sd <- sd(pull(responses, age, as_vector = TRUE))
            age_mean <- mean(pull(responses, age, as_vector = TRUE))
            x * age_sd + age_mean
        }
        
        predictions |> 
            filter(lp %in% input$predictions_lp,
                   dominance %in% input$predictions_dominance,
                   .category %in% input$predictions_category) |>
            collect() |> 
            sample_draws(input$trajectories_ndraws) |>
            mutate(age = rescale_age(age_std)) |> 
            filter(between(age,
                           input$predictions_age[1], 
                           input$predictions_age[2])) |> 
            ggplot(aes(age, .value, colour = interaction(dominance, lp, sep = " - "))) +
            facet_wrap(~ .category) +
            {
                if (input$predictions_uncertainty) {
                    geom_line(aes(group =  interaction(.draw, dominance, lp, 
                                                       sep = " - ")),
                              alpha = 1/20,
                              linewidth = 1) 
                }
            } +
            {
                if (input$predictions_summary != "none") {
                    stat_summary(geom = "line",
                                 fun = input$predictions_summary,
                                 linewidth = 1)
                }
            }+
            labs(x = "Age (months)",
                 y = "P(acquisition|model)",
                 colour = "Dominance - LP") +
            scale_colour_manual(values = rev(clrs[c(2, 3, 4, 5)])) +
            scale_y_continuous(labels = percent) +
            theme(legend.position = "top",
                  legend.title = element_blank(),
                  legend.key.width = unit(1, "cm"))
    }, res = 100)
    
    output$trajectories_plot_te <- renderPlot({
        rescale_age <- function(x){
            age_sd <- sd(pull(responses, age, as_vector = TRUE))
            age_mean <- mean(pull(responses, age, as_vector = TRUE))
            x * age_sd + age_mean
        }
        
        predictions_te |> 
            filter(lp %in% input$predictions_lp,
                   dominance %in% input$predictions_dominance,
                   .category %in% input$predictions_category,
                   te %in% input$trajectories_te_te) |>
            collect() |> 
            sample_draws(input$trajectories_te_ndraws) |>
            mutate(age = rescale_age(age_std)) |> 
            filter(between(age,
                           input$predictions_age[1],
                           input$predictions_age[2])) |>
            ggplot(aes(age, .value, colour = interaction(dominance, lp, sep = " - "))) +
            facet_wrap(~ .category) +
            {
                if (input$predictions_uncertainty) {
                    geom_line(aes(group =  interaction(.draw, dominance, lp, sep = " - ")),
                              alpha = 1/20,
                              linewidth = 1) 
                }
            } +
            {
                if (input$predictions_summary != "none") {
                    stat_summary(geom = "line",
                                 fun = input$predictions_summary,
                                 linewidth = 1)
                }
            } +
            labs(x = "Age (months)",
                 y = "P(acquisition|model)",
                 colour = "Dominance - LP") +
            scale_colour_manual(values = rev(clrs[c(2, 3, 4, 5)])) +
            scale_y_continuous(labels = percent) +
            theme(legend.title = element_blank(),
                  legend.key.width = unit(1, "cm"),
                  legend.position = "left")
    }, res = 100)
    
    output$trajectories_plot_id <- renderPlot({
        rescale_age <- function(x){
            age_sd <- sd(pull(responses, age, as_vector = TRUE))
            age_mean <- mean(pull(responses, age, as_vector = TRUE))
            x * age_sd + age_mean
        }
        
        predictions_id |> 
            filter(lp %in% input$predictions_lp,
                   dominance %in% input$predictions_dominance,
                   .category %in% input$predictions_category,
                   id %in% input$trajectories_id_id) |>
            collect() |> 
            sample_draws(input$trajectories_id_ndraws) |>
            mutate(age = rescale_age(age_std)) |> 
            filter(between(age, input$predictions_age[1], input$predictions_age[2])) |> 
            mutate(age = as.factor(floor(age))) |>   
            ggplot(aes(age, .value, fill = dominance)) +
            facet_wrap(~ .category) +
            {
                if (input$predictions_uncertainty) {
                    stat_slab(trim = FALSE,
                              scale = 0.5,
                              linewidth = 0.5,
                              adjust = 2,
                              aes(side = ifelse(dominance=="L1", "left", "right")))
                }
            } +
            {
                if (input$predictions_summary != "none") {
                    stat_summary(geom = "errorbar",
                                 width = 0.2,
                                 linewidth = 0.75,
                                 fun.data = mean_se,
                                 position = position_dodge(width = 1.5))
                }
            } +
            {
                if (input$predictions_summary != "none") {
                    stat_summary(geom = "point",
                                 size = 2,
                                 fun = mean,
                                 position = position_dodge(width = 1.5),
                                 show.legend = FALSE)
                }
            } +
            labs(x = "Age (months)",
                 y = "P(acquisition|model)",
                 colour = "Dominance - LP",
                 title = input$trajectories_id_id) +
            scale_fill_manual(values = rev(clrs[c(2, 3, 4, 5)])) +
            scale_y_continuous(labels = percent) +
            theme(legend.position = "top",
                  legend.title = element_blank(),
                  legend.key.width = unit(1, "cm"))
    }, res = 100)
}


