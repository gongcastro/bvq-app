# server
server <- function(input, output) {
    output$responses_age_plot <- renderPlot({
        bvq$logs %>%
            filter(
                between(
                    age,
                    input$participants_age[1],
                    input$participants_age[2]
                ),
                between(
                    time_stamp,
                    input$participants_time_stamp[1],
                    input$participants_time_stamp[2]
                ),
                lp %in% input$participants_lp
            ) %>%
            mutate(age = floor(age)) %>%
            count(age) %>%
            ggplot(aes(age, n)) +
            geom_col(fill = clrs[1], colour = "white") +
            geom_text(aes(label = n),
                      size = 3,
                      position = position_nudge(y = 1))
    })

    output$responses_date_plot <- renderPlot({
        bvq$logs %>%
            filter(
                between(
                    age,
                    input$participants_age[1],
                    input$participants_age[2]
                ),
                between(
                    time_stamp,
                    input$participants_time_stamp[1],
                    input$participants_time_stamp[2]
                ),
                lp %in% input$participants_lp
            ) %>%
            count(time_stamp, lp) %>%
            group_by(lp) %>%
            mutate(n = cumsum(n)) %>%
            ungroup() %>%
            ggplot(aes(time_stamp, n, colour = lp)) +
            geom_line(aes(group = lp), linewidth = 1)
    })

    output$items_table <- renderTable({
        bvq$items %>%
            distinct(te, language, .keep_all = TRUE) %>%
            # filter(language %in% input$items_language,
            #        class %in% input$items_class,
            #        semantic_category %in% inout$semantic_category,
            #        n_lemmas <= input$items_n_lemmas,
            #        is_multiword == input$items_is_multiword,
            #        between(n_phon, input$items_n_phon[1], input$items_n_phon[2])) %>%
            select(language,
                   te,
                   label,
                   ipa,
                   sampa,
                   class,
                   semantic_category) %>%
            pivot_wider(
                id_cols = c(te, class, semantic_category),
                names_from = language,
                values_from = c(label, ipa, sampa, starts_with("n_")),
                names_repair = make_clean_names
            ) %>%
            relocate(te,
                     ends_with("_catalan"),
                     ends_with("_spanish"),
                     class,
                     semantic_category)
    }, striped = TRUE, na = "--")

    output$epreds_plot <- renderPlot({
        epreds %>%
            filter(te %in% input$epreds_te,
                   exposure_std %in% input$epreds_exposure,
                   .category != "No") %>%
            collect() %>%
            pivot_wider(names_from = .category,
                        values_from = .epred) %>%
            mutate(
                exposure_std = as.factor(paste0(exposure_std, " SD")),
                Understands = `Understands` + `Understands and Says`
            ) %>%
            pivot_longer(
                c(Understands, `Understands and Says`),
                names_to = ".category",
                values_to = ".epred"
            )  %>%
            distinct(te, age_std, exposure_std, .draw, .category, .keep_all = TRUE) %>%
            ggplot(aes(age_std, .epred, colour = .category)) +
            geom_hline(
                yintercept = 0.5,
                colour = "grey",
                linetype = "dashed"
            ) +
            geom_line(aes(group = interaction(.draw, .category)),
                      linewidth = 0.5,
                      alpha = 0.2) +
            stat_summary(
                aes(group = .category),
                fun = mean,
                geom = "line",
                linewidth = 1
            ) +
            labs(
                x = "Age (months)",
                y = "P(Acquisition)",
                colour = "Category",
                fill = "Category"
            ) +
            scale_color_manual(values = clrs) +
            scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.25)) +
            theme(
                panel.grid = element_line(colour = "grey", linetype = "dotted"),
                legend.position = "top",
                legend.title = element_blank()
            )
    }, res = 100)
}

