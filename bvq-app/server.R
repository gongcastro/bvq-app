

server <- function(input, output) {
    
    output$responses_age_plot <- renderPlot({
        bvq$logs %>%
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
        bvq$logs %>%
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
    
    output$items_table <- renderDT({
        items %>%
            select(te, language, n_phon, lv) |> 
            left_join(distinct(bvq$pool, te, xsampa, label, semantic_category, class),
                      multiple = "first",
                      by = join_by(te)) |> 
            # filter(between(lv, input$items_lv[1], input$items_lv[2]),
            #        class %in% class %in% input$items_class,
            #        semantic_category %in% input$items_semantic_category) |> 
            mutate(label = paste0(label, " (/", xsampa, "/)")) |> 
            pivot_wider(id_cols = c(te, class, semantic_category, lv),
                        names_from = language,
                        values_from = label,
                        names_repair = make_clean_names) |> 
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
                                     colReorder = TRUE)) |> 
            formatPercentage("Cognateness", 1)
    })
    # options = list(lengthMenu = c(5, 30, 50),
    #                pageLength = 30)
}


output$responses_date_plot <- renderPlot({
    posterior$draws |> 
        ggplot(aes(.value, .variable_name)) +
        geom_slab()
        
}, res = 100)


