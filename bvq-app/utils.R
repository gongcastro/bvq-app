# retrieve BVQ data
get_bvq <- function(...) {
    participants <- bvq_participants()
    responses <- bvq_responses(participants, ...)
    logs <- bvq_logs(participants, responses)
    
    bvq_data <- list(participants, responses, logs)
    names(bvq_data) <- c("participants", "responses", "logs")
    attr(bvq_data, "updated") <- Sys.time()

    return(bvq_data)
}
