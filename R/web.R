
#' @importFrom tibble add_row
#' @importFrom purrr map map_dbl map_dfr
plan_to_network <- function(plan) {
    id <- plan_id(plan)
    name <- plan_name(plan)
    desc <- plan_desc(plan)

    tooltip <- paste0("<p><b>", name,"</b><br>", desc, "</p>")

    deps <- plan_deps(plan)

    n <- length(deps)

    if(n == 0) {
        nodes <- data.frame(id = id, label = name, title = tooltip)
        edges <- data.frame(from = numeric(0), to = numeric(0))
        return(list(nodes, edges))
    }

    dep_ids <- map_dbl(deps, plan_id)

    ntwk_list <- map(deps, plan_to_network)

    nodes <- map_dfr(ntwk_list, function(l) l[[1]])
    edges <- map_dfr(ntwk_list, function(l) l[[2]])

    nodes <- add_row(nodes, id = id, label = name, title = tooltip)
    edges <- add_row(edges,
                     from = rep.int(id, n),
                     to   = dep_ids)
    list(nodes, edges)
}


#' @importFrom visNetwork renderVisNetwork
#' @importFrom visNetwork visNetwork visEdges
#' @importFrom visNetwork visHierarchicalLayout
#' @importFrom visNetwork visInteraction
#' @importFrom visNetwork visConfigure
#' @importFrom magrittr %>%
make_server <- function(plan) {
    ntwk <- plan_to_network(plan)

    nodes <- ntwk[[1]]
    edges <- ntwk[[2]]

    print(nodes)
    print(edges)
    function(input, output) {
        output$mynetworkid <- renderVisNetwork({
            visNetwork(nodes, edges) %>%
                visEdges(arrows = "to") %>%
                ## http://datastorm-open.github.io/visNetwork/interaction.html
                visInteraction(dragNodes = FALSE,
                               dragView = FALSE,
                               navigationButtons = TRUE,
                               keyboard = TRUE) %>%
                ## same as   visLayout(hierarchical = TRUE)
                visHierarchicalLayout() %>%
                visConfigure(enabled = TRUE)
        })
    }
}

#' @importFrom visNetwork visNetworkOutput
#' @importFrom shiny fluidPage
make_ui <- function(plan) {
    fluidPage(
        visNetworkOutput("mynetworkid")
    )
}


#' @importFrom shiny shinyApp
#' @export
plan_web <- function(plan) {
    ui <- make_ui(plan)
    server <- make_server(plan)

    shinyApp(ui = ui, server = server)
}

