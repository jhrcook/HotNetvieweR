###########################################
## Extracting Results from a HotNet2 Run ##
###########################################

#### ---- Subroutines ---- ####

# gets the k for a given delta
get_k_for_delta <- function(d) {
    sig_tib <- read_tsv(paste0(d, "/significance.txt"),
                        progress = FALSE, col_types = cols())
    colnames(sig_tib) <- c("size", "expected", "actual", "pval")
    return(sig_tib)
}

# finds the optimal delta value
get_best_delta <- function(dt) {
    d_best <- dt %>%
        group_by(delta) %>%
        summarise(sig_ks = sum(pval < 0.05)) %>%
        ungroup() %>%
        filter(sig_ks > 0) %>%
        arrange(desc(sig_ks), delta) %>%
        slice(1) %>%
        pull(delta)
    if (length(d_best) > 0) {
        return(d_best)
    } else {
        # if no significant deltas
        return(NA)
    }
}

# returns the best k for a delta
get_best_k <- function(dt, d) {
    k_best <- dt %>%
        filter(delta == d) %>%
        filter(pval < 0.05) %>%
        pull(size)
    return(min(k_best))
}

uuid_list <- c()
list_to_full_graph <- function(x) {
    gr_uuid <- sample(1E8, 1)
    while (gr_uuid %in% uuid_list) gr_uuid <- sample(1E8, 1)
    uuid_list <- c(uuid_list, gr_uuid)
    gr <- make_full_graph(length(x)) %>%
        as_tbl_graph(directed = FALSE) %>%
        mutate(name = unlist(x),
               uuid = gr_uuid)
    return(gr)
}

# recursively bind a list of graphs
recursively_bind_graphs <- function(gr_list) {
    if (length(gr_list) == 1) {
        # base case
        return(gr_list[[1]])
    } else {
        # recurse
        GR <- bind_graphs(gr_list[[1]], recursively_bind_graphs(gr_list[-1]))
    }
    return(GR)
}

# returns the components of the HotNet results
get_components <- function(dir, min_size) {
    comp_file <- paste0(dir, "/components.txt")
    comps <- readLines(comp_file)
    comps <- unlist(lapply(comps, str_split, pattern = "\\\t"),
                    recursive = FALSE)
    comps_gr <- map(comps, list_to_full_graph)
    Gr <- recursively_bind_graphs(comps_gr) %>%
        convert(to_undirected, .clean = TRUE) %N>%
        select(name, uuid) %E>%
        select(from, to)
    return(Gr)
}

#### ---- Main Function ---- ####

# gets the subnet from a HotNet2 run
# just pass in the directory where the results were posted
get_HN2_subnetworks <- function(hn2_dir) {
    delta_dirs <- list.files(hn2_dir,
                             pattern = "delta",
                             full.names = TRUE)
    delta_tib <- lapply(delta_dirs, get_k_for_delta)
    names(delta_tib) <- basename(delta_dirs)
    delta_tib <- as_tibble(ldply(delta_tib, data.frame, .id = "delta")) %>%
        mutate(delta = as.numeric(str_remove_all(delta, "delta\\_")))
    d <- get_best_delta(delta_tib)
    if (is.na(d)) {
        cat("\nno significant subnetworks\n")
        HNsubs <- create_empty(n = 0, directed = FALSE) %N>%
            mutate(name = "EMPTY_NODE", uuid = "EMPTY_NODE") %>%
            slice(0)
        k <- Inf
        cat("-----------------------------------\n\n")
    } else {
        k <- max(get_best_k(delta_tib, d), 2)
        d_dir <- str_subset(delta_dirs, str_sub(as.character(d), 1, 11))
        HNsubs <- get_components(d_dir, k)
        cat("\ntrial:", basename(hn2_dir), "\n")
        cat("number of nodes:", vcount(HNsubs), "\n")
        cat("number of components:", count_components(HNsubs), "\n")
        cat("-----------------------------------\n\n")
    }
    return(list(sample_name = basename(hn2_dir),
                delta = d,
                size = k,
                subnetworks = HNsubs))
}


#### ---- Example ---- ####

# coad_a146t_path <- "OUTPUT/COAD_results/coad_hint-coad_kras_a146t_hint_heat"
# coad_a146t_hn2 <- get_HN2_subnetworks(coad_a146t_path)
