# Cross-section plotting and table helpers for hydroDataBR.
# These helpers are source-neutral and deterministic.
utils::globalVariables(c(
  ".data", "station_code", "cross_section_id", "measurement_date",
  "vertex_order", "vertex_distance_m", "vertex_stage_cm", "section_label",
  "selected_section", "n_vertices", "metric_value", "metric_label",
  "cross_section_distance_span_m", "cross_section_stage_range_cm"
))
hydrodatabr_empty_cross_sections <- function() {
  list(sections = data.frame(), vertices = data.frame())
}
hydrodatabr_first_date <- function(x) {
  x <- as.Date(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(as.Date(NA))
  }
  x[[1]]
}
hydrodatabr_min_date <- function(x) {
  x <- as.Date(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(as.Date(NA))
  }
  min(x)
}
hydrodatabr_max_date <- function(x) {
  x <- as.Date(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(as.Date(NA))
  }
  max(x)
}
hydrodatabr_as_data_frame_or_empty <- function(x) {
  if (is.null(x)) {
    return(data.frame())
  }
  if (is.data.frame(x)) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  data.frame()
}
hydrodatabr_get_list_component <- function(x, candidates) {
  if (!is.list(x) || is.data.frame(x)) {
    return(NULL)
  }
  hit <- candidates[candidates %in% names(x)]
  if (length(hit) == 0) {
    return(NULL)
  }
  x[[hit[[1]]]]
}
hydrodatabr_extract_cross_sections <- function(data) {
  if (is.null(data)) {
    return(hydrodatabr_empty_cross_sections())
  }
  if (is.data.frame(data)) {
    distance_col <- hydrodatabr_first_existing_name(
      data,
      c("vertex_distance_m", "distance_m", "horizontal_distance_m", "x_m", "x")
    )
    stage_col <- hydrodatabr_first_existing_name(
      data,
      c("vertex_stage_cm", "stage_cm", "cota_cm", "elevation_cm", "y_cm", "y")
    )
    if (!is.na(distance_col) && !is.na(stage_col)) {
      return(list(sections = data.frame(), vertices = as.data.frame(data, stringsAsFactors = FALSE)))
    }
    return(list(sections = as.data.frame(data, stringsAsFactors = FALSE), vertices = data.frame()))
  }
  if (!is.list(data)) {
    return(hydrodatabr_empty_cross_sections())
  }
  if ("results" %in% names(data) && is.list(data$results)) {
    result_names <- names(data$results)
    pieces <- lapply(seq_along(data$results), function(i) {
      out <- hydrodatabr_extract_cross_sections(data$results[[i]])
      st <- if (length(result_names) >= i) result_names[[i]] else NA_character_
      if (!is.na(st) && nzchar(st)) {
        out$sections <- hydrodatabr_add_station_if_missing(out$sections, st)
        out$vertices <- hydrodatabr_add_station_if_missing(out$vertices, st)
      }
      out
    })
    return(list(
      sections = hydrodatabr_bind_rows_base(lapply(pieces, `[[`, "sections")),
      vertices = hydrodatabr_bind_rows_base(lapply(pieces, `[[`, "vertices"))
    ))
  }
  value <- hydrodatabr_get_list_component(data, c("cross_sections", "cross_section_data"))
  if (!is.null(value)) {
    return(hydrodatabr_extract_cross_sections(value))
  }
  sections <- hydrodatabr_get_list_component(data, c("sections", "cross_sections", "section_summary", "summary"))
  vertices <- hydrodatabr_get_list_component(data, c("vertices", "cross_section_vertices", "points", "profiles"))
  if (is.null(sections) && is.null(vertices)) {
    nested <- hydrodatabr_get_list_component(data, c("data"))
    if (!is.null(nested) && !identical(nested, data)) {
      return(hydrodatabr_extract_cross_sections(nested))
    }
  }
  list(
    sections = hydrodatabr_as_data_frame_or_empty(sections),
    vertices = hydrodatabr_as_data_frame_or_empty(vertices)
  )
}
hydrodatabr_cross_section_id_vector <- function(x, n) {
  out <- as.character(x)
  out[is.na(out) | !nzchar(out)] <- NA_character_
  missing <- is.na(out)
  if (any(missing)) {
    out[missing] <- paste0("section_", seq_len(sum(missing)))
  }
  if (length(out) == 0 && n > 0) {
    out <- rep("section_1", n)
  }
  out
}
hydrodatabr_prepare_cross_section_vertices <- function(cross_sections, station_code = NULL) {
  cross_sections <- hydrodatabr_extract_cross_sections(cross_sections)
  vertices <- hydrodatabr_as_data_frame_or_empty(cross_sections$vertices)
  sections <- hydrodatabr_as_data_frame_or_empty(cross_sections$sections)
  if (nrow(vertices) == 0) {
    return(data.frame())
  }
  station_col <- hydrodatabr_first_existing_name(vertices, c("station_code", "station", "codigo_estacao"))
  id_col <- hydrodatabr_first_existing_name(vertices, c("cross_section_id", "section_id", "profile_id", "perfil_id"))
  date_col <- hydrodatabr_first_existing_name(vertices, c("measurement_datetime", "measurement_date", "section_date", "survey_date", "date"))
  order_col <- hydrodatabr_first_existing_name(vertices, c("vertex_order", "point_order", "order", "sequence", "seq"))
  distance_col <- hydrodatabr_first_existing_name(vertices, c("vertex_distance_m", "distance_m", "horizontal_distance_m", "x_distance_m", "x_m", "x"))
  stage_col <- hydrodatabr_first_existing_name(vertices, c("vertex_stage_cm", "stage_cm", "cota_cm", "elevation_cm", "y_cm", "y"))
  stage_m_col <- hydrodatabr_first_existing_name(vertices, c("vertex_stage_m", "stage_m", "cota_m", "elevation_m", "y_m"))
  if (is.na(distance_col)) {
    stop("Could not identify the cross-section distance column.", call. = FALSE)
  }
  if (is.na(stage_col) && is.na(stage_m_col)) {
    stop("Could not identify the cross-section stage/elevation column.", call. = FALSE)
  }
  n <- nrow(vertices)
  out <- data.frame(
    station_code = if (!is.na(station_col)) as.character(vertices[[station_col]]) else rep(NA_character_, n),
    cross_section_id = if (!is.na(id_col)) hydrodatabr_cross_section_id_vector(vertices[[id_col]], n) else rep("section_1", n),
    measurement_date = if (!is.na(date_col)) as.Date(hydrodatabr_as_date_safe(vertices[[date_col]])) else as.Date(NA),
    vertex_order = if (!is.na(order_col)) suppressWarnings(as.numeric(vertices[[order_col]])) else seq_len(n),
    vertex_distance_m = suppressWarnings(as.numeric(vertices[[distance_col]])),
    vertex_stage_cm = if (!is.na(stage_col)) {
      suppressWarnings(as.numeric(vertices[[stage_col]]))
    } else {
      100 * suppressWarnings(as.numeric(vertices[[stage_m_col]]))
    },
    stringsAsFactors = FALSE
  )
  if (nrow(sections) > 0 && is.na(date_col)) {
    section_id_col <- hydrodatabr_first_existing_name(sections, c("cross_section_id", "section_id", "profile_id", "perfil_id"))
    section_date_col <- hydrodatabr_first_existing_name(sections, c("measurement_datetime", "measurement_date", "section_date", "survey_date", "date"))
    if (!is.na(section_id_col) && !is.na(section_date_col)) {
      section_dates <- data.frame(
        cross_section_id = as.character(sections[[section_id_col]]),
        measurement_date_join = as.Date(hydrodatabr_as_date_safe(sections[[section_date_col]])),
        stringsAsFactors = FALSE
      )
      section_dates <- section_dates[!duplicated(section_dates$cross_section_id), , drop = FALSE]
      out <- merge(out, section_dates, by = "cross_section_id", all.x = TRUE, sort = FALSE)
      out$measurement_date <- out$measurement_date_join
      out$measurement_date_join <- NULL
    }
  }
  if (!is.null(station_code)) {
    out <- out[out$station_code %in% as.character(station_code), , drop = FALSE]
  }
  out <- out[is.finite(out$vertex_distance_m) & is.finite(out$vertex_stage_cm), , drop = FALSE]
  out <- out[order(out$station_code, out$cross_section_id, out$vertex_order, out$vertex_distance_m), , drop = FALSE]
  row.names(out) <- NULL
  out
}
hydrodatabr_section_summary_from_vertices <- function(vertices) {
  if (nrow(vertices) == 0) {
    return(data.frame())
  }
  groups <- split(vertices, paste(vertices$station_code, vertices$cross_section_id, sep = "\r"), drop = TRUE)
  pieces <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    pieces[[i]] <- data.frame(
      station_code = g$station_code[[1]],
      cross_section_id = g$cross_section_id[[1]],
      measurement_date = hydrodatabr_first_date(g$measurement_date),
      n_vertices = nrow(g),
      vertex_distance_min_m = min(g$vertex_distance_m, na.rm = TRUE),
      vertex_distance_max_m = max(g$vertex_distance_m, na.rm = TRUE),
      vertex_stage_min_cm = min(g$vertex_stage_cm, na.rm = TRUE),
      vertex_stage_max_cm = max(g$vertex_stage_cm, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
  out <- hydrodatabr_bind_rows_base(pieces)
  out$cross_section_distance_span_m <- out$vertex_distance_max_m - out$vertex_distance_min_m
  out$cross_section_stage_range_cm <- out$vertex_stage_max_cm - out$vertex_stage_min_cm
  out[order(out$station_code, out$measurement_date, out$cross_section_id), , drop = FALSE]
}
hydrodatabr_prepare_cross_sections_table <- function(cross_sections, station_code = NULL) {
  cross_sections <- hydrodatabr_extract_cross_sections(cross_sections)
  vertices <- hydrodatabr_prepare_cross_section_vertices(cross_sections, station_code = station_code)
  derived <- hydrodatabr_section_summary_from_vertices(vertices)
  sections <- hydrodatabr_as_data_frame_or_empty(cross_sections$sections)
  if (nrow(sections) == 0) {
    return(derived)
  }
  station_col <- hydrodatabr_first_existing_name(sections, c("station_code", "station", "codigo_estacao"))
  id_col <- hydrodatabr_first_existing_name(sections, c("cross_section_id", "section_id", "profile_id", "perfil_id"))
  date_col <- hydrodatabr_first_existing_name(sections, c("measurement_datetime", "measurement_date", "section_date", "survey_date", "date"))
  survey_col <- hydrodatabr_first_existing_name(sections, c("survey_number", "survey", "levantamento"))
  consistency_col <- hydrodatabr_first_existing_name(sections, c("consistency_level", "consistency", "nivel_consistencia"))
  n_vertices_col <- hydrodatabr_first_existing_name(sections, c("n_vertices", "vertex_count", "n_points"))
  distance_min_col <- hydrodatabr_first_existing_name(sections, c("vertex_distance_min_m", "distance_min_m", "x_distance_min_m"))
  distance_max_col <- hydrodatabr_first_existing_name(sections, c("vertex_distance_max_m", "distance_max_m", "x_distance_max_m"))
  stage_min_col <- hydrodatabr_first_existing_name(sections, c("vertex_stage_min_cm", "stage_min_cm", "cota_min_cm"))
  stage_max_col <- hydrodatabr_first_existing_name(sections, c("vertex_stage_max_cm", "stage_max_cm", "cota_max_cm"))
  n <- nrow(sections)
  out <- data.frame(
    station_code = if (!is.na(station_col)) as.character(sections[[station_col]]) else rep(NA_character_, n),
    cross_section_id = if (!is.na(id_col)) hydrodatabr_cross_section_id_vector(sections[[id_col]], n) else paste0("section_", seq_len(n)),
    measurement_date = if (!is.na(date_col)) as.Date(hydrodatabr_as_date_safe(sections[[date_col]])) else as.Date(NA),
    survey_number = if (!is.na(survey_col)) as.character(sections[[survey_col]]) else NA_character_,
    consistency_level = if (!is.na(consistency_col)) suppressWarnings(as.integer(sections[[consistency_col]])) else NA_integer_,
    n_vertices = if (!is.na(n_vertices_col)) suppressWarnings(as.integer(sections[[n_vertices_col]])) else NA_integer_,
    vertex_distance_min_m = if (!is.na(distance_min_col)) suppressWarnings(as.numeric(sections[[distance_min_col]])) else NA_real_,
    vertex_distance_max_m = if (!is.na(distance_max_col)) suppressWarnings(as.numeric(sections[[distance_max_col]])) else NA_real_,
    vertex_stage_min_cm = if (!is.na(stage_min_col)) suppressWarnings(as.numeric(sections[[stage_min_col]])) else NA_real_,
    vertex_stage_max_cm = if (!is.na(stage_max_col)) suppressWarnings(as.numeric(sections[[stage_max_col]])) else NA_real_,
    stringsAsFactors = FALSE
  )
  if (!is.null(station_code)) {
    out <- out[out$station_code %in% as.character(station_code), , drop = FALSE]
  }
  if (nrow(derived) > 0) {
    derived_keep <- derived[, c(
      "station_code", "cross_section_id", "n_vertices", "vertex_distance_min_m",
      "vertex_distance_max_m", "vertex_stage_min_cm", "vertex_stage_max_cm",
      "cross_section_distance_span_m", "cross_section_stage_range_cm"
    ), drop = FALSE]
    names(derived_keep)[names(derived_keep) != "station_code" & names(derived_keep) != "cross_section_id"] <- paste0(
      names(derived_keep)[names(derived_keep) != "station_code" & names(derived_keep) != "cross_section_id"],
      "_from_vertices"
    )
    out <- merge(out, derived_keep, by = c("station_code", "cross_section_id"), all.x = TRUE, sort = FALSE)
    for (nm in c("n_vertices", "vertex_distance_min_m", "vertex_distance_max_m", "vertex_stage_min_cm", "vertex_stage_max_cm")) {
      derived_nm <- paste0(nm, "_from_vertices")
      if (derived_nm %in% names(out)) {
        out[[nm]][is.na(out[[nm]])] <- out[[derived_nm]][is.na(out[[nm]])]
        out[[derived_nm]] <- NULL
      }
    }
    for (nm in c("cross_section_distance_span_m", "cross_section_stage_range_cm")) {
      derived_nm <- paste0(nm, "_from_vertices")
      if (derived_nm %in% names(out)) {
        out[[nm]] <- out[[derived_nm]]
        out[[derived_nm]] <- NULL
      }
    }
  }
  if (!"cross_section_distance_span_m" %in% names(out)) {
    out$cross_section_distance_span_m <- out$vertex_distance_max_m - out$vertex_distance_min_m
  }
  if (!"cross_section_stage_range_cm" %in% names(out)) {
    out$cross_section_stage_range_cm <- out$vertex_stage_max_cm - out$vertex_stage_min_cm
  }
  out[order(out$station_code, out$measurement_date, out$cross_section_id), , drop = FALSE]
}
hydrodatabr_cross_section_summary_by_station <- function(sections) {
  if (nrow(sections) == 0) {
    return(data.frame())
  }
  groups <- split(sections, sections$station_code, drop = TRUE)
  pieces <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    pieces[[i]] <- data.frame(
      station_code = g$station_code[[1]],
      n_cross_sections = nrow(g),
      first_cross_section_date = hydrodatabr_min_date(g$measurement_date),
      last_cross_section_date = hydrodatabr_max_date(g$measurement_date),
      max_distance_span_m = suppressWarnings(max(g$cross_section_distance_span_m, na.rm = TRUE)),
      max_stage_range_cm = suppressWarnings(max(g$cross_section_stage_range_cm, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
  }
  out <- hydrodatabr_bind_rows_base(pieces)
  numeric_cols <- c("max_distance_span_m", "max_stage_range_cm")
  for (nm in numeric_cols) {
    out[[nm]][!is.finite(out[[nm]])] <- NA_real_
  }
  out[order(out$station_code), , drop = FALSE]
}
table_cross_sections <- function(cross_sections, station_code = NULL,
                                 level = c("sections", "vertices", "summary")) {
  level <- match.arg(level)
  sections <- hydrodatabr_prepare_cross_sections_table(cross_sections, station_code = station_code)
  if (identical(level, "sections")) {
    return(sections)
  }
  if (identical(level, "vertices")) {
    return(hydrodatabr_prepare_cross_section_vertices(cross_sections, station_code = station_code))
  }
  hydrodatabr_cross_section_summary_by_station(sections)
}
hydrodatabr_cross_section_station_values <- function(cross_sections) {
  cross_sections <- hydrodatabr_extract_cross_sections(cross_sections)
  vertices <- hydrodatabr_prepare_cross_section_vertices(cross_sections)
  if (nrow(vertices) > 0 && "station_code" %in% names(vertices)) {
    return(hydrodatabr_station_values(vertices))
  }
  sections <- hydrodatabr_prepare_cross_sections_table(cross_sections)
  hydrodatabr_station_values(sections)
}
hydrodatabr_filter_cross_sections <- function(cross_sections, station_code = NULL) {
  cross_sections <- hydrodatabr_extract_cross_sections(cross_sections)
  if (is.null(station_code)) {
    return(cross_sections)
  }
  list(
    sections = hydrodatabr_filter_station(cross_sections$sections, station_code),
    vertices = hydrodatabr_filter_station(cross_sections$vertices, station_code)
  )
}
hydrodatabr_select_cross_section_id <- function(sections, section_id = NULL,
                                                section_date = NULL) {
  if (!is.null(section_id)) {
    section_id <- as.character(section_id[[1]])
    if (tolower(section_id) %in% c("latest", "recent", "last")) {
      section_id <- NULL
    } else if (tolower(section_id) %in% c("first", "oldest")) {
      if (nrow(sections) == 0) {
        return(NA_character_)
      }
      dated <- sections[!is.na(sections$measurement_date), , drop = FALSE]
      if (nrow(dated) > 0) {
        return(as.character(dated$cross_section_id[which.min(dated$measurement_date)]))
      }
      return(as.character(sections$cross_section_id[[1]]))
    } else {
      return(section_id)
    }
  }
  if (!is.null(section_date)) {
    section_date <- as.Date(section_date[[1]])
    dated <- sections[!is.na(sections$measurement_date), , drop = FALSE]
    dated <- dated[dated$measurement_date == section_date, , drop = FALSE]
    if (nrow(dated) == 0) {
      stop("Selected cross-section date was not found in the section table.", call. = FALSE)
    }
    return(as.character(dated$cross_section_id[[1]]))
  }
  if (nrow(sections) == 0) {
    return(NA_character_)
  }
  dated <- sections[!is.na(sections$measurement_date), , drop = FALSE]
  if (nrow(dated) > 0) {
    return(as.character(dated$cross_section_id[which.max(dated$measurement_date)]))
  }
  as.character(sections$cross_section_id[[1]])
}
hydrodatabr_cross_section_selected_label <- function(sections, selected_id) {
  if (nrow(sections) == 0 || is.na(selected_id)) {
    return(NULL)
  }
  selected <- sections[as.character(sections$cross_section_id) %in% as.character(selected_id), , drop = FALSE]
  if (nrow(selected) == 0) {
    return(paste0("Se\u00e7\u00e3o selecionada: ", selected_id))
  }
  selected <- selected[1, , drop = FALSE]
  date_text <- if (is.na(selected$measurement_date[[1]])) {
    "sem data"
  } else {
    format(selected$measurement_date[[1]], "%Y-%m-%d")
  }
  paste0("Se\u00e7\u00e3o selecionada: ", date_text, " | ", selected$cross_section_id[[1]])
}
hydrodatabr_cross_section_highlight_color <- function() {
  hydrodatabr_plot_palette("Selecionada")[[1]]
}
hydrodatabr_cross_section_labels <- function(sections) {
  if (nrow(sections) == 0) {
    return(character(0))
  }
  date_text <- ifelse(is.na(sections$measurement_date), "Sem data", format(sections$measurement_date, "%Y-%m-%d"))
  label <- paste0(date_text, " | ", sections$cross_section_id)
  label
}
plot_cross_sections <- function(cross_sections,
                                type = c("profile", "overlay", "timeline"),
                                station_code = NULL,
                                section_id = NULL,
                                section_date = NULL,
                                show_selected_label = TRUE,
                                title = NULL,
                                subtitle = NULL,
                                base_size = 11) {
  type <- match.arg(type)
  cross_sections <- hydrodatabr_filter_cross_sections(cross_sections, station_code = station_code)
  vertices <- hydrodatabr_prepare_cross_section_vertices(cross_sections)
  sections <- hydrodatabr_prepare_cross_sections_table(cross_sections)
  if (nrow(vertices) == 0 && !identical(type, "timeline")) {
    stop("No cross-section vertex data found.", call. = FALSE)
  }
  if (nrow(sections) == 0) {
    sections <- hydrodatabr_section_summary_from_vertices(vertices)
  }
  selected_id <- hydrodatabr_select_cross_section_id(
    sections,
    section_id = section_id,
    section_date = section_date
  )
  selected_caption <- if (isTRUE(show_selected_label)) {
    hydrodatabr_cross_section_selected_label(sections, selected_id)
  } else {
    NULL
  }
  selected_color <- hydrodatabr_cross_section_highlight_color()
  if (identical(type, "profile")) {
    plot_data <- vertices[vertices$cross_section_id %in% selected_id, , drop = FALSE]
    if (nrow(plot_data) == 0) {
      stop("Selected cross section was not found in the vertex data.", call. = FALSE)
    }
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = .data[["vertex_distance_m"]], y = .data[["vertex_stage_cm"]])
    ) +
      ggplot2::geom_line(linewidth = 0.9, color = selected_color) +
      ggplot2::geom_point(size = 1.9, color = selected_color) +
      ggplot2::labs(
        x = "Dist\u00e2ncia horizontal (m)",
        y = "Cota (cm)",
        title = title,
        subtitle = subtitle,
        caption = selected_caption
      ) +
      theme_hydrodatabr(base_size = base_size)
    return(p)
  }
  if (identical(type, "overlay")) {
    plot_data <- vertices
    plot_data$selected_section <- as.character(plot_data$cross_section_id) %in% selected_id
    other <- plot_data[!plot_data$selected_section, , drop = FALSE]
    selected <- plot_data[plot_data$selected_section, , drop = FALSE]
    p <- ggplot2::ggplot() +
      ggplot2::geom_path(
        data = other,
        ggplot2::aes(
          x = .data[["vertex_distance_m"]],
          y = .data[["vertex_stage_cm"]],
          group = .data[["cross_section_id"]]
        ),
        linewidth = 0.4,
        alpha = 0.35,
        color = "grey60"
      ) +
      ggplot2::geom_path(
        data = selected,
        ggplot2::aes(
          x = .data[["vertex_distance_m"]],
          y = .data[["vertex_stage_cm"]],
          group = .data[["cross_section_id"]]
        ),
        linewidth = 1.15,
        alpha = 0.98,
        color = selected_color
      ) +
      ggplot2::geom_point(
        data = selected,
        ggplot2::aes(
          x = .data[["vertex_distance_m"]],
          y = .data[["vertex_stage_cm"]]
        ),
        size = 1.7,
        alpha = 0.9,
        color = selected_color
      ) +
      ggplot2::labs(
        x = "Dist\u00e2ncia horizontal (m)",
        y = "Cota (cm)",
        title = title,
        subtitle = subtitle,
        caption = selected_caption
      ) +
      theme_hydrodatabr(base_size = base_size) +
      ggplot2::theme(legend.position = "none")
    return(p)
  }
  if (nrow(sections) == 0) {
    stop("No cross-section section table found.", call. = FALSE)
  }
  plot_data <- sections[!is.na(sections$measurement_date), , drop = FALSE]
  if (nrow(plot_data) == 0) {
    stop("No cross-section dates found for the timeline plot.", call. = FALSE)
  }
  plot_data$selected_section <- as.character(plot_data$cross_section_id) %in% selected_id
  if (any(is.finite(plot_data$cross_section_stage_range_cm))) {
    plot_data$metric_value <- plot_data$cross_section_stage_range_cm
    y_label <- "Amplitude vertical (cm)"
  } else if (any(is.finite(plot_data$cross_section_distance_span_m))) {
    plot_data$metric_value <- plot_data$cross_section_distance_span_m
    y_label <- "Amplitude horizontal (m)"
  } else {
    plot_data$metric_value <- plot_data$n_vertices
    y_label <- "N\u00famero de v\u00e9rtices"
  }
  plot_data <- plot_data[is.finite(plot_data$metric_value), , drop = FALSE]
  if (nrow(plot_data) == 0) {
    stop("No numeric cross-section metric found for the timeline plot.", call. = FALSE)
  }
  other <- plot_data[!plot_data$selected_section, , drop = FALSE]
  selected <- plot_data[plot_data$selected_section, , drop = FALSE]
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[["measurement_date"]], y = .data[["metric_value"]])) +
    ggplot2::geom_line(alpha = 0.45, color = "grey55") +
    ggplot2::geom_point(
      data = other,
      ggplot2::aes(size = .data[["n_vertices"]]),
      alpha = 0.55,
      color = "grey45"
    ) +
    ggplot2::geom_point(
      data = selected,
      ggplot2::aes(size = .data[["n_vertices"]]),
      alpha = 0.95,
      shape = 17,
      color = selected_color
    ) +
    ggplot2::geom_text(
      data = selected,
      ggplot2::aes(label = .data[["cross_section_id"]]),
      vjust = -1.0,
      size = 3.2,
      color = selected_color
    ) +
    ggplot2::scale_size_continuous(name = "V\u00e9rtices", range = c(2, 6)) +
    ggplot2::labs(
      x = "Data do levantamento",
      y = y_label,
      title = title,
      subtitle = subtitle,
      caption = selected_caption
    ) +
    theme_hydrodatabr(base_size = base_size)
}
