html <- rvest::read_html("https://epsg.org/whatsnew.html")
table_node <- rvest::html_nodes(html, "table")
versions <- as.data.frame(rvest::html_table(table_node)[[1]])
rows_old <- as.integer(readLines("rows_old.txt"))
old_versions <- readRDS("old_versions.rds")
status <- "unchanged"
cat("old: ", rows_old, " new: ", nrow(versions), "\n")
if (rows_old < nrow(versions)) {
    cat("parsing as new rows detected\n")
    versions[,1] |> 
        gsub("([0-9])(EPSG.*$)", "\\1", x=_) |> 
        strsplit("[\\.,] ") |> 
        do.call("rbind", args=_) |>
        as.data.frame() -> v1
    names(v1) <- c("release", "date")
    nms <- c("Change request", "New data for", "Minor revision to data for",
        "Significant revision to data", "New and revised data")
    versions[,2] |> 
        gsub("\u00a0", " ", x=_) |>
        sub("^- ", "", x=_) |> 
        strsplit("[:\\. ]- ") -> v2_a
    v2_a |>
        lapply(function(x) pmatch(nms, x)) -> v2_b
    sapply(seq(along=v2_a), function(i) v2_a[[i]][v2_b[[i]]]) |>
        t() |>
        as.data.frame() -> v2
    names(v2) <- c("chReq", "newData", "minorRev", "sigRev", "newNRev")
    names(versions) <- c("field1", "field2")
    older_versions <- old_versions
    old_versions <- cbind(v1, v2, versions)
    saveRDS(older_versions, file="older_versions.rds")
    write.csv2(old_versions, file="whats_new.csv", row.names=FALSE)
    saveRDS(old_versions, file="old_versions.rds")
    cat(nrow(old_versions), "\n", file="rows_old.txt")
    old_versions[,3] |> 
        gsub("(202[0-9]\\.[0-9][0-9][0-9])",
            "[\\1](https://apps.epsg.org/api/v1/Change/\\1/)",
            x=_) -> old_versions[,3]
    old_versions[, 1:6] |> 
        as.matrix() |> 
        apply(1, paste, collapse=" ") |> 
        cat(sep="\n\n", file="whats_new.md")
    status <- "updated"
}
cat(status, "\n")
cat(status, "\n", file="status.txt")

