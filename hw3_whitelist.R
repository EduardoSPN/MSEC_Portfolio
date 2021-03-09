allowed_files <- c("hw3.Rmd",
                  "hw3.md",
                  "README.md",
                  "wercker.yml",
                  "hw3.Rproj",
                  "hw3_whitelist.R",
                  "map.png",
                  "Data",
                  "hw3_cache",
                  "hw3_files",
                  "hw3.pdf",
                  "hw3.html",
                  "hw3.css",
                  ".gitignore")

files <- dir()
disallowed_files <- files[!(files %in% allowed_files)]

if (length(disallowed_files != 0)) {
  cat("Disallowed files found:\n")
  cat("  (remove the following files from your repo)\n\n")

  for (file in disallowed_files)
    cat("*", file, "\n")

  quit("no", 1, FALSE)
}
