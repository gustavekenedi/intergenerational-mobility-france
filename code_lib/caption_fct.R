# function to create caption for graphs
caption_fct <- function(fs = fs_predict[[1]][[8]],
                        instru_labels = instru_labs[[1]]) {
  
  setDT(fs)
  
  caption_lab = c(paste0("Instrument(s): ", instru_labels),
                  paste0("# obs fathers: ", fs[call == "Number of fathers on which first-stage prediction is done", .(stat)], "  |  ",
                         "# obs mothers: ", fs[call == "Number of mothers on which first-stage prediction is done", .(stat)], "\n",
                         "Mean R2 fathers: ", fs[call == "Average R2 for fathers", .(round(as.numeric(stat), 2))], "  |  ",
                         "Mean R2 mothers: ", fs[call == "Average R2 for mothers", .(round(as.numeric(stat), 2))]))
}