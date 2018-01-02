#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// much faster that the R version:
// fun <- function(x) {rownames(x) <- x[, 1]; x[, 1]<-NULL; return(x)}

// to table with row names

// [[Rcpp::export]]
DataFrame as_table_with_rownames(DataFrame x)
{
  DataFrame output;
  int j = 0;
  for(int i = 1; i < x.ncol(); ++i)
  {
    output.push_back(x[i]);
  }
  output.attr("row.names") = x[0];
  x.erase(0);
  output.attr("names") = x.attr("names");
  output.attr("class") = "data.frame";
  return output;
}
