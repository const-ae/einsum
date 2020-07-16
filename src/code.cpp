#include <Rcpp.h>
using namespace Rcpp;



std::function<int (IntegerVector)> pos2idx_gen(const IntegerVector& str_vec, const IntegerVector& lengths_vec){
  IntegerVector lookup_vec(str_vec.size());
  size_t cum_prod = 1;
  for(size_t i = 0; i < str_vec.size(); i++){
    lookup_vec[i]  = cum_prod;
    cum_prod *= lengths_vec[str_vec[i]];
  }
  return [lookup_vec, str_vec](IntegerVector pos) {
    int res = 0;
    for(size_t i = 0; i < str_vec.size(); i++){
      res += pos[str_vec[i]] * lookup_vec[i];
    }
    return res;
  };
}


std::function<IntegerVector (int, IntegerVector&)> idx2pos_gen(const IntegerVector& str_vec, const IntegerVector& lengths_vec){
  IntegerVector lookup_vec(str_vec.size());
  size_t cum_prod = 1;
  for(size_t i = 0; i < str_vec.size(); i++){
    lookup_vec[i]  = cum_prod;
    cum_prod *= lengths_vec[str_vec[i]];
  }
  return [lookup_vec, str_vec, lengths_vec](int index, /*OutParam*/ IntegerVector& position) {
    IntegerVector res(str_vec.size());
    for(size_t i = 0; i < str_vec.size(); i++){
      // Relies on integer division which automatically rounds down
      position[str_vec[i]] =  (index / lookup_vec[i]) % lengths_vec[str_vec[i]];
    }
    return res;
  };
}


// [[Rcpp::export]]
NumericVector einsum_impl_fast(IntegerVector lengths_vec,
                               ListOf<IntegerVector> array_vars_list,
                               IntegerVector not_result_vars_vec,
                               IntegerVector result_vars_vec,
                               ListOf<NumericVector> arrays){
  // Init output
  size_t sum_size = 1;
  for(auto e : not_result_vars_vec){
    sum_size *= lengths_vec[e];
  }
  size_t output_size = 1;
  IntegerVector dim(std::max(1L, result_vars_vec.size()), 1);
  for(size_t i = 0; i < result_vars_vec.size(); i++){
    output_size *= lengths_vec[result_vars_vec[i]];
    dim[i] = lengths_vec[result_vars_vec[i]];
  }
  NumericVector output(output_size);
  output.attr("dim") = dim;

  auto out_pos2idx = pos2idx_gen(result_vars_vec, lengths_vec);
  auto out_idx2pos = idx2pos_gen(result_vars_vec, lengths_vec);
  auto sum_idx2pos = idx2pos_gen(not_result_vars_vec, lengths_vec);
  std::vector<std::function<int (IntegerVector)>> arr_pos2idx_list(arrays.size());
  std::transform(array_vars_list.begin(), array_vars_list.end(), arr_pos2idx_list.begin(), [lengths_vec](IntegerVector vec){
    return pos2idx_gen(vec, lengths_vec);
  });

  IntegerVector pos(result_vars_vec.size() + not_result_vars_vec.size());
  for(size_t out_idx = 0; out_idx < output_size; out_idx++){
    out_idx2pos(out_idx, pos);
    double sum = 0.0;
    for(size_t sum_idx = 0; sum_idx < sum_size; sum_idx++){
      sum_idx2pos(sum_idx, pos);
      double product = 1;
      for(size_t arr_idx = 0; arr_idx < arrays.size(); arr_idx++) {
        int lookup_idx = arr_pos2idx_list[arr_idx](pos);
        product *= arrays[arr_idx][lookup_idx];
      }
      sum += product;
    }
    output[out_pos2idx(pos)] = sum;
  }
  return output;
}




/*** R
mat1 <- matrix(rnorm(n = 4 * 8), nrow = 4, ncol = 8)
mat2 <- matrix(rnorm(n = 8 * 3), nrow = 8, ncol = 3)

# j is 8 and 3
einsum("ij,jk -> ik", mat1, mat2)
einsum_fast("ij,jk -> ik", mat1, mat2)



# arrays <- list(mat1, mat2)
# lengths_vec <- as.integer(c(i = 4, j = 80000, k = 3))
# lhs <- c("ij", "jk")
# strings <- stringr::str_split(lhs, "")
# result_string <- c("i", "k")
#
# all_vars <- sort(unique(unlist(strings)))
# array_vars_list <- lapply(strings, function(st){
#   sapply(st, function(s) which(all_vars == s) - 1)
# })
# result_vars_vec <-  sapply(result_string, function(s) which(all_vars == s) - 1)
# not_result_vars_vec <- setdiff(seq_along(all_vars) - 1, result_vars_vec)
#
# einsum_impl_fast(lengths_vec, array_vars_list, not_result_vars_vec, result_vars_vec, arrays)
# mat1 %*% mat2



*/

