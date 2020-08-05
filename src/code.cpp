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
  IntegerVector dim(std::max((R_xlen_t) 1L, result_vars_vec.size()), 1);
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
cat(einsum_generator("ij,jk-> ik", compile_function = FALSE))



*/

// This function is just to see if there were any warnings about the generated code
NumericVector einsum_mat_mult(NumericVector array1, NumericVector array2){
  NumericVector size(3);
  IntegerVector array1_dim = array1.hasAttribute("dim") ? array1.attr("dim") : IntegerVector::create(array1.length());
  IntegerVector array2_dim = array2.hasAttribute("dim") ? array2.attr("dim") : IntegerVector::create(array2.length());
  size[0] = array1_dim[0];
  size[1] = array1_dim[1];
  if(size[1] != array2_dim[0]) stop("Dimension 1 of object array2 does not match!");
  size[2] = array2_dim[1];

  NumericVector result(size[0] * size[2]);
  for(int i = 0; i < size[0]; ++i){
    for(int k = 0; k < size[2]; ++k){
      double sum = 0.0;
      for(int j = 0; j < size[1]; ++j){
        sum += array1[1 * (i + size[0] * (j))] * array2[1 * (j + size[1] * (k))];
      }
      result[1 * (i + size[0] * (k))] = sum;
    }
  }
  result.attr("dim") = IntegerVector::create(size[0],size[2]);
  return result;

}
