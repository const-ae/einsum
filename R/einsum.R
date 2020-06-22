
#' @export
einsum <- function(equation_string, ...){

  objects <- list(...)
  tmp <- stringr::str_split_fixed(equation_string, "->", n = 2)
  result_string <- tmp[1,2]
  lhs_strings <- tmp[1,1]
  strings <- unlist(stringr::str_split(lhs_strings, ","))
  einsum_impl(strings, result_string, objects)


}




einsum_impl <- function(strings, result_string, arrays){
  stopifnot(length(strings) == length(arrays))
  stopifnot(all(stringr::str_length(strings) == vapply(arrays, function(a)length(dim(a)), 0.0)))
  result_string_vec <- stringr::str_split(result_string, "")[[1]]
  sum_string_vec <- setdiff(unique(unlist(stringr::str_split(strings, ""))), result_string_vec)

  # Get the lengths of the indices as a named vector
  lengths_vec <- get_lengths_vec(strings, arrays)

  # Make converters for output
  out_pos2idx <- pos2idx_gen(result_string, lengths_vec)
  out_idx2pos <- idx2pos_gen(result_string, lengths_vec)

  # Mke converters for summing variables
  sum_idx2pos <- idx2pos_gen(sum_string_vec, lengths_vec)
  arr_pos2idx_list <- lapply(strings, function(str)pos2idx_gen(str, lengths_vec))


  # Initialize memory for result
  res <- array(0, dim = unname(lengths_vec[result_string_vec]))

  # have a simple index running through all elements of res
  for(out_idx in seq_len(prod(lengths_vec[result_string_vec]))-1){
    out_pos <- out_idx2pos(out_idx)
    s <- 0
    for(sum_idx in seq_len(prod(lengths_vec[sum_string_vec]))-1){
      sum_pos <- c(out_pos, sum_idx2pos(sum_idx))
      s <- s + Reduce(`*`, vapply(seq_along(arrays), function(i)arrays[[i]][arr_pos2idx_list[[i]](sum_pos) + 1], FUN.VALUE = 0.0))
    }
    res[out_pos2idx(out_pos) + 1] <- s
  }
  res
}


get_lengths_vec <- function(strings, objects){
  keys <- unlist(stringr::str_split(strings, ""))
  values <- unlist(lapply(objects, dim))
  lengths <- numeric(length(unique(keys)))
  names(lengths) <- unique(keys)
  for(key in unique(keys)){
    lengths[key] <- values[which(keys == key)[1]]
  }
  lengths
}

pos2idx_gen <- function(str, lengths){
  str_vec <- unlist(stringr::str_split(str, ""))
  length_lookup <- c(1, cumprod(lengths[str_vec])[seq_len(length(str_vec)-1)])
  function(pos){
    stopifnot(all(pos[str_vec] < lengths[str_vec]))
    sum(pos[str_vec] * length_lookup)
  }
}

idx2pos_gen <- function(str, lengths){
  str_vec <- unlist(stringr::str_split(str, ""))
  length_lookup <- c(1, cumprod(lengths[str_vec])[seq_len(length(str_vec)-1)])
  length_lookup <- setNames(length_lookup, str_vec)
  function(idx){
    res <- sapply(str_vec, function(s) floor(idx / length_lookup[s]) %% lengths[s])
    setNames(res, str_vec)
  }
}
