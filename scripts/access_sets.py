from functools import *

def generate_indices(static_vals, dynamic_vals, dims):
    prod = lambda x: reduce(lambda y, acc: y*acc, dims[x+1:])
    for static_range in static_vals:
        for dynamic_range in dynamic_vals:
            for s in static_range:
                for d in dynamic_range:
                    total=0
                    print("\t- ", end='')
                    for i in range(len(dims)-1):
                        print("(" + str(s) + "+" + str(len(static_range)) + \
                              "*" + str(d) + ")*" + str(prod(i)) + "+", end='')
                        total += (s + len(static_range)*d)*prod(i)

                    print("=" + str(total))

def compute_result(dims, idx_types):
    array_dims = reduce(lambda acc, d: acc + "[" + str(d) + "]", dims, "")
    print("Accessing array a" + array_dims + ".")
    print("Accessing with the following indexes:")

    for (s, d) in idx_types:
        print("\t- idx<0.." + str(s) + ", 0.." + str(d) + ">")

    print("Computing index set:")

    idx_smap = []
    idx_dmap = []
    for (s, d) in idx_types:
        idx_smap = idx_smap + ([[i for i in range(s)]])
        idx_dmap = idx_dmap + ([[i for i in range(d)]])
    generate_indices(idx_smap, idx_dmap, dims + [1])

def process_idx(s):
    """
    Given a string of index type specifications, returns list of idx type values.
      Input: "[l1_s, h1_s, l1_d, h1_d] .. [ln_s, hn_s, ln_d, hn_d]"
      Output: [(h1_s-l1_s, h1_d-l1_d) .. (hn_s-ln_s, hn_d-ln_d)]
    """
    whitespaced           = s.replace(" ", "")
    idx_strs              = whitespaced.split(";")
    idx_numstrings_bracks = list(map(lambda x: x.split(","), idx_strs))
    remove_idx_bracks     = lambda x: x.replace("]", "").replace("[", "")
    idx_numstrings        = map(lambda idx_vals: map(remove_idx_bracks, idx_vals), \
                                idx_numstrings_bracks)
    idx_nums              = map(lambda x: list(map(int, x)), idx_numstrings)
    process_idx           = lambda idx: (idx[1]-idx[0], idx[3]-idx[2])
    idx_ranges            = list(map(process_idx, idx_nums))
    return idx_ranges

def process_dims(s):
    """
    Given a string of comma separated numbers, returns a list of numbers.
    Raises Exception if non-numeric values in string.
    """
    result = s.replace(" ", "").split(",")
    if not all(v.isnumeric() for v in result):
        raise Exception("Invalid dimensions.")
    else:
        return list(map(int, result))

if __name__ == "__main__":
    print("Please enter your array dimensions in as a comma-separated list")

    dims = []
    idx_types = []

    try:
        dims = process_dims(input("  >> "))
    except Exception as e:
        print(e)

    print("Enter your index type accessors in this format: [l1_s, h1_s, " + \
          "l1_d, h1_d]; ... [ln_s, hn_s, ln_d, hn_d]")

    idx_types = process_idx(input("  >> "))

    compute_result(dims, idx_types)
    
