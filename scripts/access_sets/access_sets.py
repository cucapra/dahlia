from functools import *
from itertools import *
import sys

bold_start = "\033[1m"
bold_end   = "\033[0m"
bullet     = "\u2022"

def generate_indices(select_tuples, static_sizes, dims, bf):
    prod = lambda x: reduce(lambda y, acc: y*acc, dims[x+1:])

    final = []

    for t in select_tuples:
        
        total = 0
        result = ""
        vals = ""
        for i in range(len(dims)-1):
            s, d = t[i]
            vals += ("s" + str(i) + "=" + str(s) + ", d" + str(i) + \
                     "=" + str(d) + "; ")
            result += ("(" + str(s) + "+" + str(static_sizes[i]) + \
                       "*" + str(d) + ")*" + str(prod(i)) + "+")
            total += (s + static_sizes[i]*d)*prod(i)

        result = result[:-1]
        vals = vals[:-2]

        final = final + [("\t" + bullet + " " + vals + " => index accessed: " + \
                result + "=" + str(total), total)]

    for (s, t) in sorted(final, key=lambda t: t[1]):
        print(s + "; bank accessed: " + str(t) + " mod " + str(bf) + \
              "=" + str(t % bf))


def compute_result(dims, idx_types, bf):
    array_dims = reduce(lambda acc, d: acc + "[" + str(d) + "]", dims, "")
    print(bold_start + "Accessing array a" + array_dims + ".")
    print("Accessing with the following indexes:" + bold_end)

    for (s, d) in idx_types:
        print("\t" + bullet + " idx<0.." + str(s) + ", 0.." + str(d) + ">")

    print(bold_start + "Computing index set:" + bold_end)

    idx_map = []
    static_sizes = []
    for (s, d) in idx_types:
        sd_prod = [ list(product( [ i for i in range(s) ], [ i for i in range(d) ] )) ]
        idx_map = idx_map + sd_prod
        static_sizes = static_sizes + [s]

    select_tuples = list(product(*idx_map))
    generate_indices(select_tuples, static_sizes, dims + [1], bf)


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
    print(bold_start + "Please enter your array dimensions in as a comma-separated list:" + bold_end)

    dims = []
    idx_types = []

    try:
        dims = process_dims(input("  >> "))
    except Exception as e:
        print(e)
        sys.exit(1)

    print(bold_start + "Enter your index type accessors in this format: [l1_s, h1_s, " + \
          "l1_d, h1_d]; ... [ln_s, hn_s, ln_d, hn_d] (starts are inclusive, ends are not)" + bold_end)

    idx_types = process_idx(input("  >> "))

    print(bold_start + "Enter a banking factor:" + bold_end)

    bf = int(input("  >> "))

    compute_result(dims, idx_types, bf)
