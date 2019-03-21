#include<iostream>
#include<fstream>
#include<vector>
#include<string>
#include "json.hpp"

using std::string;
using std::vector;
using std::map;
using json_t = nlohmann::json;

/**
 * Implementing generic flatten that taks vector<vector<...<T>>..> and produces
 * vector<T>. For an overview of the technique, see:
 * https://stackoverflow.com/questions/29995642/create-n-dimensional-vector-with-given-sizes
 */
namespace flattening {
  template<class T>struct tag{using type=T;};
  template<class Tag>using type=typename Tag::type;

  // Template to generate the type vector<vector<...N...>>
  template<class T, size_t n>
    struct n_dim_vec:tag< std::vector< type< n_dim_vec<T, n-1> > > > {};
  template<class T>
    struct n_dim_vec<T, 0>:tag<T>{};
  template<class T, size_t n>
    using n_dim_vec_t = type<n_dim_vec<T,n>>;

  template <class T, int N = 1, class I=vector<T>>
    vector<T> flatten_tensor(vector<T> vect) {
      return vect;
    }

  template<class T, int N, class I=n_dim_vec_t<T, N>>
    vector<T> flatten_tensor(I tensor) {
      vector<T> ret = {};

      for (auto vect: tensor) {
        auto res = flatten_tensor<T, N - 1>(vect);
        for (auto elem: res) {
          ret.push_back(elem);
        }
      }

      return ret;
    }
}

/**
 * Function to extract a base value type from a Json object.
 */
template <typename T>
T get_arg(string field, string type_name, json_t &json) {
  try {
    return json[field].get<T>();
  } catch(nlohmann::json::type_error err) {
    std::cerr << "[Error] Expected `" << field << "' field with type " << type_name << std::endl;
    exit(2);
  }
}


json_t parse_data(int argc, char** argv) {
  if (argc != 2) {
    std::cerr << "Required argument <data>: Path to the JSON data file." << std::endl;
    exit(2);
  }

  std::ifstream file(argv[1]);
  json_t j;
  try {
    file >> j;
  } catch (nlohmann::detail::parse_error err) {
    std::cerr << err.what() << std::endl;
    exit(2);
  }

  return j;
}

