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
 * Implementing generic flatten that takes vector<vector<...<T>>..> and produces
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
}

/**
 * Function to extract a base value type from a Json object.
 */
template <typename T>
T get_arg(const string& field, const string& type_name, const json_t &json) {
  try {
    return json[field]["data"].get<T>();
  } catch(nlohmann::json::type_error err) {
    std::cerr << "[Error] Expected `" << field << ".data' field with type " << type_name << std::endl;
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
