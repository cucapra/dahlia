#include<iostream>
#include<fstream>
#include<vector>
#include<string>
#include <streambuf>
#include "picojson.h"

using std::string;
using std::vector;

/**
 * Function to extract a base value type from a Json object. See
 * https://github.com/kazuho/picojson#accessing-the-values for all accessors
 * possible.
 */
template <typename T>
T get_arg(string name, string type_name, picojson::value &json) {
  // check if the type of the value is "object"
  if (! json.is<picojson::object>()) {
    std::cerr << "JSON is not an object" << std::endl;
    exit(2);
  }

  if (json.get(name).is<T>()) {
    return json.get(name).get<T>();
  } else {
    std::cerr << "[Error] Expected `" << name << "' field with type " << type_name << std::endl;
    exit(2);
  }
}

/**
 * Functions to cast elements of a Json array into a numerical type. Expects
 * double to be castable to T.
 */
template <typename T>
vector<T> to_order_1_tensor(picojson::array &arr, unsigned int len) {
  vector<T> ret = {};

  for (auto elem: arr) {
    if (elem.is<double>()) {
      ret.push_back((T) elem.get<double>());
    }
  }

  if (ret.size() != len) {
    std::cerr << "Expected length of vector: " << len << " received: " << ret.size() << std::endl;
    exit(2);
  }

  return ret;
}

template <typename T>
T to_num(double x) {
  return (T) x;
}

/**
 * Functions to cast elements of a Json array of arrays into a matrix
 * of give type T. Expects double to be castable to T.
 */
template <typename T>
vector<vector<T>> to_order_2_tensor(picojson::array &arr, unsigned int row_dim, unsigned int col_dim) {
  vector<vector<T>> ret = {};

  for(picojson::value vect: arr) {
    if (vect.is<picojson::array>()) {
      ret.push_back(to_order_1_tensor<T>(vect.get<picojson::array>(), row_dim));
    }
  }

  if (ret.size() != col_dim) {
    std::cerr << "Expected column dimension of matrix: " << col_dim << " received: " << ret.size() << std::endl;
    exit(2);
  }

  return ret;
}

template <typename T>
vector<T> flatten_matrix(vector<vector<T>> mat) {
  vector<T> ret = {};

  for (auto vect: mat) {
    for (auto elem: vect) {
      ret.push_back(elem);
    }
  }

  return ret;
}

/**
 * For debugging purpose.
 */
template <typename T>
void print_matrix(vector<vector<T>> vect) {
  for (unsigned i = 0; i < vect.size(); i++) {
    for (unsigned j = 0; j < vect[i].size(); j++)
      std::cout << vect[i][j] << " ";
    std::cout << std::endl;
  }
}

template <typename T>
void print_vector(vector<T> vect) {
  for (unsigned i = 0; i < vect.size(); i++) {
      std::cout << vect[i] << " ";
  }
}

string read_file(string filepath) {
  std::ifstream data(filepath);
  if (!data) {
    std::cerr << "[Error] File " << filepath << " does not exist!" << std::endl;
    exit(2);
  }
  std::string str((std::istreambuf_iterator<char>(data)),
                 std::istreambuf_iterator<char>());

  return str;
}

picojson::value parse_data(int argc, char** argv) {
  picojson::value v;

  if (argc != 2) {
    std::cerr << "Required argument <data>: Path to the JSON data file." << std::endl;
    exit(2);
  }

  string err = picojson::parse(v, read_file(argv[1]));
  if (! err.empty()) {
    std::cerr << err << std::endl;
    exit(1);
  }

  return v;
}

