//fizzbuzz.cpp
#include <Rcpp.h>
  #include <iostream>
  #include <vector>
  using namespace Rcpp;
  using namespace std;
  // [[Rcpp::plugins("cpp11")]]
  // [[Rcpp::export]]
  void fizzbuzz(vector<int> numbers)
  {
  for(unsigned int j = 0; j < numbers.size(); j++) {
  int number = numbers[j];
  if( number%15 == 0) {
  cout << "FizzBuzz" << endl; 
  }
  else if(number%5 == 0 ){
  cout << "Buzz" << endl; 
  }
  else if(number%3 == 0) {
  cout << "Fizz" << endl;
  }
  else {
  cout<<number<< endl;
  }
 }
}