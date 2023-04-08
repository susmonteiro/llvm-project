#include "DebugLifetimes.h"
#include <iostream>

// TODO remove this file

void debugLifetimes(std::string txt) { std::cout << txt << '\n'; }

void debugLifetimes(std::string txt1, std::string txt2) {
  std::cout << txt1 << ": " << txt2 << '\n';
}

void debugLifetimes(std::string txt, int i) {
  std::cout << txt << ": " << i << '\n';
}