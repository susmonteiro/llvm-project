// RUN: %clang_cc1 -fsyntax-only -Wprint-lifetimes -verify %s

// TODO erase me
// ! some tips
// - "expected-warning@-1" -> specifies that the warning should happen at the line above
// - can also use "\" to put multiple warnings/notes on the same code line, but different test lines

#define $(l) [[clang::annotate_type("lifetime", #l)]]

#define $a $(a)
#define $b $(b)

int *$a simple_correct_return(int *$a p) {
  return p; // no warning
}

int *$b simple_incorrect_return(int *$a p) {
  return p; 
  // expected-warning@-1 {{function should return data with lifetime '$b' but it is returning data with lifetime '$a'}}
  // expected-note@-3 {{declared with lifetime '$a' here}}
}