// RUN: %clang_cc1 -fsyntax-only -Wprint-lifetimes -verify %s

#define $(l) [[clang::annotate_type("lifetime", #l)]]

#define $a $(a)
#define $b $(b)

void simple_correct_assign(int *$a x, int *$a y) {
  y = x;    // no warning
}

void simple_incorrect_assign(int *$a x, int *$b y) {
  y = x;    
  // expected-warning@-1 {{assignment requires that '$a' outlives '$b'}}
  // expected-note@-3 {{declared with lifetime '$a' here}}
  // expected-note@-4 {{declared with lifetime '$b' here}}

}