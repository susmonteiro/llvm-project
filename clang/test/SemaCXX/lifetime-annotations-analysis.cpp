// RUN: %clang_cc1 -fsyntax-only -Wprint-lifetimes -verify %s

// TODO delete this or warn-return-lifetimes-differ and warn-assign-lifetimes-differ
// This files joins the examples presented in warn-return-lifetimes-differ and warn-assign-lifetimes-differ

// Warns tested in this test:
// * warn_assign_lifetimes_differ
// * warn_return_lifetimes_differ

// TODO erase me
// ! some tips
// - "expected-warning@-1" -> specifies that the warning should happen at the line above
// - can also use "\" to put multiple warnings/notes on the same code line, but different test lines

#define $(l) [[clang::annotate_type("lifetime", #l)]]

#define $a $(a)
#define $b $(b)

int *$a simple_correct_return_and_assign(int *$a x, int *$a y) {
  x = y;  // no warning
  return y; // no warning
}

int *$a simple_incorrect_return_and_assign(int *$a x, int *$b y) {
  x = y;    // expected-warning {{assignment requires that '$a' outlives '$b'}} \
            // expected-note@-1 {{declared with lifetime '$a' here}} \
            // expected-note@-1 {{declared with lifetime '$b' here}}

  return y; // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$b'}} \
            // expected-note@-5 {{declared with lifetime '$b' here}}
}