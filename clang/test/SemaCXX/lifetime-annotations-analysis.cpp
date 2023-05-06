// RUN: %clang_cc1 -fsyntax-only -Wprint-lifetimes -verify %s

// TODO delete this or warn-return-lifetimes-differ and
// warn-assign-lifetimes-differ This files joins the examples presented in
// warn-return-lifetimes-differ and warn-assign-lifetimes-differ

// Warns tested in this test:
// * warn_assign_lifetimes_differ
// * warn_return_lifetimes_differ

// TODO erase me
// ! some tips
// - "expected-warning@-1" -> specifies that the warning should happen at the
// line above
// - can also use "\" to put multiple warnings/notes on the same code line, but
// different test lines

#define $(l) [[clang::annotate_type("lifetime", #l)]]

#define $a $(a)
#define $b $(b)

int *$a correct_simple_return(int *$a x) {
  return x; // no warning
}

int *$b incorrect_simple_return(int *$a x) {
  return x; // expected-warning {{function should return data with lifetime '$b' but it is returning data with lifetime '$a'}} \
            // expected-note@-1 {{declared with lifetime '$a' here}}
}


int *$a correct_simple_return_and_assign(int *$a x, int *$a y) {
  x = y;     // no warning
  return y;  // no warning
}

int *$a incorrect_simple_return_and_assign(int *$a x, int *$b y) {
  x = y;  // expected-warning {{assignment requires that '$b' outlives '$a'}} \
            // expected-note@-1 {{declared with lifetime '$a' here}} \
            // expected-note@-1 {{declared with lifetime '$b' here}}

  return y;  // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$b'}} \
            // expected-note@-5 {{declared with lifetime '$b' here}}
}

int *$a correct_simple_dependencies(int *$a x) {
  int *p = x;
  return p;  // no warning
}

int *$b incorrect_simple_dependencies(int *$a x) {
  int *p = x;
  return p;  // expected-warning {{function should return data with lifetime '$b' but it is returning data with lifetime '$a'}} \
            // expected-note@-1 {{declared with lifetime '$a' here}}
}

int *$a correct_simple_control_flow(int *$a x, int *$a y, int num) {
  int *p;
  if (num > 0) {
    p = x;
  } else {
    p = y;
  }
  return p;  // no warning
}

int *$a incorrect_simple_control_flow(int *$a x, int *$b y, int num) {
  int *p;
  if (num > 0) {
    p = x;
  } else {
    p = y;
  }
  return p;  // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$b'}}
  // TODO "expected-note"
}

int *$b incorrect_simple_control_flow_2(int *$a x, int *$b y, int num) {
  int *p;
  if (num > 0) {
    p = x;
  } else {
    p = y;
  }
  return p;  // expected-warning {{function should return data with lifetime '$b' but it is returning data with lifetime '$a'}}
  // TODO "expected-note"
}

void var_decl_with_annot(int *$a x, int *$b y) {
  int *$a p;
  p = x;  // no warn
  // TODO fix this warning -> message should be inverted
  p = y;  // expected-warning {{assignment requires that '$b' outlives '$a'}} \
          // expected-note@-3 {{declared with lifetime '$a' here}} \
          // expected-note@-4 {{declared with lifetime '$b' here}}
  int *$a q = x;  // no warn
  int *$a r = y;  // TODO warn
}

int *$a correct_dependencies_cycle(int *$a x, int *$a y) {
  int *p;
  int *q;
  int *r;
  p = q;
  q = r;
  r = p;
  p = x;
  q = y;
  return r;
}

int *$b incorrect_dependencies_cycle(int *$a x, int *$b y) {
  int *p;
  int *q;
  int *r;
  p = q;
  q = r;
  r = p;
  p = x;
  q = y;
  return r;               // error -> what is shortest($a, $b) ?
  // expected-warning@-1 {{function should return data with lifetime '$b' but it is returning data with lifetime '$a'}}
  // TODO "expected-note"
}

void simple_function_call(int* $a x, int *$b y) {
  int *$a p;
  p = correct_simple_return(x); // no warning
  p = incorrect_simple_return(x); // no warning
  p = correct_simple_return(y); // expected-warning {{assignment requires that '$b' outlives '$a'}} \
          // expected-note@-3 {{declared with lifetime '$a' here}} \
          // expected-note@-4 {{declared with lifetime '$b' here}}
}