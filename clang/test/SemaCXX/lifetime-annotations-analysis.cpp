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
#define $c $(c)
#define $d $(d)

#define $static $(static)

int *$a correct_simple_return(int *$a x) {
  return x; // no warning
}

int *$b incorrect_simple_return(int *$a x) {
  return x; // expected-warning {{function should return data with lifetime '$b' but it is returning data with lifetime '$a'}} \
            // expected-note@-1 {{declared with lifetime '$a' here}}
}

int &$b incorrect_simple_return_references(int &$a x) {
  return x; // expected-warning {{function should return data with lifetime '$b' but it is returning data with lifetime '$a'}} \
            // expected-note@-1 {{declared with lifetime '$a' here}}
}

int *$a no_params_one_indirection(int *x) {
        return x; // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$local'}} \
            // expected-note@-1 {{declared with lifetime '$local' here}}
}

int *$a *$b no_params_two_indirections(int *$a *x) {
        return x; // expected-warning {{function should return data with lifetime '$b' but it is returning data with lifetime '$local'}} \
            // expected-note@-1 {{declared with lifetime '$local' here}}
}

int *$a correct_simple_return(int *$a x, int *$b y) {
  return x; // no warning
}

int *$a constraint_simple_return(int *$a x, int *$a y) {
  return x; // no warning
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
  return p;  // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$b'}} \
            // expected-note@-2 {{declared with lifetime '$b' here}}
}

int *$b incorrect_simple_control_flow_2(int *$a x, int *$b y, int num) {
  int *p;
  if (num > 0) {
    p = x;
  } else {
    p = y;
  }
  return p;  // expected-warning {{function should return data with lifetime '$b' but it is returning data with lifetime '$a'}} \
            // expected-note@-4 {{declared with lifetime '$a' here}}
}

void var_decl_with_annot(int *$a x, int *$b y) {
  int *$a p;
  p = x;  // no warn
  p = y;  // expected-warning {{assignment requires that '$b' outlives '$a'}} \
          // expected-note@-2 {{declared with lifetime '$a' here}} \
          // expected-note@-3 {{declared with lifetime '$b' here}}
  int *$a q = x;  // no warn
  int *$a r = y;  // expected-warning {{initialization requires that '$b' outlives '$a'}} \
                  // expected-note@-7 {{declared with lifetime '$b' here}}
  int *t = x;
  t = y;
  int *$a s = t;  // expected-warning {{initialization requires that '$b' outlives '$a'}} \
                  // expected-note@-1 {{declared with lifetime '$b' here}}
}

int *$a correct_dependencies_propagation(int *$a x, int *$a y) {
  int *p = x; 
  int *q = p;
  p = y;
  return q;
}

int *$a incorrect_dependencies_propagation(int *$a x, int *$b y) {
  int *p = x; // p -> $a
  int *q = p; // q -> $a
  p = y;      // p -> $a, $b | q -> $a, $b
  return q;   // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$b'}} \
             // expected-note@-2 {{declared with lifetime '$b' here}}
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
  // expected-warning@-1 {{function should return data with lifetime '$b' but it is returning data with lifetime '$a'}} \
                  // expected-note@-4 {{declared with lifetime '$a' here}}
}

void simple_function_call(int* $a x, int *$b y) {
  int *$a p;
  p = correct_simple_return(x); // no warning
  p = incorrect_simple_return(x); // no warning
  p = correct_simple_return(y); // expected-warning {{assignment requires that '$b' outlives '$a'}} \
          // expected-note@-3 {{declared with lifetime '$a' here}} \
          // expected-note@-4 {{declared with lifetime '$b' here}}
}

int *$a correct_function_call_two_args_1(int *$a x, int *$b y) {
  int *$a p;
  p = correct_simple_return(x, y);
  return p;
}

int *$a incorrect_function_call_two_args_1(int *$a x, int *$b y) {
  int *p;
  p = constraint_simple_return(x, y);
  return p; // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$b'}} \
                  // expected-note@-1 {{declared with lifetime '$b' here}}
}

int *$a correct_function_call_two_args_2(int *$a x, int *$b y) {
  return correct_simple_return(x, y);
}

int *$a incorrect_function_call_two_args_2(int *$a x, int *$b y) {
  return constraint_simple_return(x, y);  // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$b'}} \
          // expected-note@-1 {{declared with lifetime '$b' here}}
}

int *$a correct_function_call_two_args_3(int* $a x, int *$b y) {
  int *$a p;
  p = correct_simple_return(x, y);
  int *$a q = correct_simple_return(x, y);
  return p;
}

int *$a incorrect_function_call_two_args_3(int* $a x, int *$b y) {
  int *$a p;
  p = constraint_simple_return(x, y); // expected-warning {{assignment requires that '$b' outlives '$a'}} \
    // expected-note@-1 {{declared with lifetime '$a' here}} \
    // expected-note@-2 {{declared with lifetime '$b' here}}
  int *$a q = constraint_simple_return(x, y); // expected-warning {{initialization requires that '$b' outlives '$a'}} \
    // expected-note@-5 {{declared with lifetime '$b' here}}
  return p; // ? no warning
}

int return_int(int* $a x, int* $b y) {
	int *p = x;
	p = y;
	return *p;
}	

int *$b multiple_notes_incorrect(int *$a x, int *$b y) {
	int *p;
	int *q = x;
	int *$a r;
	r = y;			// expected-warning {{assignment requires that '$b' outlives '$a'}} \
              // expected-note@-1 {{declared with lifetime '$a' here}} \
              // expected-note@-4 {{declared with lifetime '$b' here}}
	p = x;
	p = q;
	return p;		// expected-warning {{function should return data with lifetime '$b' but it is returning data with lifetime '$a'}} \
          // expected-note@-2 {{declared with lifetime '$a' here}} \
          // expected-note@-1 {{declared with lifetime '$a' here}}
}

int *$a warn_on_assignment(int *$a x, int *$b y) {
	int *p;
	int *q = y;
	p = q;
	p = x;
	return p;		// expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$b'}} \
          // expected-note@-2 {{declared with lifetime '$b' here}}
}


// === Static and Local lifetimes ===

int *$a static_lifetime_return(int *$static x) {
  return x; // correct because $a <= $static
}

void static_lifetime_assign(int *$static x) {
  int *$a p = x; // correct because $a <= $static
  x = p;        // expected-warning {{assignment requires that '$a' outlives '$static'}} \
                // expected-note@-2 {{declared with lifetime '$static' here}} \
                // expected-note@-1 {{declared with lifetime '$a' here}}
}

int *$a static_dependencies_correct(int *$a x, int *$a y, int *$static s) {
  int *p = x; 
  p = s;  // $(p) = min($(x), $(z)) = min($a, $static) = $a
  y = p;  // correct
  return p; // correct
}

int *$static static_dependencies_incorrect(int *$a x, int *$a y, int *$static s) {
  int *p = x; 
  p = s;  // $(p) = min($(x), $(z)) = min($a, $static) = $a
  y = p;  // correct
  return p; // expected-warning {{function should return data with lifetime '$static' but it is returning data with lifetime '$a'}} \
            // expected-note@-3 {{declared with lifetime '$a' here}}
}

void static_lifetime_function_call(int *$static x, int *$a y, int *$b z) {
  static_lifetime_return(x);  // correct
  static_lifetime_return(y);  // TODO should be incorrect
  y = static_lifetime_return(x); // correct
  x = static_lifetime_return(x); // TODO should be incorrect
  z = static_lifetime_return(x); // TODO should be incorrect
}

void address_of_operator(int *$a x) {
  int i = 0;
  int *$a p = &i; // expected-warning {{initialization requires that '$local' outlives '$a'}} \
                // expected-note@-1 {{declared with lifetime '$local' here}}
  int *$b *$a q = &x; // expected-warning {{initialization requires that '$local' outlives '$a'}} \
                // expected-note@-4 {{declared with lifetime '$local' here}} \
                // expected-warning {{initialization requires that '$a' outlives '$b'}} \
                // expected-note@-4 {{declared with lifetime '$a' here}}
}

void local_lifetime_assign_and_return(int *$a x) {
  int i = 1;
  x = &i;       // expected-warning {{assignment requires that '$local' outlives '$a'}} \
                // expected-note@-1 {{declared with lifetime '$local' here}} \
                // expected-note@-2 {{declared with lifetime '$a' here}}
  int *p = &i;  // p has local lifetime
  x = p;        // expected-warning {{assignment requires that '$local' outlives '$a'}} \
                // expected-note@-6 {{declared with lifetime '$a' here}} \
                // expected-note@-1 {{declared with lifetime '$local' here}}
}

int *$a return_local_lifetime() {
  int i = 0;
  int *p = &i;
  return p; // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$local'}} \
            // expected-note@-1 {{declared with lifetime '$local' here}}
}

void multiple_indirections(int *$a *$b *$c *$d x) {
        int ****p;
        p = x;
        *p = *x;
        **p = **x;
        ***p = ***x;
        ****p = ****x;
        *(&p) = *(&x);
}

void simple_unary_operator(int *$a p) {
        int *$c *$b pp = &p;  // expected-warning {{initialization requires that '$local' outlives '$b'}} \
                // expected-note@-1 {{declared with lifetime '$local' here}} \
                // expected-warning {{initialization requires that '$a' outlives '$c'}} \
                // expected-note@-1 {{declared with lifetime '$a' here}}
        int i = 0;
        int *$a x = &i; // expected-warning {{initialization requires that '$local' outlives '$a'}} \
                // expected-note@-1 {{declared with lifetime '$local' here}}
}

int *$a unary_op_1(int *$a p) {
        int **pp = &p;
        p = *pp;
        return *pp;
}

int *$a *$c *$a multiple_indirections_1(int *$a *$b *$c *$d x, int *$a y) {
        int * *$a **p;  
        p = x;    // expected-warning {{assignment requires that '$b' outlives '$a'}} \
                  // expected-note@-1 {{declared with lifetime '$a' here}} \
                  // expected-note@-2 {{declared with lifetime '$b' here}}
        int **q = &y;
        int ***xx = *x;
        int **$b *$a r = *x;  // expected-warning {{initialization requires that '$c' outlives '$a'}} \
                // expected-note@-7 {{declared with lifetime '$c' here}}
        return *p;  // expected-warning {{function should return data with lifetime '$c' but it is returning data with lifetime '$a'}} \
            // expected-note@-8 {{declared with lifetime '$a' here}} \
            // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$c'}} \
            // expected-note@-7 {{declared with lifetime '$c' here}}
}
