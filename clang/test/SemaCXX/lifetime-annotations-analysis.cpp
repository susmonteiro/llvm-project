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
#define $e $(e)
#define $f $(f)

#define $static $(static)
#define $local $(local)

int *$a correct_simple_return(int *$a x) {
  return x; // no warning
}

int *$b incorrect_simple_return(int *$a x) {
  return x; // expected-warning {{function should return data with lifetime '$b' but it is returning data with lifetime '$a'}} \
            // expected-note@-1 {{declared with lifetime '$a' here}}
}

int &$a correct_simple_return_references(int &$a x) {
  return x;
}

int &$b incorrect_simple_return_references(int &$a x) {
  return x; // expected-warning {{function should return data with lifetime '$b' but it is returning data with lifetime '$a'}} \
            // expected-note@-1 {{declared with lifetime '$a' here}}
}

// TODO
// int *$a correct_simple_return_pointers_and_references(int &$a x) {
//   return &x;
// }

// TODO
// int &$a correct_simple_return_references_and_pointers(int *$a x) {
//   return *x;
// }

int *$a no_params_one_indirection(int *x) {
        return x; // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$local'}} \
            // expected-note@-1 {{declared with lifetime '$local' here}}
}

int *$a no_params_two_indirections_1(int *$a *$b x) {
        return *x;
}

int *$b no_params_two_indirections_2(int *$a *$b x) {
        return *x; // expected-warning {{function should return data with lifetime '$b' but it is returning data with lifetime '$a'}} \
            // expected-note@-1 {{declared with lifetime '$a' here}}
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

int &$a correct_simple_dependencies_references(int &$a x) {
  int &p = x;
  return p;  // no warning
}

int *$b incorrect_simple_dependencies(int *$a x) {
  int *p = x;
  return p;  // expected-warning {{function should return data with lifetime '$b' but it is returning data with lifetime '$a'}} \
            // expected-note@-1 {{declared with lifetime '$a' here}}
}

int &$b incorrect_simple_dependencies_references(int &$a x) {
  int &p = x;
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

int *$a ternary_conditions(int *$a x, int *$b y, int *$a z, bool b) {
  int *$a p = b ? x : z;
  int *$a q = b ? z : y;  // expected-warning {{initialization requires that '$b' outlives '$a'}} \
                  // expected-note@-2 {{declared with lifetime '$b' here}}
  if (b) {
    return b ? x : y; // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$b'}} \
            // expected-note@-5 {{declared with lifetime '$b' here}}
  } else {
    return b ? x : z;
  }
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

void no_warn_assignments(int *$a x, int *$b y, int *$c z, int *$d w) {
	int *p = x;
  p = y;
	int *q = z;
  q = w;
  p = q;
	p = q;
	p = x;
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
  static_lifetime_return(y);  // expected-warning {{argument 'y' requires that '$a' outlives '$static'}} \
                              // expected-note@-2 {{lifetime of 'y' is '$a'}} \
                              // expected-note@-28 {{lifetime of 'x' is '$static'}}
  y = static_lifetime_return(x); // correct
  x = static_lifetime_return(x); // TODO should be incorrect
  z = static_lifetime_return(x); // TODO should be incorrect
}

void aux_static_lifetimes(int *$static x1, int *$static *$a x2, int *$b *$static x3);

void static_lifetime_function_call_2(int *$a p1, int *$a *$static p2, int *$static *$c p3, int *$b p4) {
  aux_static_lifetimes(p1, p2, p3); // expected-warning {{argument 'p3' requires that '$c' outlives '$static'}} \
                              // expected-note@-1 {{lifetime of 'p3' is '$c'}} \
                              // expected-note@-3 {{lifetime of 'x3' is '$static'}} \
                              // expected-warning {{argument 'p1' requires that '$a' outlives '$static'}} \
                              // expected-note@-1 {{lifetime of 'p1' is '$a'}} \
                              // expected-note@-3 {{lifetime of 'x1' is '$static'}} \
                              // expected-warning {{argument '*p2' requires that '$a' outlives '$static'}} \
                              // expected-note@-1 {{lifetime of '*p2' is '$a'}} \
                              // expected-note@-3 {{lifetime of '*x2' is '$static'}}
  int *p = p1;
  p = p4;
  static_lifetime_return(p);  // expected-warning {{argument 'p' requires that '$a' outlives '$static'}} \
                              // expected-note@-2 {{lifetime of 'p' is '$a'}} \
                              // expected-note@-50 {{lifetime of 'x' is '$static'}} \
                              // expected-warning {{argument 'p' requires that '$b' outlives '$static'}} \
                              // expected-note@-1 {{lifetime of 'p' is '$b'}} \
                              // expected-note@-50 {{lifetime of 'x' is '$static'}}
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

int *$a return_local_lifetime_1() {
  int i = 0;
  int *p = &i;
  return p; // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$local'}} \
            // expected-note@-1 {{declared with lifetime '$local' here}}
}

int *$local return_local_lifetime_2() { // expected-warning {{the return lifetime cannot be '$local'}}
  int i = 0;
  int *p = &i;
  return p; 
}

int *return_local_lifetime_3() {  // expected-warning {{the return lifetime cannot be '$local'}}
  int i = 0;
  int *p = &i;
  return p; 
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

void pointer_aliasing_1(int *$a *$b x) {
  int **p01 = x;
  int *$a *p02 = x;
  int *$b *p03 = x; // expected-warning {{initialization requires that '$a' outlives '$b'}} \
                // expected-note@-3 {{declared with lifetime '$a' here}}
  int **$a p04 = x; // expected-warning {{initialization requires that '$b' outlives '$a'}} \
                // expected-note@-5 {{declared with lifetime '$b' here}}
  int **$b p05 = x;
  int *$a *$b p06 = x;
  int *$b *$a p07 = x; // expected-warning {{initialization requires that '$b' outlives '$a'}} \
                // expected-note@-9 {{declared with lifetime '$a' here}} \
                // expected-warning {{initialization requires that '$a' outlives '$b'}} \
                // expected-note@-9 {{declared with lifetime '$b' here}}
  int *$a *$a p08 = x;  // expected-warning {{initialization requires that '$b' outlives '$a'}} \
                // expected-note@-13 {{declared with lifetime '$b' here}}
  int *$b *$b p09 = x;  // expected-warning {{initialization requires that '$a' outlives '$b'}} \
                // expected-note@-15 {{declared with lifetime '$a' here}}
}

void pointer_aliasing_2(int *$a *$b x) {
  int **p01;
  p01 = x;
  *p01 = *x;
  int *$a *p02;
  p02 = x;
  *p02 = *x;
  int *$b *p03;
  p03 = x; // expected-warning {{assignment requires that '$a' outlives '$b'}} \
                // expected-note@-8 {{declared with lifetime '$a' here}} \
                // expected-note@-1 {{declared with lifetime '$b' here}}
  *p03 = *x;  // expected-warning {{assignment requires that '$a' outlives '$b'}} \
                // expected-note@-11 {{declared with lifetime '$a' here}} \
                // expected-note@-4 {{declared with lifetime '$b' here}}
  int **$b p05;
  p05 = x;
  *p05 = *x;
  int *$a *$b p06;
  p06 = x;
  *p06 = *x;
  int *$b *$a p07;
  p07 = x; // expected-warning {{assignment requires that '$b' outlives '$a'}} \
                // expected-note@-21 {{declared with lifetime '$b' here}} \
                // expected-note@-1 {{declared with lifetime '$a' here}} \
                // expected-warning {{assignment requires that '$a' outlives '$b'}} \
                // expected-note@-21 {{declared with lifetime '$a' here}} \
                // expected-note@-1 {{declared with lifetime '$b' here}}
  *p07 = *x;    // expected-warning {{assignment requires that '$a' outlives '$b'}} \
                // expected-note@-27 {{declared with lifetime '$a' here}} \
                // expected-note@-7 {{declared with lifetime '$b' here}}
  int *$a *$a p08;
  p08 = x;  // expected-warning {{assignment requires that '$b' outlives '$a'}} \
                // expected-note@-31 {{declared with lifetime '$b' here}}\
                // expected-note@-1 {{declared with lifetime '$a' here}}
  *p08 = *x;
  int *$b *$b p09;
  p09 = x;  // expected-warning {{assignment requires that '$a' outlives '$b'}} \
                // expected-note@-36 {{declared with lifetime '$a' here}}\
                // expected-note@-1 {{declared with lifetime '$b' here}}
  *p09 = *x;  // expected-warning {{assignment requires that '$a' outlives '$b'}} \
                // expected-note@-39 {{declared with lifetime '$a' here}}\
                // expected-note@-4 {{declared with lifetime '$b' here}}
}

int *$a *$b pointer_aliasing_3(int *$a *$b x, int *$c *$d y, int num) {
  int **p01 = x;
  if (num == 1) return p01;
  int **p02 = y;
  if (num == 2) return p02; // expected-warning {{function should return data with lifetime '$b' but it is returning data with lifetime '$d'}} \
                // expected-note@-1 {{declared with lifetime '$d' here}} \
                // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$c'}} \
                // expected-note@-1 {{declared with lifetime '$c' here}}
  int *$a *p03 = x;
  if (num == 3) return p03;
  int **p04;
  p04 = x;
  if (num == 4) return p04;
  int **p05;
  p05 = y;
  if (num == 5) return p05; // expected-warning {{function should return data with lifetime '$b' but it is returning data with lifetime '$d'}} \
                // expected-note@-1 {{declared with lifetime '$d' here}} \
                // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$c'}} \
                // expected-note@-1 {{declared with lifetime '$c' here}}
  int *$a *p06;
  p06 = x;
  if (num == 6) return p06; 

  int **p07 = x;
  if (num == 7) return p07; // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$c'}} \
                // expected-note@+2 {{declared with lifetime '$c' here}}
  *p07 = *y;
  return x;
}


int *$a *$c *$a pointer_aliasing_4(int *$a *$b *$c *$d x, int *$a y) {
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

void pointer_aliasing_5(int *$a x, int *$a *$b y) {
  int **p = &x;
  int i = 0;
  *p = &i;

  int *$a *q = y;
  *q = &i;  // expected-warning {{assignment requires that '$local' outlives '$a'}} \
            // expected-note@-1 {{declared with lifetime '$a' here}} \
            // expected-note@-4 {{declared with lifetime '$local' here}}

  int **r = y;
  *r = &i;
}

void pointer_aliasing_6(int *$a *$c x, int *$a *$b y, int *$b *$b z) {
  int *p = *y;
  *y = *x;
  *x = p;

  int *q = *y;
  *y = *z;  // expected-warning {{assignment requires that '$b' outlives '$a'}} \
            // expected-note@-6 {{declared with lifetime '$a' here}} \
            // expected-note@-6 {{declared with lifetime '$b' here}}
  *z = q;   // expected-warning {{assignment requires that '$a' outlives '$b'}} \
            // expected-note@-4 {{declared with lifetime '$a' here}} \
            // expected-note@-9 {{declared with lifetime '$b' here}}
}

int *$e *$d pointer_aliasing_7(int *$a *$b x, int *$c *$d y, int *$e *$f z) {
  int **p;
  int **q;
  int **r;
  p = q;
  r = p;

  p = x;
  q = y;
  r = z;
  y = q;  // expected-warning {{assignment requires that '$a' outlives '$c'}} \
          // expected-note@-6 {{declared with lifetime '$a' here}} \
          // expected-note@-10 {{declared with lifetime '$c' here}} \
          // expected-warning {{assignment requires that '$e' outlives '$c'}} \
          // expected-note@-6 {{declared with lifetime '$e' here}} 
  x = p;  // expected-warning {{assignment requires that '$c' outlives '$a'}} \
          // expected-note@-11 {{declared with lifetime '$c' here}} \
          // expected-note@-10 {{declared with lifetime '$c' here}} \
          // expected-note@-15 {{declared with lifetime '$a' here}} \
          // expected-warning {{assignment requires that '$e' outlives '$a'}} \
          // expected-note@-10 {{declared with lifetime '$e' here}} \
          // expected-note@-11 {{declared with lifetime '$e' here}} \
          // expected-warning {{assignment requires that '$d' outlives '$b'}} \
          // expected-note@-11 {{declared with lifetime '$d' here}} \
          // expected-note@-15 {{declared with lifetime '$b' here}}
  int *$e *$f zz = r;  // expected-warning {{initialization requires that '$a' outlives '$e'}} \
          // expected-note@-20 {{declared with lifetime '$a' here}} \
          // expected-warning {{initialization requires that '$c' outlives '$e'}} \
          // expected-note@-20 {{declared with lifetime '$c' here}} \
          // expected-warning {{initialization requires that '$b' outlives '$f'}} \
          // expected-note@-20 {{declared with lifetime '$b' here}} \
          // expected-warning {{initialization requires that '$d' outlives '$f'}} \
          // expected-note@-20 {{declared with lifetime '$d' here}}
  return p; // expected-warning {{function should return data with lifetime '$e' but it is returning data with lifetime '$a'}} \
            // expected-note@-26 {{declared with lifetime '$a' here}} \
            // expected-note@-29 {{declared with lifetime '$a' here}} \
            // expected-note@-28 {{declared with lifetime '$a' here}} \
            // expected-note@-18 {{declared with lifetime '$a' here}} \
            // expected-warning {{function should return data with lifetime '$e' but it is returning data with lifetime '$c'}} \
            // expected-note@-29 {{declared with lifetime '$c' here}} \
            // expected-note@-28 {{declared with lifetime '$c' here}} \
            // expected-warning {{function should return data with lifetime '$d' but it is returning data with lifetime '$b'}} \
            // expected-note@-26 {{declared with lifetime '$b' here}}
}

// === FUNCTION CALLS ===

void fn11(int *$a x, int *$b y);
void fn12(int *$a x, int *$a y);
void fn21(int *$a *$b x);
void fn22(int *$a *$a x);
void fn31(int *$a *$b x, int *$static y);
void fn32(int *$a *$b x, int *$b y);
void fn33(int *$a *$b x, int *$a y);
void fn34(int *$a *$b x, int *$local y);
void fn41(int *$a *$b x, int *$c *$b y);
void fn42(int *$a *$b x, int *$a *$b y);
void fn43(int *$a *$b x, int *$a *$c y);
void fn44(int *$a *$b x, int *$b *$a y);
void fn51(int *$a x, int *$a *$a y, int *$a *$a z);
void fn52(int *$a *$a x, int *$b *$a y, int *$b *$a z);
void fn61(int *$a *$b *$c x, int *$b *$a y);

void function_calls_1() {
  int *$a p;
  int *$b q;
  fn11(p, p);
  fn11(p, q);
  fn12(p, p);
  fn12(p, q);

  int *$a *$a pp;
  int *$a *$b qq;
  int *$b *$b rr;
  int *$b *$a ss;
  fn21(pp);
  fn21(qq);
  fn22(pp);
  fn22(qq);

  // TODO call fn31
  fn32(pp, p);
  fn32(pp, q);
  fn32(qq, p);
  fn32(qq, q);
  fn33(pp, p);
  fn33(pp, q);  // expected-warning {{when calling function 'fn33', the lifetime of 'q' cannot be shorter than the lifetime of '*pp'}} \
                // expected-note@-21 {{lifetime of 'q' is '$b'}} \
                // expected-note@-15 {{lifetime of '*pp' is '$a'}}
  fn33(qq, p);
  fn33(qq, q);  // expected-warning {{when calling function 'fn33', the lifetime of 'q' cannot be shorter than the lifetime of '*qq'}} \
                // expected-note@-25 {{lifetime of 'q' is '$b'}} \
                // expected-note@-18 {{lifetime of '*qq' is '$a'}}
  fn34(pp, p);
  fn34(pp, q);
  fn34(qq, p);
  fn34(qq, q);

  fn41(pp, pp);
  fn41(pp, qq); 
  fn41(pp, rr);
  fn42(pp, qq); 
  fn42(pp, rr); // expected-warning {{when calling function 'fn42', the lifetimes of arguments '*pp' and '*rr' should be the same}} \
                // expected-note@-31 {{lifetime of '*pp' is '$a}} \
                // expected-note@-29 {{lifetime of '*rr' is '$b}}
  fn42(pp, ss); // expected-warning {{when calling function 'fn42', the lifetimes of arguments '*pp' and '*ss' should be the same}} \
                // expected-note@-34 {{lifetime of '*pp' is '$a}} \
                // expected-note@-31 {{lifetime of '*ss' is '$b}}
  fn43(pp, qq); 
  fn43(pp, rr); // expected-warning {{when calling function 'fn43', the lifetimes of arguments '*pp' and '*rr' should be the same}} \
                // expected-note@-38 {{lifetime of '*pp' is '$a}} \
                // expected-note@-36 {{lifetime of '*rr' is '$b}}
  fn43(pp, ss); // expected-warning {{when calling function 'fn43', the lifetimes of arguments '*pp' and '*ss' should be the same}} \
                // expected-note@-41 {{lifetime of '*pp' is '$a}} \
                // expected-note@-38 {{lifetime of '*ss' is '$b}}
  fn44(pp, pp);
  fn44(pp, qq); 
  fn44(pp, rr);
  fn44(pp, ss);
  
  fn51(p, rr, ss); // expected-warning {{when calling function 'fn51', the lifetime of 'p' cannot be shorter than the lifetime of '*rr'}} \
                   // expected-warning {{when calling function 'fn51', the lifetime of 'p' cannot be shorter than the lifetime of '*ss'}} \
                   // expected-note@-56 {{lifetime of 'p' is '$a'}} \
                   // expected-note@-47 {{lifetime of '*rr' is '$b'}} \
                   // expected-note@-56 {{lifetime of 'p' is '$a'}} \
                   // expected-note@-46 {{lifetime of '*ss' is '$b'}}

  fn52(qq, pp, ss); // expected-warning {{when calling function 'fn52', the lifetimes of arguments '*pp' and '*ss' should be the same}} \
                    // expected-note@-56 {{lifetime of '*pp' is '$a}} \
                    // expected-note@-53 {{lifetime of '*ss' is '$b}}
  // TODO call fn61

}

void function_calls_2(int *$a x, int *$b y, int *$a z, int *$c w, int *$d v) {
  int *p = y;
  p = w;
  int *q = w;
  q = v;

  int **xx;
  int **xy;
  int **yw;
  int **xyw;
  *xx = x;
  *xx = z;
  *xy = x;
  *xy = y;
  *yw = y;
  *yw = w;
  *xyw = x;
  *xyw = y;
  *xyw = w;

  int *$a *$a pp;
  int *$a *$b qq;
  int *$b *$b rr;
  int *$b *$a ss;
  fn21(xx);
  fn21(xy);
  fn22(xx);
  fn22(xy);

  fn32(xx, x);
  fn32(xx, y);
  fn32(xy, x);
  fn32(xy, y);

  fn33(xx, x);
  fn33(xx, y);  // expected-warning {{when calling function 'fn33', the lifetime of 'y' cannot be shorter than the lifetime of '*xx'}} \
                // expected-note@-35 {{lifetime of 'y' is '$b'}} \
                // expected-note@-25 {{lifetime of '*xx' is '$a'}} \
                // expected-note@-24 {{lifetime of '*xx' is '$a'}}
  fn33(xy, x);
  fn33(xy, y); 
  fn33(xy, w);  // expected-warning {{when calling function 'fn33', the lifetime of 'w' cannot be shorter than the lifetime of '*xy'}} \
                // expected-note@-41 {{lifetime of 'w' is '$c'}} \
                // expected-note@-29 {{lifetime of '*xy' is '$a'}} \
                // expected-note@-28 {{lifetime of '*xy' is '$b'}}
  fn33(xx, *xyw);
  // FIXME -> *yw is read with type int** instead of type int*
  fn33(xx, *yw);  // 'expected-warning {{when calling function 'fn33', the lifetime of '*yw' cannot be shorter than the lifetime of '*xx'}} \
                // 'expected-note@-32 {{lifetime of '*yw' is '$b'}} \
                // 'expected-note@-31 {{lifetime of '*yw' is '$c'}} \
                // 'expected-note@-35 {{lifetime of '*xx' is '$a'}} \
                // 'expected-note@-37 {{lifetime of '*xx' is '$a'}}
  fn33(xx, p);  // expected-warning {{when calling function 'fn33', the lifetime of 'p' cannot be shorter than the lifetime of '*xx'}} \
                // expected-note@-51 {{lifetime of 'p' is '$b'}} \
                // expected-note@-50 {{lifetime of 'p' is '$c'}} \
                // expected-note@-42 {{lifetime of '*xx' is '$a'}} \
                // expected-note@-41 {{lifetime of '*xx' is '$a'}}
  fn33(xy, q);  // expected-warning {{when calling function 'fn33', the lifetime of 'q' cannot be shorter than the lifetime of '*xy'}} \
                // expected-note@-54 {{lifetime of 'q' is '$c'}} \
                // expected-note@-53 {{lifetime of 'q' is '$d'}} \
                // expected-note@-45 {{lifetime of '*xy' is '$a'}} \
                // expected-note@-44 {{lifetime of '*xy' is '$b'}}
  fn42(xx, xy); // expected-warning {{when calling function 'fn42', the lifetimes of arguments '*xx' and '*xy' should be the same}} \
                // expected-note@-52 {{lifetime of '*xx' is '$a}} \
                // expected-note@-51 {{lifetime of '*xx' is '$a}} \
                // expected-note@-49 {{lifetime of '*xy' is '$b}}
  fn42(xy, xy);
  fn42(xy, xx); // expected-warning {{when calling function 'fn42', the lifetimes of arguments '*xy' and '*xx' should be the same}} \
                // expected-note@-57 {{lifetime of '*xx' is '$a}} \
                // expected-note@-56 {{lifetime of '*xx' is '$a}} \
                // expected-note@-54 {{lifetime of '*xy' is '$b}}
  fn42(xy, xyw); // expected-warning {{when calling function 'fn42', the lifetimes of arguments '*xy' and '*xyw' should be the same}} \
                // expected-note@-53 {{lifetime of '*xyw' is '$c}}
}

int *$a arrays(int *$b *$c arr) {
  int *p = arr[0];
  int *q = arr[1];
  int *$a r = arr[2]; // expected-warning {{initialization requires that '$b' outlives '$a'}} \
                      // expected-note@-3 {{declared with lifetime '$b' here}}
  int *$b s = arr[3];
  p = q;
  return p;  // expected-warning {{function should return data with lifetime '$a' but it is returning data with lifetime '$b'}} \
            // expected-note@-6 {{declared with lifetime '$b' here}} \
            // expected-note@-1 {{declared with lifetime '$b' here}}
}

