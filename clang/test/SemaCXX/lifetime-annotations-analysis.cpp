// RUN: %clang_cc1 -fsyntax-only -Wprint-lifetimes -verify %s

// TODO delete this or warn-return-lifetimes-differ and
// warn-assign-lifetimes-differ This files joins the examples presented in
// warn-return-lifetimes-differ and warn-assign-lifetimes-differ

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

int *$b incorrect_simple_return(int *$a x) { // expected-warning {{at least one parameter must be annotated with '$b'}}
  return x; // expected-warning {{function should return data with lifetime '*$b' but it is returning data with lifetime '*$a'}} \
            // expected-note@-1 {{declared with lifetime '*$a' here}}
}

int &$a correct_simple_return_references(int &$a x) {
  return x;
}

int &$b incorrect_simple_return_references(int &$a x) { // expected-warning {{at least one parameter must be annotated with '$b'}}
  return x; // expected-warning {{function should return data with lifetime '*$b' but it is returning data with lifetime '*$a'}} \
            // expected-note@-1 {{declared with lifetime '*$a' here}}
}

int *$a no_params_one_indirection(int *x) { // expected-warning {{at least one parameter must be annotated with '$a'}}
        return x; // expected-warning {{cannot return data with lifetime '*$local'}} \
            // expected-note@-1 {{declared with lifetime '*$local' here}}
}

int *$a no_params_two_indirections_1(int *$a *$b x) {
        return *x;
}

int *$b no_params_two_indirections_2(int *$a *$b x) {
        return *x; // expected-warning {{function should return data with lifetime '*$b' but it is returning data with lifetime '*$a'}} \
            // expected-note@-1 {{declared with lifetime '*$a' here}}
}

int *$a *$b no_params_two_indirections(int *$a *x) { // expected-warning {{at least one parameter must be annotated with '$b'}}
        return x; // expected-warning {{cannot return data with lifetime '**$local'}} \
        // expected-note@-1 {{declared with lifetime '**$local' here}}
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
  x = y;  // expected-warning {{assignment requires that '*$b' outlives '*$a'}} \
            // expected-note@-1 {{declared with lifetime '*$a' here}} \
            // expected-note@-1 {{declared with lifetime '*$b' here}}

  return y;  // expected-warning {{function should return data with lifetime '*$a' but it is returning data with lifetime '*$b'}} \
            // expected-note@-5 {{declared with lifetime '*$b' here}}
}

int *$a correct_simple_dependencies(int *$a x) {
  int *p = x;
  return p;  // no warning
}

int &$a correct_simple_dependencies_references(int &$a x) {
  int &p = x;
  return p;  // no warning
}

int *$b incorrect_simple_dependencies(int *$a x) { // expected-warning {{at least one parameter must be annotated with '$b'}}
  int *p = x;
  return p;  // expected-warning {{function should return data with lifetime '*$b' but it is returning data with lifetime '*$a'}} \
            // expected-note@-1 {{declared with lifetime '*$a' here}}
}

int &$b incorrect_simple_dependencies_references(int &$a x) { // expected-warning {{at least one parameter must be annotated with '$b'}}
  int &p = x;
  return p;  // expected-warning {{function should return data with lifetime '*$b' but it is returning data with lifetime '*$a'}} \
            // expected-note@-1 {{declared with lifetime '*$a' here}}
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
  return p;  // expected-warning {{function should return data with lifetime '*$a' but it is returning data with lifetime '*$b'}} \
            // expected-note@-2 {{declared with lifetime '*$b' here}}
}

int *$a ternary_conditions(int *$a x, int *$b y, int *$a z, bool b) {
  int *$a p = b ? x : z;
  int *$a q = b ? z : y;  // expected-warning {{initialization requires that '*$b' outlives '*$a'}} \
                  // expected-note@-2 {{declared with lifetime '*$b' here}}
  if (b) {
    return b ? x : y; // expected-warning {{function should return data with lifetime '*$a' but it is returning data with lifetime '*$b'}} \
            // expected-note@-5 {{declared with lifetime '*$b' here}}
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
  return p;  // expected-warning {{function should return data with lifetime '*$b' but it is returning data with lifetime '*$a'}} \
            // expected-note@-4 {{declared with lifetime '*$a' here}}
}

void var_decl_with_annot(int *$a x, int *$b y) {
  int *$a p;
  p = x;  // no warn
  p = y;  // expected-warning {{assignment requires that '*$b' outlives '*$a'}} \
          // expected-note@-2 {{declared with lifetime '*$a' here}} \
          // expected-note@-3 {{declared with lifetime '*$b' here}}
  int *$a q = x;  // no warn
  int *$a r = y;  // expected-warning {{initialization requires that '*$b' outlives '*$a'}} \
                  // expected-note@-7 {{declared with lifetime '*$b' here}}
  int *t = x;
  t = y;
  int *$a s = t;  // expected-warning {{initialization requires that '*$b' outlives '*$a'}} \
                  // expected-note@-1 {{declared with lifetime '*$b' here}}
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
  return q;   // expected-warning {{function should return data with lifetime '*$a' but it is returning data with lifetime '*$b'}} \
             // expected-note@-2 {{declared with lifetime '*$b' here}}
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
                  // expected-warning@-1 {{function should return data with lifetime '*$b' but it is returning data with lifetime '*$a'}} \
                  // expected-note@-4 {{declared with lifetime '*$a' here}}
}

void simple_function_call(int* $a x, int *$b y) {
  int *$a p;
  p = correct_simple_return(x); // no warning
  p = incorrect_simple_return(x); // no warning
  p = correct_simple_return(y); // expected-warning {{assignment requires that '*$b' outlives '*$a'}} \
                                // expected-note@-3 {{declared with lifetime '*$a' here}} \
                                // expected-note@-4 {{declared with lifetime '*$b' here}}
}

int *$a correct_function_call_two_args_1(int *$a x, int *$b y) {
  int *$a p;
  p = correct_simple_return(x, y);
  return p;
}

int *$a incorrect_function_call_two_args_1(int *$a x, int *$b y) {
  int *p;
  p = constraint_simple_return(x, y);
  return p; // expected-warning {{function should return data with lifetime '*$a' but it is returning data with lifetime '*$b'}} \
                  // expected-note@-1 {{declared with lifetime '*$b' here}}
}

int *$a correct_function_call_two_args_2(int *$a x, int *$b y) {
  return correct_simple_return(x, y);
}

int *$a incorrect_function_call_two_args_2(int *$a x, int *$b y) {
  return constraint_simple_return(x, y);  // expected-warning {{function should return data with lifetime '*$a' but it is returning data with lifetime '*$b'}} \
          // expected-note@-1 {{declared with lifetime '*$b' here}}
}

int *$a correct_function_call_two_args_3(int* $a x, int *$b y) {
  int *$a p;
  p = correct_simple_return(x, y);
  int *$a q = correct_simple_return(x, y);
  return p;
}

int *$a incorrect_function_call_two_args_3(int* $a x, int *$b y) {
  int *$a p;
  p = constraint_simple_return(x, y); // expected-warning {{assignment requires that '*$b' outlives '*$a'}} \
    // expected-note@-1 {{declared with lifetime '*$a' here}} \
    // expected-note@-2 {{declared with lifetime '*$b' here}}
  int *$a q = constraint_simple_return(x, y); // expected-warning {{initialization requires that '*$b' outlives '*$a'}} \
    // expected-note@-5 {{declared with lifetime '*$b' here}}
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
	r = y;			// expected-warning {{assignment requires that '*$b' outlives '*$a'}} \
              // expected-note@-1 {{declared with lifetime '*$a' here}} \
              // expected-note@-4 {{declared with lifetime '*$b' here}}
	p = x;
	p = q;
	return p;		// expected-warning {{function should return data with lifetime '*$b' but it is returning data with lifetime '*$a'}} \
          // expected-note@-2 {{declared with lifetime '*$a' here}} \
          // expected-note@-1 {{declared with lifetime '*$a' here}}
}

int *$a warn_on_assignment(int *$a x, int *$b y) {
	int *p;
	int *q = y;
	p = q;
	p = x;
	return p;		// expected-warning {{function should return data with lifetime '*$a' but it is returning data with lifetime '*$b'}} \
          // expected-note@-2 {{declared with lifetime '*$b' here}}
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

int *$a static_lifetime_return(int *$static x) { // expected-warning {{at least one parameter must be annotated with '$a'}}
  return x; // correct because $a <= $static
}

void static_lifetime_assign(int *$static x) {
  int *$a p = x; // correct because $a <= $static
  x = p;        // expected-warning {{assignment requires that '*$a' outlives '*$static'}} \
                // expected-note@-2 {{declared with lifetime '*$static' here}} \
                // expected-note@-1 {{declared with lifetime '*$a' here}}
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
  return p; // expected-warning {{function should return data with lifetime '*$static' but it is returning data with lifetime '*$a'}} \
            // expected-note@-3 {{declared with lifetime '*$a' here}}
}

void static_lifetime_function_call(int *$static x, int *$a y, int *$b z) {
  static_lifetime_return(x);  // correct
  static_lifetime_return(y);  // expected-warning {{argument requires that '*$a' outlives '*$static'}} \
                              // expected-note@-2 {{lifetime is '*$a'}} \
                              // expected-note@-28 {{lifetime is '*$static'}}
  y = static_lifetime_return(x); // correct
  x = static_lifetime_return(x);
  z = static_lifetime_return(x);
}

void aux_static_lifetimes(int *$static x1, int *$static *$a x2, int *$b *$static x3);

void static_lifetime_function_call_2(int *$a p1, int *$a *$static p2, int *$static *$c p3, int *$b p4) {
  aux_static_lifetimes(p1, p2, p3); // expected-warning {{argument requires that '**$c' outlives '**$static'}} \
                              // expected-note@-1 {{lifetime is '**$c'}} \
                              // expected-note@-3 {{lifetime is '**$static'}} \
                              // expected-warning {{argument requires that '*$a' outlives '*$static'}} \
                              // expected-note@-1 {{lifetime is '*$a'}} \
                              // expected-note@-3 {{lifetime is '*$static'}} \
                              // expected-warning {{argument requires that '*$a' outlives '*$static'}} \
                              // expected-note@-1 {{lifetime is '*$a'}} \
                              // expected-note@-3 {{lifetime is '*$static'}}
  int *p = p1;
  p = p4;
  static_lifetime_return(p);  // expected-warning {{argument requires that '*$a' outlives '*$static'}} \
                              // expected-note@-2 {{lifetime is '*$a'}} \
                              // expected-note@-50 {{lifetime is '*$static'}} \
                              // expected-warning {{argument requires that '*$b' outlives '*$static'}} \
                              // expected-note@-1 {{lifetime is '*$b'}} \
                              // expected-note@-50 {{lifetime is '*$static'}}
} 

void address_of_operator(int *$a x) {
  int i = 0;
  int *$a p = &i; // expected-warning {{initialization requires that '*$local' outlives '*$a'}} \
                // expected-note@-1 {{declared with lifetime '*$local' here}}
  int *$b *$a q = &x; // expected-warning {{initialization requires that '**$local' outlives '**$a'}} \
                // expected-note@-4 {{declared with lifetime '**$local' here}} \
                // expected-warning {{initialization requires that '*$a' outlives '*$b'}} \
                // expected-note@-4 {{declared with lifetime '*$a' here}}
}

void local_lifetime_assign_and_return(int *$a x) {
  int i = 1;
  x = &i;       // expected-warning {{assignment requires that '*$local' outlives '*$a'}} \
                // expected-note@-1 {{declared with lifetime '*$local' here}} \
                // expected-note@-2 {{declared with lifetime '*$a' here}}
  int *p = &i;  // p has local lifetime
  x = p;        // expected-warning {{assignment requires that '*$local' outlives '*$a'}} \
                // expected-note@-6 {{declared with lifetime '*$a' here}} \
                // expected-note@-1 {{declared with lifetime '*$local' here}}
}

int *$a return_local_lifetime_1() { // expected-warning {{at least one parameter must be annotated with '$a'}}
  int i = 0;
  int *p = &i;
  return p; // expected-warning {{cannot return data with lifetime '*$local'}} \
            // expected-note@-1 {{declared with lifetime '*$local' here}}
}

int *$local return_local_lifetime_2() { // expected-warning {{the return lifetime cannot be '$local'}}
  int i = 0;
  int *p = &i;
  return p; // expected-warning {{cannot return data with lifetime '*$local'}} \
  // expected-note@-1 {{declared with lifetime '*$local' here}}
}

int *return_local_lifetime_3() {
  int i = 0;
  int *p = &i;
  return p; // expected-warning {{cannot return data with lifetime '*$local'}} \
            // expected-note@-1 {{declared with lifetime '*$local' here}}
}

void multiple_indirections(int *$a *$b *$c *$d x) {
  int ****p;
  p = x;
  *p = *x;
  **p = **x;
  ***p = ***x;
  ****p = ****x;
}

void simple_unary_operator(int *$a p) {
  int *$c *$b pp = &p;  // expected-warning {{initialization requires that '**$local' outlives '**$b'}} \
          // expected-note@-1 {{declared with lifetime '**$local' here}} \
          // expected-warning {{initialization requires that '*$a' outlives '*$c'}} \
          // expected-note@-1 {{declared with lifetime '*$a' here}}
  int i = 0;
  int *$a x = &i; // expected-warning {{initialization requires that '*$local' outlives '*$a'}} \
          // expected-note@-1 {{declared with lifetime '*$local' here}}
}

int *$a unary_op_1(int *$a p) {
  int **pp = &p;
  p = *pp;
  return *pp;
}

void pointer_aliasing_1(int *$a *$b x) {
  int **p01 = x;
  int *$a *p02 = x;
  int *$b *p03 = x; // expected-warning {{initialization requires that '*$a' outlives '*$b'}} \
                // expected-note@-3 {{declared with lifetime '*$a' here}}
  int **$a p04 = x; // expected-warning {{initialization requires that '**$b' outlives '**$a'}} \
                // expected-note@-5 {{declared with lifetime '**$b' here}}
  int **$b p05 = x;
  int *$a *$b p06 = x;
  int *$b *$a p07 = x; // expected-warning {{initialization requires that '**$b' outlives '**$a'}} \
                // expected-note@-9 {{declared with lifetime '**$b' here}} \
                // expected-warning {{initialization requires that '*$a' outlives '*$b'}} \
                // expected-note@-9 {{declared with lifetime '*$a' here}}
  int *$a *$a p08 = x;  // expected-warning {{initialization requires that '**$b' outlives '**$a'}} \
                // expected-note@-13 {{declared with lifetime '**$b' here}}
  int *$b *$b p09 = x;  // expected-warning {{initialization requires that '*$a' outlives '*$b'}} \
                // expected-note@-15 {{declared with lifetime '*$a' here}}
}

void pointer_aliasing_2(int *$a *$b x) {
  int **p01;
  p01 = x;
  *p01 = *x;
  int *$a *p02;
  p02 = x;
  *p02 = *x;
  int *$b *p03;
  p03 = x; // expected-warning {{assignment requires that '*$a' outlives '*$b'}} \
                // expected-note@-8 {{declared with lifetime '*$a' here}} \
                // expected-note@-1 {{declared with lifetime '*$b' here}}
  *p03 = *x;  // expected-warning {{assignment requires that '*$a' outlives '*$b'}} \
                // expected-note@-11 {{declared with lifetime '*$a' here}} \
                // expected-note@-4 {{declared with lifetime '*$b' here}}
  int **$b p05;
  p05 = x;
  *p05 = *x;
  int *$a *$b p06;
  p06 = x;
  *p06 = *x;
  int *$b *$a p07;
  p07 = x; // expected-warning {{assignment requires that '**$b' outlives '**$a'}} \
                // expected-note@-21 {{declared with lifetime '**$b' here}} \
                // expected-note@-1 {{declared with lifetime '**$a' here}} \
                // expected-warning {{assignment requires that '*$a' outlives '*$b'}} \
                // expected-note@-21 {{declared with lifetime '*$a' here}} \
                // expected-note@-1 {{declared with lifetime '*$b' here}}
  *p07 = *x;    // expected-warning {{assignment requires that '*$a' outlives '*$b'}} \
                // expected-note@-27 {{declared with lifetime '*$a' here}} \
                // expected-note@-7 {{declared with lifetime '*$b' here}}
  int *$a *$a p08;
  p08 = x;  // expected-warning {{assignment requires that '**$b' outlives '**$a'}} \
                // expected-note@-31 {{declared with lifetime '**$b' here}}\
                // expected-note@-1 {{declared with lifetime '**$a' here}}
  *p08 = *x;
  int *$b *$b p09;
  p09 = x;  // expected-warning {{assignment requires that '*$a' outlives '*$b'}} \
                // expected-note@-36 {{declared with lifetime '*$a' here}}\
                // expected-note@-1 {{declared with lifetime '*$b' here}}
  *p09 = *x;  // expected-warning {{assignment requires that '*$a' outlives '*$b'}} \
                // expected-note@-39 {{declared with lifetime '*$a' here}}\
                // expected-note@-4 {{declared with lifetime '*$b' here}}
}

int *$a *$b pointer_aliasing_3(int *$a *$b x, int *$c *$d y, int num) {
  int **p01 = x;
  if (num == 1) return p01;
  int **p02 = y;
  if (num == 2) return p02; // expected-warning {{function should return data with lifetime '**$b' but it is returning data with lifetime '**$d'}} \
                // expected-note@-1 {{declared with lifetime '**$d' here}} \
                // expected-warning {{function should return data with lifetime '*$a' but it is returning data with lifetime '*$c'}} \
                // expected-note@-1 {{declared with lifetime '*$c' here}}
  int *$a *p03 = x;
  if (num == 3) return p03;
  int **p04;
  p04 = x;
  if (num == 4) return p04;
  int **p05;
  p05 = y;
  if (num == 5) return p05; // expected-warning {{function should return data with lifetime '**$b' but it is returning data with lifetime '**$d'}} \
                // expected-note@-1 {{declared with lifetime '**$d' here}} \
                // expected-warning {{function should return data with lifetime '*$a' but it is returning data with lifetime '*$c'}} \
                // expected-note@-1 {{declared with lifetime '*$c' here}}
  int *$a *p06;
  p06 = x;
  if (num == 6) return p06; 

  int **p07 = x;
  if (num == 7) return p07; // expected-warning {{function should return data with lifetime '*$a' but it is returning data with lifetime '*$c'}} \
                // expected-note@+2 {{declared with lifetime '*$c' here}}
  *p07 = *y;    // expected-warning {{assignment requires that '*$c' outlives '*$a'}} \
                // expected-note@-26 {{declared with lifetime '*$c' here}} \
                // expected-note@-3 {{declared with lifetime '*$a' here}}
  return x;
}


int *$a *$c *$a pointer_aliasing_4(int *$a *$b *$c *$d x, int *$a y) {
  int * *$a **p;  
  p = x;    // expected-warning {{assignment requires that '**$b' outlives '**$a'}} \
            // expected-note@-1 {{declared with lifetime '**$a' here}} \
            // expected-note@-2 {{declared with lifetime '**$b' here}}
  int **q = &y;
  int ***xx = *x;
  int **$b *$a r = *x;  // expected-warning {{initialization requires that '***$c' outlives '***$a'}} \
          // expected-note@-7 {{declared with lifetime '***$c' here}}
  return *p;  // expected-warning {{function should return data with lifetime '**$c' but it is returning data with lifetime '**$a'}} \
      // expected-note@-8 {{declared with lifetime '**$a' here}} \
      // expected-warning {{function should return data with lifetime '***$a' but it is returning data with lifetime '***$c'}} \
      // expected-note@-7 {{declared with lifetime '***$c' here}}
}

void pointer_aliasing_5(int *$a x, int *$a *$b y) {
  int **p = &x;
  int i = 0;
  *p = &i;  // expected-warning {{assignment requires that '*$local' outlives '*$a'}} \
            // expected-note@-1 {{declared with lifetime '*$local' here}} \
            // expected-note@-2 {{declared with lifetime '*$a' here}}

  int *$a *q = y;
  *q = &i;  // expected-warning {{assignment requires that '*$local' outlives '*$a'}} \
            // expected-note@-1 {{declared with lifetime '*$a' here}} \
            // expected-note@-6 {{declared with lifetime '*$local' here}}

  int **r = y;
  *r = &i;  // expected-warning {{assignment requires that '*$local' outlives '*$a'}} \
            // expected-note@-11 {{declared with lifetime '*$local' here}} \
            // expected-note@-1 {{declared with lifetime '*$a' here}}
}

void pointer_aliasing_6(int *$a *$c x, int *$a *$b y, int *$b *$b z) {
  int *p = *y;
  *y = *x;
  *x = p;

  int *q = *y;
  *y = *z;  // expected-warning {{assignment requires that '*$b' outlives '*$a'}} \
            // expected-note@-6 {{declared with lifetime '*$a' here}} \
            // expected-note@-6 {{declared with lifetime '*$b' here}}
  *z = q;   // expected-warning {{assignment requires that '*$a' outlives '*$b'}} \
            // expected-note@-4 {{declared with lifetime '*$a' here}} \
            // expected-note@-9 {{declared with lifetime '*$b' here}}
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
  y = q;  // expected-warning {{assignment requires that '*$a' outlives '*$c'}} \
          // expected-note@-6 {{declared with lifetime '*$a' here}} \
          // expected-note@-10 {{declared with lifetime '*$c' here}} \
          // expected-warning {{assignment requires that '*$e' outlives '*$c'}} \
          // expected-note@-6 {{declared with lifetime '*$e' here}} 
  x = p;  // expected-warning {{assignment requires that '*$c' outlives '*$a'}} \
          // expected-note@-11 {{declared with lifetime '*$c' here}} \
          // expected-note@-10 {{declared with lifetime '*$c' here}} \
          // expected-note@-15 {{declared with lifetime '*$a' here}} \
          // expected-warning {{assignment requires that '*$e' outlives '*$a'}} \
          // expected-note@-10 {{declared with lifetime '*$e' here}} \
          // expected-note@-11 {{declared with lifetime '*$e' here}} \
          // expected-warning {{assignment requires that '**$d' outlives '**$b'}} \
          // expected-note@-11 {{declared with lifetime '**$d' here}} \
          // expected-note@-15 {{declared with lifetime '**$b' here}}
  int *$e *$f zz = r;  // expected-warning {{initialization requires that '*$a' outlives '*$e'}} \
          // expected-note@-20 {{declared with lifetime '*$a' here}} \
          // expected-warning {{initialization requires that '*$c' outlives '*$e'}} \
          // expected-note@-20 {{declared with lifetime '*$c' here}} \
          // expected-warning {{initialization requires that '**$b' outlives '**$f'}} \
          // expected-note@-20 {{declared with lifetime '**$b' here}} \
          // expected-warning {{initialization requires that '**$d' outlives '**$f'}} \
          // expected-note@-20 {{declared with lifetime '**$d' here}}
  return p; // expected-warning {{function should return data with lifetime '*$e' but it is returning data with lifetime '*$a'}} \
            // expected-note@-26 {{declared with lifetime '*$a' here}} \
            // expected-note@-29 {{declared with lifetime '*$a' here}} \
            // expected-note@-28 {{declared with lifetime '*$a' here}} \
            // expected-note@-18 {{declared with lifetime '*$a' here}} \
            // expected-warning {{function should return data with lifetime '*$e' but it is returning data with lifetime '*$c'}} \
            // expected-note@-29 {{declared with lifetime '*$c' here}} \
            // expected-note@-28 {{declared with lifetime '*$c' here}} \
            // expected-warning {{function should return data with lifetime '**$d' but it is returning data with lifetime '**$b'}} \
            // expected-note@-26 {{declared with lifetime '**$b' here}}
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
void fn61(int *$a *$b *$c x, int *$a *$b y);

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


  fn32(pp, p);
  fn32(pp, q);
  fn32(qq, p);
  fn32(qq, q);
  fn33(pp, p);
  fn33(pp, q);  // expected-warning {{when calling function 'fn33', the lifetime of 'q' cannot be shorter than the lifetime of '*pp'}} \
                // expected-note@-21 {{lifetime is '*$b'}} \
                // expected-note@-15 {{lifetime is '*$a'}}
  fn33(qq, p);
  fn33(qq, q);  // expected-warning {{when calling function 'fn33', the lifetime of 'q' cannot be shorter than the lifetime of '*qq'}} \
                // expected-note@-25 {{lifetime is '*$b'}} \
                // expected-note@-18 {{lifetime is '*$a'}}
  fn34(pp, p);
  fn34(pp, q);
  fn34(qq, p);
  fn34(qq, q);

  fn41(pp, pp);
  fn41(pp, qq); 
  fn41(pp, rr);
  fn42(pp, qq); 
  fn42(pp, rr); // expected-warning {{when calling function 'fn42', the lifetimes of '*pp' and '*rr' should be the same}} \
                // expected-note@-31 {{lifetime is '*$a'}} \
                // expected-note@-29 {{lifetime is '*$b'}}
  fn42(pp, ss); // expected-warning {{when calling function 'fn42', the lifetimes of '*pp' and '*ss' should be the same}} \
                // expected-note@-34 {{lifetime is '*$a'}} \
                // expected-note@-31 {{lifetime is '*$b'}}
  fn43(pp, qq); 
  fn43(pp, rr); // expected-warning {{when calling function 'fn43', the lifetimes of '*pp' and '*rr' should be the same}} \
                // expected-note@-38 {{lifetime is '*$a'}} \
                // expected-note@-36 {{lifetime is '*$b'}}
  fn43(pp, ss); // expected-warning {{when calling function 'fn43', the lifetimes of '*pp' and '*ss' should be the same}} \
                // expected-note@-41 {{lifetime is '*$a'}} \
                // expected-note@-38 {{lifetime is '*$b'}}
  fn44(pp, pp);
  fn44(pp, qq); 
  fn44(pp, rr);
  fn44(pp, ss);
  
  fn51(p, rr, ss); // expected-warning {{when calling function 'fn51', the lifetime of 'p' cannot be shorter than the lifetime of '*rr'}} \
                   // expected-warning {{when calling function 'fn51', the lifetime of 'p' cannot be shorter than the lifetime of '*ss'}} \
                   // expected-note@-56 {{lifetime is '*$a'}} \
                   // expected-note@-47 {{lifetime is '*$b'}} \
                   // expected-note@-56 {{lifetime is '*$a'}} \
                   // expected-note@-46 {{lifetime is '*$b'}}

  fn52(qq, pp, ss); // expected-warning {{when calling function 'fn52', the lifetimes of '*pp' and '*ss' should be the same}} \
                    // expected-note@-56 {{lifetime is '*$a'}} \
                    // expected-note@-53 {{lifetime is '*$b'}}
  // TODO call fn61
  fn31(pp, p);      // expected-warning {{argument requires that '*$a' outlives '*$static'}} \
                    // expected-note@-67 {{lifetime is '*$a'}} \
                    // expected-note@-80 {{lifetime is '*$static'}}
  int *$static s;
  fn31(pp, s);

  int *$b *$b *$c xxx;
  fn61(xxx, pp);  // expected-warning {{when calling function 'fn61', the lifetime of 'pp' cannot be shorter than the lifetime of '*xxx'}} \
                  // expected-note@-67 {{lifetime is '**$a'}} \
                  // expected-note@-1 {{lifetime is '**$b'}} \
                  // expected-warning {{when calling function 'fn61', the lifetimes of '**xxx' and '*pp' should be the same}} \
                  // expected-note@-1 {{lifetime is '*$b'}} \
                  // expected-note@-67 {{lifetime is '*$a'}}
  int *$a *$a *$b yyy;
  fn61(yyy, pp);
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
  *xy = x;  // expected-warning {{assignment requires that '*$a' outlives '*$b'}} expected-note@-12 {{declared with lifetime '*$a' here}} expected-note@+1 {{declared with lifetime '*$b' here}}
  *xy = y;  // expected-warning {{assignment requires that '*$b' outlives '*$a'}} expected-note@-13 {{declared with lifetime '*$b' here}} expected-note@-1 {{declared with lifetime '*$a' here}}
  *yw = y;  // expected-warning {{assignment requires that '*$b' outlives '*$c'}} expected-note@-14 {{declared with lifetime '*$b' here}} expected-note@+1 {{declared with lifetime '*$c' here}}
  *yw = w;  // expected-warning {{assignment requires that '*$c' outlives '*$b'}} expected-note@-15 {{declared with lifetime '*$c' here}} expected-note@-1 {{declared with lifetime '*$b' here}}
  *xyw = x; // expected-warning {{assignment requires that '*$a' outlives '*$b'}} expected-note@-16 {{declared with lifetime '*$a' here}} expected-note@+1 {{declared with lifetime '*$b' here}} expected-warning {{assignment requires that '*$a' outlives '*$c'}} expected-note@-16 {{declared with lifetime '*$a' here}} expected-note@+2 {{declared with lifetime '*$c' here}}
  *xyw = y; // expected-warning {{assignment requires that '*$b' outlives '*$a'}} expected-note@-17 {{declared with lifetime '*$b' here}} expected-note@-1 {{declared with lifetime '*$a' here}} expected-warning {{assignment requires that '*$b' outlives '*$c'}} expected-note@-17 {{declared with lifetime '*$b' here}} expected-note@+1 {{declared with lifetime '*$c' here}}
  *xyw = w; // expected-warning {{assignment requires that '*$c' outlives '*$a'}} expected-note@-18 {{declared with lifetime '*$c' here}} expected-note@-2 {{declared with lifetime '*$a' here}} expected-warning {{assignment requires that '*$c' outlives '*$b'}} expected-note@-18 {{declared with lifetime '*$c' here}} expected-note@-1 {{declared with lifetime '*$b' here}}

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
                // expected-note@-35 {{lifetime is '*$b'}} \
                // expected-note@-25 {{lifetime is '*$a'}} \
                // expected-note@-24 {{lifetime is '*$a'}}
  fn33(xy, x);
  fn33(xy, y); 
  fn33(xy, w);  // expected-warning {{when calling function 'fn33', the lifetime of 'w' cannot be shorter than the lifetime of '*xy'}} \
                // expected-note@-41 {{lifetime is '*$c'}} \
                // expected-note@-29 {{lifetime is '*$a'}} \
                // expected-note@-28 {{lifetime is '*$b'}}
  fn33(xx, *xyw); // expected-warning {{when calling function 'fn33', the lifetime of '*xyw' cannot be shorter than the lifetime of '*xx'}} \
                // expected-note@-28 {{lifetime is '*$b'}} \
                // expected-note@-27 {{lifetime is '*$c'}} \
                // expected-note@-35 {{lifetime is '*$a'}} \
                // expected-note@-34 {{lifetime is '*$a'}}
                
  fn33(xx, *yw);  // expected-warning {{when calling function 'fn33', the lifetime of '*yw' cannot be shorter than the lifetime of '*xx'}} \
                // expected-note@-37 {{lifetime is '*$b'}} \
                // expected-note@-36 {{lifetime is '*$c'}} \
                // expected-note@-40 {{lifetime is '*$a'}} \
                // expected-note@-41 {{lifetime is '*$a'}}
  fn33(xx, p);  // expected-warning {{when calling function 'fn33', the lifetime of 'p' cannot be shorter than the lifetime of '*xx'}} \
                // expected-note@-55 {{lifetime is '*$b'}} \
                // expected-note@-54 {{lifetime is '*$c'}} \
                // expected-note@-46 {{lifetime is '*$a'}} \
                // expected-note@-45 {{lifetime is '*$a'}}
  fn33(xy, q);  // expected-warning {{when calling function 'fn33', the lifetime of 'q' cannot be shorter than the lifetime of '*xy'}} \
                // expected-note@-58 {{lifetime is '*$c'}} \
                // expected-note@-57 {{lifetime is '*$d'}} \
                // expected-note@-49 {{lifetime is '*$a'}} \
                // expected-note@-48 {{lifetime is '*$b'}}
  fn42(xx, xy); // expected-warning {{when calling function 'fn42', the lifetimes of '*xx' and '*xy' should be the same}} \
                // expected-note@-56 {{lifetime is '*$a}} \
                // expected-note@-55 {{lifetime is '*$a}} \
                // expected-note@-53 {{lifetime is '*$b}}
  fn42(xy, xy);
  fn42(xy, xx); // expected-warning {{when calling function 'fn42', the lifetimes of '*xy' and '*xx' should be the same}} \
                // expected-note@-61 {{lifetime is '*$a}} \
                // expected-note@-60 {{lifetime is '*$a}} \
                // expected-note@-58 {{lifetime is '*$b}}
  fn42(xy, xyw); // expected-warning {{when calling function 'fn42', the lifetimes of '*xy' and '*xyw' should be the same}} \
                // expected-note@-57 {{lifetime is '*$c}}
}

int *$a arrays(int *$b *$c arr) { // expected-warning {{at least one parameter must be annotated with '$a'}}
  int *p = arr[0];
  int *q = arr[1];
  int *$a r = arr[2]; // expected-warning {{initialization requires that '*$b' outlives '*$a'}} \
                      // expected-note@-3 {{declared with lifetime '*$b' here}}
  int *$b s = arr[3];
  p = q;
  return p;  // expected-warning {{function should return data with lifetime '*$a' but it is returning data with lifetime '*$b'}} \
            // expected-note@-6 {{declared with lifetime '*$b' here}} \
            // expected-note@-1 {{declared with lifetime '*$b' here}}
}

void max_lifetimes(int *$a x, int *$b y, int *$c *$b z) {
        int ***p;
        *p = z;
        **p = x; // expected-warning {{assignment requires that '*$a' outlives '*$b'}} \
                // expected-note@-3 {{declared with lifetime '*$a' here}} \
                // expected-note@+6 {{declared with lifetime '*$b' here}} \
                // expected-warning{{assignment requires that '*$a' outlives '*$c'}} \
                // expected-note@-3 {{declared with lifetime '*$a' here}} \
                // expected-note@-1 {{declared with lifetime '*$c' here}}
        **p = y;  // expected-warning {{assignment requires that '*$b' outlives '*$a'}} \
                // expected-note@-9 {{declared with lifetime '*$b' here}} \
                // expected-note@-6 {{declared with lifetime '*$a' here}} \
                // expected-warning {{assignment requires that '*$b' outlives '*$c'}} \
                // expected-note@-9 {{declared with lifetime '*$b' here}} \
                // expected-note@-7{{declared with lifetime '*$c' here}}
}

int *no_return_lifetime(int *$a x) {
  return x;
}

int *$a call_no_return_lifetime(int *$a x) {
  int *p = no_return_lifetime(x);
  if (*x < 0) return p; // expected-warning {{cannot return data with lifetime '*$local'}} \
                        // expected-note@-1 {{declared with lifetime '*$local' here}}

  int *q = x;
  q = no_return_lifetime(x);
  if (*x < 1) return q; // expected-warning {{cannot return data with lifetime '*$local'}} \
                        // expected-note@-1 {{declared with lifetime '*$local' here}}

  int *r = x;
  r = no_return_lifetime(x) + 1;
  if (*x < 2) return r; // expected-warning {{cannot return data with lifetime '*$local'}} \
                        // expected-note@-1 {{declared with lifetime '*$local' here}}
  return x;
}

int *return_local() {
  int *$local p;
  return p; // expected-warning {{cannot return data with lifetime '*$local'}} \
            // expected-note@-1 {{declared with lifetime '*$local' here}}
}

int **return_local2() {
  int *$local *p;
  return p; // expected-warning {{cannot return data with lifetime '*$local'}} \
            // expected-note@-1 {{declared with lifetime '*$local' here}}
}

int **return_local3() {
  int *$local *$local p;
  return p; // expected-warning {{cannot return data with lifetime '*$local'}} \
            // expected-warning {{cannot return data with lifetime '**$local'}} \
            // expected-note@-1 {{declared with lifetime '*$local' here}} \
            // expected-note@-1 {{declared with lifetime '**$local' here}}
}

int **return_local4() {
  int **$local p;
  return p; // expected-warning {{cannot return data with lifetime '**$local'}} \
            // expected-note@-1 {{declared with lifetime '**$local' here}}
}

int *$a dead1(int *$a x, int **y);
void dead2(int *$a *$b x);

int *$a dead_lifetimes(int *$a x, int *$b *$a y) {
    int *p = dead1(x, y); 
    int **q = y;  // expected-warning {{cannot read data with lifetime '*$dead'}} \
                  // expected-note@-1 {{declared with lifetime '*$dead' here}} \
                  // expected-note@-5 {{parameter was declared with lifetime '*$local'}}
    dead2(y);     // expected-warning {{cannot read data with lifetime '*$dead'}} \
                  // expected-note@-4 {{declared with lifetime '*$dead' here}} \
                  // expected-note@-8 {{parameter was declared with lifetime '*$local'}}
    return *q;
}

void dead4(int **x);
void dead5(int *$a x, int *$b *$a y) {
    int *p = dead1(x, y); // expected-warning {{cannot read data with lifetime '*$dead'}} \
                  // expected-note@+3 {{declared with lifetime '*$dead' here}} \
                  // expected-note@-2 {{parameter was declared with lifetime '*$local'}}
    dead4(y);     // expected-warning {{cannot read data with lifetime '*$dead'}} \
                  // expected-note@-3 {{declared with lifetime '*$dead' here}} \
                  // expected-note@-19 {{parameter was declared with lifetime '*$local'}}
}

struct S {
        int *p;
        int num;
};

struct SS {
        struct S s;
};


int *$a f(struct SS *$a ss) {
        return ss->s.p;
}

int *$a f(struct S *$a s) {
        return &s->num;
}