// Part of the Crubit project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "Lifetime.h"

namespace clang {
namespace tidy {
namespace lifetimes {

// TODO do I need this?
constexpr int INVALID_LIFETIME_NAME_EMPTY = 0;
// TODO do I need this?

constexpr int INVALID_LIFETIME_NAME_TOMBSTONE = -1;
constexpr int STATIC_LIFETIME = 1;
constexpr int LOCAL_LIFETIME = 2;
constexpr int VARIABLE_LIFETIME = 3;

// TODO need this?
// Lifetime::Lifetime() : id_(INVALID_LIFETIME_NAME_EMPTY) {}

Lifetime Lifetime::CreateVariable(std::string name) {
  return Lifetime(name, VARIABLE_LIFETIME);
}

Lifetime Lifetime::Static() { return Lifetime(STATIC_LIFETIME); }

Lifetime Lifetime::CreateLocal() { return Lifetime(LOCAL_LIFETIME); }

bool Lifetime::IsVariable() const {
  assert(IsValid());
  return type_ == VARIABLE_LIFETIME;
}

bool Lifetime::IsConstant() const {
  assert(IsValid());
  return !IsVariable();
}

bool Lifetime::IsLocal() const {
  assert(IsValid());
  return type_ == LOCAL_LIFETIME;
}

std::string Lifetime::ToString() const {
  assert(IsValid());
  switch (type_) {
  case INVALID_LIFETIME_NAME_EMPTY:
    return "INVALID_EMPTY";
  case INVALID_LIFETIME_NAME_TOMBSTONE:
    return "INVALID_TOMBSTONE";
  case STATIC_LIFETIME:
    return "'static";
  case LOCAL_LIFETIME:
    return "'local";
  default:
    return "'" + lifetime_;
  }
}

Lifetime::Lifetime(std::string name, int type) : lifetime_(name), type_(type) {}

Lifetime Lifetime::InvalidEmpty() {
  return Lifetime(INVALID_LIFETIME_NAME_EMPTY);
}

Lifetime Lifetime::InvalidTombstone() {
  return Lifetime(INVALID_LIFETIME_NAME_TOMBSTONE);
}

bool Lifetime::IsValid() const {
  return type_ != INVALID_LIFETIME_NAME_EMPTY &&
         type_ != INVALID_LIFETIME_NAME_TOMBSTONE;
}

std::ostream &operator<<(std::ostream &os, Lifetime lifetime) {
  return os << lifetime.ToString();
}

} // namespace lifetimes
} // namespace tidy
} // namespace clang
