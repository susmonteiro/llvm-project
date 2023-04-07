#ifndef LIFETIME_ANNOTATIONS_LIFETIME_H_
#define LIFETIME_ANNOTATIONS_LIFETIME_H_

#include <atomic>
#include <cassert>
#include <functional>
#include <ostream>
#include <string>
#include <utility>

#include "llvm/ADT/Hashing.h"

namespace clang {

// A lifetime variable or constant lifetime.
class Lifetime {
public:
  // Creates an invalid lifetime.
  //
  // This is provided because containers need default constructors. It is not
  // legal to perform any operations on an invalid lifetime except to copy or
  // delete it.
  //
  // Use one of the static member functions below to create a valid lifetime.
  Lifetime();

  Lifetime(const Lifetime &) = default;
  Lifetime &operator=(const Lifetime &) = default;

  // Creates a new lifetime variable.
  static Lifetime CreateVariable();

  // Returns the 'static lifetime constant.
  static Lifetime Static();

  // Creates a new local lifetime constant.
  static Lifetime CreateLocal();

  // Returns whether this lifetime is a lifetime variable.
  bool IsVariable() const;

  // Returns whether this lifetime is a constant lifetime.
  bool IsConstant() const;

  // Returns whether this lifetime is a local lifetime.
  bool IsLocal() const;

  // Returns a unique numeric ID for the lifetime.
  int Id() const { return id_; }

  // Returns a textual representation of the lifetime for debug logging.
  std::string DebugString() const;

  bool operator==(Lifetime other) const {
    assert(IsValid());
    assert(other.IsValid());
    return id_ == other.id_;
  }

  bool operator!=(Lifetime other) const { return !(*this == other); }

private:
  explicit Lifetime(int id);

  static Lifetime InvalidEmpty();
  static Lifetime InvalidTombstone();

  bool IsValid() const;

  // TODO check what I need from here and implement my own lifetime class 

  friend class llvm::DenseMapInfo<Lifetime, void>;
  friend class std::less<Lifetime>;

  int id_;
  static std::atomic<int> next_variable_id_;
  static std::atomic<int> next_local_id_;
};

std::ostream &operator<<(std::ostream &os, Lifetime lifetime);

} // namespace lifetimes


#endif // LIFETIME_ANNOTATIONS_LIFETIME_H_
