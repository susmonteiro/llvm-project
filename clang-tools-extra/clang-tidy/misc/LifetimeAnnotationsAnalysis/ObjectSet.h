// TODO change me
#ifndef DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_OBJECT_SET_H_
#define DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_OBJECT_SET_H_

#include <initializer_list>
#include <string>

#include "Object.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallSet.h"

namespace clang {
namespace tidy {
namespace lifetimes {

// A set of `Object`s.
class ObjectSet {
public:
  using const_iterator = llvm::SmallSet<const Object *, 2>::const_iterator;
  using value_type = const Object *;

  ObjectSet() = default;

  ObjectSet(const ObjectSet &) = default;
  ObjectSet(ObjectSet &&) = default;
  ObjectSet &operator=(const ObjectSet &) = default;
  ObjectSet &operator=(ObjectSet &&) = default;

  // Initializes the object set with `objects`.
  ObjectSet(std::initializer_list<const Object *> objects) {
    for (const Object *object : objects) {
      objects_.insert(object);
    }
  }

  // Returns a human-readable string representation of the object set.
  std::string ObjectSet::DebugString() const {
    std::vector<std::string> parts;
    for (const Object *object : objects_) {
      parts.push_back(object->DebugString());
    }
    return absl::StrJoin(parts, ", ");
  }

  const_iterator begin() const { return objects_.begin(); }

  const_iterator end() const { return objects_.end(); }

  bool empty() const { return objects_.empty(); }

  size_t size() const { return objects_.size(); }

  // Returns whether this set contains `object`.
  bool Contains(const Object *object) const {
    return objects_.contains(object);
  }

  // Returns whether this set contains all objects in `other`, i.e. whether
  // this set is a superset of `other`.
  bool Contains(const ObjectSet &other) const {
    for (const Object *object : other) {
      if (!Contains(object)) {
        return false;
      }
    }
    return true;
  }

  // Returns a `ObjectSet` containing the union of the pointees from this
  // `ObjectSet` and `other`.
  ObjectSet Union(const ObjectSet &other) const {
    ObjectSet result = *this;
    result.Add(other);
    return result;
  }

  // Returns a `ObjectSet` containing the intersection of the pointees from this
  // `ObjectSet` and `other`.
  ObjectSet Intersection(const ObjectSet &other) const {
    ObjectSet result;
    for (const Object *obj : *this) {
      if (other.Contains(obj))
        result.Add(obj);
    }
    return result;
  }

  // Adds `object` to this object set.
  void Add(const Object *object) { objects_.insert(object); }

  // Adds the `other` objects to this object set.
  void Add(const ObjectSet &other) {
    objects_.insert(other.objects_.begin(), other.objects_.end());
  }

  bool operator==(const ObjectSet &other) const {
    return objects_ == other.objects_;
  }
  bool operator!=(const ObjectSet &other) const { return !(*this == other); }

private:
  friend std::ostream &operator<<(std::ostream &os,
                                  const ObjectSet &object_set) {
    return os << object_set.DebugString();
  }

  llvm::SmallSet<const Object *, 2> objects_;
};

} // namespace lifetimes
} // namespace tidy
} // namespace clang

#endif // DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_OBJECT_SET_H_
