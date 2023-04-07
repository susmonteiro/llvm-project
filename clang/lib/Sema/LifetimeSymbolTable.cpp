#include "LifetimeSymbolTable.h"

#include <iostream>
#include <optional>
#include <string>

#include "llvm/Support/ErrorHandling.h"

namespace clang {

static std::string NameFromIndex(int index) {
  int num_chars = 1;
  // Number of combinations that are possible with `num_chars` characters.
  int num_combinations = 26;
  while (index >= num_combinations) {
    index -= num_combinations;
    ++num_chars;
    num_combinations *= 26;
  }
  std::string name;
  name.reserve(num_chars);
  for (int i = 0; i < num_chars; ++i) {
    name.insert(0, static_cast<size_t>(1), 'a' + index % 26);
    index /= 26;
  }
  return name;
}

Lifetime LifetimeSymbolTable::LookupNameAndMaybeDeclare(llvm::StringRef name) {
  if (name == "static") {
    return Lifetime::Static();
  }

  auto [iter, inserted] =
      name_to_lifetime_.try_emplace(name, Lifetime::Static());
  if (inserted) {
    Lifetime lifetime = Lifetime::CreateVariable();
    iter->second = lifetime;
    assert(!lifetime_to_name_.count(lifetime));
    lifetime_to_name_[lifetime] = name;
  }
  return iter->second;
}

llvm::StringRef LifetimeSymbolTable::LookupLifetimeAndMaybeDeclare(
    Lifetime lifetime) {
  if (lifetime == Lifetime::Static()) {
    return "static";
  }

  auto lifetime_to_name_iter = lifetime_to_name_.find(lifetime);
  if (lifetime_to_name_iter != lifetime_to_name_.end()) {
    return lifetime_to_name_iter->second;
  }

  while (true) {
    std::string name = NameFromIndex(next_name_index_++);
    auto [name_to_lifetime_iter, inserted] =
        name_to_lifetime_.try_emplace(name, lifetime);
    if (inserted) {
      lifetime_to_name_[lifetime] = name;
      return name_to_lifetime_iter->first();
    }
  }
}
}  // namespace clang