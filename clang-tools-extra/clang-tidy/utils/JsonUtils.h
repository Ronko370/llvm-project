#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_JSONUTILS_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_JSONUTILS_H

#include "llvm/Support/JSON.h"
#include "clang/Basic/FileManager.h"

namespace clang::tidy::utils {

void writeJSONToFile(FileManager& Manager, llvm::json::Object* Obj, std::string jsonFile);

llvm::json::Object* readJSONFromFile(FileManager& Manager, std::string jsonFile);
}

#endif