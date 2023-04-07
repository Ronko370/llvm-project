#include "JsonUtils.h"

namespace clang::tidy::utils {

using namespace llvm::json;

void writeJSONToFile(FileManager& Manager, Object* Obj, std::string jsonFile) {
  std::error_code EC;
  llvm::raw_fd_ostream OS(jsonFile, EC);
  if (EC) {
    llvm::errs() << "Error opening output file: " << EC.message() << '\n';
    assert(0);
  }

  OStream JOS(OS, 2);
  JOS.value(Value(std::move(*Obj)));
  JOS.flush();
  OS.close();
}

Object* readJSONFromFile(FileManager& Manager, std::string jsonFile) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> Buffer = Manager.getBufferForFile(jsonFile);

  if (Buffer) {
    llvm::Expected<Value> V = parse(Buffer.get()->getBuffer());
    if(!V) {
      assert(0 && "JSON parse error");
    }

    return new Object(std::move(*V->getAsObject()));
  }

  return new Object();
}
}