#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/mlvalues.h>

#include "nlohmann/json.hpp"
#include "xeus/xhelper.hpp"
#include "xeus/xinterpreter.hpp"
#include "xeus/xkernel.hpp"
#include "xeus/xkernel_configuration.hpp"
#include "xeus-zmq/xserver_zmq.hpp"
#include "xeus-zmq/xzmq_context.hpp"

namespace nl = nlohmann;
using xeus::xinterpreter;

class satysfi_interpreter : public xinterpreter {
public:

  satysfi_interpreter() = default;
  virtual ~satysfi_interpreter() = default;

private:

  void configure_impl() {}

  nl::json execute_request_impl(
    int execution_counter,
    const std::string& code,
    bool silent,
    bool store_history,
    nl::json user_expressions,
    bool allow_stdin
  ) {
    const static value *execute = nullptr;
    if (execute == nullptr) {
      execute = caml_named_value("satysfi_jupyter_kernel_execute");
    }
    auto result = caml_callback(*execute, caml_copy_string(code.c_str()));
    nl::json pub_data;
    pub_data["text/plain"] = String_val(result);
    publish_execution_result(execution_counter, std::move(pub_data), nl::json::object());
    return xeus::create_successful_reply();
  }

  nl::json complete_request_impl(
    const std::string& code,
    int cursor_pos
  ) {
    return xeus::create_successful_reply();
  }

  nl::json inspect_request_impl(
    const std::string& code,
    int cursor_pos,
    int detail_level
  ) {
    return xeus::create_successful_reply();
  }

  nl::json is_complete_request_impl(
    const std::string& code
  ) {
    return xeus::create_successful_reply();
  }

  nl::json kernel_info_request_impl() {
    return xeus::create_info_reply(
      "",
      "SATySFi Kernel",
      "0.1.0",
      "satysfi",
      "0.1.0",
      "text/x-satysfi",
      ".saty"
    );
  }

  void shutdown_request_impl() {}
};

extern "C" {
  void start_kernel(value file_name) {
    auto config = xeus::load_configuration(String_val(file_name));
    auto context = xeus::make_zmq_context();
    using interpreter_ptr = std::unique_ptr<satysfi_interpreter>;
    interpreter_ptr interpreter = interpreter_ptr(new satysfi_interpreter());
    xeus::xkernel kernel(config, xeus::get_user_name(), std::move(context), std::move(interpreter), xeus::make_xserver_zmq);
    kernel.start();
  }
}
