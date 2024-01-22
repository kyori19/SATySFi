#include <fstream>
#include <spanstream>
#include <sstream>
#include <stdlib.h>

#include <b64/encode.h>
#include <poppler/cpp/poppler-document.h>
#include <poppler/cpp/poppler-image.h>
#include <poppler/cpp/poppler-page-renderer.h>
#include <nlohmann/json.hpp>
#include <xeus/xhelper.hpp>
#include <xeus/xinterpreter.hpp>
#include <xeus/xkernel.hpp>
#include <xeus/xkernel_configuration.hpp>
#include <xeus-zmq/xserver_zmq.hpp>
#include <xeus-zmq/xzmq_context.hpp>

// caml/compatibility.h conflicts with zmq_addon.hpp
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/mlvalues.h>

namespace nl = nlohmann;
using xeus::xinterpreter;

class satysfi_interpreter : public xinterpreter {
public:

  satysfi_interpreter() {
    xeus::register_interpreter(this);
  }

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
    (void) silent;
    (void) store_history;
    (void) user_expressions;
    (void) allow_stdin;
    const static value *execute = nullptr;
    if (execute == nullptr) {
      execute = caml_named_value("satysfi_jupyter_kernel_execute");
    }
    caml_callback2(*execute, Val_int(execution_counter), caml_copy_string(code.c_str()));
    return xeus::create_successful_reply();
  }

  nl::json complete_request_impl(
    const std::string& code,
    int cursor_pos
  ) {
    (void) code;
    (void) cursor_pos;
    return xeus::create_successful_reply();
  }

  nl::json inspect_request_impl(
    const std::string& code,
    int cursor_pos,
    int detail_level
  ) {
    (void) code;
    (void) cursor_pos;
    (void) detail_level;
    return xeus::create_successful_reply();
  }

  nl::json is_complete_request_impl(
    const std::string& code
  ) {
    (void) code;
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

void return_successful_data(int execution_count, std::string mime, std::string data) {
  auto& interpreter = xeus::get_interpreter();
  nl::json pub_data;
  pub_data[mime] = data;
  interpreter.publish_execution_result(execution_count, std::move(pub_data), nl::json::object());
}

void return_successful_bytes(int execution_count, std::string mime, std::istream &data) {
  std::ostringstream out;
  base64::encoder enc;
  enc.encode(data, out);
  return_successful_data(execution_count, mime, out.str());
}

extern "C" {
  void start_kernel(value file_name) {
    auto config = xeus::load_configuration(String_val(file_name));
    auto context = xeus::make_zmq_context();
    using interpreter_ptr = std::unique_ptr<satysfi_interpreter>;
    interpreter_ptr interpreter = interpreter_ptr(new satysfi_interpreter());
    xeus::xkernel kernel(config, xeus::get_user_name(), std::move(context), std::move(interpreter), xeus::make_xserver_zmq);
    kernel.start();
  }

  void return_text(value execution_count, value data) {
    return_successful_data(Int_val(execution_count), "text/plain", String_val(data));
  }

  void return_pdf(value execution_count, value data, value rasterize) {
    auto in_bytes = (char *) Bytes_val(data);
    auto in_len = caml_string_length(data);
    if (Bool_val(rasterize)) {
      auto doc = poppler::document::load_from_raw_data(in_bytes, in_len);
      poppler::page_renderer renderer;
      renderer.set_render_hints(
        poppler::page_renderer::antialiasing |
        poppler::page_renderer::text_antialiasing |
        poppler::page_renderer::text_hinting);
      for (int i = 0; i < doc->pages(); i++) {
        auto page = doc->create_page(i);
        auto img = renderer.render_page(page, 198, 198);
        auto path = std::filesystem::temp_directory_path() / std::filesystem::path("XXXXXX");
        auto name = path.string();
        if (mkstemp(name.data()) > -1 && img.save(name, "png")) {
          std::ifstream ifs(name);
          return_successful_bytes(Int_val(execution_count), "image/png", ifs);
        }
      }
    } else {
      std::span data_span(in_bytes, in_len);
      std::ispanstream ss(data_span);
      return_successful_bytes(Int_val(execution_count), "application/pdf", ss);
    }
  }

  void return_error_message(value ename, value evalue, value trace) {
    auto& interpreter = xeus::get_interpreter();
    std::vector<std::string> trace_vector;
    while (Is_block(trace)) {
      trace_vector.push_back(String_val(Field(trace, 0)));
      trace = Field(trace, 1);
    }
    interpreter.publish_execution_error(String_val(ename), String_val(evalue), trace_vector);
  }
}
