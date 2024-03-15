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
      "0.0.10",
      "SATySFi",
      "0.0.10",
      "text/x-satysfi",
      ".saty"
    );
  }

  void shutdown_request_impl() {}
};

void return_successful_data(const int execution_count, const std::string &mime, const std::string &data) {
  auto& interpreter = xeus::get_interpreter();

  nl::json pub_data;
  pub_data[mime] = data;
  interpreter.publish_execution_result(execution_count, std::move(pub_data), nl::json::object());
}

void return_successful_file(const int execution_count, const std::string &mime, const std::string &path) {
  std::ifstream ifs(path, std::ios::binary);
  std::ostringstream oss;
  base64::encoder enc;
  enc.encode(ifs, oss);
  return_successful_data(execution_count, mime, oss.str());
}

void return_error_string(const std::string &ename, const std::string &evalue) {
  auto& interpreter = xeus::get_interpreter();

  interpreter.publish_execution_error(ename, evalue, std::vector<std::string>());
}

extern "C" {
  void start_kernel(value connection_file) {
    auto config = xeus::load_configuration(String_val(connection_file));
    auto context = xeus::make_zmq_context();
    using interpreter_ptr = std::unique_ptr<satysfi_interpreter>;
    interpreter_ptr interpreter = interpreter_ptr(new satysfi_interpreter());
    xeus::xkernel kernel(
      config,
      xeus::get_user_name(),
      std::move(context),
      std::move(interpreter),
      xeus::make_xserver_zmq
    );
    kernel.start();
  }

  void return_text(value execution_count, value data) {
    return_successful_data(Int_val(execution_count), "text/plain", String_val(data));
  }

  void return_pdf(value execution_count, value doc_path, value rasterize) {
    auto path = String_val(doc_path);

    if (Bool_val(rasterize)) {
      auto doc = poppler::document::load_from_file(path);
      if (doc == nullptr) {
        return_error_string("Kernel", "Failed to load PDF");
        return;
      }

      poppler::page_renderer renderer;
      renderer.set_render_hints(
        poppler::page_renderer::antialiasing |
        poppler::page_renderer::text_antialiasing |
        poppler::page_renderer::text_hinting
      );

      for (int i = 0; i < doc->pages(); i++) {
        auto page = doc->create_page(i);
        auto img = renderer.render_page(page, 198, 198);

        auto img_path = std::filesystem::temp_directory_path() / std::filesystem::path("XXXXXX");
        auto img_name = img_path.string();

        if (mkstemp(img_name.data()) > -1 && img.save(img_name, "png")) {
          return_successful_file(Int_val(execution_count), "image/png", img_name);
        } else {
          return_error_string("Kernel", "Failed to save preview image");
        }
      }
    } else {
      return_successful_file(Int_val(execution_count), "application/pdf", path);
    }
  }

  void return_error(value ename, value evalue) {
    return_error_string(String_val(ename), String_val(evalue));
  }
}
