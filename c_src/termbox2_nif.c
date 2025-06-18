#define TB_IMPL
#include "erl_nif.h"
#include "termbox2/termbox2.h"

// tb_init/0
static ERL_NIF_TERM nif_tb_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc; (void)argv;
    int result = tb_init();
    return enif_make_int(env, result);
}

// tb_shutdown/0
static ERL_NIF_TERM nif_tb_shutdown(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc; (void)argv;
    int result = tb_shutdown();
    return enif_make_int(env, result);
}

// tb_width/0
static ERL_NIF_TERM nif_tb_width(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc; (void)argv;
    int result = tb_width();
    return enif_make_int(env, result);
}

// tb_height/0
static ERL_NIF_TERM nif_tb_height(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc; (void)argv;
    int result = tb_height();
    return enif_make_int(env, result);
}

// tb_clear/0
static ERL_NIF_TERM nif_tb_clear(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc; (void)argv;
    int result = tb_clear();
    return enif_make_int(env, result);
}

// tb_present/0
static ERL_NIF_TERM nif_tb_present(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc; (void)argv;
    int result = tb_present();
    return enif_make_int(env, result);
}

// tb_set_cursor/2
static ERL_NIF_TERM nif_tb_set_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc; (void)argv;
    int cx, cy;
    if (!enif_get_int(env, argv[0], &cx) || !enif_get_int(env, argv[1], &cy)) {
        return enif_make_badarg(env);
    }
    int result = tb_set_cursor(cx, cy);
    return enif_make_int(env, result);
}

// tb_hide_cursor/0
static ERL_NIF_TERM nif_tb_hide_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc; (void)argv;
    int result = tb_hide_cursor();
    return enif_make_int(env, result);
}

// tb_set_cell/5
static ERL_NIF_TERM nif_tb_set_cell(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc; (void)argv;
    int x, y;
    unsigned int ch;
    unsigned int fg, bg;
    if (!enif_get_int(env, argv[0], &x) ||
        !enif_get_int(env, argv[1], &y) ||
        !enif_get_uint(env, argv[2], &ch) ||
        !enif_get_uint(env, argv[3], &fg) ||
        !enif_get_uint(env, argv[4], &bg)) {
        return enif_make_badarg(env);
    }
    int result = tb_set_cell(x, y, ch, fg, bg);
    return enif_make_int(env, result);
}

// tb_set_input_mode/1
static ERL_NIF_TERM nif_tb_set_input_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc; (void)argv;
    int mode;
    if (!enif_get_int(env, argv[0], &mode)) {
        return enif_make_badarg(env);
    }
    int result = tb_set_input_mode(mode);
    return enif_make_int(env, result);
}

// tb_set_output_mode/1
static ERL_NIF_TERM nif_tb_set_output_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc; (void)argv;
    int mode;
    if (!enif_get_int(env, argv[0], &mode)) {
        return enif_make_badarg(env);
    }
    int result = tb_set_output_mode(mode);
    return enif_make_int(env, result);
}

// tb_print/5 (x, y, fg, bg, string)
static ERL_NIF_TERM nif_tb_print(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc; (void)argv;
    int x, y;
    unsigned int fg, bg;
    ErlNifBinary bin;
    if (!enif_get_int(env, argv[0], &x) ||
        !enif_get_int(env, argv[1], &y) ||
        !enif_get_uint(env, argv[2], &fg) ||
        !enif_get_uint(env, argv[3], &bg) ||
        !enif_inspect_binary(env, argv[4], &bin)) {
        return enif_make_badarg(env);
    }
    // Ensure null-terminated string
    char* str = (char*)malloc(bin.size + 1);
    memcpy(str, bin.data, bin.size);
    str[bin.size] = '\0';
    int result = tb_print(x, y, fg, bg, str);
    free(str);
    return enif_make_int(env, result);
}

// Platform-specific implementation for setting terminal title
static ERL_NIF_TERM tb_set_title(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc != 1)
  {
    return enif_make_badarg(env);
  }
  char title[256];
  if (!enif_get_string(env, argv[0], title, sizeof(title), ERL_NIF_LATIN1))
  {
    return enif_make_badarg(env);
  }
#ifdef _WIN32
  if (SetConsoleTitle(title))
  {
    return enif_make_atom(env, "ok");
  }
  else
  {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), 
        enif_make_string(env, "SetConsoleTitle failed", ERL_NIF_LATIN1));
  }
#else
  // Use escape sequence for Unix-like systems
  // \033]0;TITLE\007 is the standard escape sequence for setting terminal title
  if (printf("\033]0;%s\007", title) < 0)
  {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), 
        enif_make_string(env, "Failed to set title", ERL_NIF_LATIN1));
  }
  fflush(stdout);
  return enif_make_atom(env, "ok");
#endif
}

// Platform-specific implementation for setting terminal window position
static ERL_NIF_TERM tb_set_position(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc != 2)
  {
    return enif_make_badarg(env);
  }
  int x, y;
  if (!enif_get_int(env, argv[0], &x) || !enif_get_int(env, argv[1], &y))
  {
    return enif_make_badarg(env);
  }
#ifdef _WIN32
  HWND hwnd = GetConsoleWindow();
  if (hwnd == NULL)
  {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), 
        enif_make_string(env, "GetConsoleWindow failed", ERL_NIF_LATIN1));
  }
  RECT rect;
  if (!GetWindowRect(hwnd, &rect))
  {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), 
        enif_make_string(env, "GetWindowRect failed", ERL_NIF_LATIN1));
  }
  int width = rect.right - rect.left;
  int height = rect.bottom - rect.top;
  if (MoveWindow(hwnd, x, y, width, height, TRUE))
  {
    return enif_make_atom(env, "ok");
  }
  else
  {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), 
        enif_make_string(env, "MoveWindow failed", ERL_NIF_LATIN1));
  }
#else
  // Note: Most terminals ignore this escape sequence, but we provide it
  // as a best-effort attempt to set window position
  // \033[3;Y;Xt is a non-standard escape sequence that some terminals support
  if (printf("\033[3;%d;%dt", y, x) < 0)
  {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), 
        enif_make_string(env, "Failed to set position", ERL_NIF_LATIN1));
  }
  fflush(stdout);
  // Return a tuple indicating that position setting may not be supported
  return enif_make_tuple2(env, enif_make_atom(env, "ok"), 
      enif_make_atom(env, "unsupported"));
#endif
}

// Add more wrappers as needed...

static ErlNifFunc nif_funcs[] = {
    {"tb_init", 0, nif_tb_init, 0},
    {"tb_shutdown", 0, nif_tb_shutdown, 0},
    {"tb_width", 0, nif_tb_width, 0},
    {"tb_height", 0, nif_tb_height, 0},
    {"tb_clear", 0, nif_tb_clear, 0},
    {"tb_present", 0, nif_tb_present, 0},
    {"tb_set_cursor", 2, nif_tb_set_cursor, 0},
    {"tb_hide_cursor", 0, nif_tb_hide_cursor, 0},
    {"tb_set_cell", 5, nif_tb_set_cell, 0},
    {"tb_set_input_mode", 1, nif_tb_set_input_mode, 0},
    {"tb_set_output_mode", 1, nif_tb_set_output_mode, 0},
    {"tb_print", 5, nif_tb_print, 0},
    {"tb_set_title", 1, tb_set_title, 0},
    {"tb_set_position", 2, tb_set_position, 0}
    // Add more as needed
};

ERL_NIF_INIT(termbox2_nif, nif_funcs, NULL, NULL, NULL, NULL)