#define TB_IMPL
#include "erl_nif.h"
#include "termbox2/termbox2.h"

// tb_init/0
static ERL_NIF_TERM nif_tb_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int result = tb_init();
    return enif_make_int(env, result);
}

// tb_shutdown/0
static ERL_NIF_TERM nif_tb_shutdown(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int result = tb_shutdown();
    return enif_make_int(env, result);
}

// tb_width/0
static ERL_NIF_TERM nif_tb_width(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int result = tb_width();
    return enif_make_int(env, result);
}

// tb_height/0
static ERL_NIF_TERM nif_tb_height(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int result = tb_height();
    return enif_make_int(env, result);
}

// tb_clear/0
static ERL_NIF_TERM nif_tb_clear(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int result = tb_clear();
    return enif_make_int(env, result);
}

// tb_present/0
static ERL_NIF_TERM nif_tb_present(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int result = tb_present();
    return enif_make_int(env, result);
}

// tb_set_cursor/2
static ERL_NIF_TERM nif_tb_set_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int cx, cy;
    if (!enif_get_int(env, argv[0], &cx) || !enif_get_int(env, argv[1], &cy)) {
        return enif_make_badarg(env);
    }
    int result = tb_set_cursor(cx, cy);
    return enif_make_int(env, result);
}

// tb_hide_cursor/0
static ERL_NIF_TERM nif_tb_hide_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int result = tb_hide_cursor();
    return enif_make_int(env, result);
}

// tb_set_cell/5
static ERL_NIF_TERM nif_tb_set_cell(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
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
    int mode;
    if (!enif_get_int(env, argv[0], &mode)) {
        return enif_make_badarg(env);
    }
    int result = tb_set_input_mode(mode);
    return enif_make_int(env, result);
}

// tb_set_output_mode/1
static ERL_NIF_TERM nif_tb_set_output_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int mode;
    if (!enif_get_int(env, argv[0], &mode)) {
        return enif_make_badarg(env);
    }
    int result = tb_set_output_mode(mode);
    return enif_make_int(env, result);
}

// tb_print/5 (x, y, fg, bg, string)
static ERL_NIF_TERM nif_tb_print(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
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

// Add more wrappers as needed...

static ErlNifFunc nif_funcs[] = {
    {"tb_init", 0, nif_tb_init},
    {"tb_shutdown", 0, nif_tb_shutdown},
    {"tb_width", 0, nif_tb_width},
    {"tb_height", 0, nif_tb_height},
    {"tb_clear", 0, nif_tb_clear},
    {"tb_present", 0, nif_tb_present},
    {"tb_set_cursor", 2, nif_tb_set_cursor},
    {"tb_hide_cursor", 0, nif_tb_hide_cursor},
    {"tb_set_cell", 5, nif_tb_set_cell},
    {"tb_set_input_mode", 1, nif_tb_set_input_mode},
    {"tb_set_output_mode", 1, nif_tb_set_output_mode},
    {"tb_print", 5, nif_tb_print}
    // Add more as needed
};

ERL_NIF_INIT(termbox2_nif, nif_funcs, NULL, NULL, NULL, NULL)