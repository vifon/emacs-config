#include <stdlib.h>
#include <stdio.h>
#include "game.h"

struct game_state {
};

static struct game_state* game_init()
{
    struct game_state* state = malloc(sizeof(*state));
    return state;
}

static void game_reload(struct game_state* state)
{
}

static void game_unload(struct game_state* state)
{
}

static void game_finalize(struct game_state* state)
{
    free(state);
}

static bool game_step(struct game_state* state)
{
    return true;
}

const struct game_api GAME_API = {
    .init = game_init,
    .reload = game_reload,
    .step = game_step,
    .unload = game_unload,
    .finalize = game_finalize
};
