#include <stdio.h>
#include <stdbool.h>

typedef struct Coc Coc;
typedef void (*CocFunc)(Coc);

struct Coc {
    CocFunc _[8];
};

#define COCABI \
    __attribute__((always_inline)) \
    static inline void

enum {
    Integer,
    String,
    Boolean,
};

COCABI initial(void) {
    return;
}

COCABI print(Coc coc) {
    uintptr_t type_tag = (uintptr_t)coc._[0];
    CocFunc value = coc._[1];
    // proof is coc._[2];
    CocFunc world = coc._[3];

    world(coc);

    switch (type_tag) {
    case Integer:
        printf("Integer: %ld\n", (long)value);
        break;

    case String:
        printf("String: %s\n", (char *)value);
        break;

    case Boolean:
        printf("Boolean: %s\n", value ? "true" : "false");
        break;

    default:
        __builtin_unreachable();
    }
}
