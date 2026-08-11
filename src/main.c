#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>

typedef union {
    const char *var;
    struct {
        const char *param;
        int type, body, special;
    };
    struct { int func, arg; };
    struct { const char *lname; int ltype, lvalue, lbody; };
} term;

#define UNIVERSE 1
#define VAR 2
#define LAM 4
#define PI 8
#define APP 16
#define LET 32

#define NTERMS 1024*1024
term *ts;
int *tags;
static int *locations;
static const char **syms;
int ts_count = 1;
const char *file_name;

const char *input;
int pos = 0;

int alloc(int id, int tag, term t) {
    int n = ts_count++;
    ts[n] = t;
    tags[n] = tag;
    if (id) {
        syms[n] = syms[id];
        locations[n] = locations[id];
    }
    return n;
}

void skip_ws(void) {
    while (input[pos]) {
        if (input[pos] == '#') {
            while (input[pos] && input[pos] != '\n') ++pos;
        }
        if (!isspace(input[pos])) break;
        ++pos;
    }
}

int parse_str(const char *s) {
    int l = strlen(s);
    skip_ws();
    if (strncmp(input + pos, s, l)) return 0;
    skip_ws();
    pos += l;
    return 1;
}

const char *parse_name(void) {
    skip_ws();
    int b = pos;
    int sq = parse_str("[");
    if (sq) {
        while (input[pos] && input[pos] != ']') {
            ++pos;
        }
        assert(input[pos] == ']');
        ++pos;
    } else {
        while (input[pos] &&
            !isspace(input[pos]) &&
            input[pos] != '(' &&
            input[pos] != ')' &&
            input[pos] != ':' &&
            input[pos] != ';' &&
            input[pos] != ',' &&
            input[pos] != '=')
        {
            ++pos;
        }
    }
    if (b == pos) return 0;
    char *name = strndup(input + b, pos - b);
    if (sq) {
        name[pos - b - 1] = 0;
        name++;
    }
    if (!strcmp(name, "->")) return 0;
    if (!strcmp(name, "=>")) return 0;
    if (!strcmp(name, "&&")) return 0;
    return name;
}

int parse_term(void);
int parse_apps(void);

int parse_var(void) {
    skip_ws();
    int p = pos;
    const char *n = parse_name();
    if (!n) return 0;
    term t;
    if (!strcmp(n, "Type")) {
        return alloc(0, UNIVERSE, t);
    }
    t.var = n;
    int id = alloc(0, VAR, t);
    syms[id] = n;
    locations[id] = p;
    return id;
}

int parse_atom(void) {
    int b = pos;
    if (parse_str(",")) return parse_term();
    int id = parse_var();
    if (id) return id;
    pos = b;
    if (!parse_str("(")) return 0;
    id = parse_term();
    if (!parse_str(")")) return 0;
    return id;
}

int parse_anon(void) {
    term t = {0};
    int p = pos;
    t.type = parse_apps();
    if (!t.type) return 0;
    if (!parse_str("->")) return t.type;
    t.body = parse_term();
    if (!t.body) return 0;
    t.param = "_";
    int id = alloc(0, PI, t);
    syms[id] = "_";
    locations[id] = p;
    return id;
}

int parse_anon_lam(void) {
    int p = pos;
    term t={0};
    const char *params[16];
    int count=0;
    while (1) {
        t.param = parse_name();
        int p2 = pos;
        if (!t.param) {
            pos = p2;
            break;
        }
        params[count++] = t.param;
    }
    if (!parse_str("=>")) return 0;
    t.body = parse_term();
    if (!t.body) return 0;
    for (int i=count-1; i >= 0; --i) {
        t.param = params[i];
        int id = alloc(0, LAM, t);
        syms[id] = t.param;
        locations[id] = p;
        t.body = id;
    }
    return t.body;
}

int parse_ffi(void) {
    int p = pos;
    term t;
    t.param = parse_name();
    if (!t.param) return 0;
    if (!parse_str(":") || parse_str("=")) return 0;
    t.type = parse_term();
    if (!t.type) return 0;
    if (!parse_str(";")) return 0;
    t.body = parse_term();
    if (!t.body) return 0;
    t.special=1;
    int id = alloc(0, LAM, t);
    syms[id] = t.param;
    locations[id] = p;
    return id;
}

int parse_lam(void) {
    int p = pos;
    term t = {0};
    if (!parse_str("(")) return 0;
    t.param = parse_name();
    if (!t.param) return 0;
    if (!parse_str(":") || parse_str("=")) return 0;
    t.type = parse_term();
    if (!t.type) return 0;
    if (!parse_str(")")) return 0;
    if (!parse_str("=>")) return 0;
    t.body = parse_term();
    if (!t.body) return 0;
    int id = alloc(0, LAM, t);
    syms[id] = t.param;
    locations[id] = p;
    return id;
}

int parse_pi(void) {
    term t = {0};
    int p = pos;
    if (!parse_str("(")) return 0;
    t.param = parse_name();
    if (!t.param) return 0;
    if (!parse_str(":") || parse_str("=")) return 0;
    t.type = parse_term();
    if (!t.type) return 0;
    if (!parse_str(")")) return 0;
    if (!parse_str("->")) return 0;
    t.body = parse_term();
    if (!t.body) return 0;
    int id = alloc(0, PI, t);
    syms[id] = t.param;
    locations[id] = p;
    return id;
}

int subst(const char *name, int value, int id);
int parse_binding(void) {
    int p = pos;
    const char *name = parse_name();
    if (!name) return 0;
    if (!parse_str(":=")) return 0;
    int value = parse_term();
    if (!value) return 0;
    if (!parse_str(";")) return 0;
    int body = parse_term();
    if (!body) return 0;
    return subst(name, value, body);
}

int parse_let(void) {
    term t = {0};
    int p = pos;
    t.lname = parse_name();
    if (!t.lname) return 0;
    if (!parse_str(":")) return 0;
    t.ltype = parse_term();
    if (!t.ltype) return 0;
    if (!parse_str("=")) return 0;
    t.lvalue = parse_term();
    if (!t.lvalue) return 0;
    if (!parse_str(";")) return 0;
    t.lbody = parse_term();
    if (!t.lbody) return 0;
    int id = alloc(0, LET, t);
    locations[id] = p;
    return id;
}

int parse_apps(void) {
    term t;
    int p = pos;
    t.func = parse_atom();
    if (!t.func) return 0;

    while (1) {
        t.arg = parse_atom();
        if (!t.arg) break;
        t.func = alloc(0, APP, t);
        locations[t.func] = p;
    }

    return t.func;
}

int parse_term(void) {
    int b = pos;
    int id = parse_let();
    if (id) return id;
    pos = b;
    id = parse_anon_lam();
    if (id) return id;
    pos = b;
    id = parse_ffi();
    if (id) return id;
    pos = b;
    id = parse_lam();
    if (id) return id;
    pos = b;
    id = parse_binding();
    if (id) return id;
    pos = b;
    id = parse_pi();
    if (id) return id;
    pos = b;
    return parse_anon();
}

int isfree(const char *name, int id) {
    term t = ts[id];
    if (tags[id] == UNIVERSE) return 1;
    else if (tags[id] == VAR) return strcmp(name, t.var);
    else if (tags[id] & (LAM | PI)) {
        if (t.type && !isfree(name, t.type)) return 0;
        if (!strcmp(name, t.param)) return 1;
        return isfree(name, t.body);
    } else if (tags[id] == APP) {
        return isfree(name, t.func) && isfree(name, t.arg);
    } else {
        assert(0);
    }
}

int unique = 0;
int subst(const char *name, int value, int id) {
    assert(id);
    term t = ts[id];
    if (tags[id] == UNIVERSE) return 1;
    else if (tags[id] == VAR) {
        if (!strcmp(name, t.var)) {
            int v2 = alloc(value, tags[value], ts[value]);
            locations[v2] = locations[id];
            return v2;
        }
        return id;
    }
    else if (tags[id] & (LAM | PI)) {
        if (t.type) t.type = subst(name, value, t.type);
        if (!strcmp(name, t.param)) return alloc(id, tags[id], t);

        t.body = subst(name, value, t.body);
        return alloc(id, tags[id], t);
    } else if (tags[id] == APP) {
        t.func = subst(name, value, t.func);
        t.arg = subst(name, value, t.arg);
        return alloc(id, APP, t);
    } else {
        t.ltype = subst(name, value, t.ltype);
        t.lvalue = subst(name, value, t.lvalue);
        t.lbody = subst(name, value, t.lbody);
        return alloc(id, LET, t);
    }
}

int eq(int a, int b) {
    term x=ts[a],
    y=ts[b];
    if (tags[a] != tags[b]) return 0;
    if (tags[a] == UNIVERSE) return 1;
    else if (tags[a] == VAR) return !strcmp(x.var, y.var);
    else if (tags[a] & (LAM | PI)) {
        if (x.type && y.type && !eq(x.type, y.type)) return 0;
        term p;
        p.var = x.param;
        y.body = subst(y.param, alloc(a, VAR, p), y.body);
        return eq(x.body, y.body);
    } else if (tags[a] == APP) {
        return eq(x.func, y.func) && eq(x.arg, y.arg);
    } else assert(0);
}

const char *let_names[NTERMS];
int let_values[NTERMS];
int lets_count=0;

const char *globl_let_names[NTERMS];
int globl_let_values[NTERMS];
int globl_lets_count=0;

int skiplines = 0;
void print_location(int id)
{
    int line = 1, column = 1;
    for (int i=0; i < pos && i < locations[id]; ++i) {
        if (input[i] == '\n') {
            line++;
            column=1;
        } else column++;
    }
    printf("%s:%d:%d: ", file_name, line - skiplines, column); 
}

void print_term(int id) {
    term t = ts[id];
    if (tags[id] == UNIVERSE) {
        printf("Type");
    } else if (tags[id] == VAR) {
        printf("%s", syms[id] ?: t.var);
    } else if (tags[id] == PI) {
        if (isfree(t.param, t.body)) {
            int pt = tags[t.type] & (LAM | PI);
            if (pt) printf("(");
            print_term(t.type);
            if (pt) printf(")");
            printf(" -> ");
        } else {
            printf("(%s", syms[id] ?: t.param);
            printf(": ");
            print_term(t.type);
            printf(") -> ");
        }
        print_term(t.body);
    } else if (tags[id] == LAM) {
        if (t.type) {
            printf("(%s", syms[id] ?: t.param);
            printf(": ");
            print_term(t.type);
            printf(") => ");
            print_term(t.body);
        } else {
            printf("%s => ", syms[id] ?: t.param);
            print_term(t.body);
        }
    } else if (tags[id] == APP) {
        int pf = tags[t.func] & (LAM | PI);
        int pa = tags[t.arg] & (LAM | PI | APP);
        if (pf) printf("(");
        print_term(t.func);
        if (pf) printf(")");
        printf(" ");
        if (pa) printf("(");
        print_term(t.arg);
        if (pa) printf(")");
    } else assert(0);
}

int evaluate(int id) {
    term t = ts[id];
    if (tags[id] == UNIVERSE) return id;
    else if (tags[id] == VAR) {
        for (int i=lets_count-1; i >= 0; --i) {
            if (!strcmp(let_names[i], t.var)) {
                int id2 = alloc(let_values[i], tags[let_values[i]], ts[let_values[i]]);
                locations[id2] = id;
                return id2;
            }
        }
        return id;
    } else if (tags[id] & (LAM | PI)) {
        if (t.type) t.type = evaluate(t.type);
        return alloc(id, tags[id], t);
    } else if (tags[id] == APP) {
        int new_func = evaluate(t.func);
        if (tags[new_func] != LAM) {
            t.arg = evaluate(t.arg);
            return alloc(id, APP, t);
        }
        term f = ts[new_func];
        int id2 = evaluate(subst(f.param, t.arg, f.body));
        return id2;
    } else {
        return evaluate(subst(t.lname, t.lvalue, t.lbody));
    }
}

int fully_evaluate(int id);
int eta_expand(int id, int ty) {
  if (tags[ty] == PI) {
    term t = ts[ty];
    term a;
    term p;
    p.var = t.param;
    a.func = id;
    a.arg = alloc(ty, VAR, p);
    t.body = fully_evaluate(alloc(0, APP, a));
    t.body = eta_expand(t.body, ts[ty].body);
    return alloc(ty, LAM, t);
  }
  return id;
}

int fully_evaluate(int id) {
    term t = ts[id];
    if (tags[id] == UNIVERSE) return id;
    else if (tags[id] == VAR) return evaluate(id);
    else if (tags[id] & (LAM | PI)) {
        if (t.type) t.type = fully_evaluate(t.type);
            int u = unique++;
            char s[32];
            sprintf(s, "_%d", u);
            term v;
            v.var = strdup(s);
            int v2 = alloc(id, VAR, v);
            t.body = subst(t.param, v2, t.body);
            t.param = v.var;

        t.body = fully_evaluate(t.body);
        return alloc(id, tags[id], t);
    } else if (tags[id] == APP) {
        int old = t.func;
        t.arg = fully_evaluate(t.arg);
        t.func = fully_evaluate(t.func);
        if (tags[t.func] != LAM) {
            return alloc(id, APP, t);
        }
        term f = ts[t.func];
        int id2 = fully_evaluate(subst(f.param, t.arg, f.body));
        return id2;
    } else {
        t.lvalue = fully_evaluate(t.lvalue);
        let_names[lets_count] = t.lname;
        let_values[lets_count++] = t.lvalue;
        globl_let_names[globl_lets_count] = t.lname;
        globl_let_values[globl_lets_count++] = t.lvalue;

        t.lbody = fully_evaluate(t.lbody);
        --lets_count;
        return t.lbody;
    }
}

const char *ctx_names[NTERMS];
int ctx_types[NTERMS];
int ctx_count=0;

int infer(int id, int check) {
    term t = ts[id];
    if (tags[id] == UNIVERSE) {
        if (check && !eq(id, check)) assert(0);
        return id;
    } else if (tags[id] == VAR) {
        for (int i=ctx_count-1; i >= 0; --i) {
            if (!strcmp(ctx_names[i], t.var)) {
                int ty = evaluate(ctx_types[i]);
                if (check && !eq(ty, check)) {
                    print_location(id);
                    printf("ERROR: variable type mismatch, wanted '");
                    print_term(check);
                    printf("', but found '");
                    print_term(ty);
                    printf("'\n");
                    exit(1);
                }

                return ty;
            }
        }
        print_location(id);
        printf("ERROR: variable '%s' is not declared\n", t.var);
        exit(1);
    } else if (tags[id] & (LAM | PI)) {
        if (tags[id] == PI && check) {
            if (!eq(check, 1)) {
                print_location(id);
                exit(69);
                assert(0);
            }
        }
        if (tags[id] == LAM && check) {
            if (tags[check] != PI) {
            print_location(id);
            exit(1);
            assert(0);
          }
            if (t.type) {
                t.type = evaluate(t.type);
                if (!eq(t.type, ts[check].type)) assert(0);
                infer(t.type, 1);
            } else {
                t.type = ts[check].type;
            }
        } else {
            infer(t.type, 1);
        }

            int u = unique++;
            char s[32];
            sprintf(s, "_%d", u);
            term v;
            v.var = strdup(s);
            int v2 = alloc(id, VAR, v);
            t.body = subst(t.param, v2, t.body);
            t.param = v.var;

        term t2 = ts[check];
        if (tags[id] == LAM && check) {
            term p;
            p.var = t.param;
            t2.body = subst(t2.param, alloc(id, VAR, p), t2.body);
        }

        ctx_names[ctx_count] = t.param;
        ctx_types[ctx_count++] = t.type;

        t.body = fully_evaluate(infer(t.body, check && tags[check] == PI ? t2.body : 0));
        --ctx_count;
        int id2 = alloc(id, tags[id] == LAM ? PI : UNIVERSE, t);

        return id2;
    } else if (tags[id] == APP) {
        int func_ty = fully_evaluate(infer(t.func, 0));
        term f = ts[func_ty];
        if (tags[func_ty] != PI) {
            print_location(t.func);
            printf("ERROR: could not apply argument to value of type '\n");
            print_term(func_ty);
            printf("'\n");
            exit(1);
        }
        int arg_ty = fully_evaluate(infer(t.arg, f.type));

        t.arg = fully_evaluate(t.arg);
        int id2 = fully_evaluate(subst(f.param, t.arg, f.body));
        if (check && !eq(id2, check)) {
            print_term(check);
            print_term(func_ty);
            exit(1);
        }
        return id2;
    } else {
        infer(t.ltype, 1);
        int ty = fully_evaluate(t.ltype);
        int val_ty = fully_evaluate(infer(t.lvalue, ty));

        t.ltype = fully_evaluate(t.ltype);
        t.lvalue = fully_evaluate(t.lvalue);

        ctx_names[ctx_count] = t.lname;
        ctx_types[ctx_count++] = t.ltype;
        let_names[lets_count] = t.lname;
        let_values[lets_count++] = t.lvalue;
        globl_let_names[globl_lets_count] = t.lname;
        globl_let_values[globl_lets_count++] = t.lvalue;
        t.lbody = infer(t.lbody, check);
        --ctx_count;
        --lets_count;
        id = t.lbody;
        if (check && !eq(id, check)) assert(0);
        return id;
    }
}

FILE *outfile;
int codegen_counter=0;

void compile(int id)
{
    if (tags[id] == LAM && ts[id].special) {
        compile(ts[id].body);
        return;
    }
    const char *params[16];
    int pcount = 0;

    int body = id;
    while (tags[body] == LAM) {
        params[pcount++] = syms[body] ?: ts[body].param;
        body = ts[body].body;
    }

    int args[16];
    int count = 0;

    while (tags[body] == APP) {
        if (tags[ts[body].arg] != PI && tags[ts[body].arg] != UNIVERSE) {
            args[count++] = ts[body].arg;
        }
        body = ts[body].func;
    }

    char *buf = malloc(2048);
    buf[0] = 0;
    int len = 0;

    if (codegen_counter==0) {
        len += sprintf(buf + len, "int main(void) {\n");
        len += sprintf(buf + len, "    Coc coc;\n");
        codegen_counter++;
    } else {
        len += sprintf(buf + len, "COCABI _%d(Coc coc) {\n", codegen_counter++);
        for (int i=0; i < pcount; ++i) {
            len += sprintf(buf + len, "    CocFunc %s = coc._[%d];\n", params[i], i);
        }
    }

    const char *counters[8];
    int ccount = 0;

    for (int i=0; i < count; ++i) {
        if (tags[args[i]] == VAR) {
            counters[ccount++] = syms[args[i]] ?: ts[args[i]].var;
        } else {
            char buf[256];
            sprintf(buf, "_%d", codegen_counter);
            counters[ccount++] = strdup(buf);
            compile(args[i]);
        }
    }

    for (int i=ccount-1; i >= 0; --i) {
        len += sprintf(buf + len, "    coc._[%d] = (CocFunc)%s;\n", ccount-1-i, counters[i]);
    }

    len += sprintf(buf + len, "    %s(coc);\n", syms[body] ?: ts[body].var);
    len += sprintf(buf+len, "}\n");
    fprintf(outfile, "%.*s\n", len, buf);
}

int main(int argc, char **argv) {
    assert(argc >= 2);
    file_name = argv[1];
    char *buf = malloc(16*4096);
    memset(buf, 0, 16*4096);
       

    ts = malloc(sizeof(*ts)*NTERMS);
    tags = malloc(sizeof(*tags)*NTERMS);
    locations = malloc(sizeof(*locations)*NTERMS);
    syms = malloc(sizeof(*syms)*NTERMS);
 
    int si = 16*4096;
    FILE*f; int n=0;
    f = fopen(file_name, "rb");
    assert(f);
    n += fread(buf + n, 1, si-n, f);
    buf[n] = 0;
    fclose(f);
    input = buf;

    tags[1] = UNIVERSE; ts_count=2;

    int v = parse_term();
    assert(v);
    skip_ws();
    if (input[pos]) {
        printf("leftover: %s\n", input + pos);
        return 1;
    }

    int ty = infer(v, 0);
    v = fully_evaluate(v);

    outfile = fopen("out.c", "w+");
    fprintf(outfile, "#include <stdlib.h>\n");
    compile(v);
    fclose(outfile);
    char cmd[1024];
    cmd[0]=0;

    n = sprintf(cmd, "clang out.c -o out -O3");
    if (argc == 3) {
        n += sprintf(cmd + n, " -include %s", argv[2]);
    }
    cmd[n] = 0;
    fflush(stdout);
    system(cmd);
}
