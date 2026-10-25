// Dilsad Erdem 251ADB186
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resource used: ChatGPT for explanation, design guidance, debugging and code review.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>
typedef struct {
    int is_float;
    long long i;
    double d;
} Value;

typedef struct {
    const char *text;
    size_t length;
    size_t pos;
    size_t base_pos;
    size_t error_pos;
    int has_error;
} Parser;
static Value parse_expr(Parser *p);
static Value parse_term(Parser *p);
static Value parse_power(Parser *p);
static Value parse_primary(Parser *p);

static void skip_spaces(Parser *p);
static void set_error(Parser *p, size_t pos);
static Value make_int(long long x);
static Value make_float(double x);
static Value make_int(long long x) {
    Value v;
    v.is_float = 0;
    v.i = x;
    v.d = (double)x;
    return v;
}

static Value make_float(double x) {
    Value v;
    v.is_float = 1;
    v.i = (long long)x;
    v.d = x;
    return v;
}

static void skip_spaces(Parser *p) {
    while (p->pos < p->length &&
           (p->text[p->pos] == ' ' || p->text[p->pos] == '\t')) {
        p->pos++;
    }
}

static void set_error(Parser *p, size_t pos) {
    if (!p->has_error) {
        p->has_error = 1;
        p->error_pos = p->base_pos + pos + 1;
    }
}
static Value parse_primary(Parser *p) {
    skip_spaces(p);

    if (p->pos >= p->length) {
        set_error(p, p->pos);
        return make_int(0);
    }

    if (p->text[p->pos] == '(') {
        p->pos++;

        Value v = parse_expr(p);

        if (p->has_error) {
            return v;
        }

        skip_spaces(p);

        if (p->pos >= p->length || p->text[p->pos] != ')') {
            set_error(p, p->pos);
            return make_int(0);
        }

        p->pos++;
        return v;
    }

    if (!isdigit((unsigned char)p->text[p->pos])) {
        set_error(p, p->pos);
        return make_int(0);
    }

    size_t start = p->pos;

    while (p->pos < p->length &&
           isdigit((unsigned char)p->text[p->pos])) {
        p->pos++;
    }

    if (p->pos < p->length && p->text[p->pos] == '.') {
        p->pos++;

        if (p->pos >= p->length ||
            !isdigit((unsigned char)p->text[p->pos])) {
            set_error(p, p->pos - 1);
            return make_int(0);
        }

        while (p->pos < p->length &&
               isdigit((unsigned char)p->text[p->pos])) {
            p->pos++;
        }

        double x = strtod(p->text + start, NULL);
        return make_float(x);
    }

    long long x = strtoll(p->text + start, NULL, 10);
    return make_int(x);
}
static Value parse_power(Parser *p) {
    Value left = parse_primary(p);

    if (p->has_error) {
        return left;
    }

    skip_spaces(p);

    if (p->pos + 1 < p->length &&
        p->text[p->pos] == '*' &&
        p->text[p->pos + 1] == '*') {

        size_t op_pos = p->pos;
        p->pos += 2;

        skip_spaces(p);
        size_t exponent_pos = p->pos;

        Value right = parse_power(p);

        if (p->has_error) {
            return left;
        }

        if (!right.is_float && right.i < 0) {
            set_error(p, exponent_pos);
            return make_int(0);
        }

        if (!left.is_float && !right.is_float) {
            long long result = 1;

            for (long long i = 0; i < right.i; i++) {
                result *= left.i;
            }

            return make_int(result);
        }

        double base = left.is_float ? left.d : (double)left.i;
        double exponent = right.is_float ? right.d : (double)right.i;
        double result = pow(base, exponent);

        if (!isfinite(result)) {
            set_error(p, op_pos);
            return make_float(0.0);
        }

        return make_float(result);
    }

    return left;
}
static Value parse_term(Parser *p) {
    Value left = parse_power(p);

    while (!p->has_error) {
        skip_spaces(p);

        if (p->pos >= p->length) {
            break;
        }

        char op = p->text[p->pos];

        if (op != '*' && op != '/') {
            break;
        }

        if (op == '*' &&
            p->pos + 1 < p->length &&
            p->text[p->pos + 1] == '*') {
            break;
        }

        p->pos++;
        skip_spaces(p);

        size_t right_pos = p->pos;
        Value right = parse_power(p);

        if (p->has_error) {
            return left;
        }

        if (op == '/' &&
            ((!right.is_float && right.i == 0) ||
             (right.is_float && right.d == 0.0))) {
            set_error(p, right_pos);
            return make_int(0);
        }

        if (!left.is_float && !right.is_float) {
            if (op == '*') {
                left = make_int(left.i * right.i);
            } else {
                left = make_int(left.i / right.i);
            }
        } else {
            double a = left.is_float ? left.d : (double)left.i;
            double b = right.is_float ? right.d : (double)right.i;

            if (op == '*') {
                left = make_float(a * b);
            } else {
                left = make_float(a / b);
            }
        }
    }

    return left;
}
static Value parse_expr(Parser *p) {
    Value left = parse_term(p);

    while (!p->has_error) {
        skip_spaces(p);

        if (p->pos >= p->length) {
            break;
        }

        char op = p->text[p->pos];

        if (op != '+' && op != '-') {
            break;
        }

        p->pos++;
        Value right = parse_term(p);

        if (p->has_error) {
            return left;
        }

        if (!left.is_float && !right.is_float) {
            if (op == '+') {
                left = make_int(left.i + right.i);
            } else {
                left = make_int(left.i - right.i);
            }
        } else {
            double a = left.is_float ? left.d : (double)left.i;
            double b = right.is_float ? right.d : (double)right.i;

            if (op == '+') {
                left = make_float(a + b);
            } else {
                left = make_float(a - b);
            }
        }
    }

    return left;
}
static int evaluate_line(const char *text, size_t length,
                         size_t base_pos, Value *result,
                         size_t *error_pos) {
    Parser p;

    p.text = text;
    p.length = length;
    p.pos = 0;
    p.base_pos = base_pos;
    p.error_pos = 0;
    p.has_error = 0;

    *result = parse_expr(&p);

    if (p.has_error) {
        *error_pos = p.error_pos;
        return 0;
    }

    skip_spaces(&p);

    if (p.pos != p.length) {
        set_error(&p, p.pos);
        *error_pos = p.error_pos;
        return 0;
    }

    return 1;
}
static void print_value(FILE *fp, Value v) {
    if (v.is_float) {
        fprintf(fp, "%.15g\n", v.d);
    } else {
        fprintf(fp, "%lld\n", v.i);
    }
}

static char *read_line(FILE *fp, size_t *len) {
    size_t capacity = 128;
    size_t n = 0;

    char *line = malloc(capacity);

    if (line == NULL) {
        return NULL;
    }

    int c;

    while ((c = fgetc(fp)) != EOF) {
        if (n + 1 >= capacity) {
            capacity *= 2;

            char *temp = realloc(line, capacity);

            if (temp == NULL) {
                free(line);
                return NULL;
            }

            line = temp;
        }

        line[n++] = (char)c;

        if (c == '\n') {
            break;
        }
    }

    if (n == 0 && c == EOF) {
        free(line);
        return NULL;
    }

    line[n] = '\0';
    *len = n;

    return line;
}
static int process_file(const char *input_path, const char *output_path) {
    FILE *in = fopen(input_path, "r");

    if (in == NULL) {
        fprintf(stderr, "Cannot open input file: %s\n", input_path);
        return 1;
    }

    FILE *out = fopen(output_path, "w");

    if (out == NULL) {
        fprintf(stderr, "Cannot open output file: %s\n", output_path);
        fclose(in);
        return 1;
    }

        size_t base_pos = 0;
        size_t len;
        char *line;

        while ((line = read_line(in, &len)) != NULL) {
            size_t expr_len = len;
  
        if (expr_len > 0 && line[expr_len - 1] == '\n') {
            expr_len--;
        }

        if (expr_len == 0) {
        base_pos += len;
        free(line);
        continue;
    }
        size_t first = 0;

while (first < expr_len &&
       (line[first] == ' ' || line[first] == '\t')) {
    first++;
}

if (first == expr_len || line[first] == '#') {
    base_pos += len;
    free(line);
    continue;
}

        Value result;
        size_t error_pos;

        if (!evaluate_line(line, expr_len, base_pos, &result, &error_pos)) {
            free(line);
            fclose(in);

            out = freopen(output_path, "w", out);

            if (out == NULL) {
                return 1;
            }

            fprintf(out, "ERROR:%zu\n", error_pos);

            fclose(out);
            return 0;
        }

        print_value(out, result);

        base_pos += len;
        free(line);
    }

    fclose(in);
    fclose(out);

    return 0;
}
int main(int argc, char *argv[]) {
    const char *input = NULL;
    const char *dir = NULL;
    const char *output_dir = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0 ||
            strcmp(argv[i], "--output-dir") == 0) {

            if (i + 1 >= argc) {
                fprintf(stderr, "Missing output directory.\n");
                return 1;
            }

            output_dir = argv[++i];

        } else if (strcmp(argv[i], "-d") == 0 ||
                   strcmp(argv[i], "--dir") == 0) {

            if (i + 1 >= argc) {
                fprintf(stderr, "Missing input directory.\n");
                return 1;
            }

            dir = argv[++i];

        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            return 1;

        } else {
            if (input != NULL) {
                fprintf(stderr, "Too many input files.\n");
                return 1;
            }

            input = argv[i];
        }
    }

    if ((input == NULL && dir == NULL) ||
        (input != NULL && dir != NULL)) {
        fprintf(stderr, "Specify one input file or one directory.\n");
        return 1;
    }

    char default_dir[512];

    if (output_dir == NULL) {
        const char *source = input != NULL ? input : dir;
        const char *base = strrchr(source, '/');

        base = base ? base + 1 : source;

        char temp[256];
        snprintf(temp, sizeof(temp), "%s", base);

        char *dot = strrchr(temp, '.');

        if (input != NULL && dot != NULL) {
            *dot = '\0';
        }

        snprintf(default_dir, sizeof(default_dir),
                 "%s_dilsad_251ADB186", temp);

        output_dir = default_dir;
    }

    if (mkdir(output_dir, 0777) != 0 && errno != EEXIST) {
        fprintf(stderr, "Cannot create output directory.\n");
        return 1;
    }

    if (input != NULL) {
        const char *base = strrchr(input, '/');
        base = base ? base + 1 : input;

        char name[256];
        snprintf(name, sizeof(name), "%s", base);

        char *dot = strrchr(name, '.');
        if (dot != NULL) {
            *dot = '\0';
        }

        char output_path[1024];

        snprintf(output_path, sizeof(output_path),
                 "%s/%s_dilsad_erdem_251ADB186.txt",
                 output_dir, name);

        return process_file(input, output_path);
    }

    DIR *dp = opendir(dir);

    if (dp == NULL) {
        fprintf(stderr, "Cannot open input directory: %s\n", dir);
        return 1;
    }

    struct dirent *entry;
    int status = 0;

    while ((entry = readdir(dp)) != NULL) {
        const char *filename = entry->d_name;
        size_t len = strlen(filename);

        if (len < 4 ||
            strcmp(filename + len - 4, ".txt") != 0) {
            continue;
        }

        char input_path[1024];
        char output_path[1024];
        char name[256];

        snprintf(input_path, sizeof(input_path),
                 "%s/%s", dir, filename);

        struct stat st;

        if (stat(input_path, &st) != 0 || !S_ISREG(st.st_mode)) {
            continue;
        }

        snprintf(name, sizeof(name), "%s", filename);
        name[strlen(name) - 4] = '\0';

        snprintf(output_path, sizeof(output_path),
                 "%s/%s_dilsad_erdem_251ADB186.txt",
                 output_dir, name);

        if (process_file(input_path, output_path) != 0) {
            status = 1;
        }
    }

    closedir(dp);

    return status;
}