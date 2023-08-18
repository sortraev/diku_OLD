#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <time.h>

#define KEY_MAX 65536
#define TREE_MAX_DEPTH 16
#define MAX_STRING_LENGTH 97
typedef struct _tree_t {
    struct _tree_t *left;
    int64_t key;
    struct _tree_t *right;
    char pw[MAX_STRING_LENGTH];
} tree_t;

float rand_float();
char *rand_pw();
int32_t rand_key();

extern char *hugtree(tree_t*);

tree_t *tree_create(int32_t key, char *pw) {
  tree_t *t = malloc(sizeof(tree_t));

  if (!t)
    return NULL;

  t->left = NULL;
  t->right = NULL;
  t->key = key;

  memset(t->pw,   0, MAX_STRING_LENGTH);
  strncpy(t->pw, pw, strlen(pw));

  return t;
}

void tree_destroy(tree_t *t) {
  if (!t)
    return;
  tree_destroy(t->left);
  tree_destroy(t->right);
  free(t);
}

size_t tree_size(tree_t *t) {
  return t == NULL ? 0 : 1 + tree_size(t->left) + tree_size(t->right);
}


tree_t *__tree_gen_random(size_t current_depth, size_t max_depth) {


  if (current_depth >= max_depth || rand_float() < 0.05)
    return NULL;

  int32_t key  = rand_key();
  char    *pw  = rand_pw();
  tree_t  *new = tree_create(key, pw);


  new->left  = __tree_gen_random(current_depth + 1, max_depth);
  new->right = __tree_gen_random(current_depth + 1, max_depth);
  return new;
}

void insert_tag_and_pw(tree_t *t, int size, char *pw) {

  int n = rand() % size;

  int i = 0;
  int k = 0;
  tree_t **nodes = calloc(size, sizeof(tree_t*));
  nodes[k++] = t;

  tree_t *tmp;

  while (i < n) {
    tmp = nodes[--k];

    if (tmp == NULL)
     continue;

    nodes[k++] = tmp->right;
    nodes[k++] = tmp->left;

    i++;
  }
  free(nodes);

  tmp->key = 0x1337cafe;
  memset(tmp->pw, 0, MAX_STRING_LENGTH);
  strncpy(tmp->pw, pw, strlen(pw));

  return;
}


tree_t *tree_gen_random(char *secret_pw) {
  tree_t *res = __tree_gen_random(0, TREE_MAX_DEPTH);
  size_t size = tree_size(res);
  printf("size of randomly generated tree = %ld\n", size);
  insert_tag_and_pw(res, size, secret_pw);
  return res;
}



void tree_show(tree_t *t) {
  if (!t)
    return;

  printf("<tree_t>\n  me    = %x\n", t->key);
  if (t->left)
    printf("  left  = %x\n", t->left->key);
  else
    printf("  left  = ---\n");
  if (t->right)
    printf("  right = %x\n", t->right->key);
  else
    printf("  right = ---\n");
  printf("  text  = %s\n", t->pw);

  tree_show(t->left);
  tree_show(t->right);
}

int main(int argc, char **argv) {

  char *pw = argc == 2 ? argv[1] : "<foo>";

  srand(time(NULL));

  tree_t *t = tree_gen_random(pw);

  char *res = hugtree(t);
  printf("res == '%s'\n", res);
  tree_destroy(t);
}




float rand_float() {
  return (float) rand() / (float) RAND_MAX;
}

char *rand_pw() {
  size_t len = (rand() % 20) + 10; // rand lengths between 10..30

  char *pw_out = malloc(len * sizeof(char));
  if (!pw_out)
    return NULL;

  for (int i = 0; i < len; i++)
    pw_out[i] = (rand() % 92) + 33;
  return pw_out;
}

int32_t rand_key() {
  return rand() % KEY_MAX;
}
