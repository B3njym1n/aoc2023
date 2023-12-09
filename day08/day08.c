/* Advent of Code 2023 - Day 08: Haunted Wasteland
#include <stdio.h>
#include <stdlib.h>
#include "../../aoc.h" /* file, time, math */
struct node {
int id, 1, r;
short z1, z2;
};
char *dir;
struct node *nodes;
int entries;
int start1 = -1; int instances;
int *start2;
int cmpfunc(const void *a, const void *b) {
}
return ((const struct node *)a)->id - ((const struct node *)b)->id;
void load (const char *fn) {
size_t size;
void *data = loadfile(fn, &size);
if (!(dir = malloc(2000000u)))
fatal("can't alloc %u bytes\n", 2000000u);
size_t offset = Ou;
getline(data, size, &offset, dir, 2000000u);
offset++; /* In */
entries = (int)numlines(data, size, offset);
if (!(nodes = malloc(sizeof(struct node) * entries)))
fatal("can't alloc %d bytes\n", sizeof(struct node) * entries);
instances = 0;
int validates = 0;
for (int i = 0; i < entries; i++) {
char buffer[32];
if (getline(data, size, &offset, buffer, sizeof(buffer)) != 16) fatal("unknown data error\n");
struct node *n = &nodes[i];
char *p = buffer;
n->id = 676 * (*p - 'A') + 26 * (*(p+1) - 'A') + (*(p+2) - 'A'); p += 7; n->1 = 676 * (*p - 'A') + 26 * (*(p+1) - 'A') + (*(p+2) - 'A'); p += 5; n->r = 676 * (*p - 'A') + 26 * (*(p+1) - 'A') + (*(p+2) - 'A');
n->z1 = n->id == 17575;
n->z2 = n->id % 26 == 25;
if (n->id % 26 == 0) instances++;
else if (n->id % 26 == 25) validates++;
}
free(data);
if (!instances || instances != validates)
fatal("data error or dismatch (%d != %d)\n", instances, validates);
if (!(start2 = malloc(sizeof(int) * instances)))
fatal("can't alloc %d bytes\n", sizeof(int) * instances);
qsort (nodes, entries, sizeof(struct node), cmpfunc);
validates = 0;
for (int i = 0; i < entries; i++) {
}
struct node *n = &nodes[i];
if (n->id == 0)
start1 = i;
if (n->id % 26 == 0)
start2[validates++] = i;
if (start1 == -1 || instances != validates) fatal("start offset no found\n");
struct node *find(int id) {
int 1 = 0, r = entries-1;
while (1 <= r) {
}
int m = 1 + ((r-1) >> 1);
if (nodes[m].id == id) return &nodes[m];
if (nodes [m].id < id)
else
1 = m + 1;
r = m - 1;
fatal("id %d not found\n", id);
int scan(struct node *n, short (*cmp) (struct node *)) {
const char *p = dir;
int steps = 1;
while (1) {
n = find(*p == 'L' ? n->1 : n->r);
if (cmp(n))
return steps;
if (*++p == '\0')
p = dir;
steps++;
short fini1(struct node *n) {
}
return n->z1;
void part1() {
int steps = scan(&nodes [start1], fini1);
fprintf(stdout, "PART 1: %d\n", steps);
short fini2(struct node *n) {
return n->z2;
void part2() {
long long total = 1;
for (int i = 0; i < instances; i++) {
total = lcm(total, steps);
int steps = scan (&nodes[start2[i]], fini2);
}
fprintf(stdout, "PART 2: %11d\n", total);
int main(int argc, char *argv[]) {
if (argc != 2) {
}
fprintf(stderr, "USAGE: %s <input>\n", argv[0]);
return EXIT_FAILURE;
unsigned long long t1 = getime();
load (argv[1]);
unsigned long long t2 = getime();
part1();
part2();
unsigned long long t3 = getime();
free(start2);
free(nodes);
free(dir);
fprintf(stdout, "TIME load: %.2f ms, exec: %.2f ms, total: %.2f ms\n",
);
timediff_ms(t1, t2), timediff_ms(t2, t3), timediff_ms(t1, t3)
return EXIT_SUCCESS;