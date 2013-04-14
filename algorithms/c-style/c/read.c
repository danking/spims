#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>

int load_file(char* fn, char ** out) {

  /* get file size */
  struct stat st;
  if(stat(fn, &st) != 0) {
    fprintf(stderr, "File not found: %s\n", fn);
    return 0;
  }

  /* open file */
  FILE * file = fopen(fn, "rb");
  if (!file) {
    fprintf(stderr, "Unable to open file for reading %s\n", fn);
    return 0;
  }

  /* try to allocate memory */
  char * buffer = (char*) malloc(sizeof(char) * (st.st_size + 1));
  if (!buffer) {
    fprintf(stderr, "Memory error!\n");
    fclose(file);
    return 0;
  }

  //Read file contents into buffer
  int itemsread = fread(buffer, st.st_size, 1, file);
  fclose(file);
  if (itemsread != 1) {
    fprintf(stderr, "Could not read from file!\n");
    return 0;
  }

  *out = buffer;

  return 1;
}

int write_file(char* fn, int * buffer, int size) {

  /* open file */
  FILE * file = fopen(fn, "wb");
  if (!file) {
    fprintf(stderr, "Unable to open file for writing %s\n", fn);
    return 0;
  }

  // write file contents into buffer
  int itemswritten = fwrite(buffer, sizeof(int), size, file);
  fclose(file);
  if (itemswritten != size) {
    fprintf(stderr, "Could not write (or not completely) to file!\n");
    return 0;
  }

  return 1;
}
