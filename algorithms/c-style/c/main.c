#include<xmmintrin.h>
#include<stdio.h>
#include<read.h>

#define WIDTH1 (35 * 4)
#define HEIGHT1 (44)

#define WIDTH2 (1280 * 4)
#define HEIGHT2 (960)

struct prematch {
  int val;
  int x;
  int y;
};

struct prematch max_pm;
struct prematch min_pm;

struct prematch get_max() {
  return max_pm;
}

struct prematch get_min() {
  return min_pm;
}

int * runsad(char* img1, int width1, int height1,
             char* img2, int width2, int height2);
int inner_sad(char* pattern, char* source, int w, int h, int source_w);

int * from_racket(char* pattern, char* source,
                  int pattern_w, int pattern_h,
                  int source_w, int source_h) {

  int * res = runsad(pattern, pattern_w, pattern_h,
                     source, source_w, source_h);

  /* int success; */
  /* success = write_file("sad.raw", */
  /*                      res, */
  /*                      (source_w/4 - pattern_w/4 + 1)*(source_h - pattern_h + 1)); */
  /* if (!success) { */
  /*   return (int*)0; */
  /* } */

  int sad_w = (source_w - pattern_w + 1);
  int sad_h = (source_h - pattern_h + 1);

  int max = res[0];
  int max_x = 0;
  int max_y = 0;

  int min = res[0];
  int min_x = 0;
  int min_y = 0;


  int y;
  for(y = 0; y < sad_h; ++y) {
    int x;
    for(x = 0; x < sad_w; ++x) {
      int cur = res[x + y * sad_w];
      if (cur > max) {
        max = cur;
        max_x = x;
        max_y = y;
      } else if (cur < min) {
        min = cur;
        min_x = x;
        min_y = y;
      }
    }
  }

  max_pm.val = max;
  max_pm.x = max_x;
  max_pm.y = max_y;

  min_pm.val = min;
  min_pm.x = min_x;
  min_pm.y = min_y;

  fprintf(stderr, "max is at (%d, %d): %d\n", max_x, max_y, max);
  fprintf(stderr, "min is at (%d, %d): %d\n", min_x, min_y, min);

  return res;

}

int sad_at(char* pattern, char * source,
           int tlx, int tly,
           int w, int h, int source_w) {
  int tl_index = (tly * source_w * 4) + tlx * 4;
  return inner_sad(  pattern
                   , (source + tl_index)
                   , w * 4
                   , h
                   , source_w * 4);

}

int inner_sad(char* pattern, char* source, int w, int h, int source_w) {
  int words = w/8;

  int sad = 0;

  int y;
  for (y = 0; y < h; ++y) {

    __m64* wpattern = (__m64*) (pattern + w*y);
    __m64* wsource = (__m64*) (source + source_w*y);

    int word;
    for(word = 0; word < words; ++word) {

      __m64 res = _mm_sad_pu8(wpattern[word], wsource[word]);

      /* the max diff of a byte is 255 (2^8 - 1),
       * there are 8 (2^3) bytes, so the total max diff
       * is 8 * 255 or < 2^11, which fits in a 32 bit int */

      sad += ((long long int) res) & 0xffff;
    }

    /* if we have 20 pixels, we'll do 16 the quick way and four this way */
    int extra;
    for(extra = words * 8; extra < w; ++extra) {
      char temp = pattern[w*y + extra] - source[source_w*y + extra];
      if(temp < 0)
        temp = -temp;
      sad += temp;
    }
  }

  return sad;
}

int * runsad(char* img1, int width1, int height1,
             char* img2, int width2, int height2) {

  int * sad
    = malloc(sizeof(int) *
             ((width2 - width1 + 1)*(height2 - height1 + 1)));


  /* the top left y-coord of the pattern within the source */
  int tly;
  for(tly = 0; tly < (height2 - height1 + 1); ++tly) {
    /* the top left x-coord of the pattern within the source */
    int tlx;
    for (tlx = 0; tlx < (width2 - width1 + 1); ++tlx) {
      /* a byte index */
      int tl_index = (tly * width2 * 4) + tlx * 4;

      sad[tly * (width2 - width1 + 1) + tlx]
        = inner_sad(  img1
                    , (img2 + tl_index)
                    , width1 * 4
                    , height1
                    , width2 * 4);
    }
  }

  return sad;
}
