#ifndef DATA_ANALYSIS_H_
#define DATA_ANALYSIS_H_

typedef struct {
    double min_x;
    double max_x;
    double min_y;
} search_range;

#ifdef __cplusplus
extern "C" {
#endif

int c_smooth_data(int n, const double *x, const double *y, double factor, double *ys);
int c_find_peaks(int n, const double *x, const double *y, int nranges, 
    const search_range *ranges, double *pks);

#ifdef __cplusplus
}
#endif
#endif
