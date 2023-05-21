#include <RcppArmadillo.h>
#include <cmath>
using namespace Rcpp;

// compute outside indices of a vector as a mirror
IntegerVector locus_neigh(int size, int leg) {
    IntegerVector res(size + 2 * leg);
    for (int i = 0; i < res.length(); ++i) {
        if (i < leg)
            res(i) = leg - i - 1;
        else if (i < size + leg)
            res(i) = i - leg;
        else
            res(i) = 2 * size + leg - i - 1;
    }
    return res;
}

// [[Rcpp::export]]
NumericVector bayes_smoother_fraction(const NumericMatrix& logits,
                                      const int& nrows,
                                      const int& ncols,
                                      const int& window_size,
                                      const NumericVector& smoothness,
                                      const double& neigh_fraction
) {
    // initialize result vectors
    NumericMatrix res(logits.nrow(), logits.ncol());
    NumericVector neigh(window_size * window_size);
    // compute window leg
    int leg = window_size / 2;
    // compute locus mirror
    IntegerVector loci = locus_neigh(nrows, leg);
    IntegerVector locj = locus_neigh(ncols, leg);
    // compute number of neighbors to be used
    int neigh_high = std::ceil(neigh_fraction * window_size * window_size);
    // compute values for each pixel
    for (int i = 0; i < nrows; ++i) {
        for (int j = 0; j < ncols; ++j) {
            // for all bands
            for (int band = 0; band < logits.ncol(); ++band) {
                // compute the neighborhood
                for (int wi = 0; wi < window_size; ++wi)
                    for (int wj = 0; wj < window_size; ++wj)
                        neigh(wi * window_size + wj) =
                            logits(loci(wi + i) * ncols + locj(wj + j), band);
                if (neigh_fraction < 1.0)
                    // Sort the neighbor logit values
                    neigh.sort(true);
                // Create a vector to store the highest values
                NumericVector high_values(neigh_high);
                // copy the highest values to the new vector
                int nh = 0;
                for(NumericVector::iterator it = neigh.begin();
                    it != neigh.begin() + neigh_high; ++it) {
                    high_values(nh++) = (*it);
                }
                // get the estimates for prior
                // normal with mean m0 and variance s0
                double s0 = var(high_values);
                double m0 = mean(high_values);
                // get the current value
                double x0 = logits(i * ncols + j, band);
                // weight for Bayesian estimator
                double w = s0/(s0 + smoothness(band));
                // apply Bayesian smoother
                res(i * ncols + j, band) = w * x0 + (1 - w) * m0;
            }
        }
    }
    return res;
}
//
// Data structures to compute variance
struct _neigh {
    arma::mat data;
    arma::colvec weights;
    arma::uword n_rows;
    _neigh(const arma::mat& m, const arma::mat& w):
        data(w.n_elem, m.n_cols, arma::fill::zeros),
        weights(w.n_elem, arma::fill::zeros),
        n_rows(0) {}
};

typedef _neigh neigh_t;
//
void neigh_vec(neigh_t& n,
               const arma::mat& m,
               const arma::uword m_nrow,
               const arma::uword m_ncol,
               const arma::mat& w,
               const arma::uword m_b,
               const arma::uword m_i,
               const arma::uword m_j) {

    arma::uword w_leg_i = w.n_rows / 2, w_leg_j = w.n_cols / 2;

    // copy values
    arma::uword k = 0;
    for (arma::uword i = 0; i < w.n_rows; ++i)
        for (arma::uword j = 0; j < w.n_cols; ++j)
            if (m_i + i >= w_leg_i && m_j + j >= w_leg_j &&
                m_i + i < w_leg_i + m_nrow &&
                m_j + j < w_leg_j + m_ncol &&
                arma::is_finite(m(m_j + m_i * m_ncol, 0)) &&
                arma::is_finite(m((m_j + j - w_leg_j) + (m_i + i - w_leg_i) * m_ncol, m_b))) {

                n.data(k, m_b) = m((m_j + j - w_leg_j) +
                    (m_i + i - w_leg_i) * m_ncol, m_b);
                n.weights(k++) = w(i, j);
            }
            n.n_rows = k;
}
// [[Rcpp::export]]
arma::mat bayes_var(const arma::mat& m,
                    const arma::uword m_nrow,
                    const arma::uword m_ncol,
                    const arma::mat& w,
                    const double neigh_fraction){

    // initialize result matrix
    arma::mat res(arma::size(m), arma::fill::none);
    res.fill(arma::datum::nan);

    // variance
    arma::rowvec var0(m.n_cols, arma::fill::zeros);

    // neighbourhood
    neigh_t neigh(m, w);

    // compute values for each pixel
    for (arma::uword i = 0; i < m_nrow; ++i) {
        for (arma::uword j = 0; j < m_ncol; ++j) {

            // fill neighbor values
            for (arma::uword b = 0; b < m.n_cols; ++b)
                neigh_vec(neigh, m, m_nrow, m_ncol, w, b, i, j);

            if (neigh.n_rows * neigh_fraction < 1) continue;

            if (neigh_fraction < 1.0 ) {
                // sort the data
                neigh.data.rows(0, neigh.n_rows - 1) =
                    arma::sort(neigh.data.rows(0, neigh.n_rows - 1), "descend");

                // number of sorted values
                arma::uword n_sort = neigh.n_rows * neigh_fraction;

                // compute variance
                var0 = arma::var(neigh.data.rows(0, n_sort - 1), 0, 0).as_row();
            }
            else {
                // compute variance
                var0 = arma::var(neigh.data.rows(0, neigh.n_rows - 1), 0, 0).as_row();
            }

            // return values
            res.row(j + i * m_ncol) = var0;
        }
    }
    return res;
}
