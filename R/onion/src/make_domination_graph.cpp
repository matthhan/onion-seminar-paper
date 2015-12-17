#include <Rcpp.h>
using namespace Rcpp;
#include<vector>
#include<iostream>
using namespace std;

bool dominates_by_transitivity(int i, int j, LogicalMatrix graph);
bool dominates_by_pspace(int i, int j,IntegerMatrix pspace,IntegerVector list_oc_ids);
bool k_dominates_by_pspace(int i, int j,IntegerMatrix pspace,IntegerVector list_oc_ids,int k);
// [[Rcpp::export]]
LogicalMatrix make_domination_graph(IntegerMatrix pspace,IntegerVector list_oc_ids) {
    LogicalMatrix output(pspace.ncol(),pspace.ncol());
    for(int i = 0; i < output.nrow();i++) {
        for(int j = 0; j < output.nrow();j++) {
            if(output(i,j)) {
                continue;
            }
            if(dominates_by_transitivity(i,j,output) || dominates_by_pspace(i,j,pspace,list_oc_ids)) {
                output(i,j) = true;
                output(j,i) = true;
            }
        } 
    }
    return output;
}

bool dominates_by_transitivity(int i, int j, LogicalMatrix graph) {
    for(int k = 0; k < graph.nrow();k++) {
        if((graph(i,k) && graph(k,j)) || (graph(j,k) && graph(k,i))) {
            return true;
        }
    }
    return false;
}

bool dominates_by_pspace(int i, int j,IntegerMatrix pspace,IntegerVector list_oc_ids) {
    for(int k = 0; k < pspace.nrow();k++) {
        if(!k_dominates_by_pspace(i,j,pspace,list_oc_ids,k)) return false;
    }
    return true;
}
bool k_dominates_by_pspace(int i, int j,IntegerMatrix pspace,IntegerVector list_oc_ids,int k) {
    int oc_id_i = list_oc_ids[i];
    int oc_id_j = list_oc_ids[j];
    for(int l = 0;l < pspace.ncol();l++) {
        if(pspace(k,l) == oc_id_i) {
            return true;
        }
        if(pspace(k,l) == oc_id_j) {
            return false;
        }
    }
    cout << "error in c++ code: function k_dominates_by_pspace has not found the oc_id" << endl;
    return false;
}
int main(void) {
    cout << "Hello, World" << endl;
}
