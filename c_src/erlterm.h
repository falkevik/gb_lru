#include "erl_nif.h"

class ErlTerm {
    public:
    ERL_NIF_TERM t;
    
    static void *operator new(size_t size) {
	return enif_alloc(size);
    }
   
    static void operator delete(void *block) {
	enif_free(block);
    }
     
    bool operator< (const ErlTerm &term) {
	if (enif_compare(t, term.t) < 0)
	    return true;
	return false;
    }

    bool operator< (ErlTerm &term) {
	if (enif_compare(t, term.t) < 0)
	    return true;
	return false;
    }

    bool operator> (const ErlTerm &term) {
	if (enif_compare(t, term.t) > 0)
	    return true;
	return false;
    }
    
    bool operator> (ErlTerm &term) {
	if (enif_compare(t, term.t) > 0)
	    return true;
	return false;
    }
    
    bool operator== (const ErlTerm &term) {
	if (enif_compare(t, term.t) == 0)
	    return true;
	return false;
    }
    
    bool operator== (ErlTerm &term) {
	if (enif_compare(t, term.t) == 0)
	    return true;
	return false;
    }
};

inline bool operator < (const ErlTerm &a, const ErlTerm &b) {
    if (enif_compare(a.t, b.t) < 0)
	return true;
    return false;
}


#if 0
// extend std::hash to understand ErlTerm used by hashmap not btree
namespace std {
    template <>
    struct hash<ErlTerm> 
    {
	size_t operator()(const ErlTerm& term) const 
	{ 
	    return (size_t) enif_hash_term(term.t);
	}
    };
}
#endif
